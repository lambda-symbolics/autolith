/* Trusted Linux lifecycle and inherited syscall policy for a whole agent.
 * Build: cc -O2 -Wall -Wextra -Werror -o autolith-access-linux this-file.c
 * The parent supplies a private delegated cgroup v2 subtree. Never mount that
 * subtree in the guest. The executable and its parents must be guest read-only.
 */
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <linux/audit.h>
#include <linux/filter.h>
#include <linux/magic.h>
#include <linux/seccomp.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/prctl.h>
#include <sys/stat.h>
#include <sys/statfs.h>
#include <sys/syscall.h>
#include <time.h>
#include <unistd.h>

static void fail(const char *what) {
  fprintf(stderr, "autolith-access-linux: %s: %s\n", what, strerror(errno));
  exit(125);
}
static void require(int condition, const char *what) {
  if (!condition) { if (!errno) errno = EINVAL; fail(what); }
}
static int directory(const char *path) {
  int fd = open(path, O_RDONLY | O_DIRECTORY | O_CLOEXEC | O_NOFOLLOW);
  if (fd < 0) fail("open cgroup directory");
  struct statfs fs;
  require(fstatfs(fd, &fs) == 0, "inspect cgroup filesystem");
  require(fs.f_type == CGROUP2_SUPER_MAGIC, "require cgroup v2");
  return fd;
}
static void write_control(int dir, const char *name, const char *text) {
  int fd = openat(dir, name, O_WRONLY | O_CLOEXEC | O_NOFOLLOW);
  if (fd < 0) fail(name);
  size_t length = strlen(text);
  require(write(fd, text, length) == (ssize_t)length, name);
  require(close(fd) == 0, "close cgroup control");
}
static int populated(int dir) {
  int fd = openat(dir, "cgroup.events", O_RDONLY | O_CLOEXEC | O_NOFOLLOW);
  if (fd < 0) fail("open cgroup events");
  char buffer[4096];
  ssize_t count = read(fd, buffer, sizeof(buffer) - 1);
  require(count > 0 && count < (ssize_t)sizeof(buffer) - 1, "read cgroup events");
  buffer[count] = 0;
  require(close(fd) == 0, "close cgroup events");
  if (strstr(buffer, "populated 0\n")) return 0;
  require(strstr(buffer, "populated 1\n") != NULL, "parse cgroup events");
  return 1;
}
static void token(const char *text) {
  require(strlen(text) > 3 && strlen(text) < 96 && !strncmp(text, "al-", 3), "cgroup token");
  for (const char *p = text + 3; *p; ++p)
    require((*p >= '0' && *p <= '9') || (*p >= 'a' && *p <= 'f') || *p == '-', "cgroup token");
}
static void filter(void) {
#if !defined(__x86_64__)
#error This helper supports Linux x86-64 only.
#endif
#define STMT(code, value) BPF_STMT(code, value)
#define DENY(number) BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, number, 0, 1), STMT(BPF_RET | BPF_K, SECCOMP_RET_ERRNO | EPERM)
  struct sock_filter rules[] = {
    STMT(BPF_LD | BPF_W | BPF_ABS, offsetof(struct seccomp_data, arch)),
    BPF_JUMP(BPF_JMP | BPF_JEQ | BPF_K, AUDIT_ARCH_X86_64, 1, 0),
    STMT(BPF_RET | BPF_K, SECCOMP_RET_KILL_PROCESS),
    STMT(BPF_LD | BPF_W | BPF_ABS, offsetof(struct seccomp_data, nr)),
    BPF_JUMP(BPF_JMP | BPF_JSET | BPF_K, 0x40000000, 0, 1),
    STMT(BPF_RET | BPF_K, SECCOMP_RET_ERRNO | EPERM),
    DENY(__NR_link), DENY(__NR_linkat), DENY(__NR_open_by_handle_at),
    DENY(__NR_name_to_handle_at), DENY(__NR_ptrace), DENY(__NR_process_vm_readv),
    DENY(__NR_process_vm_writev), DENY(__NR_pidfd_getfd), DENY(__NR_mount),
    DENY(__NR_umount2), DENY(__NR_pivot_root), DENY(__NR_setns),
    DENY(__NR_unshare), DENY(__NR_bpf), DENY(__NR_perf_event_open),
    DENY(__NR_userfaultfd), DENY(__NR_io_uring_setup),
    /* Reject all ioctl operations. This includes FICLONE, FICLONERANGE and
     * FIDEDUPERANGE, independent of architecture-specific request sizes. */
    DENY(__NR_ioctl),
    STMT(BPF_RET | BPF_K, SECCOMP_RET_ALLOW)
  };
  struct sock_fprog program = {(unsigned short)(sizeof(rules) / sizeof(rules[0])), rules};
  require(prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0) == 0, "no new privileges");
  require(prctl(PR_SET_SECCOMP, SECCOMP_MODE_FILTER, &program) == 0, "inherited syscall filter");
#undef DENY
#undef STMT
}
int main(int argc, char **argv) {
  if (argc == 2 && !strcmp(argv[1], "--version")) {
    puts("autolith-access-linux-v1"); return 0;
  }
  if (argc >= 3 && !strcmp(argv[1], "--inner")) {
    filter();
    execv(argv[2], argv + 2); fail("exec isolated-network helper");
  }
  require(argc >= 4, "arguments");
  token(argv[3]);
  int root = directory(argv[2]);
  if (!strcmp(argv[1], "--prepare")) {
    require(argc == 4, "prepare arguments");
    require(mkdirat(root, argv[3], 0700) == 0, "create invocation cgroup");
    int child = openat(root, argv[3], O_RDONLY | O_DIRECTORY | O_CLOEXEC | O_NOFOLLOW);
    if (child < 0) fail("open invocation cgroup");
    /* Check required kernel support before any agent starts. */
    int kill = openat(child, "cgroup.kill", O_WRONLY | O_CLOEXEC | O_NOFOLLOW);
    if (kill < 0) {
      int saved = errno;
      close(child);
      unlinkat(root, argv[3], AT_REMOVEDIR);
      errno = saved; fail("require cgroup.kill");
    }
    close(kill); close(child); close(root); return 0;
  }
  int child = openat(root, argv[3], O_RDONLY | O_DIRECTORY | O_CLOEXEC | O_NOFOLLOW);
  if (child < 0) fail("open invocation cgroup");
  if (!strcmp(argv[1], "--run")) {
    require(argc >= 5, "run arguments");
    char pid[64]; snprintf(pid, sizeof(pid), "%ld", (long)getpid());
    write_control(child, "cgroup.procs", pid);
    close(child); close(root);
    execv(argv[4], argv + 4); fail("exec bubblewrap");
  }
  if (!strcmp(argv[1], "--cleanup")) {
    require(argc == 5, "cleanup arguments");
    char *end = NULL;
    long milliseconds = strtol(argv[4], &end, 10);
    require(end && !*end && milliseconds > 0 && milliseconds <= 3600000, "cleanup timeout");
    write_control(child, "cgroup.kill", "1");
    struct timespec delay = {0, 10000000};
    while (populated(child)) {
      require(milliseconds > 0, "cgroup cleanup timeout");
      nanosleep(&delay, NULL); milliseconds -= 10;
    }
    close(child);
    require(unlinkat(root, argv[3], AT_REMOVEDIR) == 0, "remove empty cgroup");
    close(root);
    puts("autolith-cgroup-empty-v1"); return 0;
  }
  errno = EINVAL; fail("unknown operation");
}
