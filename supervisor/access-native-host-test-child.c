/* Native whole-agent enforcement fixture. Build with a native C compiler.
 * Windows: add -municode -lws2_32 with a MinGW-compatible compiler. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <winsock2.h>
#include <windows.h>
#include <wchar.h>
typedef wchar_t path_char;
#define TEXT_(s) L##s
#define SAME(a,b) (!wcscmp(a,b))
static int readable(const path_char *path) {
  HANDLE h = CreateFileW(path, GENERIC_READ, 7, NULL, OPEN_EXISTING, 0, NULL);
  if (h == INVALID_HANDLE_VALUE) return 0;
  CloseHandle(h); return 1;
}
static int writable(const path_char *path) {
  HANDLE h = CreateFileW(path, FILE_APPEND_DATA, 7, NULL, OPEN_EXISTING, 0, NULL);
  if (h == INVALID_HANDLE_VALUE) return 0;
  CloseHandle(h); return 1;
}
static int alias(const path_char *source, const path_char *target) {
  return CreateHardLinkW(target, source, NULL) != 0;
}
static void remove_alias(const path_char *path) { DeleteFileW(path); }
static int environment_ok(const path_char *expected) {
  wchar_t value[512], unrelated[16];
  DWORD size = GetEnvironmentVariableW(L"AUTOLITH_PARENT_INSTANCE_ID", value, 512);
  return size > 0 && size < 512 && (!wcscmp(expected, L"*") || !wcscmp(value, expected)) &&
    GetEnvironmentVariableW(L"AUTOLITH_PRIVATE_UNRELATED", unrelated, 16) == 0;
}
static void report(const char *kind, unsigned mask) {
  char line[128]; DWORD written;
  int size = snprintf(line, sizeof(line), "%s %u\n", kind, mask);
  if (size <= 0 || !WriteFile(GetStdHandle(STD_INPUT_HANDLE), line, (DWORD)size, &written, NULL) ||
      written != (DWORD)size) ExitProcess(120);
}
static void linger(void) { for (;;) Sleep(1000); }
static int blocked_network(unsigned port) {
  WSADATA data;
  if (WSAStartup(MAKEWORD(2,2), &data)) return 0;
  SOCKET s = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (s == INVALID_SOCKET) return WSAGetLastError() == WSAEACCES;
  struct sockaddr_in target;
  memset(&target, 0, sizeof(target));
  target.sin_family = AF_INET; target.sin_port = htons((u_short)port);
  target.sin_addr.s_addr = htonl(0x7f000001);
  int result = connect(s, (struct sockaddr *)&target, sizeof(target));
  int error = WSAGetLastError(); closesocket(s);
  return result == SOCKET_ERROR && error == WSAEACCES;
}
static int descendant(int argc, path_char **argv) {
  size_t size = 1;
  for (int i = 0; i < argc; ++i) size += 2 * wcslen(argv[i]) + 4;
  wchar_t *command = calloc(size, sizeof(wchar_t)), *out = command;
  if (!command) return 0;
  for (int i = 0; i < argc; ++i) {
    if (i) *out++ = L' ';
    *out++ = L'"';
    const wchar_t *p = i == 1 ? L"leaf" : argv[i];
    while (*p) {
      unsigned slashes = 0;
      while (*p == L'\\') { ++slashes; ++p; }
      unsigned copies = *p == L'"' || !*p ? 2 * slashes : slashes;
      while (copies--) *out++ = L'\\';
      if (*p == L'"') *out++ = L'\\';
      if (*p) *out++ = *p++;
    }
    *out++ = L'"';
  }
  *out = 0;
  STARTUPINFOW startup = {0}; PROCESS_INFORMATION process = {0};
  startup.cb = sizeof(startup); startup.dwFlags = STARTF_USESTDHANDLES;
  startup.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
  startup.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
  startup.hStdError = GetStdHandle(STD_ERROR_HANDLE);
  BOOL success = CreateProcessW(argv[0], command, NULL, NULL, TRUE, CREATE_NO_WINDOW,
                                NULL, NULL, &startup, &process);
  free(command);
  if (!success) return 0;
  CloseHandle(process.hThread); CloseHandle(process.hProcess); return 1;
}
#define ENTRY wmain
#else
#include <errno.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
typedef char path_char;
#define TEXT_(s) s
#define SAME(a,b) (!strcmp(a,b))
static int readable(const path_char *path) {
  int fd = open(path, O_RDONLY); if (fd < 0) return 0; close(fd); return 1;
}
static int writable(const path_char *path) {
  int fd = open(path, O_WRONLY | O_APPEND); if (fd < 0) return 0; close(fd); return 1;
}
static int alias(const path_char *source, const path_char *target) { return link(source, target) == 0; }
static void remove_alias(const path_char *path) { unlink(path); }
static int environment_ok(const path_char *expected) {
  const char *value = getenv("AUTOLITH_PARENT_INSTANCE_ID");
  return value && (!strcmp(expected, "*") || !strcmp(value, expected)) && !getenv("AUTOLITH_PRIVATE_UNRELATED");
}
static void report(const char *kind, unsigned mask) {
  char line[128]; int size = snprintf(line, sizeof(line), "%s %u\n", kind, mask);
  if (size <= 0 || write(0, line, (size_t)size) != size) _exit(120);
}
static void linger(void) { for (;;) pause(); }
static int blocked_network(unsigned port) {
  (void)port;
  int fd = socket(AF_INET, SOCK_STREAM, 0);
  if (fd >= 0) { close(fd); return 0; }
  return errno == EPERM || errno == EACCES;
}
static int descendant(int argc, path_char **argv) {
  (void)argc; (void)argv;
  pid_t child = fork(); if (child < 0) return 0;
  return child == 0 ? 2 : 1;
}
#define ENTRY main
#endif

int ENTRY(int argc, path_char **argv) {
  if (argc == 4 && SAME(argv[1], TEXT_("host-link"))) return alias(argv[2], argv[3]) ? 0 : 1;
  if (argc != 12) return 119;
  int leaf = SAME(argv[1], TEXT_("leaf"));
  unsigned port;
#ifdef _WIN32
  port = (unsigned)wcstoul(argv[11], NULL, 10);
#else
  port = (unsigned)strtoul(argv[11], NULL, 10);
#endif
  if (!leaf) {
    int result = descendant(argc, argv);
    if (!result) return 118;
    if (result == 2) leaf = 1;
  }
  unsigned mask = 0;
  if (readable(argv[2]) && writable(argv[2])) mask |= 1u << 0;
  if (!readable(argv[3]) && !writable(argv[3])) mask |= 1u << 1;
  if (!readable(argv[4]) && !writable(argv[4])) mask |= 1u << 2;
  if (readable(argv[5])) mask |= 1u << 3;
  if (writable(argv[5])) mask |= 1u << 4;
  if (readable(argv[6])) mask |= 1u << 5;
  if (writable(argv[6])) mask |= 1u << 6;
  /* Each process gets its own target, so no alias attempt masks another result. */
  const path_char *target = leaf ? argv[8] : argv[7];
  if (!alias(argv[3], target)) mask |= 1u << 7; else remove_alias(target);
  if (environment_ok(argv[9])) mask |= 1u << 8;
  if (blocked_network(port)) mask |= 1u << 9;
  report(leaf ? "leaf" : "root", mask);
  linger(); return 0;
}
