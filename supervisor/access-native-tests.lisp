;;;; -- Native Access Enforcement Checks --

(in-package #:autolith-supervisor)

(defparameter *native-access-test-source*
  "#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <spawn.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/clonefile.h>
#include <string.h>
extern char **environ;
struct report { int good; int pid; int group; };
static int readable(const char *p) { int f=open(p,O_RDONLY); if(f<0)return 0; close(f);return 1; }
static int writable(const char *p) { int f=open(p,O_WRONLY|O_APPEND); if(f<0)return 0; close(f);return 1; }
static int executes(const char *p) {
  pid_t c=fork(); if(c<0)return -1;
  if(c==0){execl(p,p,(char*)0);_exit(errno==EPERM?99:98);}
  int s=0;if(waitpid(c,&s,0)<0)return -1;
  return WIFEXITED(s)?WEXITSTATUS(s):-1;
}
static struct report worker(const char *secret) {
  struct report r={0,(int)getpid(),(int)getpgrp()};
  int a=setsid();int e=errno;int b=setpgid(0,0);int f=errno;
  posix_spawnattr_t attr;posix_spawnattr_init(&attr);
  posix_spawnattr_setflags(&attr,POSIX_SPAWN_SETPGROUP);posix_spawnattr_setpgroup(&attr,0);
  pid_t c=0;char *args[]={\"/usr/bin/true\",0};
  int s=posix_spawn(&c,args[0],0,&attr,args,environ);posix_spawnattr_destroy(&attr);
  if(s==0){int status;waitpid(c,&status,0);}
  char alias[64];snprintf(alias,sizeof(alias),\"hardlink-%d\",r.pid);
  int linked=link(secret,alias);if(linked==0)unlink(alias);
  r.good=!readable(secret)&&a==-1&&e==EPERM&&b==-1&&f==EPERM&&s==EPERM&&linked==-1;
  return r;
}
int main(int argc,char **argv) {
  if(argc!=11)return 97;
  unsigned int mask=0;
  if(readable(argv[1]))mask|=1u<<0;
  if(!readable(argv[2]))mask|=1u<<1;
  if(!writable(argv[3]))mask|=1u<<2;
  if(writable(argv[1]))mask|=1u<<3;
  if(!readable(argv[4]))mask|=1u<<4;
  if(readable(argv[5]))mask|=1u<<5;
  if(!writable(argv[5]))mask|=1u<<6;
  if(writable(argv[6]))mask|=1u<<7;
  if(!readable(argv[6]))mask|=1u<<8;
  if(!readable(argv[7]))mask|=1u<<9;
  if(executes(argv[7])==0)mask|=1u<<10;
  if(executes(argv[8])==99)mask|=1u<<11;
  int pipefd[2];if(pipe(pipefd)<0)return 96;
  pid_t c=fork();if(c<0)return 95;
  if(c==0){
    close(pipefd[0]);pid_t leaf=fork();if(leaf<0)_exit(94);
    struct report r=worker(argv[2]);write(pipefd[1],&r,sizeof(r));
    for(;;)pause();
  }
  close(pipefd[1]);struct report r[2];
  for(int i=0;i<2;i++){size_t n=0;while(n<sizeof(r[i])){ssize_t x=read(pipefd[0],((char*)&r[i])+n,sizeof(r[i])-n);if(x<=0)return 93;n+=(size_t)x;}}
  close(pipefd[0]);
  if(r[0].good)mask|=1u<<12;
  if(r[1].good)mask|=1u<<13;
  if(r[0].group==(int)getpgrp()&&r[1].group==(int)getpgrp())mask|=1u<<14;
  if(!readable(argv[10]))mask|=1u<<15;
  dprintf(0,\"%u %d %d\\n\",mask,r[0].pid,r[1].pid);
  unsigned char instruction;
  while(read(0,&instruction,1)==1){
    unsigned char result=(!readable(argv[5])?1:0)|(!readable(argv[9])?2:0);
    if(instruction==2){
      result=4;
      for(int i=0;i<64;i++){
        char alias[64];snprintf(alias,sizeof(alias),\"race-%d-%d\",(int)getpid(),i);
        if(link(argv[2],alias)==0){unlink(alias);result=0;}
        if(linkat(AT_FDCWD,argv[2],AT_FDCWD,alias,0)==0){unlink(alias);result=0;}
        if(clonefile(argv[2],alias,0)==0){unlink(alias);result=0;}
      }
    }
    write(0,&result,1);
  }
  for(;;)pause();
}
"
  "Native fixture for actual Seatbelt checks. It forks two contained workers.")

(declaim (ftype (function (string) list) access-native--test-words))

(defun access-native--test-words (line)
  "Read only the fixture's bounded integer result, without reader evaluation."
  (let ((*read-eval* nil))
    (let ((result (read-from-string (concatenate 'string "(" line ")"))))
      (assert (and (= (length result) 3) (every #'integerp result)))
      result)))

(declaim (ftype (function (stream) string) access-native--test-line))

(defun access-native--test-line (stream)
  "Read one bounded ASCII result through the private inherited connection."
  (with-output-to-string (output)
    (loop repeat 128 for byte = (read-byte stream)
          when (= byte 10) do (return-from access-native--test-line (get-output-stream-string output))
          do (assert (<= 32 byte 126)) (write-char (code-char byte) output)
          finally (error "Native fixture did not finish its result."))))

(declaim (ftype (function (function keyword) boolean) access-native--test-rejected-p))

(defun access-native--test-rejected-p (thunk code)
  "Require a typed refusal with the expected fixed code."
  (handler-case (progn (funcall thunk) nil)
    (access-policy-error (condition) (eq code (access-policy-error-code condition)))))

(declaim (ftype (function (pathname string) null) access-native--test-write))

(defun access-native--test-write (path contents)
  "Write a non-secret file owned by this test."
  (with-open-file (output path :direction ':output :if-exists ':supersede)
    (write-string contents output))
  nil)

(declaim (ftype (function () integer) run-native-access-tests))

(defun run-native-access-tests ()
  "Check real host enforcement, inherited restrictions and replacement cleanup.
Darwin executes a compiled native fixture under the actual OS profile. Other
hosts use RUN-NATIVE-HOST-LIVE-ACCESS-TESTS for host enforcement checks."
  (assert (access-native--test-rejected-p
           (lambda () (access-native--host-class ':unsupported)) ':unsupported-native-host))
  (unless (uiop:os-macosx-p)
    (return-from run-native-access-tests 1))
  (let* ((root             (merge-pathnames (format nil "autolith-access-native-~A-~A/"
                                                   (get-universal-time) (random 1000000000))
                                           (uiop:temporary-directory)))
         (workspace        (merge-pathnames "workspace/" root))
         (temporary        (merge-pathnames "temporary/" root))
         (private          (merge-pathnames "private/" root))
         (outside          (merge-pathnames "outside/" root))
         (executable       (merge-pathnames "fixture" workspace))
         (source           (merge-pathnames "fixture.c" root))
         (allowed          (merge-pathnames "allowed" workspace))
         (secret           (merge-pathnames "sentinel" private))
         (blocked-write    (merge-pathnames "blocked-write" outside))
         (link             (merge-pathnames "secret-link" workspace))
         (read-only        (merge-pathnames "read-only" outside))
         (write-only       (merge-pathnames "write-only" outside))
         (execute-only     (merge-pathnames "execute-only" outside))
         (read-binary      (merge-pathnames "read-binary" outside))
         (replacement      (merge-pathnames "replace/" workspace))
         (replacement-file (merge-pathnames "sentinel" replacement))
         (outside-read     (merge-pathnames "outside-read" outside))
         (backend nil) (context nil) (application nil) (report nil) (checks 1))
    (unwind-protect
         (progn
           (dolist (directory (list workspace temporary private outside replacement))
             (ensure-directories-exist directory))
           (dolist (path (list allowed secret blocked-write read-only write-only replacement-file outside-read))
             (access-native--test-write path "test-sentinel"))
           (uiop:copy-file #P"/usr/bin/true" execute-only)
           (uiop:copy-file #P"/usr/bin/true" read-binary)
           (uiop:run-program (list "/bin/chmod" "755" (namestring execute-only) (namestring read-binary)))
           (uiop:run-program (list "/bin/ln" "-s" (namestring secret) (namestring link)))
           (access-native--test-write source *native-access-test-source*)
           (uiop:run-program (list "/usr/bin/cc" "-Wall" "-Wextra" "-Werror"
                                   (namestring source) "-o" (namestring executable)))
           (setf backend
                 (make-native-access-backend
                  :command        (mapcar #'namestring
                                          (list executable allowed secret blocked-write link read-only write-only
                                                execute-only read-binary replacement-file outside-read))
                  :environment    '("PATH=/usr/bin:/bin")
                  :runtime-roots  '("/usr/lib/" "/System/Library/")
                  :temporary-root (namestring temporary)
                  :startup        (lambda (started)
                                    (setf application started)
                                    (handler-case
                                        (setf report (access-native--test-words
                                                      (access-native--test-line
                                                       (private-process-connection
                                                        (native-access-application-process started)))))
                                      (end-of-file (condition)
                                        (sleep 0.05)
                                        (format *error-output* "Native child diagnostics: ~S~%"
                                                (private-process-diagnostics
                                                 (native-access-application-process started)))
                                        (error condition))))))
           (setf context (make-instance 'access-context :pathname (merge-pathnames "policy" private)
                                       :session-id "native-test-session" :workspace (namestring workspace)
                                       :private-roots             (list (namestring private))
                                       :private-authorities nil :services '("known-service")
                                       :backend backend :terminal nil))
           ;; Prove that a host-created alias contains the private data, then
           ;; reject that inode in each class of admitted scope before launch.
           (dolist (kind '(:workspace :temporary :explicit :runtime))
             (let* ((alias    (merge-pathnames "preexisting-alias"
                                               (case kind (:workspace
                                                           workspace)
                                                          (:temporary
                                                           temporary) (t
                                                                       outside))))
                    (selected (if (eq kind ':runtime)
                                  (make-native-access-backend
                                   :command       (access-native--command backend)
                                   :environment   (access-native--environment backend)
                                   :runtime-roots (cons (namestring outside)
                                                        (access-native--runtime-roots backend))
                                   :temporary-root (namestring temporary) :startup #'identity)
                                  backend)))
               (uiop:run-program (list "/bin/ln" (namestring secret) (namestring alias)))
               (unwind-protect
                    (progn
                      (with-open-file (input alias)
                        (assert (string= (read-line input) "test-sentinel")))
                      (assert (access-native--test-rejected-p
                               (lambda ()
                                 (access-backend-bind
                                  selected context
                                  (list :resource ':filesystem
                                        :target   (namestring (if (eq kind ':explicit) alias read-only))
                                        :operations '("read") :duration ':instance)))
                               ':private-inode-alias))
                      (incf checks 2))
                 (delete-file alias))))
           (dolist (resource '(:network :service))
             (assert (access-native--test-rejected-p
                      (lambda () (access-backend-bind
                                  backend context
                                  (list :resource resource
                                        :target   (if (eq resource ':network) "example.org:443" "known-service")
                                        :operations '("connect") :duration ':instance)))
                      ':unsupported-native-authority))
             (incf checks))
           (dolist (target (list (namestring private) (namestring root) (namestring link)))
             (assert (access-native--test-rejected-p
                      (lambda () (access-backend-bind
                                  backend context (list :resource ':filesystem :target target
                                                        :operations '("read") :duration ':session)))
                      ':private-authority))
             (incf checks))
           (let ((grants (loop for path in (list read-only write-only execute-only read-binary)
                               for operation in '("read" "write" "execute" "read")
                               collect (access-backend-bind
                                        backend context
                                        (list :resource ':filesystem :target (namestring path)
                                              :operations (list operation) :duration ':instance)))))
             (setf application (access-backend-start backend context "native-instance-1"
                                                     :revision 7 :grants grants))
             (assert (equal grants (access-application-grants application)))
             (assert (eql 7 (access-application-revision application)))
             (assert (= (first report) 65535))
             (incf checks 16)
             (let ((connection (private-process-connection (native-access-application-process application))))
               (write-byte 2 connection) (finish-output connection)
               (assert (= (read-byte connection) 4)))
             (incf checks)
             ;; Replace both a granted file and a writable workspace directory
             ;; with links to the forbidden root while the same policy runs.
             (delete-file read-only)
             (uiop:run-program (list "/bin/ln" "-s" (namestring secret) (namestring read-only)))
             (rename-file replacement (merge-pathnames "replace-old/" workspace))
             (uiop:run-program (list "/bin/ln" "-s" (namestring private)
                                     (string-right-trim "/" (namestring replacement))))
             (let ((connection (private-process-connection (native-access-application-process application))))
               (write-byte 1 connection) (finish-output connection)
               (assert (= (read-byte connection) 3)))
             (incf checks 2)
             (assert (access-native--test-rejected-p
                      (lambda () (access-backend-start backend context "native-instance-2"
                                                       :revision 8 :grants nil)) ':stop-required))
             (incf checks)
             (assert (eq t (access-backend-stop backend context "native-instance-1")))
             (let ((evidence (native-access-application-evidence application)))
               (assert (equal (getf evidence :instance-id) "native-instance-1"))
               (assert (= (getf evidence :revision) 7))
               (assert (getf (getf evidence :process-scope) :native-scope-clean-p)))
             (incf checks 3)
             (assert (eq t (access-backend-stop backend context "native-instance-1")))
             (assert (access-native--test-rejected-p
                      (lambda () (access-backend-start backend context "native-instance-1"
                                                       :revision 8 :grants nil)) ':instance-reuse))
             (assert (access-native--test-rejected-p
                      (lambda () (access-backend-stop backend context "unknown-instance"))
                      ':unknown-native-instance))
             (incf checks 3)
             (setf application (access-backend-start backend context "native-instance-2"
                                                     :revision 8 :grants nil))
             (assert (= (first report) (- 65535 32 128 1024)))
             (assert (null (access-application-grants application)))
             (assert (eq t (access-backend-stop backend context "native-instance-2")))
             (assert (equal (getf (native-access-application-evidence application) :instance-id)
                            "native-instance-2"))
             (incf checks 4)
             (let* ((failed-application nil)
                    (failed-backend
                      (make-native-access-backend
                       :command        (copy-tree (access-native--command backend))
                       :environment    (copy-tree (access-native--environment backend))
                       :runtime-roots  (copy-tree (access-native--runtime-roots backend))
                       :temporary-root (access-native--temporary-root backend)
                       :startup        (lambda (started)
                                         (setf failed-application started)
                                         (access-native--test-line
                                          (private-process-connection (native-access-application-process started)))
                                         (access-policy--reject ':test-startup-refusal)))))
               (assert (access-native--test-rejected-p
                        (lambda () (access-backend-start failed-backend context "failed-startup"
                                                         :revision 9 :grants nil)) ':test-startup-refusal))
               (assert (getf (getf (native-access-application-evidence failed-application) :process-scope)
                             :native-scope-clean-p))
               (assert (eq t (access-backend-stop failed-backend context "failed-startup")))
               (incf checks 3)))
           checks)
      (when (and application backend context)
       (access-backend-stop backend context (access-application-instance-id application)))
      (when (probe-file root)
        (uiop:run-program (list "/bin/rm" "-rf" (namestring root)))))))
