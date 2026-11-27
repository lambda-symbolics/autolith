(in-package #:autolith)

;;;; -- Mandatory Folder Policy Tests --

(-> path-blacklist-tests--denied-p (function) boolean)
(defun path-blacklist-tests--denied-p (function)
  "Return whether FUNCTION signals a structured mandatory path refusal."
  (handler-case
      (progn (funcall function) nil)
    (tool-path-blacklisted (condition)
      (and (eq (tool-failure-code condition) ':path-blacklisted)
           (pathnamep (tool-path-blacklisted-pathname condition))
           (pathnamep (tool-path-blacklisted-root condition))))))

(-> test-tool-path-blacklist-resources () null)
(defun test-tool-path-blacklist-resources ()
  "Refuse reads and retained-revision edits before authorization at every permission level."
  (with-test-configuration (base root)
    (let* ((workspace           (merge-pathnames "workspace/" root))
           (denied              (merge-pathnames "private/" workspace))
           (outside             (merge-pathnames "outside/" root))
           (configuration       (configuration-copy base :working-directory workspace))
           (registry            (make-default-tool-registry :configuration configuration))
           (authorization-count 0))
      (test-write-text (merge-pathnames "secret.txt" denied) "private content")
      (test-write-text (merge-pathnames "secret.txt" outside) "outside content")
      (test-write-text (merge-pathnames "private-neighbor/visible.txt" workspace) "visible content")
      (unwind-protect
           (progn
            (dolist (permission '(:deny :sandboxed :full-access))
              (let* ((context
                       (make-instance 'tool-context :configuration configuration :registry registry
                                      :worker nil :conversation (conversation-create configuration)
                                      :command-authorization-function
                                      (lambda (command directory)
                                        (declare (ignore command directory))
                                        (incf authorization-count)
                                        permission)))
                     (read                  (test-resource-call registry context :namespace "resource" :name "read"
                                                                :arguments '("uri" "workspace:private/secret.txt")))
                     (revision              (test-resource-field (tool-result-content read) "Revision: "))
                     (*tool-path-blacklist* (list denied outside)))
                (setf authorization-count 0)
                (dolist (uri (list "workspace:private" "workspace:private/"
                                  "workspace:private/secret.txt" "workspace:private/new.txt"
                                  "workspace:private/child/missing.txt"
                                  "workspace:private-neighbor/../private/secret.txt"
                                  (uiop:native-namestring (merge-pathnames "secret.txt" outside))))
                  (let ((result (test-resource-call registry context :namespace "resource" :name "read"
                                                    :arguments (list "uri" uri))))
                    (test-assert (and (not (tool-result-success-p result)) (tool-result-denied-p result))
                                 (format nil "mandatory refusal for ~A with ~A" uri permission))))
                (let ((edited
                        (test-resource-call registry context :namespace "resource" :name "edit"
                                            :arguments
                                            (list "uri" "workspace:private/secret.txt" "base-revision" revision
                                                  "operations" (vector (json-object "op" "replace-lines"
                                                                                   "start-line" 1 "end-line" 1
                                                                                  "content" "changed"))))))
                  (test-assert (tool-result-denied-p edited) "deny edits using an earlier allowed revision"))
                (test-assert (zerop authorization-count) "blacklisting precedes permission prompts and full access")
                (test-assert
                 (tool-result-success-p
                  (test-resource-call registry context :namespace "resource" :name "read"
                                      :arguments '("uri" "workspace:private-neighbor/visible.txt")))
                 "directory prefixes do not deny similarly named siblings")))
            (let* ((configuration         (configuration-copy configuration :working-directory denied))
                   (context               (make-instance 'tool-context :configuration configuration :registry registry
                                                         :worker nil :conversation (conversation-create configuration)
                                                         :command-authorization-function
                                                         (lambda (command directory)
                                                           (declare (ignore command directory))
                                                           ':full-access)))
                   (*tool-path-blacklist* (list denied))
                   (result                (test-resource-call
                                           registry context :namespace "resource" :name "read"
                                           :arguments (list "uri" (uiop:native-namestring
                                                                   (merge-pathnames "secret.txt" outside))))))
              (test-assert (and (tool-result-success-p result)
                                (search "outside content" (tool-result-content result)))
                           "a denied cwd does not forbid an approved absolute resource elsewhere")))
        (tool-registry-close-runtime-state registry))
      (test-assert (string= "private content" (uiop:read-file-string (merge-pathnames "secret.txt" denied)))
                   "refused edits preserve the original bytes")))
  nil)

(-> test-tool-path-blacklist-native-paths () null)
(defun test-tool-path-blacklist-native-paths ()
  "Apply the common policy to explicit image, Lisp, and command-directory arguments."
  (with-test-configuration (base root)
    (let* ((configuration         (configuration-copy base :working-directory root))
           (registry              (make-default-tool-registry :configuration configuration))
           (denied                (merge-pathnames "private/" root))
           (*tool-path-blacklist* (list denied))
           (context               (make-instance 'tool-context :configuration configuration :registry registry
                                                 :worker nil :conversation (conversation-create configuration)
                                                 :command-authorization-function
                                                 (lambda (command directory)
                                                   (declare (ignore command directory))
                                                   (error "A blacklisted path reached command authorization.")))))
      (ensure-directories-exist denied)
      (push (merge-pathnames "private/" (lisp-scratchpad-root context)) *tool-path-blacklist*)
      (unwind-protect
           (progn
            (dolist (request '(("fs" "view-image" "path" "private/image.png")
                               ("lisp" "paren-check" "path" "private/code.lisp")
                               ("lisp" "paren-check" "path" ".")
                               ("lisp" "load-system" "asd" "private/system.asd" "system" "private")
                               ("shell" "run" "directory" "private/" "command" "unused")
                               ("resource" "read" "uri" "scratchpad:private/code.lisp")))
              (let ((result (test-resource-call registry context :namespace (first request)
                                               :name (second request) :arguments (rest (rest request)))))
                (test-assert (tool-result-denied-p result)
                             (format nil "deny ~A.~A before execution" (first request) (second request)))))
            (let ((*tool-path-blacklist* (list (lisp-scratchpad-root context))))
              (test-assert
               (tool-result-denied-p
                (test-resource-call registry context :namespace "resource" :name "read"
                                    :arguments '("uri" "scratchpad:.")))
               "deny a blacklisted scratchpad root as well as descendants")))
        (tool-registry-close-runtime-state registry))))
  nil)

(-> test-tool-path-blacklist-aliases () null)
(defun test-tool-path-blacklist-aliases ()
  "Check aliases in both directions, missing tails, link retargeting, and resolution failures."
  (with-test-fixture (':symbolic-links "mandatory folder policy through filesystem aliases")
    (with-test-configuration (configuration root)
      (declare (ignore configuration))
      (let* ((denied  (merge-pathnames "private/" root))
             (allowed (merge-pathnames "allowed/" root))
             (alias   (merge-pathnames "alias" root))
             (escape  (merge-pathnames "escape" denied)))
        (test-write-text (merge-pathnames "secret.txt" denied) "private")
        (test-write-text (merge-pathnames "visible.txt" allowed) "visible")
        (test-fixture-make-symbolic-link *platform* (namestring denied) (namestring alias))
        (test-fixture-make-symbolic-link *platform* (namestring allowed) (namestring escape))
        (unwind-protect
             (progn
               (let ((*tool-path-blacklist* (list denied)))
                 (dolist (path '("alias/secret.txt" "alias/new/child.txt" "private/escape/visible.txt"))
                   (test-assert
                    (path-blacklist-tests--denied-p
                     (lambda () (tool-path-blacklist-check (merge-pathnames path root))))
                    (format nil "deny alias path ~A" path))))
               (let ((*tool-path-blacklist* (list alias)))
                 (test-assert
                  (path-blacklist-tests--denied-p
                   (lambda () (tool-path-blacklist-check (merge-pathnames "secret.txt" denied))))
                  "a configured alias also denies its canonical directory")
                 (test-fixture-remove-link *platform* (namestring alias))
                 (test-fixture-make-symbolic-link *platform* (namestring allowed) (namestring alias))
                 (test-assert
                  (path-blacklist-tests--denied-p
                   (lambda () (tool-path-blacklist-check (merge-pathnames "visible.txt" allowed))))
                  "retargeting a configured link changes its denied target immediately")
                 (test-assert (pathnamep (tool-path-blacklist-check (merge-pathnames "secret.txt" denied)))
                              "the former link target can be accessed after retargeting")
                 (test-fixture-remove-link *platform* (namestring alias))
                 (test-fixture-make-symbolic-link *platform* (namestring alias) (namestring alias))
                 (test-assert
                  (handler-case
                      (progn (tool-path-blacklist-check (merge-pathnames "visible.txt" allowed)) nil)
                    (tool-error () t)
                    (file-operation-failed () t)
                    (platform-error () t))
                  "an unresolvable configured root fails closed")))
          (test-fixture-remove-link *platform* (namestring alias))
          (test-fixture-remove-link *platform* (namestring escape))))))
  nil)

(-> test-tool-path-blacklist-retained-change () null)
(defun test-tool-path-blacklist-retained-change ()
  "Revalidate direct observations and prepared commits after the policy changes."
  (with-test-configuration (base root)
    (let* ((configuration (configuration-copy base :working-directory root))
           (registry      (make-default-tool-registry :configuration configuration))
           (context       (workspace-change-tests--context configuration registry))
           (path          (test-write-text (merge-pathnames "private/note.txt" root) "before")))
      (unwind-protect
           (let* ((resource    (resource-registry-resolve (tool-registry-resource-registry registry)
                                                         "workspace:private/note.txt" context))
                  (observation (resource-observe resource context))
                  (state       (resource-observation-state-ensure
                                (tool-context-conversation context) observation :visible-ranges nil))
                  (change      (cl-resources:resource-stage-operations
                                resource context :base-revision (resource-observation-state-alias state)
                                :operations                     (list (make-instance 'workspace-content-operation :content "after")))))
             (unwind-protect
                  (let ((*tool-path-blacklist* (list (merge-pathnames "private/" root))))
                    (test-assert (path-blacklist-tests--denied-p (lambda () (resource-observe resource context)))
                                 "deny direct observers of retained resource objects")
                    (test-assert
                     (path-blacklist-tests--denied-p (lambda () (cl-resources:prepared-change-commit change context)))
                     "deny a previously staged commit")
                    (test-assert (and (null (workspace-prepared-change-backup change))
                                      (string= "before" (uiop:read-file-string path)))
                                 "refuse publication before creating undo files or changing content"))
               (cl-resources:prepared-change-discard change context)))
        (tool-registry-close-runtime-state registry))))
  nil)

(-> test-tool-path-blacklist-configuration () null)
(defun test-tool-path-blacklist-configuration ()
  "Require absolute directory policy entries and expand home-relative configuration."
  (dolist (value '("/private" (42) ("") ("relative/private") ("/private" . "tail")))
    (let ((*tool-path-blacklist* value))
      (test-assert (handler-case (progn (tool-path-blacklist--roots) nil)
                     (configuration-error () t))
                   "invalid folder policy is rejected rather than ignored")))
  (let ((*tool-path-blacklist* '("~/.autolith-private")))
    (test-assert
     (path-blacklist-tests--denied-p
      (lambda () (tool-path-blacklist-check (merge-pathnames ".autolith-private/missing.txt"
                                                                           (user-homedir-pathname)))))
     "home-relative configuration protects missing descendants"))
  nil)

(-> test-tool-path-blacklist-inherited-shell-directory () null)
(defun test-tool-path-blacklist-inherited-shell-directory ()
  "Recheck a named shell's retained cwd even when the next call omits directory."
  (with-test-fixture (':posix-shell "blacklisted retained shell working directories")
    (with-test-configuration (base root)
      (let* ((configuration (configuration-copy base :working-directory root))
             (registry      (make-default-tool-registry :configuration configuration))
             (denied        (merge-pathnames "private/" root))
             (context       (make-instance 'tool-context :configuration configuration :registry registry
                                           :worker nil :conversation (conversation-create configuration)
                                           :command-authorization-function
                                           (lambda (command directory)
                                             (declare (ignore command directory))
                                             ':full-access))))
        (ensure-directories-exist denied)
        (unwind-protect
             (progn
               (test-assert
                (tool-result-success-p
                 (test-resource-call registry context :namespace "shell" :name "run"
                                     :arguments '("session" "blacklist-cwd" "directory" "private/" "command" ":")))
                "start a full-access shell in a currently allowed folder")
               (let ((*tool-path-blacklist* (list denied)))
                 (test-assert
                  (tool-result-denied-p
                   (test-resource-call registry context :namespace "shell" :name "run"
                                       :arguments '("session" "blacklist-cwd" "command" ":")))
                  "deny the retained cwd after its folder is blacklisted")))
          (test-resource-call registry context :namespace "shell" :name "close"
                              :arguments '("session" "blacklist-cwd"))
          (tool-registry-close-runtime-state registry)))))
  nil)
