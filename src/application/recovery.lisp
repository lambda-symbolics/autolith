(in-package #:autolith)

;;;; -- Fatal Control Path --

(define-condition fatal-control-path-error (autolith-error)
  ((cause
    :initarg :cause
    :reader fatal-control-path-error-cause
    :type serious-condition
    :documentation "The unexpected condition that made the active path untrustworthy.")
   (capsule-pathname
    :initarg :capsule-pathname
    :reader fatal-control-path-error-capsule-pathname
    :type pathname
    :documentation "The best-effort crash capsule written before recovery."))
  (:documentation "An unexpected active-agent failure requiring stable recovery."))

(-> application-safe-backtrace () list)
(defun application-safe-backtrace ()
  "Return bounded call names without argument values from the current stack."
  (handler-case
      (mapcar #'first
              (sb-debug:list-backtrace :count 30
                                       :argument-limit 0
                                       :from :current-frame))
    (error ()
      nil)))

(-> application-crash-condition-report (serious-condition) string)
(defun application-crash-condition-report (condition)
  "Return an allowlisted, secret-free report for crash CONDITION."
  (if (typep condition 'autolith-error)
      (autolith-error-message condition)
      (format nil "Unexpected condition of type ~S." (type-of condition))))

(-> application-publish-crash-pointer (application pathname) null)
(defun application-publish-crash-pointer (application capsule-pathname)
  "Publish CAPSULE-PATHNAME to this launcher's private pointer, when configured."
  (let ((pointer-value (uiop:getenv "AUTOLITH_CRASH_POINTER")))
    (when (non-empty-string-p pointer-value)
      (let* ((configuration (application-configuration application))
             (pointer-pathname (pathname pointer-value))
             (temporary-pathname
               (make-pathname :name (format nil ".crash-pointer.~D"
                                            (sb-posix:getpid))
                              :type "tmp"
                              :defaults pointer-pathname)))
        (when (uiop:subpathp pointer-pathname
                             (configuration-state-root configuration))
          (ensure-directories-exist pointer-pathname)
          (with-open-file (stream temporary-pathname
                                  :direction ':output
                                  :if-exists ':supersede
                                  :if-does-not-exist ':create
                                  :external-format ':utf-8)
            (write-line (namestring capsule-pathname) stream)
            (finish-output stream))
          (platform-make-private *platform* temporary-pathname)
          (uiop:rename-file-overwriting-target temporary-pathname
                                               pointer-pathname)))))
  nil)

(-> application-write-crash-capsule
    (application serious-condition &key (:backtrace list))
    pathname)
(defun application-write-crash-capsule
    (application condition &key (backtrace (application-safe-backtrace)))
  "Write a secret-free crash capsule for CONDITION and return its pathname."
  (let* ((configuration (application-configuration application))
         (identifier (make-identifier))
         (pathname (merge-pathnames
                    (make-pathname :name identifier :type "sexp")
                    (merge-pathnames "crashes/"
                                     (configuration-state-root configuration))))
         (commit
           (handler-case
               (string-trim
                '(#\Space #\Tab #\Newline #\Return)
                (self-git-command configuration '("rev-parse" "HEAD")))
             (error ()
               nil)))
         (safe-backtrace
           (mapcar (lambda (frame-name)
                     (if (or (symbolp frame-name) (stringp frame-name))
                         (bounded-string frame-name :limit 300)
                         (bounded-string (type-of frame-name) :limit 300)))
                   (subseq backtrace 0 (min 30 (length backtrace))))))
    (generation--write-form-atomically
     pathname
     (list :crash
           :version 1
           :id identifier
           :time (get-universal-time)
           :condition-type (bounded-string (type-of condition) :limit 300)
           :condition
            (bounded-string (application-crash-condition-report condition)
                            :limit 2000)
           :backtrace safe-backtrace
           :conversation-id
           (let ((conversation (application-conversation application)))
             (and (conversation-persisted-p conversation)
                  (conversation-identifier conversation)))
           :rendered-sequence (application-rendered-sequence application)
           :history-floor-sequence
           (application-history-floor-sequence application)
           :git-commit commit
           :journal-position
           (let ((journal (configuration-journal-path configuration)))
             (if (probe-file journal)
                 (with-open-file (stream journal
                                         :direction ':input
                                         :element-type '(unsigned-byte 8))
                   (file-length stream))
                 0))))
    (platform-make-private *platform* pathname)
    (application-publish-crash-pointer application pathname)
    pathname))

;;;; -- Recovered Diagnosis --

(defparameter *application-recovery-diagnostic-tool-names*
  '("resource.read"
    "search.files" "search.glob" "search.content"
    "lisp.describe" "lisp.source" "self.status" "self.diff"
    "self.generations")
  "Read-only native tools available to the recovered diagnosis turn.")

(defparameter *application-recovery-pointer-byte-limit* 4096
  "Maximum crash-pointer file size accepted during recovered startup.")

(defparameter *application-recovery-capsule-byte-limit* 65536
  "Maximum crash-capsule file size accepted during recovered startup.")

(-> application--bounded-file-p (pathname integer) boolean)
(defun application--bounded-file-p (pathname limit)
  "Return true when PATHNAME names a file no larger than LIMIT bytes."
  (handler-case
      (with-open-file (stream pathname
                              :direction ':input
                              :element-type '(unsigned-byte 8))
        (<= (file-length stream) limit))
    (error ()
      nil)))

(-> application--bounded-string-p (t integer &key (:empty-p boolean)) boolean)
(defun application--bounded-string-p (value limit &key (empty-p nil))
  "Return true when VALUE is a string no longer than LIMIT characters."
  (and (stringp value)
       (<= (length value) limit)
       (or empty-p (plusp (length value)))
       t))

(-> application--crash-capsule-record-p (t) boolean)
(defun application--crash-capsule-record-p (record)
  "Return true when RECORD is one bounded version-one crash capsule."
  (handler-case
      (let* ((properties (and (application-command--proper-list-p record)
                              (rest record)))
             (keys '(:version :id :time :condition-type :condition
                     :backtrace :conversation-id :rendered-sequence
                     :history-floor-sequence :git-commit
                     :journal-position))
             (conversation-id (and properties
                                   (getf properties :conversation-id)))
             (commit (and properties (getf properties :git-commit))))
        (and properties
             (= (length record) 23)
             (eq (first record) :crash)
             (loop for key in keys
                   always
                   (= (loop for candidate in properties by #'cddr
                            count (eq candidate key))
                      1))
             (= (getf properties :version) 1)
             (application--bounded-string-p (getf properties :id) 128)
             (typep (getf properties :time) '(integer 0))
             (application--bounded-string-p
              (getf properties :condition-type) 300)
             (application--bounded-string-p
              (getf properties :condition) 2000 :empty-p t)
             (let ((backtrace (getf properties :backtrace)))
               (and (application-command--proper-list-p backtrace)
                    (<= (length backtrace) 30)
                    (every (lambda (frame)
                             (application--bounded-string-p frame 300
                                                            :empty-p t))
                           backtrace)))
             (or (null conversation-id)
                 (application--bounded-string-p conversation-id 128))
             (typep (getf properties :rendered-sequence) '(integer 0))
             (typep (getf properties :history-floor-sequence)
                    '(option (integer 1)))
             (or (null commit)
                 (and (application--bounded-string-p commit 40)
                      (every (lambda (character)
                               (digit-char-p character 16))
                             commit)))
             (typep (getf properties :journal-position) '(integer 0))))
    (error ()
      nil)))

(-> application--recovery-crash-capsule-pathname
    (configuration)
    (option pathname))
(defun application--recovery-crash-capsule-pathname (configuration)
  "Return this recovered launcher's contained crash capsule, when available."
  (handler-case
      (let ((recovered (uiop:getenv "AUTOLITH_RECOVERED"))
            (pointer-value (uiop:getenv "AUTOLITH_CRASH_POINTER")))
        (when (and (non-empty-string-p recovered)
                   (non-empty-string-p pointer-value))
          (let* ((pointer-pathname (pathname pointer-value))
                 (pointer-root
                   (merge-pathnames "crash-pointers/"
                                    (configuration-state-root configuration)))
                 (crash-root
                   (merge-pathnames "crashes/"
                                    (configuration-state-root configuration))))
            (when (and (uiop:absolute-pathname-p pointer-pathname)
                       (uiop:subpathp pointer-pathname pointer-root)
                       (probe-file pointer-pathname)
                       (application--bounded-file-p
                        pointer-pathname
                        *application-recovery-pointer-byte-limit*))
              (with-open-file (stream pointer-pathname
                                      :direction ':input
                                      :external-format ':utf-8)
                (let ((capsule (read-line stream nil nil))
                      (trailing-line (read-line stream nil nil)))
                  (when (and (stringp capsule)
                             (plusp (length capsule))
                             (<= (length capsule) 4096)
                             (null trailing-line))
                    (let ((capsule-pathname (pathname capsule)))
                      (and (uiop:absolute-pathname-p capsule-pathname)
                           (uiop:subpathp capsule-pathname crash-root)
                           (probe-file capsule-pathname)
                           (application--bounded-file-p
                            capsule-pathname
                            *application-recovery-capsule-byte-limit*)
                           capsule-pathname)))))))))
    (error ()
      nil)))

(-> application--recovery-crash-capsule-record
    (configuration)
    (option list))
(defun application--recovery-crash-capsule-record (configuration)
  "Return this recovered launcher's validated bounded crash capsule."
  (let ((pathname
          (application--recovery-crash-capsule-pathname configuration)))
    (when pathname
      (handler-case
          (multiple-value-bind (record complete-p)
              (snapshot-read pathname)
            (and complete-p
                 (application--crash-capsule-record-p record)
                 record))
        (error ()
          nil)))))

(-> application--recovery-session-state
    (configuration)
    (values (option string) (option integer) (option integer)))
(defun application--recovery-session-state (configuration)
  "Return validated per-launch recovery session state for CONFIGURATION."
  (block nil
    (handler-case
        (let ((pointer-value
                (uiop:getenv "AUTOLITH_RECOVERY_SESSION_POINTER")))
          (unless (non-empty-string-p pointer-value)
            (return (values nil nil nil)))
          (let* ((pointer-pathname (pathname pointer-value))
                 (pointer-root
                   (merge-pathnames
                    "recovery-session-pointers/"
                    (configuration-state-root configuration))))
            (unless (and (uiop:absolute-pathname-p pointer-pathname)
                         (uiop:subpathp pointer-pathname pointer-root)
                         (probe-file pointer-pathname)
                         (application--bounded-file-p
                          pointer-pathname
                          *application-recovery-pointer-byte-limit*))
              (return (values nil nil nil)))
            (multiple-value-bind (record complete-p)
                (snapshot-read pointer-pathname)
              (unless (and complete-p
                           (application-command--proper-list-p record))
                (return (values nil nil nil)))
              (let* ((properties (rest record))
                     (conversation-id (getf properties :conversation-id))
                     (resolved-conversation-id
                       (and
                        (application--bounded-string-p conversation-id 128)
                        (conversation-identifier-migration-resolve
                         configuration conversation-id)))
                     (rendered-sequence (getf properties :rendered-sequence))
                     (history-floor-sequence
                       (getf properties :history-floor-sequence))
                     (history-floor-present-p
                       (loop for key in properties by #'cddr
                             thereis (eq key :history-floor-sequence))))
                (unless
                    (and (member (length record) '(7 9) :test #'=)
                         (eq (first record) :recovery-session)
                         (= (loop for key in properties by #'cddr
                                  count (eq key :version))
                            1)
                         (= (loop for key in properties by #'cddr
                                  count (eq key :conversation-id))
                            1)
                         (= (loop for key in properties by #'cddr
                                  count (eq key :rendered-sequence))
                            1)
                         (<= (loop for key in properties by #'cddr
                                   count (eq key :history-floor-sequence))
                             1)
                         (= (getf properties :version) 1)
                         (identifier-p
                          resolved-conversation-id)
                         (typep rendered-sequence '(integer 0))
                         (or (not history-floor-present-p)
                             (null history-floor-sequence)
                             (typep history-floor-sequence '(integer 1)))
                         (loop for key in properties by #'cddr
                               always
                               (member key
                                       '(:version :conversation-id
                                         :rendered-sequence
                                         :history-floor-sequence)
                                       :test #'eq)))
                  (return (values nil nil nil)))
                (values resolved-conversation-id
                        rendered-sequence
                        history-floor-sequence)))))
      (error ()
        (values nil nil nil)))))

(-> application-recovery-state
    (configuration)
    (values (option string) (option integer) (option integer)))
(defun application-recovery-state (configuration)
  "Return trustworthy conversation and cursor state for recovered startup."
  (let ((crash-pointer (uiop:getenv "AUTOLITH_CRASH_POINTER")))
    (if (and (non-empty-string-p (uiop:getenv "AUTOLITH_RECOVERED"))
             (non-empty-string-p crash-pointer)
             (null (application--recovery-crash-capsule-record configuration)))
        (application--recovery-session-state configuration)
        (values (application--recovery-conversation-id)
                (application--recovery-sequence
                 "AUTOLITH_RECOVERY_RENDERED_SEQUENCE")
                (application--recovery-sequence
                 "AUTOLITH_RECOVERY_HISTORY_FLOOR_SEQUENCE")))))

(-> application-recovery-diagnosis-prompt
    (configuration)
    (option string))
(defun application-recovery-diagnosis-prompt (configuration)
  "Return one read-only crash diagnosis prompt for CONFIGURATION, when recovered."
  (let ((record
          (application--recovery-crash-capsule-record configuration)))
    (when record
      (let* ((properties (rest record))
             (backtrace (getf properties :backtrace))
             (commit (getf properties :git-commit)))
        (format
         nil
         "Autolith recovered this conversation after its active process ~
          crashed. Diagnose the likely cause from the bounded crash context ~
          below and the recent conversation history. This is a diagnosis-only ~
           turn. Only bounded read-only diagnostic tool rounds are available ~
           for inspecting the restored workspace, tracked source, and active ~
           state. Filesystem reads are confined to the workspace and source ~
           roots. Do not modify source, files, private mutation state, ~
           generations, or the active image. Explain the failure, the ~
          supporting evidence, your confidence, and the safest specific repair. ~
          End by asking the user whether you should apply that repair to the ~
          active image. Do not apply any repair until the user explicitly agrees.~
          ~%~%Crash condition type: ~A~%Crash condition: ~A~%~
          Source commit: ~A~%Mutation journal position: ~D~%~
          Bounded backtrace:~%~{  ~A~%~}"
         (sanitize-text (getf properties :condition-type)
                        :single-line-p t)
         (sanitize-text (getf properties :condition)
                        :single-line-p t)
         (sanitize-text (or commit "unknown") :single-line-p t)
         (getf properties :journal-position)
         (mapcar (lambda (frame)
                   (sanitize-text frame :single-line-p t))
                 backtrace))))))

(-> application-raise-fatal
    (application serious-condition list)
    null)
(defun application-raise-fatal (application condition backtrace)
  "Write fatal CONDITION context and leave APPLICATION through recovery status."
  (let ((capsule (application-write-crash-capsule
                  application condition :backtrace backtrace)))
    (error 'fatal-control-path-error
           :message "The active agent path failed unexpectedly."
           :cause condition
           :capsule-pathname capsule)))
