(in-package #:autolith)

;;;; -- Fatal Control Path --

(define-condition fatal-control-path-error (autolith-error)
  ((cause
    :initarg       :cause
    :reader        fatal-control-path-error-cause
    :type          serious-condition
    :documentation "The unexpected condition that made the active path untrustworthy.")
   (capsule-pathname
    :initarg        :capsule-pathname
    :reader         fatal-control-path-error-capsule-pathname
     :type          (option pathname)
     :documentation "The private crash capsule, if it could be written."))
  (:documentation "An unexpected failure that ends the active session."))

(-> application-safe-backtrace () list)
(defun application-safe-backtrace ()
  "Return bounded call names without argument values from the current stack."
  (safe-backtrace))

(-> application-crash-condition-report (serious-condition) string)
(defun application-crash-condition-report (condition)
  "Return an allowlisted, secret-free report for crash CONDITION.

For a generic function call that no method accepts, the report names the
function and the classes of its arguments."
  (if (typep condition 'autolith-error)
      (autolith-error-message condition)
      (format nil "Unexpected condition of type ~S.~@[ ~A~]"
              (type-of condition) (safe-condition-report condition))))

(-> application-write-crash-capsule
    (application serious-condition &key (:backtrace list))
    pathname)
(defun application-write-crash-capsule
    (application condition &key (backtrace (application-safe-backtrace)))
  "Write a secret-free crash capsule for CONDITION and return its pathname."
  (let* ((configuration (application-configuration application))
         (identifier    (make-identifier))
         (pathname      (merge-pathnames
                         (make-pathname :name identifier :type "sexp")
                         (merge-pathnames "crashes/"
                                          (config :state-root configuration))))
         (commit
           (handler-case
               (source-revision
                (make-source-provenance (config :source-root configuration)))
             (error ()
               nil)))
         (safe-backtrace
           (mapcar (lambda (frame-name)
                     (if (or (symbolp frame-name) (stringp frame-name))
                         (bounded-string frame-name :limit 300)
                         (bounded-string (type-of frame-name) :limit 300)))
                   (subseq backtrace 0 (min 30 (length backtrace))))))
    (snapshot-write
     pathname
     (list :crash
           :version 1
           :id                identifier
           :time              (get-universal-time)
           :condition-type    (bounded-string (type-of condition) :limit 300)
           :condition
            (bounded-string (application-crash-condition-report condition)
                            :limit 2000)
           :backtrace         safe-backtrace
           :conversation-id
           (let ((conversation (application-conversation application)))
             (and (conversation-persisted-p conversation)
                  (conversation-identifier conversation)))
           :rendered-sequence (application-rendered-sequence application)
           :history-floor-sequence
           (application-history-floor-sequence application)
           :git-commit        commit
           :journal-position
           (let ((journal (configuration-journal-path configuration)))
             (if (probe-file journal)
                 (with-open-file (stream journal
                                         :direction    ':input
                                         :element-type '(unsigned-byte 8))
                   (file-length stream))
                 0))))
    (platform-make-private *platform* pathname)
    pathname))

(-> application-raise-fatal
    (application serious-condition list)
    nil)
(defun application-raise-fatal (application condition backtrace)
  "Record fatal CONDITION context if possible, then end APPLICATION."
  (let ((capsule (handler-case
                     (application-write-crash-capsule application condition :backtrace backtrace)
                   (error () nil))))
    (error 'fatal-control-path-error
           :message          "The active agent path failed unexpectedly."
           :cause            condition
           :capsule-pathname capsule)))
