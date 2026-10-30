;;;; -- Trusted Session and Job Command Dispatch --

(in-package #:autolith-supervisor)

(serapeum/types:-> parent-session-list (parent-configuration) list)
(defun parent-session-list (configuration)
  "List literal parent and legacy session identities without running the agent."
  (let* ((private    (uiop:ensure-directory-pathname (parent-configuration-get configuration ':private-root)))
         (legacy     (parent-configuration-get configuration ':legacy-conversations-root))
         (identities nil))
    (dolist (directory (uiop:subdirectories (merge-pathnames "sessions/" private)))
      (when (probe-file (merge-pathnames "session.sexp" directory))
        (push (first (last (pathname-directory directory))) identities)))
    (when legacy
      (dolist (file (uiop:directory-files (uiop:ensure-directory-pathname legacy) "*.sexp"))
        (push (pathname-name file) identities))
      (dolist (directory (uiop:subdirectories (uiop:ensure-directory-pathname legacy)))
        (when (parent-session--legacy-segments (pathname legacy)
                                              (first (last (pathname-directory directory))))
          (push (first (last (pathname-directory directory))) identities))))
    (sort (remove-duplicates
           (remove-if-not (lambda (identity)
                            (handler-case (progn (parent-session--component identity) t)
                              (parent-session-compatibility-error () nil))) identities)
           :test #'equal) #'string<)))

(serapeum/types:-> parent-session-pick
                   (parent-configuration &key (:input stream) (:output stream)) (or null string))
(defun parent-session-pick (configuration &key (input *standard-input*) (output *standard-output*))
  "Select one existing session by its printed number or literal identifier."
  (let ((sessions (parent-session-list configuration)))
    (unless sessions (return-from parent-session-pick nil))
    (loop for identifier in sessions for index from 1 do (format output "~D  ~A~%" index identifier))
    (write-string "Session (empty cancels): " output)
    (finish-output output)
    (let ((answer (read-line input nil "")))
      (cond
        ((zerop (length answer))
         nil)
        ((member answer sessions :test #'equal)
         answer)
        (t
         (let ((number (ignore-errors (parse-integer answer :junk-allowed nil))))
           (unless (and number (<= 1 number (length sessions)))
             (parent-session--reject ':invalid-selection))
           (nth (1- number) sessions)))))))

(serapeum/types:-> parent-session--resume-request (parent-cli-request string) parent-cli-request)
(defun parent-session--resume-request (request identifier)
  "Copy all session flags while replacing only the selected session identity."
  (let ((copy (copy-parent-cli-request request)))
    (setf (parent-cli-request-options copy) (copy-list (parent-cli-request-options request))
          (parent-cli-request-command copy) ':resume
          (parent-cli-request-arguments copy) (list identifier)
          (getf (parent-cli-request-options copy) ':session-id) identifier)
    copy))

(serapeum/types:-> parent-job--read-text (pathname integer) string)
(defun parent-job--read-text (path limit)
  "Read a bounded regular file without following its final link."
  (with-open-stream (stream (ls-compat.posix:open-regular-file path))
    (let ((size (file-length stream)))
      (when (> size limit) (parent-session--reject ':job-artifact-limit))
      (let ((bytes (make-array size :element-type '(unsigned-byte 8))))
        (unless (= (read-sequence bytes stream) size) (parent-session--reject ':job-artifact-changed))
        (babel:octets-to-string bytes :encoding ':utf-8)))))

(serapeum/types:-> parent-job--read-form (string) t)
(defun parent-job--read-form (source)
  "Read exactly one bounded inert result or event, without evaluating peer source."
  (sexp-config:read-source
   source
   (sexp-config:make-source-grammar
    :label "Job transport" :maximum-depth 128 :maximum-nodes 262144
    :maximum-string-characters (* 16 1024 1024)
    :allowed-atom-predicate    (lambda (value)
                                 (or (null value) (eq value t) (keywordp value)
                                     (stringp value) (realp value))))))

(serapeum/types:-> parent-job--result-valid-p (t) boolean)
(defun parent-job--result-valid-p (result)
  "Validate the fixed terminal result envelope before parent publication."
  (and (sexp-store:record-shape-p result)
       (eq (first result) ':autolith-job-result)
       (let ((fields (rest result)))
         (and (parent-configuration--plist
               fields '(:version :id :status :result :failure :trace-id :usage :started-at :finished-at))
              (eql (getf fields ':version) 1)
              (stringp (getf fields ':id))
              (member (getf fields ':status) '(:succeeded :failed :timed-out :cancelled))
              (stringp (getf fields ':started-at)) (stringp (getf fields ':finished-at))
              (or (eq (getf fields ':status) ':succeeded)
                  (let ((failure (getf fields ':failure)))
                    (and (parent-configuration--plist failure '(:category :message))
                         (keywordp (getf failure ':category)) (stringp (getf failure ':message)))))
              t))))

(serapeum/types:-> parent-job--failure (keyword) list)
(defun parent-job--failure (category)
  "Build a content-free process failure artifact without depending on the agent system."
  (let ((time (local-time:format-timestring nil (local-time:now) :timezone local-time:+utc-zone+)))
    (list ':autolith-job-result :version 1 :id "" :status ':failed
          :failure (list ':category category ':message "The job could not complete.")
          :usage   '(:input-tokens 0 :output-tokens 0 :provider-requests 0)
          :started-at time :finished-at time)))

(serapeum/types:-> parent-job--event-callback (stream) function)
(defun parent-job--event-callback (output)
  "Validate event envelopes and per-run sequence before writing machine output."
  (let ((run nil) (sequence 0) (output-alive-p t))
    (lambda (kind fields)
      (declare (ignore kind))
      (let ((text (gethash "run_job_event" fields)))
        (when text
          (unless (and (stringp text) (<= (length text) 1048576))
            (parent-session--reject ':invalid-job-event))
          (let* ((record (parent-job--read-form text)) (properties (rest record))
                 (id (getf properties ':run-id)) (next (getf properties ':seq)))
            (unless (and (sexp-store:record-shape-p record)
                         (eq (first record) ':autolith-event)
                         (parent-configuration--plist properties '(:version :run-id :seq :time :kind :data))
                         (eql (getf properties ':version) 1) (stringp id)
                         (stringp (getf properties ':time)) (keywordp (getf properties ':kind))
                         (eql next (1+ sequence)) (or (null run) (equal run id)))
              (parent-session--reject ':invalid-job-event))
            (setf run id sequence next)
            (when output-alive-p
              (handler-case
                  (let ((*print-readably* t) (*print-pretty* nil))
                    (write record :stream output) (terpri output) (finish-output output))
                (stream-error () (setf output-alive-p nil))))))))))

(serapeum/types:-> parent-run-job-cli-dispatch (parent-configuration parent-cli-request) integer)
(defun parent-run-job-cli-dispatch (configuration request)
  "Run one job in the mandatory native child and publish only its validated result file."
  (let* ((output (pathname (parent-cli-option request ':output)))
         (input  (pathname (parent-cli-option request ':input)))
         (events (equal (parent-cli-option request ':events) "sexp"))
         (application nil) (launch nil)
         (result-path nil) (input-read-p nil))
    (handler-case
        (unwind-protect
             (let* ((source (parent-job--read-text input 1048576))
                    (job    (make-hash-table :test #'equal)))
              (setf input-read-p t)
              (setf result-path
                    (merge-pathnames (format nil "job-~A.sexp" (parent-application--identifier))
                                     (uiop:ensure-directory-pathname
                                      (parent-configuration-get configuration ':temporary-root)))
                    application (make-parent-application
                                 configuration :input-source (constantly nil)
                                 :renderer                   (lambda (&rest arguments) (declare (ignore arguments)) nil))
                    launch (parent-application-open application))
              (when events
                (setf (slot-value (parent-launch-runtime launch) 'application-event-callback)
                      (parent-job--event-callback *standard-output*)))
              (setf (gethash "source" job) source
                    (gethash "output_path" job) (namestring result-path)
                    (gethash "events" job) events)
              (parent-launch-turn-run
               launch (parent-application--identifier) "run-job"
               :timeout     86410
               :application (parent-proto:make-application-action
                             :kind ':run-job :source "run-job" :job (protocol-data->map job)))
              (let* ((reply  (session-runtime-application-result (parent-launch-runtime launch)))
                     (fields (and reply (protocol-map->data (parent-proto:application-result.job reply))))
                     (code   (and fields (gethash "exit_code" fields)))
                     (result (parent-job--read-form (parent-job--read-text result-path (* 16 1024 1024)))))
                (unless (and (typep code '(integer 0 255)) (parent-job--result-valid-p result)
                             (eq (zerop code) (eq (getf (rest result) ':status) ':succeeded)))
                  (parent-session--reject ':invalid-job-result))
                (sexp-store:log-write output (list result))
                code))
          (when launch (parent-launch-close launch))
          (when (and result-path (probe-file result-path)) (delete-file result-path)))
      (error ()
       (handler-case
           (sexp-store:log-write
            output (list (parent-job--failure
                          (if input-read-p ':process-failure
                              (if (probe-file input) ':invalid-input ':input-not-found)))))
         (error () nil))
       (if input-read-p 1 64)))))

(serapeum/types:-> parent-session-cli-dispatch (parent-configuration parent-cli-request) integer)
(defun parent-session-cli-dispatch (configuration request)
  "Dispatch every session-owned CLI entry without loading or evaluating the agent system."
  (let* ((arguments (parent-cli-request-arguments request))
         (identity  (first arguments)))
    (case (parent-cli-request-command request)
      (:resume
       (let ((selected (or identity (parent-session-pick configuration))))
         (if selected
             (let ((store (parent-session-ensure configuration selected)))
               (parent-cli-exit-code
                (parent-application-start (parent-session-configuration configuration store)
                                          :request (parent-session--resume-request request (session-store-id store)))))
             0)))
      (:fork
       (let ((store (parent-session-fork configuration identity :selection (rest arguments)
                                         :identifier                       (parent-cli-option request ':id))))
         (parent-cli-exit-code
          (parent-application-start (parent-session-configuration configuration store)
                                    :request (parent-session--resume-request request (session-store-id store))))))
      (:replay
       (parent-session-replay-run (parent-session-ensure configuration identity :read-only-p t)
                                  :selection (rest arguments))
       0)
      (:run-job
       (parent-run-job-cli-dispatch configuration request))
      (:acp
       (multiple-value-bind (handled-p outcome) (parent-acp-cli-dispatch configuration request)
         (declare (ignore handled-p))
         (parent-cli-exit-code outcome)))
      (:localgroup
       (multiple-value-bind (handled-p outcome) (parent-detached-cli-dispatch configuration request)
         (declare (ignore handled-p))
         (parent-cli-exit-code outcome)))
      (otherwise
       (parent-cli--reject ':unknown-command)))))
