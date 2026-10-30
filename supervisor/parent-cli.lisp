;;;; -- Trusted Parent Command Line --

(in-package #:autolith-supervisor)

(defstruct parent-cli-request
  "An inert command line. No value in this record is evaluated as Lisp."
  (command ':session :type keyword)
  (options nil :type list)
  (arguments nil :type list)
  (original-arguments nil :type list))

(define-condition parent-cli-error (error)
  ((reason :initarg :reason :reader parent-cli-error-reason
           :documentation "A fixed command-line validation category."))
  (:documentation "An invalid trusted-parent command line.")
  (:report (lambda (condition stream)
             (format stream "Invalid command line: ~A." (parent-cli-error-reason condition)))))

(defparameter *parent-cli-session-flags*
  '("fullscreen" "almighty" "immutable" "pristine" "simulate-crash" "from-source")
  "Boolean session options accepted before and after a sub-command.")

(defparameter *parent-cli-session-values*
  '("site-config-root" "permissions" "image" "localgroup-handoff")
  "Session options that consume exactly one argument.")

(serapeum/types:-> parent-cli--reject (keyword) nil)
(defun parent-cli--reject (reason)
  "Reject an invalid command without printing supplied values."
  (error 'parent-cli-error :reason reason))

(serapeum/types:-> parent-cli-option (parent-cli-request keyword &optional t) t)
(defun parent-cli-option (request key &optional default)
  "Return a parsed option, with DEFAULT for an absent option."
  (getf (parent-cli-request-options request) key default))

(serapeum/types:-> parent-cli--option-key (string) keyword)
(defun parent-cli--option-key (name)
  "Map an accepted option spelling to its inert keyword."
  (if (string= name "image") ':images (intern (string-upcase name) '#:keyword)))

(serapeum/types:-> parent-cli--command (string) keyword)
(defun parent-cli--command (name)
  "Resolve only documented top-level command names."
  (or (rest (assoc name '(("resume" . :resume) ("replay" . :replay) ("fork" . :fork)
                          ("auth" . :auth) ("models" . :models) ("run-job" . :run-job)
                          ("acp" . :acp) ("data" . :data) ("localgroup" . :localgroup)
                          ("lg" . :localgroup) ("update" . :update) ("uninstall" . :uninstall))
                   :test #'string=))
      (parent-cli--reject ':unknown-command)))

(serapeum/types:-> parent-cli--local-options (keyword list) list)
(defun parent-cli--local-options (command arguments)
  "Return option arities for the selected command and its sub-command."
  (case command
    (:fork
     '(("id" . :value)))
    (:run-job
     '(("input" . :value) ("output" . :value) ("events" . :value)))
    (:data
     (append '(("workspace" . :value))
             (when (equal (first arguments) "export") '(("all" . :flag)))))
    (:localgroup
     (cond
       ((equal (first arguments) "attach")
        '(("read-only" . :flag) ("take-over" . :flag)))
       ((or (null arguments) (equal (first arguments) "status"))
        '(("sexp" . :flag)))))
    (:uninstall
     '(("yes" . :flag)))))

(serapeum/types:-> parent-cli--validate (parent-cli-request) parent-cli-request)
(defun parent-cli--validate (request)
  "Check operand cardinality and related option constraints."
  (let* ((command   (parent-cli-request-command request))
         (arguments (parent-cli-request-arguments request))
         (count     (length arguments)))
    (when (parent-cli-option request ':localgroup-handoff)
      (parent-cli--reject ':unsupported-localgroup-handoff))
    (when (and (parent-cli-option request ':from-source)
               (parent-cli-option request ':images))
      (parent-cli--reject ':conflicting-source-image))
    (unless (or (parent-cli-option request ':help) (parent-cli-option request ':version))
      (unless
          (case command
            ((:session :models :acp :run-job :update :uninstall)
             (zerop count))
            (:resume
             (<= count 1))
            ((:replay :fork)
             (or (= count 1)
                 (and (= count 3)
                      (member (second arguments) '("turn" "date" "time" "sequence")
                              :test #'string=))))
            (:auth
             (<= count 2))
            (:data
             (and (= count 2) (member (first arguments) '("export" "import") :test #'string=)))
            (:localgroup
             (or (zerop count)
                 (let ((operation (first arguments)))
                   (cond
                     ((string= operation "tell")
                      (<= 2 count 3))
                     ((member operation '("status" "attach" "detach" "pause" "kill") :test #'string=)
                      (<= count 2)))))))
        (parent-cli--reject ':invalid-operands))
      (when (eq command ':run-job)
        (unless (and (parent-cli-option request ':input) (parent-cli-option request ':output))
          (parent-cli--reject ':missing-job-path))
        (unless (member (parent-cli-option request ':events) '(nil "sexp") :test #'equal)
          (parent-cli--reject ':invalid-events-format))))
    (when (and (eq command ':auth) (second arguments)
               (not (member (second arguments) '("browser" "device" "device-code") :test #'string-equal)))
      (parent-cli--reject ':invalid-authentication-method))
    (when (and (parent-cli-option request ':all) (parent-cli-option request ':workspace))
      (parent-cli--reject ':conflicting-data-selection))
    (when (and (parent-cli-option request ':read-only) (parent-cli-option request ':take-over))
      (parent-cli--reject ':conflicting-attach-mode))
    (let ((mode (parent-cli-option request ':permissions)))
      (when (and mode (not (member mode '("ask" "auto" "sandbox" "full") :test #'equal)))
        (parent-cli--reject ':invalid-permission-mode)))
    request))

(serapeum/types:-> parent-cli-parse (list) parent-cli-request)
(defun parent-cli-parse (arguments)
  "Parse current public CLI syntax into inert data, without loading the agent."
  (unless (and (listp arguments) (list-length arguments) (every #'stringp arguments))
    (parent-cli--reject ':invalid-argument-list))
  (let ((request         (make-parent-cli-request :original-arguments (copy-list arguments)))
        (command-seen-p  nil)
        (options-ended-p nil))
    (labels ((put-option (name value)
               (let ((key (parent-cli--option-key name)))
                 (if (eq key ':images)
                     (setf (getf (parent-cli-request-options request) key)
                           (append (getf (parent-cli-request-options request) key)
                                   (uiop:split-string value :separator ",")))
                     (setf (getf (parent-cli-request-options request) key) value)))))
      (loop while arguments
            for argument = (pop arguments)
            do (cond
                 (options-ended-p
                  (setf (parent-cli-request-arguments request)
                        (append (parent-cli-request-arguments request) (list argument))))
                 ((string= argument "--")
                  (setf options-ended-p t))
                 ((member argument '("-h" "--help") :test #'string=)
                  (put-option "help" t))
                 ((string= argument "--version")
                  (put-option "version" t))
                 ((or (and (> (length argument) 2) (string= argument "--" :end1 2))
                      (and (>= (length argument) 2) (string= argument "-i" :end1 2)))
                  (let* ((short-p    (and (>= (length argument) 2) (string= argument "-i" :end1 2)))
                         (assignment (unless short-p (position #\= argument)))
                         (name       (if short-p "image" (subseq argument 2 assignment)))
                         (local      (assoc name
                                            (parent-cli--local-options
                                             (parent-cli-request-command request)
                                             (parent-cli-request-arguments request))
                                            :test #'string=))
                         (value-p    (or (member name *parent-cli-session-values* :test #'string=)
                                         (eq (rest local) ':value)))
                         (flag-p     (or (member name *parent-cli-session-flags* :test #'string=)
                                         (eq (rest local) ':flag))))
                    (cond
                      (value-p
                       (let ((value (cond
                                      ((and short-p (> (length argument) 2))
                                       (subseq argument 2))
                                      (assignment
                                       (subseq argument (1+ assignment)))
                                      (t
                                       (pop arguments)))))
                         (unless (and value (plusp (length value)))
                           (parent-cli--reject ':missing-option-value))
                         (put-option name value)))
                      (flag-p
                       (when assignment (parent-cli--reject ':flag-has-value))
                       (put-option name t))
                      (t
                       (parent-cli--reject ':unknown-option)))))
                 ((and (plusp (length argument)) (char= (char argument 0) #\-))
                  (parent-cli--reject ':unknown-option))
                 ((not command-seen-p)
                  (setf (parent-cli-request-command request) (parent-cli--command argument)
                        command-seen-p t))
                 (t
                  (setf (parent-cli-request-arguments request)
                        (append (parent-cli-request-arguments request) (list argument)))))))
    (parent-cli--validate request)))

(serapeum/types:-> parent-cli-exit-code (t) (integer 0 255))
(defun parent-cli-exit-code (outcome)
  "Map a controller result to a stable process status. Unknown results fail."
  (typecase outcome
    ((integer 0 255)
     outcome)
    (keyword
     (case outcome
       ((:complete :completed :quit :eof :success)
        0)
       ((:cancelled :interrupted)
        130)
       ((:invalid-request :configuration-error)
        64)
       (:protocol-error
        76)
       (otherwise
        1)))
    (t
     1)))
