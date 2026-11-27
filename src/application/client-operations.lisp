(in-package #:autolith)

;;;; -- Headless Operation Admission --

(define-condition application-client-operation-unavailable (error)
  ()
  (:documentation "An operation cannot be invoked without terminal ownership.")
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "This operation requires terminal ownership or has no headless adapter." stream))))

(define-condition application-client-operation-busy (error)
  ()
  (:documentation "An operation cannot run beside the channel's active turn.")
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "The operation cannot run while the channel is busy." stream))))

(-> application-client-operation-terminal-p (application-operation) boolean)
(defun application-client-operation-terminal-p (operation)
  "Return whether OPERATION always requires terminal ownership."
  (and (typep operation 'application-command-operation)
       (eq (application-command-terminal-behavior
            (application-operation-backend operation)) ':exclusive)))

(-> application-client-operation-admitted-p
    (application-operation &optional t) boolean)
(defun application-client-operation-admitted-p (operation &optional (arguments nil supplied-p))
  "Return whether OPERATION supports headless use of decoded ARGUMENTS."
  (or (typep operation 'application-tool-operation)
      (and (typep operation 'application-command-operation)
           (not (application-client-operation-terminal-p operation))
           (or (not supplied-p)
               (not (application-command-terminal-owner-p
                     (application-operation-backend operation) arguments))))))

(-> application-client-operation-find (application string) application-operation)
(defun application-client-operation-find (application name)
  "Find registered NAME and require its unconditional headless adapter."
  (let ((operation (application-operation-find application name)))
    (unless (and operation (application-client-operation-admitted-p operation))
      (error 'application-client-operation-unavailable))
    operation))

(-> application-client-command-invocation
    (application-command list) application-command-invocation)
(defun application-client-command-invocation (command arguments)
  "Capture native ARGUMENTS for COMMAND without parsing or printing their values."
  (application-operation--command-invocation command arguments))

(-> application-client-operation--invoke-presented
    (application-operation application t &key (:context (option tool-context))) t)
(defun application-client-operation--invoke-presented (operation application arguments &key context)
  "Return the native operation result while capturing command presentation."
  (let* ((*application-command-presentation-capture-p* t)
         (*application-command-presentation-text*      nil)
         (result                                       (application-operation-invoke operation application arguments :context context)))
    result))

(-> application-client-operation-invoke
    (application application-operation t) (values t (option string) &optional))
(defun application-client-operation-invoke (application operation arguments)
  "Invoke decoded ARGUMENTS under native admission; return result and delivery.

Idle calls execute as channel work. Busy commands use the registered active-turn
policy, including safe-boundary scheduling. A scheduled reply is acceptance only."
  (unless (application-client-operation-admitted-p operation arguments)
    (error 'application-client-operation-unavailable))
  (let ((controller (application-input-controller application)))
    (cond
      ((not (and controller (application-input-controller-busy-p controller)))
       (values
        (application-clients-call-as-work
         application
         (lambda ()
           (application-client-operation--invoke-presented
            operation application arguments
            :context (and (typep operation 'application-tool-operation)
                          (application-tool-context application)))))
        nil))
      ((typep operation 'application-command-operation)
       (case (application-operation-active-turn-action operation arguments)
         (:execute
          (values (application-client-operation--invoke-presented operation application arguments) nil))
         (:apply
           (let ((invocation
                   (application-operation--command-invocation
                    (application-operation-backend operation) arguments))
                 (*application-command-presentation-capture-p* t)
                 (*application-command-presentation-text*      nil))
             (unless (application-input-controller--schedule-apply
                      controller (application-command-invocation-input invocation) invocation)
               (error 'application-client-operation-busy))
            (values (format nil "~{~A~^~%~}" (nreverse *application-command-presentation-text*))
                    "scheduled")))
         (otherwise
          (error 'application-client-operation-busy))))
      (t
       (error 'application-client-operation-busy)))))

(-> application-client-permission-modes nil list)
(defun application-client-permission-modes ()
  "Return the available channel permission mode IDs in catalogue order."
  (mapcar #'first (application--permission-mode-catalogue)))

(-> application-client-permission-input (keyword) list)
(defun application-client-permission-input (mode)
  "Return native command arguments for an available channel permission MODE."
  (let ((row (assoc mode (application--permission-mode-catalogue))))
    (unless row
      (error 'application-client-operation-unavailable))
    (list (second row))))
