(in-package #:autolith)

;;;; -- Plugin Records --

(deftype plugin-state ()
  "The plugin states phase 1 owns.

A declared plugin becomes enabled after a successful load, failed after a
failed load, or disabled when the user disables it. An enabled or failed
plugin can later become disabled, and a disabled plugin can be declared
again. The intermediate resolve and load states belong to phase 6."
  '(member :declared :enabled :failed :disabled))

(-> plugin-state-p (t) boolean)
(defun plugin-state-p (value)
  "Return true when VALUE is a phase 1 plugin state."
  (typep value 'plugin-state))

(define-condition plugin-record-error (autolith-error)
  ((plugin
    :initarg :plugin
    :reader plugin-record-error-plugin
    :type (option string)
    :documentation "The plugin name involved in the failure, when known.")
   (from-state
    :initarg :from-state
    :reader plugin-record-error-from-state
    :type (option plugin-state)
    :documentation "The state the record had before the invalid transition.")
   (to-state
    :initarg :to-state
    :reader plugin-record-error-to-state
    :type (option plugin-state)
    :documentation "The state the transition tried to enter."))
  (:documentation "A plugin record operation is invalid.")
  (:report
   (lambda (condition stream)
     (format stream "Plugin ~@[~S ~]cannot move from ~S to ~S: ~A"
             (plugin-record-error-plugin condition)
             (plugin-record-error-from-state condition)
             (plugin-record-error-to-state condition)
             (autolith-error-message condition)))))

(defclass plugin-record ()
  ((name
    :initarg :name
    :reader plugin-record-name
    :type non-empty-string
    :documentation "The plugin name that identifies this declaration.")
   (source
    :initarg :source
    :reader plugin-record-source
    :type non-empty-string
    :documentation "The requested git repository URL or local checkout path.")
   (ref
    :initarg :ref
    :initform nil
    :reader plugin-record-ref
    :type t
    :documentation "The requested ref form, or NIL when the source is used as-is.")
   (identity
    :initarg :identity
    :initform nil
    :reader plugin-record-identity
    :type (option plugin-identity)
    :documentation "The resolved plugin identity, or NIL before resolution.")
   (state
    :initarg :state
    :initform ':declared
    :accessor plugin-record-state
    :type plugin-state
    :documentation "The current phase 1 lifecycle state.")
   (failure
    :initarg :failure
    :initform nil
    :accessor plugin-record-failure
    :type (option string)
    :documentation "The failure reason when the state is failed."))
  (:documentation "One plugin declaration and its phase 1 lifecycle state."))

(-> plugin-record-p (t) boolean)
(defun plugin-record-p (value)
  "Return true when VALUE is a plugin record."
  (typep value 'plugin-record))

(-> plugin-state-transition-allowed-p (plugin-state plugin-state) boolean)
(defun plugin-state-transition-allowed-p (current next)
  "Return true when a plugin may move from CURRENT to NEXT in phase 1."
  (case current
    (:declared (and (member next '(:enabled :failed :disabled)) t))
    (:enabled  (eq next ':disabled))
    (:failed   (eq next ':disabled))
    (:disabled (eq next ':declared))
    (t nil)))

(-> plugin-record-transition
    (plugin-record plugin-state &key (:reason (option string)))
    plugin-record)
(defun plugin-record-transition (record state &key reason)
  "Move RECORD to STATE and return it.

The phase 1 flow is :declared -> :enabled / :failed / :disabled. An enabled or
failed plugin can become disabled and a disabled plugin can be declared again.
A failed transition stores REASON as the failure description. Other transitions
clear the failure description."
  (let ((current (plugin-record-state record)))
    (unless (plugin-state-p state)
      (error 'plugin-record-error
             :message (format nil "~S is not a plugin state." state)
             :plugin (plugin-record-name record)
             :from-state current
             :to-state nil))
    (unless (plugin-state-transition-allowed-p current state)
      (error 'plugin-record-error
             :message "the transition is not part of the plugin lifecycle"
             :plugin (plugin-record-name record)
             :from-state current
             :to-state state))
    (when (and (eq state ':failed) (null reason))
      (error 'plugin-record-error
             :message "a failed plugin needs a reason"
             :plugin (plugin-record-name record)
             :from-state current
             :to-state state))
    (setf (plugin-record-state record) state)
    (setf (plugin-record-failure record)
          (if (eq state ':failed) reason nil))
    record))

(-> plugin-record-resolve (plugin-record plugin-identity) plugin-record)
(defun plugin-record-resolve (record identity)
  "Attach IDENTITY to RECORD and return the record."
  (setf (slot-value record 'identity) identity)
  record)

(defmethod print-object ((record plugin-record) stream)
  "Print RECORD with its name, state, and resolved commit."
  (print-unreadable-object (record stream :type t)
    (format stream "~S ~S~@[ ~S~]"
            (plugin-record-name record)
            (plugin-record-state record)
            (plugin-record-identity record))))
