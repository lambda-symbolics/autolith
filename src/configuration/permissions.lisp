(in-package #:autolith)

;;;; -- Persistent Command Permissions --

(defparameter *permissions-version* 1
  "The readable command permission file format version.")

(defclass command-permission ()
  ((command
    :initarg :command
    :reader command-permission-command
    :type non-empty-string
    :documentation "The exact shell command approved by the user.")
   (directory
    :initarg :directory
    :reader command-permission-directory
    :type non-empty-string
    :documentation "The canonical working directory in which COMMAND is approved."))
  (:documentation "One exact persistent shell command approval."))

(defclass permission-state ()
  ((rules
    :initarg :rules
    :initform nil
    :accessor permission-state-rules
    :type list
    :documentation "The exact command and working-directory approvals."))
  (:documentation "Validated persistent command approvals for one user."))

(-> permissions--directory-name ((or pathname string)) string)
(defun permissions--directory-name (directory)
  "Return DIRECTORY as a canonical directory namestring."
  (let ((existing (uiop:directory-exists-p directory)))
    (unless existing
      (error 'permissions-error
             :message (format nil "Permission directory ~A does not exist."
                              directory)
             :pathname (pathname directory)
             :operation ':validate
             :cause nil))
    (namestring (uiop:ensure-directory-pathname (truename existing)))))

(-> permissions--rule-form-p (t) boolean)
(defun permissions--rule-form-p (form)
  "Return true when FORM is one exact command permission record."
  (and (listp form)
       (= (length form) 4)
       (eq (first form) ':command)
       (non-empty-string-p (second form))
       (eq (third form) ':directory)
       (non-empty-string-p (fourth form))))

(-> permissions--form-p (t) boolean)
(defun permissions--form-p (form)
  "Return true when FORM is one complete supported permission state."
  (handler-case
      (and (listp form)
           (= (length form) 5)
           (eq (first form) ':permissions)
           (eq (second form) ':version)
           (= (third form) *permissions-version*)
           (eq (fourth form) ':rules)
           (listp (fifth form))
           (every #'permissions--rule-form-p (fifth form)))
    (error ()
      nil)))

(-> permissions--form->state (list) permission-state)
(defun permissions--form->state (form)
  "Return the permission state represented by validated FORM."
  (make-instance
   'permission-state
   :rules
   (loop for rule in (fifth form)
         collect (make-instance 'command-permission
                                :command (copy-seq (second rule))
                                :directory (copy-seq (fourth rule))))))

(-> permissions--read (configuration) permission-state)
(defun permissions--read (configuration)
  "Read CONFIGURATION's command permissions or return an empty state."
  (block nil
    (let ((pathname (configuration-permissions-path configuration)))
      (unless (probe-file pathname)
        (return (make-instance 'permission-state)))
      (handler-case
          (multiple-value-bind (form sole-form-p)
              (snapshot-read pathname)
            (unless (and sole-form-p (permissions--form-p form))
              (error 'permissions-error
                     :message (format nil
                                      "Command permissions at ~A are malformed or unsupported."
                                      pathname)
                     :pathname pathname
                     :operation ':read
                     :cause nil))
            (permissions--form->state form))
        (permissions-error (condition)
          (error condition))
        (error (cause)
          (error 'permissions-error
                 :message (format nil "Could not read command permissions at ~A: ~A"
                                  pathname cause)
                 :pathname pathname
                 :operation ':read
                 :cause cause))))))

(-> permissions-load (configuration) permission-state)
(defun permissions-load (configuration)
  "Return saved command permissions, warning and denying after corruption."
  (handler-case
      (permissions--read configuration)
    (permissions-error (condition)
      (warn 'permissions-load-warning
            :pathname (permissions-error-pathname condition)
            :cause condition)
      (make-instance 'permission-state))))

(-> permissions--state-form (permission-state) list)
(defun permissions--state-form (state)
  "Return STATE as one portable readable form."
  (list :permissions
        :version *permissions-version*
        :rules
        (loop for rule in (permission-state-rules state)
              collect (list :command
                            (command-permission-command rule)
                            :directory
                            (command-permission-directory rule)))))

(-> permissions--write (configuration permission-state) null)
(defun permissions--write (configuration state)
  "Atomically persist command permission STATE with private file permissions."
  (let ((pathname (configuration-permissions-path configuration)))
    (handler-case
        (snapshot-write pathname (permissions--state-form state))
      (error (cause)
        (error 'permissions-error
               :message (format nil "Could not persist command permissions at ~A: ~A"
                                pathname cause)
               :pathname pathname
               :operation ':write
               :cause cause))))
  nil)

(-> permissions-allowed-p
    (permission-state string (or pathname string))
    boolean)
(defun permissions-allowed-p (state command directory)
  "Return true when exact COMMAND is permanently approved in DIRECTORY."
  (let ((directory-name (permissions--directory-name directory)))
    (not
     (null
      (find-if (lambda (rule)
                 (and (string= command (command-permission-command rule))
                      (string= directory-name
                               (command-permission-directory rule))))
               (permission-state-rules state))))))

(-> permissions-allow
    (&key
     (:configuration configuration)
     (:state permission-state)
     (:command string)
     (:directory (or pathname string)))
    null)
(defun permissions-allow (&key configuration state command directory)
  "Permanently approve exact COMMAND in DIRECTORY unless already present."
  (unless (non-empty-string-p command)
    (error 'permissions-error
           :message "Cannot approve an empty shell command."
           :pathname (configuration-permissions-path configuration)
           :operation ':validate
           :cause nil))
  (unless (permissions-allowed-p state command directory)
    (let* ((rule (make-instance 'command-permission
                                :command (copy-seq command)
                                :directory
                                (permissions--directory-name directory)))
           (rules (append (permission-state-rules state) (list rule)))
           (replacement (make-instance 'permission-state :rules rules)))
      (permissions--write configuration replacement)
      (setf (permission-state-rules state) rules)))
  nil)

(-> permissions-clear (configuration permission-state) null)
(defun permissions-clear (configuration state)
  "Remove every permanently approved shell command."
  (let ((replacement (make-instance 'permission-state)))
    (permissions--write configuration replacement)
    (setf (permission-state-rules state) nil))
  nil)
