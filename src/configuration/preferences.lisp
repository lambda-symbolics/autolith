(in-package #:autolith)

;;;; -- Durable Settings --

(defclass preferences-store (setting-store)
  ()
  (:documentation
   "The durable setting store kept in each configuration's preferences file."))

(defparameter *preferences-version* 8
  "The readable global preferences file format version.

Version 8 stores durable settings as one flat property list keyed by setting
name; unknown keys survive rewrites so releases can share one file.")

(-> preferences--form-p (t) boolean)
(defun preferences--form-p (form)
  "Return true when FORM is one complete version 8 preferences record."
  (and (record-check form
                     :tag                  ':preferences
                     :versions             (list *preferences-version*)
                     :keyword-keys-p       t
                     :allow-duplicate-keys t)
       t))

(-> preferences--form->plist (list) list)
(defun preferences--form->plist (form)
  "Return the durable value plist carried by a version 8 FORM."
  (loop for (key value) on (rest form) by (partial #'nthcdr 2)
        unless (eq key ':version)
          append (list key value)))

(-> preferences--plist->form (list) list)
(defun preferences--plist->form (plist)
  "Return the version 8 record holding PLIST."
  (list* ':preferences ':version *preferences-version* plist))

(-> preferences--store
    (configuration &key (:pathname pathname) (:recover-read-error (or null function)))
    sexp-store:snapshot-store)
(defun preferences--store
    (configuration &key (pathname (configuration-preferences-path configuration)) recover-read-error)
  "Construct a validated transactional preferences store at PATHNAME."
  (make-instance 'sexp-store:snapshot-store
                 :pathname           pathname
                 :lock-pathname      (make-pathname :type "lock" :defaults pathname)
                 :initial-state      #'list
                 :validator          #'preferences--form-p
                 :decoder            #'preferences--form->plist
                 :encoder            #'preferences--plist->form
                 :duplicate-keys     ':first
                 :recover-read-error recover-read-error))

(-> preferences--migrate-legacy
    (configuration &key (:recover-read-error (or null function))) pathname)
(defun preferences--migrate-legacy (configuration &key recover-read-error)
  "Move legacy state preferences into config once, returning the usable pathname.

Hold both writer locks until the target is published and the source is removed.
Existing config preferences win, including after interrupted source cleanup.
Failed publication keeps using legacy values; failed cleanup retries without
importing them again. Remove at 0.68.0."
  (let ((target (configuration-preferences-path configuration))
        (legacy (merge-pathnames "preferences.sexp" (config :state-root configuration))))
    (flet ((target-present-p ()
             (handler-case (probe-file target)
               (file-error () nil))))
      (if (and (probe-file legacy)
               (not (or (uiop:pathname-equal target legacy)
                        (handler-case
                            (and (target-present-p)
                                 (uiop:pathname-equal (platform-truename *platform* target)
                                                      (platform-truename *platform* legacy)))
                          (file-error () nil)))))
          (handler-case
              (progn
                (configuration-ensure-root-directory configuration ':config-root)
                (call-with-file-lock
                 (make-pathname :type "lock" :defaults legacy)
                 (lambda ()
                   (when (probe-file legacy)
                     (sexp-store:store-transact
                      (preferences--store configuration)
                      (lambda (current)
                        (if (target-present-p)
                            (values current nil nil)
                            (values
                             (sexp-store:store-read
                              (preferences--store configuration :pathname legacy
                                                   :recover-read-error    recover-read-error)
                              :lock-held-p t)
                             nil t)))
                      :publish (lambda (committed)
                                 (declare (ignore committed))
                                 (delete-file legacy))))))
                target)
            (error (cause)
              (when (and (not (target-present-p))
                         (typep cause 'sexp-store:store-error)
                         (equal legacy (sexp-store:store-error-pathname cause)))
                (error cause))
              (warn 'preferences-migration-warning :source legacy :target target :cause cause)
              (if (target-present-p)
                  target
                  legacy)))
          target))))

(-> preferences--read (configuration) list)
(defun preferences--read (configuration)
  "Read CONFIGURATION's durable values from its version 8 preferences file."
  (handler-case
      (let* ((target   (configuration-preferences-path configuration))
             (pathname (preferences--migrate-legacy configuration)))
        (if (uiop:pathname-equal pathname target)
            (sexp-store:store-read (preferences--store configuration))
            (call-with-file-lock
             (make-pathname :type "lock" :defaults pathname)
             (lambda ()
               (let ((redirect-p (or (probe-file target) (not (probe-file pathname)))))
                 (sexp-store:store-read
                  (preferences--store configuration :pathname (if redirect-p target pathname))
                  :lock-held-p (not redirect-p)))))))
    ((or sexp-store:store-error file-error) (cause)
      (let ((pathname (or (etypecase cause
                            (sexp-store:store-error
                             (sexp-store:store-error-pathname cause))
                            (file-error
                             (file-error-pathname cause)))
                          (configuration-preferences-path configuration))))
        (error 'preferences-error
               :message   (format nil "Could not read preferences at ~A: ~A" pathname cause)
               :pathname  pathname
               :operation ':read
               :cause     cause)))))

(defmethod store-read-values ((store preferences-store) configuration)
  "Read CONFIGURATION's durable values from its preferences file."
  (declare (ignore store))
  (preferences-load-values configuration))

(defmethod store-write-value ((store preferences-store) configuration name &key value)
  "Merge NAME and VALUE into CONFIGURATION's preferences file."
  (declare (ignore store))
  (preferences-store configuration name value))

(defvar *preferences-store* (make-instance 'preferences-store)
  "The one preferences store of the process, shared by every configuration.")

(-> preferences-load-values (configuration) list)
(defun preferences-load-values (configuration)
  "Return CONFIGURATION's persisted durable values.

Report malformed or unsupported files through PREFERENCES-LOAD-WARNING and
return NIL. Reading never repairs or rewrites a corrupt file."
  (handler-case
      (preferences--read configuration)
    (preferences-error (condition)
      (warn 'preferences-load-warning
            :pathname (preferences-error-pathname condition)
            :cause    condition)
      nil)))

(-> preferences-store (configuration keyword t) null)
(defun preferences-store (configuration name value)
  "Persist VALUE under NAME, merging into the freshly read preferences state."
  (let ((pathname (configuration-preferences-path configuration))
        (recover  (lambda (condition)
                    (declare (ignore condition))
                    nil)))
    (handler-case
        (progn
          (setf pathname (preferences--migrate-legacy configuration :recover-read-error recover))
          (configuration-ensure-root-directory
           configuration
           (if (equal pathname (configuration-preferences-path configuration))
               ':config-root
               ':state-root))
          (flet ((replace-value (current)
                   (let ((replacement (copy-list current)))
                     (setf (getf replacement name) value)
                     (values replacement nil t))))
            (sexp-store:store-transact
             (preferences--store configuration :pathname pathname :recover-read-error recover)
             (lambda (current)
               (let ((target (configuration-preferences-path configuration)))
                 (if (and (not (uiop:pathname-equal pathname target))
                          (or (probe-file target) (not (probe-file pathname))))
                     (progn
                       (configuration-ensure-root-directory configuration ':config-root)
                       (sexp-store:store-transact
                        (preferences--store configuration :recover-read-error recover)
                        #'replace-value
                        :publish (lambda (committed)
                                   (declare (ignore committed))
                                   (when (probe-file pathname)
                                     (delete-file pathname))))
                       (values current nil nil))
                     (replace-value current)))))))
      ((or sexp-store:store-error file-error) (cause)
        (error 'preferences-error
               :message   (format nil "Could not persist preferences at ~A: ~A" pathname cause)
               :pathname  pathname
               :operation ':write
               :cause     cause))))
  nil)

(setf *configuration-store* *preferences-store*)

(define-condition preferences-migration-warning (warning)
  ((source :initarg       :source
           :reader        preferences-migration-warning-source
           :documentation "The legacy preferences pathname to remove after publication.")
   (target :initarg       :target
           :reader        preferences-migration-warning-target
           :documentation "The desired configuration preferences pathname.")
   (cause  :initarg       :cause
           :reader        preferences-migration-warning-cause
           :documentation "The condition that prevented publication or source cleanup."))
  (:documentation "Preferences migration is incomplete and will be retried.")
  (:report (lambda (condition stream)
             (format stream "Could not finish migrating preferences from ~A to ~A: ~A. Migration will be retried."
                     (preferences-migration-warning-source condition)
                     (preferences-migration-warning-target condition)
                     (preferences-migration-warning-cause condition)))))
