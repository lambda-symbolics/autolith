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
    (configuration &key (:recover-read-error (or null function)))
    sexp-store:snapshot-store)
(defun preferences--store (configuration &key recover-read-error)
  "Construct a validated transactional preferences store."
  (let ((pathname (configuration-preferences-path configuration)))
    (make-instance 'sexp-store:snapshot-store
                   :pathname           pathname
                   :lock-pathname      (make-pathname :type "lock" :defaults pathname)
                   :initial-state      #'list
                   :validator          #'preferences--form-p
                   :decoder            #'preferences--form->plist
                   :encoder            #'preferences--plist->form
                   :duplicate-keys     ':first
                   :recover-read-error recover-read-error)))

(-> preferences--read (configuration) list)
(defun preferences--read (configuration)
  "Read CONFIGURATION's durable values from its version 8 preferences file."
  (handler-case
      (sexp-store:store-read (preferences--store configuration))
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
          (configuration-ensure-root-directory configuration ':config-root)
          (sexp-store:store-transact
           (preferences--store configuration :recover-read-error recover)
           (lambda (current)
             (let ((replacement (copy-list current)))
               (setf (getf replacement name) value)
               (values replacement nil t)))))
      ((or sexp-store:store-error file-error) (cause)
        (error 'preferences-error
               :message   (format nil "Could not persist preferences at ~A: ~A" pathname cause)
               :pathname  pathname
               :operation ':write
               :cause     cause))))
  nil)

(setf *configuration-store* *preferences-store*)
