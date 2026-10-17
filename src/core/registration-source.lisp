(in-package #:autolith)

;;;; -- Registration Sources --

(defparameter *plugin-registration-layer* ':plugin
  "The layer keyword that plugin contributions carry.

The plugin layer ranks above the bundled layers and below every declarative
scope, matching the plugin skill root order.")

(defvar *extension-registration-source* ':runtime
  "The registration source designator bound by the current load context.

The value is a layer keyword or a registration source. A plugin load binds this
variable to that plugin's registration source, so every contribution it makes
carries the plugin identity.")

(defvar *registration-source-table* (make-hash-table :test #'equal)
  "Interned plugin registration sources keyed by plugin name.")

(defvar *registration-source-lock* (make-lock "Autolith registration sources")
  "The lock protecting the plugin registration source intern table.")

(defclass registration-source ()
  ((plugin
    :initarg :plugin
    :reader registration-source-plugin
    :type string
    :documentation "The plugin name that owns this source."))
  (:documentation "The identity of one plugin's registry contributions.

Contributions from the same plugin share one interned source object, so EQ
identifies an exact source. Every registry uses the source object as the token
that replaces and removes a plugin's contributions as a group."))

(deftype registration-source-designator ()
  "A value that names a registration source: a layer keyword or a plugin source."
  '(or keyword registration-source))

(-> registration-source-p (t) boolean)
(defun registration-source-p (value)
  "Return true when VALUE is a plugin registration source."
  (typep value 'registration-source))

(-> registration-source-layer-p (t) boolean)
(defun registration-source-layer-p (value)
  "Return true when VALUE is a layer keyword that names a source by itself."
  (and (keywordp value) (not (eq value *plugin-registration-layer*))))

(-> registration-source-designator-p (t) boolean)
(defun registration-source-designator-p (value)
  "Return true when VALUE names a registration source."
  (or (registration-source-p value) (registration-source-layer-p value)))

(-> registration-source-layer (registration-source-designator) keyword)
(defun registration-source-layer (designator)
  "Return the precedence layer keyword of DESIGNATOR."
  (if (registration-source-p designator)
      *plugin-registration-layer*
      designator))

(-> registration-source-plugin-p (registration-source-designator) boolean)
(defun registration-source-plugin-p (designator)
  "Return true when DESIGNATOR carries a plugin identity."
  (registration-source-p designator))

(-> plugin-registration-source (string) registration-source)
(defun plugin-registration-source (plugin)
  "Return the interned registration source that PLUGIN's contributions carry."
  (unless (and (stringp plugin) (plusp (length plugin)))
    (error 'configuration-error
           :message (format nil "A plugin registration source needs a plugin name, not ~S."
                            plugin)))
  (let ((key (copy-seq plugin)))
    (with-lock-held (*registration-source-lock*)
      (or (gethash key *registration-source-table*)
          (setf (gethash key *registration-source-table*)
                (make-instance 'registration-source :plugin plugin))))))

(-> registration-source-coerce (t) registration-source-designator)
(defun registration-source-coerce (designator)
  "Return DESIGNATOR after validating it as a registration source."
  (cond
    ((registration-source-p designator)
     designator)
    ((registration-source-layer-p designator)
     designator)
    ((eq designator *plugin-registration-layer*)
     (error 'configuration-error
            :message "A plugin registration source needs a plugin name."))
    (t
     (error 'configuration-error
            :message (format nil "~S is not a registration source." designator)))))

(-> current-registration-source () registration-source-designator)
(defun current-registration-source ()
  "Return the registration source designator of the current load context."
  (registration-source-coerce *extension-registration-source*))

(defmethod print-object ((source registration-source) stream)
  (print-unreadable-object (source stream :type t)
    (format stream "~A ~S" *plugin-registration-layer*
            (registration-source-plugin source))))
