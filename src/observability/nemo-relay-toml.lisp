(in-package #:autolith)

;;;; -- TOML Values --

(-> nemo-relay--toml-value->json (t) t)
(defun nemo-relay--toml-value->json (value)
  "Convert one CL-TOML value to an Autolith JSON value."
  (cond
    ((eq value 'cl-toml:true)
     t)
    ((eq value 'cl-toml:false)
     false)
    ((hash-table-p value)
     (let ((object (json-object)))
       (maphash
        (lambda (key nested-value)
          (unless (stringp key)
            (error 'nemo-relay-error
                   :message "Relay TOML object keys must be strings."
                   :operation "Relay TOML conversion"))
          (setf (gethash key object)
                (nemo-relay--toml-value->json nested-value)))
        value)
       object))
    ((or (stringp value) (numberp value))
     value)
    ((vectorp value)
     (map 'vector #'nemo-relay--toml-value->json value))
    ((null value)
     nil)
    (t
     (error 'nemo-relay-error
            :message
            (format nil
                    "Relay TOML value of type ~A cannot be represented as JSON."
                    (type-of value))
            :operation "Relay TOML conversion"))))

(-> nemo-relay--read-toml-document (pathname) json-object)
(defun nemo-relay--read-toml-document (pathname)
  "Read PATHNAME and return its TOML document as a JSON object."
  (handler-case
      (let ((document
              (nemo-relay--toml-value->json
               (parse-file pathname
                           :array-as ':vector
                           :table-as ':hash-table))))
        (if (json-object-p document)
            document
            (error 'nemo-relay-error
                   :message "A Relay TOML document must be an object."
                   :operation "Relay TOML conversion")))
    (nemo-relay-error (condition)
      (error condition))
    (serious-condition (condition)
      (error 'nemo-relay-error
             :message
             (format nil "Relay TOML file could not be read: ~A"
                     (nemo-relay--condition-summary condition))
             :operation "Relay TOML file"
             :cause condition))))

(-> nemo-relay--toml-pathname-p (pathname) boolean)
(defun nemo-relay--toml-pathname-p (pathname)
  "Return true when PATHNAME has a TOML file extension."
  (string= (string-downcase (or (pathname-type pathname) "")) "toml"))

(-> nemo-relay--toml-document-plugin-config (json-object) json-object)
(defun nemo-relay--toml-document-plugin-config (document)
  "Remove host-only plugin declarations from a Relay TOML document."
  (let ((plugin-config (json-object-copy document)))
    (remhash "plugins" plugin-config)
    plugin-config))

(-> nemo-relay--toml-document-dynamic-plugins (json-object) (option vector))
(defun nemo-relay--toml-document-dynamic-plugins (document)
  "Return the manifest declarations in DOCUMENT's plugins.dynamic section."
  (multiple-value-bind (plugins plugins-present-p)
      (gethash "plugins" document)
    (cond
      ((not plugins-present-p)
       nil)
      ((not (json-object-p plugins))
       (error 'nemo-relay-error
              :message "Relay TOML plugins must be an object."
              :operation "Relay dynamic-plugin configuration"))
      (t
       (multiple-value-bind (dynamic dynamic-present-p)
           (gethash "dynamic" plugins)
         (cond
           ((not dynamic-present-p)
            nil)
           ((vectorp dynamic)
            dynamic)
           (t
            (error 'nemo-relay-error
                   :message
                   "Relay TOML plugins.dynamic must be an array of tables."
                   :operation "Relay dynamic-plugin configuration"))))))))
