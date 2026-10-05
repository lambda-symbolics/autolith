(in-package #:autolith)

;;;; -- Standard Engineering Results --

(defparameter *task-engineering-output-schema*
  '(:type :object
    :properties
    (("schemaVersion" (:type :integer :enum (1)))
     ("baseline"
      (:type :object
       :properties (("kind" (:type :string :enum ("git" "snapshot" "unknown")))
                    ("workspace" (:type :string))
                    ("revision" (:type :string)))
       :required ("kind" "workspace" "revision") :additional-properties nil))
     ("change"
      (:type :object
       :properties (("kind" (:type :string :enum ("patch" "commit-range" "change-set" "none")))
                    ("reference" (:type :string)))
       :required ("kind" "reference") :additional-properties nil))
     ("paths" (:type :array :items (:type :string) :max-items 512))
     ("symbols" (:type :array :items (:type :string) :max-items 512))
     ("checks"
      (:type :array :max-items 128
       :items
       (:type :object
        :properties
        (("command" (:type :string))
         ("status" (:type :string :enum ("passed" "failed" "skipped" "not-run")))
         ("summary" (:type :string))
         ("evidence" (:type :array :items (:type :string) :max-items 128)))
        :required ("command" "status" "summary") :additional-properties nil)))
     ("evidence" (:type :array :items (:type :string) :max-items 128))
     ("issues" (:type :array :items (:type :string) :max-items 64))
     ("integrationNotes" (:type :string)))
    :required ("schemaVersion" "baseline" "change" "paths" "checks"
               "evidence" "issues" "integrationNotes")
    :additional-properties nil)
  "The versioned common portion of an optional engineering child result.
Checks and evidence describe child-reported results, not harness acceptance.")

(-> task-engineering-output-schema
    (&key (:properties list) (:required list) (:additional-properties boolean)
          (:pathname (option pathname)) (:source keyword)
          (:definition-name (option string))) list)
(defun task-engineering-output-schema
    (&key properties required additional-properties pathname
          (source ':programmatic) definition-name)
  "Return a fresh engineering contract extended by role PROPERTIES and REQUIRED.
Reject redefinition of common fields. Preserve the caller's role provenance in
contract errors. ADDITIONAL-PROPERTIES controls unlisted role-specific fields."
  (let* ((extension
           (task-output-schema-normalize
            (list :type ':object :properties properties :required required
                  :additional-properties additional-properties)
            :pathname pathname :source source :definition-name definition-name))
         (schema (copy-tree *task-engineering-output-schema*))
         (common (getf schema :properties)))
    (dolist (property (getf extension :properties))
      (when (assoc (first property) common :test #'string=)
        (task-agent-definition--error
         :pathname pathname :source source :definition-name definition-name
         :field ':output
         :cause (format nil "Engineering field ~S is reserved by the common contract."
                        (first property)))))
    (setf (getf schema :properties)
          (append common (getf extension :properties))
          (getf schema :required)
          (append (getf schema :required) (getf extension :required))
          (getf schema :additional-properties) additional-properties)
    (task-output-schema-normalize
     schema :pathname pathname :source source :definition-name definition-name)))

(-> task-engineering--expand-schema
    (t &key (:pathname (option pathname)) (:source keyword)
            (:definition-name (option string))) t)
(defun task-engineering--expand-schema
    (schema &key pathname (source ':programmatic) definition-name)
  "Expand the native :ENGINEERING contract name, leaving ordinary schemas intact."
  (cond
    ((eq schema ':engineering)
     (task-engineering-output-schema
      :pathname pathname :source source :definition-name definition-name))
    ((and (proper-list-p schema) (evenp (length schema))
          (eq (getf schema :type) ':engineering))
     (task--plist-alist
      schema '(:type :properties :required :additional-properties)
      :pathname pathname :source source :definition-name definition-name)
     (task-engineering-output-schema
      :properties (getf schema :properties)
      :required (getf schema :required)
      :additional-properties (getf schema :additional-properties)
      :pathname pathname :source source :definition-name definition-name))
    (t
     schema)))
