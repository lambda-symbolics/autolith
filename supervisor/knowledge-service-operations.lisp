(in-package #:autolith-supervisor)

;;;; -- Public Knowledge Operations --

(serapeum/types:-> knowledge-service--fields (hash-table list list) null)
(defun knowledge-service--fields (fields allowed required)
  "Check fixed public field names and required values without interning input."
  (unless (and (loop for key being the hash-keys of fields
                     always (member key allowed :test #'equal))
               (every (lambda (key) (nth-value 1 (gethash key fields))) required))
    (knowledge-service--reject ':invalid-request))
  nil)

(serapeum/types:-> knowledge-service--array (t) list)
(defun knowledge-service--array (value)
  "Return a public array as a list. Reject strings and other values."
  (unless (and (vectorp value) (not (stringp value)))
    (knowledge-service--reject ':invalid-request))
  (coerce value 'list))

(serapeum/types:-> knowledge-service--instant (hash-table string) (option timestamp))
(defun knowledge-service--instant (fields name)
  "Parse a supplied instant with the shared term parser."
  (let ((value (gethash name fields)))
    (when value
      (or (and (stringp value) (knowledge-parse-instant value))
          (knowledge-service--reject ':invalid-request)))))

(serapeum/types:-> knowledge-service--evidence (t hash-table) (values list keyword))
(defun knowledge-service--evidence (service source)
  "Check one source with trusted parent file, input, and retrieval authority."
  (let* ((request    (memory-service--source-request source))
         (checked    (memory-service--observe-source (knowledge-service-memory service) request))
         (properties (rest checked))
         (kind       (first checked))
         (quote      (getf properties :quote))
         (time       (getf properties :observed-at))
         (workspace  (knowledge--workspace (knowledge-service-context service))))
    (ecase kind
      (:file
       (let* ((path     (uiop:parse-native-namestring (getf properties :path)))
              (relative (uiop:subpathp path (uiop:ensure-directory-pathname workspace))))
         (values
          (knowledge-observe
           :source (list ':file :path (if relative (uiop:unix-namestring relative)
                                         (getf properties :path))
                         :relative (not (null relative)) :quote quote)
           :workspace (and relative workspace) :digest (getf properties :digest)
           :excerpt quote :adapter "file/1" :time time)
          ':observation)))
      (:user
       (values
        (knowledge-observe :source (list ':user :quote quote
                                         :conversation (knowledge-service-session-id service))
                           :excerpt quote :adapter "user/1" :time time)
        ':user-assertion))
      (:url
       (let* ((url     (getf properties :url))
              (receipt (funcall (knowledge-service-url-evidence service) url quote)))
         (values
          (knowledge-observe
           :source  (append (list ':url :url url :quote quote)
                            (when receipt (list :retrieval "web_extra.gist")))
           :digest (getf receipt :result-digest) :excerpt quote
           :adapter (if receipt "web-retrieval/1" "web-proposal/1")
           :time    (or (getf receipt :observed-at) time))
          (if receipt ':observation ':model-proposal)))))))

(serapeum/types:-> knowledge-service--assert (t hash-table keyword) string)
(defun knowledge-service--assert (service fields scope)
  "Assert a typed fact. Source checks determine its strongest evidence basis."
  (knowledge-service--fields fields
                            '("op" "predicate" "arguments" "negated" "sources" "basis"
                              "valid-from" "valid-until" "scope")
                            '("predicate" "arguments"))
  (let* ((context     (knowledge-service-context service))
         (schema      (or (knowledge-base-schema (knowledge-base-read context)
                                                (gethash "predicate" fields)
                                                :workspace (knowledge--workspace context))
                          (knowledge-service--reject ':invalid-request)))
         (negated     (gethash "negated" fields))
         (requested   (gethash "basis" fields))
         (basis       (and requested
                           (or (find requested '(:observation :user-assertion :assumption :model-proposal)
                                     :test #'equal :key (lambda (item) (string-downcase (symbol-name item))))
                               (knowledge-service--reject ':invalid-request))))
         (proposition (knowledge-make-proposition
                       schema (knowledge-service--array (gethash "arguments" fields))
                       :negated (and negated (not (argo:json-false-p negated)))))
         (sources     (and (gethash "sources" fields)
                           (knowledge-service--array (gethash "sources" fields))))
         (claim       nil))
    (when (and (nth-value 1 (gethash "negated" fields))
               (not (or (eq negated t) (eq negated nil) (argo:json-false-p negated))))
      (knowledge-service--reject ':invalid-request))
    (when (> (length sources) 8)
      (knowledge-service--reject ':resource-limit))
    (labels ((assert-one (evidence established)
               (setf claim
                     (knowledge-assert
                      context proposition :scope scope :evidence evidence
                      :basis               (if evidence
                                               (if (member basis '(:assumption :model-proposal)) basis established)
                                               (or basis ':model-proposal))
                      :valid-from          (knowledge-service--instant fields "valid-from")
                      :valid-until         (knowledge-service--instant fields "valid-until")
                      :source-conversation (knowledge-service-session-id service)))))
      (if sources
          (dolist (source sources)
            (multiple-value-bind (evidence established) (knowledge-service--evidence service source)
              (assert-one evidence established)))
          (assert-one nil nil)))
    (format nil "Asserted ~A: ~A." claim (knowledge-proposition-display proposition schema))))

(serapeum/types:-> knowledge-service--apply (t hash-table keyword) string)
(defun knowledge-service--apply (service fields scope)
  "Apply one fixed public operation through the shared domain engine."
  (unless (hash-table-p fields) (knowledge-service--reject ':invalid-request))
  (let ((named-scope (gethash "scope" fields))
        (name        (gethash "op" fields))
        (context     (knowledge-service-context service))
        (session     (knowledge-service-session-id service)))
    (when (and named-scope (not (equal named-scope (string-downcase (symbol-name scope)))))
      (knowledge-service--reject ':permission-denied))
    (cond
     ((equal name "predicate-define")
      (knowledge-service--fields fields
       '("op" "predicate" "arguments" "key" "template" "description" "scope")
       '("predicate" "arguments"))
      (let ((arguments
             (mapcar
              (lambda (argument)
                (unless (hash-table-p argument) (knowledge-service--reject ':invalid-request))
                (knowledge-service--fields argument '("name" "type" "unit") '("name" "type"))
                (let ((type
                       (or
                        (find (gethash "type" argument)
                              '(:entity :string :number :boolean :instant :date :duration
                                :quantity)
                              :test #'equal :key
                              (lambda (item) (string-downcase (symbol-name item))))
                        (knowledge-service--reject ':invalid-request))))
                  (append (list :name (gethash "name" argument) :type type)
                          (when (gethash "unit" argument)
                            (list :unit (gethash "unit" argument))))))
              (knowledge-service--array (gethash "arguments" fields)))))
        (multiple-value-bind (schema version)
            (knowledge-define-schema context
                                     (knowledge-make-schema :predicate (gethash "predicate" fields)
                                                            :arguments arguments :key
                                                            (and (gethash "key" fields)
                                                                 (knowledge-service--array
                                                                  (gethash "key" fields)))
                                                            :template  (gethash "template" fields)
                                                            :description
                                                            (gethash "description" fields))
                                     :scope scope)
          (format nil "Defined ~A version ~D." (knowledge-schema-predicate schema) version))))
     ((equal name "fact-assert")
      (knowledge-service--assert service fields scope))
     ((equal name "fact-retract")
      (knowledge-service--fields fields '("op" "id" "reason") '("id"))
      (let* ((base    (knowledge-base-read context))
             (id      (gethash "id" fields))
             (support (knowledge-base-support base id))
             (claim   (or (knowledge-base-claim base id)
                          (and support (knowledge-base-claim base (getf support :claim))))))
        (unless (and claim (eq scope (getf claim :scope))
                     (or (eq scope ':global)
                         (equal (getf claim :workspace) (knowledge--workspace context))))
          (knowledge-service--reject ':permission-denied))
       (format nil "Retracted ~D supports of ~A."
        (length
         (knowledge-retract context id :reason (gethash "reason" fields)
                 :source-conversation          session))
        id)))
     ((equal name "rule-propose")
      (knowledge-service--fields fields
       '("op" "id" "premises" "guards" "bindings" "conclusion" "description" "scope")
       '("id" "premises" "conclusion"))
      (multiple-value-bind (id version new-p)
          (knowledge-propose-rule context :id (gethash "id" fields) :scope scope :premises
                                  (gethash "premises" fields) :guards (gethash "guards" fields #())
                                  :bindings (gethash "bindings" fields #()) :conclusion
                                  (gethash "conclusion" fields) :description
                                  (gethash "description" fields) :source-conversation session)
        (declare (ignore new-p))
        (knowledge-service--admit-proposal service :scope scope :kind ':rule :id id :version
         version)
        (format nil "Proposed rule ~A version ~D." id version)))
     ((equal name "method-define")
      (knowledge-service--fields fields '("op" "id" "task" "when" "steps" "description" "scope")
       '("id" "task" "steps"))
      (multiple-value-bind (id changed-p)
          (knowledge-define-method context :id (gethash "id" fields) :scope scope :task
           (gethash "task" fields) :when (gethash "when" fields #()) :steps
           (gethash "steps" fields) :description (gethash "description" fields)
           :source-conversation session)
        (declare (ignore changed-p))
        (format nil "Defined method ~A." id)))
     ((equal name "contract-define")
      (knowledge-service--fields fields
       '("op" "id" "tool" "when" "requires" "effects" "observes" "accepts" "description" "scope")
       '("id" "tool"))
      (multiple-value-bind (id changed-p version)
          (knowledge-define-contract context :id (gethash "id" fields) :tool
           (gethash "tool" fields) :scope scope :when (gethash "when" fields) :requires
           (gethash "requires" fields #()) :effects (gethash "effects" fields #()) :observes
           (gethash "observes" fields #()) :accepts (gethash "accepts" fields #()) :description
           (gethash "description" fields) :admit nil :source-conversation session)
        (declare (ignore changed-p))
        (knowledge-service--admit-proposal service :scope scope :kind ':contract :id id :version
         version)
        (format nil "Proposed contract ~A version ~D." id version)))
     (t
      (knowledge-service--reject ':unsupported)))))
