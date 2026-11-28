(in-package #:autolith)

(define-condition acp-extension-unavailable
    (application-client-unavailable)
    nil
  (:documentation "An ACP frontend operation is unavailable."))

(-> acp-extension-unavailable-reason (acp-extension-unavailable) string)
(defun acp-extension-unavailable-reason (condition)
  "Return the shared reason for an unavailable frontend operation."
  (application-client-unavailable-reason condition))

(-> acp-extension--copy (t) t)
(defun acp-extension--copy (value)
  "Copy a bounded boundary value before interpreting its native or JSON shape."
  (handler-case
      (portable-value-map
       value
       (lambda (item child)
         (typecase item
           (null
            nil)
           ((satisfies argo:json-false-p)
            (argo:json-false))
           (string
            (when (> (length item) 262144)
              (error 'acp-extension-unavailable :reason "The extension string exceeds its character limit."))
            (copy-seq item))
           ((or symbol rational float character pathname)
            item)
           (hash-table
            (let ((copy (make-hash-table :test #'equal)))
              (maphash (lambda (key member)
                         (unless (or (stringp key) (symbolp key))
                           (error 'acp-extension-unavailable :reason "An extension object key is invalid."))
                         (setf (gethash (funcall child key) copy) (funcall child member)))
                       item)
              copy))
           (vector
            (map 'vector child item))
           (t
            (error 'acp-extension-unavailable :reason "The value has no extension representation."))))
       :containers-p t :proper-lists-p t :list #'identity
       :maximum-depth 32 :maximum-nodes 16384 :list-node-policy ':container)
    (portable-value-error ()
      (error 'acp-extension-unavailable :reason "The extension value exceeds its structural limit."))))

(-> acp-extension--key (symbol) string)
(defun acp-extension--key (key)
  "Spell a native field as an ACP camelCase member, omitting a predicate suffix."
  (let* ((name (string-downcase (symbol-name key)))
         (end  (if (uiop:string-suffix-p name "-p") (- (length name) 2) (length name))))
    (with-output-to-string (stream)
      (loop with uppercase-p = nil
            for character across (subseq name 0 end)
            do (if (char= character #\-)
                   (setf uppercase-p t)
                   (progn
                     (write-char (if uppercase-p (char-upcase character) character) stream)
                     (setf uppercase-p nil)))))))

(-> acp-extension-value (t) t)
(defun acp-extension-value (value)
  "Encode bounded native values, retaining ACP arrays, false booleans and nulls."
  (labels ((plist-p (item)
             (and (consp item) (evenp (length item))
                   (loop for key in item by (partial #'nthcdr 2) always (keywordp key))))

           (encode (item &optional field parent)
             (cond
               ((or (and field (uiop:string-suffix-p (string-downcase (symbol-name field)) "-p"))
                    (member field '(:accepted :complete :truncated))
                    (and (eq field ':value) (equal (getf parent :type) "boolean")))
                (if (and item (not (argo:json-false-p item))) t (argo:json-false)))
               ((and (eq field ':argument-schema) (consp item) (eq (getf item :type) ':list))
                (agentcomms:json-object "type" "array" "items" (agentcomms:json-object)))
               ((member field '(:items :options :choices :aliases :descendant-ids :shell-logs :updates))
                (map 'vector (lambda (member)
                               (if (and (eq field ':options) (equal (getf parent :type) "boolean"))
                                   (if member t (argo:json-false))
                                   (encode member)))
                     item))
               ((null item)
                nil)
               ((or (argo:json-false-p item) (eq item ':json-false))
                (argo:json-false))
               ((eq item ':null)
                nil)
               ((eq item t)
                t)
               ((stringp item)
                item)
               ((and (eq field ':state) (symbolp item))
                (case item
                  (:queued
                   "pending")
                  (:finalizing
                   "running")
                  (:success
                   "completed")
                  (:aborted
                   "cancelled")
                  (otherwise
                   (string-downcase (symbol-name item)))))
               ((and (eq field ':next-cursor) (integerp item))
                (princ-to-string item))
               ((and (eq field ':source) (getf parent :name) (getf parent :type))
                (case item
                  (:override
                   "explicit")
                  (:durable
                   "saved")
                  (otherwise
                   (string-downcase (symbol-name item)))))
               ((symbolp item)
                (string-downcase (symbol-name item)))
               ((and (realp item) (not (or (integerp item) (floatp item))))
                (princ-to-string item))
               ((pathnamep item)
                (namestring item))
               ((characterp item)
                (string item))
               ((hash-table-p item)
                (let ((object (agentcomms:json-object)))
                  (maphash (lambda (key member)
                             (setf (gethash (if (symbolp key) (acp-extension--key key) key) object)
                                   (encode member)))
                           item)
                  object))
               ((plist-p item)
                (let ((object (agentcomms:json-object)))
                 (loop for (key member) on item by (partial #'nthcdr 2)
                       do (setf (gethash (acp-extension--key key) object)
                                (encode member key item)))
                 object))
               ((or (consp item) (vectorp item))
                (map 'vector #'encode item))
               (t
                item))))
    (encode (acp-extension--copy value))))

(-> acp-extension--native-value (t &key (:command-p boolean) (:preserve-booleans-p boolean)) t)
(defun acp-extension--native-value (value &key command-p preserve-booleans-p)
  "Decode JSON, using explicit keyword tags for commands and readable false markers for replay."
  (labels ((decode (item)
             (cond
               ((null item)
                (unless command-p ':null))
               ((argo:json-false-p item)
                (when preserve-booleans-p ':json-false))
               ((and command-p (hash-table-p item) (nth-value 1 (gethash "keyword" item)))
                (let ((name (gethash "keyword" item)))
                  (unless (and (= 1 (hash-table-count item)) (non-empty-string-p name))
                    (agentcomms:acp-invalid-params "A keyword value requires exactly one nonempty string member."))
                  (intern (string-upcase name) :keyword)))
               ((hash-table-p item)
                (let ((object (make-hash-table :test #'equal)))
                  (maphash (lambda (key member) (setf (gethash key object) (decode member))) item)
                  object))
               ((and (vectorp item) (not (stringp item)))
                (map 'vector #'decode item))
               (t
                item))))
    (decode (acp-extension--copy value))))

(-> acp-extension-result (keyword &key (:value t) (:report (option string))
                                     (:condition-type (option string))) hash-table)
(defun acp-extension-result (outcome &key value report condition-type)
  "Construct a typed ACP outcome without retaining a condition or executable form."
  (agentcomms:json-object
   "outcome" (string-downcase (symbol-name outcome))
   "value" (when (eq outcome ':ok) (acp-extension-value value))
   "report" (when report (subseq report 0 (min 2000 (length report))))
   "conditionType" condition-type))

(-> acp-extension--arguments (keyword hash-table &optional (option keyword)) list)
(defun acp-extension--arguments (command params &optional operation-kind)
  "Decode one extension request to the native handler's actual argument list."
  (flet ((field (name &optional type required-p)
           (if type
               (agentcomms:acp-field params name :type type :required-p required-p)
               (agentcomms:acp-field params name :required-p required-p)))

         (identifier ()
           (agentcomms:acp-field params "jobId" :type ':string :required-p t)))
    (case command
      ((:client-state :settings :models :operations)
       nil)
      (:set-setting
       (let* ((name  (intern (string-upcase (field "name" ':string t)) :keyword))
              (value (acp-extension--native-value (field "value" nil t))))
         (list name (if (eq value ':null) nil value))))
      (:invoke
       (let ((name      (field "operation" ':string t))
             (arguments (field "arguments")))
         (list name
               (ecase operation-kind
                 (:command
                  (unless (or (null arguments) (and (vectorp arguments) (not (stringp arguments))))
                    (agentcomms:acp-invalid-params "Command arguments must be an array."))
                  (map 'list (lambda (value) (acp-extension--native-value value :command-p t)) arguments))
                 (:tool
                  (unless (or (null arguments) (hash-table-p arguments))
                    (agentcomms:acp-invalid-params "Tool arguments must be an object."))
                  (loop for key being the hash-keys of (or arguments (agentcomms:json-object))
                        using (hash-value value)
                        append (list key (acp-extension--native-value value))))))))
      (:conversations
       (list :cwd (field "cwd" ':string) :cursor (field "cursor") :limit (field "limit" ':integer)))
      (:jobs
       (list :cursor (field "cursor") :limit (field "limit" ':integer)))
      (:job
       (list (identifier)))
      (:job-transcript
       (let ((output (or (field "output" ':string) "text")))
         (unless (member output '("text" "structured") :test #'string=)
           (agentcomms:acp-invalid-params "Transcript output must be text or structured."))
         (list (identifier) :output (if (string= output "text") ':text ':structured)
               :offset (or (field "offset" ':integer) 0) :limit (field "limit" ':integer))))
      (:job-cancel
       (list (identifier)))
      (:resource
       (list (field "uri" ':string t)
             :start-line (field "startLine" ':integer) :line-count (field "lineCount" ':integer)
             :byte-offset (field "byteOffset" ':integer) :byte-count (field "byteCount" ':integer))))))

(-> acp-extension-call (acp-session keyword hash-table) list)
(defun acp-extension-call (session command params)
  "Translate ACP parameters and call a native channel command."
  (let ((operation-kind nil))
    (when (eq command ':invoke)
      (let* ((name    (agentcomms:acp-field params "operation" :type ':string :required-p t))
             (catalog (getf (channel-subscription-call session ':operations) :result)))
        (unless (eq (getf catalog :outcome) ':ok)
          (return-from acp-extension-call catalog))
        (let ((row (find name (getf (getf catalog :value) :items)
                         :test #'string-equal :key (lambda (item) (getf item :name)))))
          (unless row
            (return-from acp-extension-call
              (list :outcome :unsupported :report "The requested operation does not exist.")))
          (setf operation-kind (getf row :kind)))))
    (getf (apply #'channel-subscription-call session command
                 (acp-extension--arguments command params operation-kind))
          :result)))

(defparameter *acp-extension-methods*
  '(("_autolith/state" :client-state :server)
    ("_autolith/settings" :settings :server)
    ("_autolith/models" :models :server)
    ("_autolith/set-setting" :set-setting :server)
    ("_autolith/operations" :operations :server)
    ("_autolith/invoke" :invoke :server)
    ("_autolith/conversations" :conversations :server)
    ("_autolith/jobs" :jobs :server)
    ("_autolith/job" :job :server)
    ("_autolith/job-transcript" :job-transcript :server)
    ("_autolith/job-cancel" :job-cancel :server)
    ("_autolith/resource" :resource :server)
    ("_autolith/steer" acp-extension-steer :client)
    ("_autolith/queue" acp-extension-queue :client))
  "ACP wire methods and their native commands or local handlers.")

(-> acp-extension-capabilities () hash-table)
(defun acp-extension-capabilities ()
  "Describe the installed ACP wire handlers."
  (agentcomms:json-object
   "schemaVersion" 1
   "methods" (coerce (cons "_autolith/capabilities"
                            (loop for (name handler kind) in *acp-extension-methods*
                                  when (or (eq kind ':server) (fboundp handler))
                                    collect name))
                     'vector)
   "features" (agentcomms:json-object
                "events" t "sessionScoped" t
                "turnEvents" t
                "resourceSchemes" (vector "workspace" "shell-log")
                "automaticContinuation" t)))
