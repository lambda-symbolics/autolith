(in-package #:autolith)

;;;; -- Parent Knowledge Context --

(-> agent-knowledge-context-scope (configuration) (option agent-record-resource-scope))
(defun agent-knowledge-context-scope (configuration)
  "Return installed parent services without consulting any private path."
  (with-recursive-lock-held (*agent-record-resource-scopes-lock*)
    (gethash configuration *agent-record-resource-scopes*)))

(-> agent-knowledge-context--request (agent-record-resource-scope keyword hash-table) hash-table)
(defun agent-knowledge-context--request (scope operation fields)
  "Send bounded data to the negotiated knowledge service, with no local fallback."
  (let ((adapter (agent-record-resource-scope-knowledge scope)))
    (unless adapter
      (error 'agent-protocol-knowledge-error :code ':operation-unsupported))
    (agent-protocol-knowledge-response-fields
     (agent-protocol-knowledge-request adapter operation :scope ':workspace :fields fields))))

(-> agent-knowledge-related-context (request-context agent-record-resource-scope) list)
(defun agent-knowledge-related-context (request scope)
  "Construct normal context contributions from parent-checked recall data."
  (let* ((input (request-context-latest-user-text request))
         (query (and (non-empty-string-p input) (memory-context--query input))))
    (when (and (not (request-context-compaction-p request)) query
               (agent-record-resource-scope-knowledge scope))
      (let* ((configuration (request-context-configuration request))
             (conversation  (request-context-conversation request))
             (data          (agent-knowledge-context--request
                             scope ':context
                             (json-object "query" (subseq query 0 (min 4096 (length query)))
                                          "turn" (memory-context--turn-key conversation)
                                          "active" (knowledge-active-p configuration))))
             (memory-lines  (gethash "memory_lines" data))
             (fact-lines    (gethash "fact_lines" data))
             (stale         (gethash "stale_count" data))
             (unverified    (gethash "unverified_count" data))
             (change-count  (gethash "change_count" data))
             (changes       (gethash "changes" data)))
        (unless (and (simple-vector-p memory-lines) (<= (length memory-lines) 6)
                     (every #'stringp memory-lines)
                     (simple-vector-p fact-lines) (<= (length fact-lines) 6)
                     (every #'stringp fact-lines)
                     (typep stale '(integer 0 6)) (typep unverified '(integer 0 6))
                     (typep change-count '(integer 0)) (stringp changes))
          (error 'agent-protocol-knowledge-error :code ':invalid-response))
        (knowledge-usage-note
         (conversation-identifier conversation)
         :facts (length fact-lines) :fact-characters (reduce #'+ fact-lines :key #'length)
         :changes change-count :change-characters (length changes))
        (remove
         nil
         (list
          (when (or (plusp (length memory-lines)) (plusp (length fact-lines)))
            (change-class
             (make-context-contribution
              :identifier  "related-memories"
              :instruction (memory-context--instruction
                            :memories (length memory-lines) :facts (length fact-lines)
                            :status-p (plusp (+ stale unverified)))
              :evidence    (memory-context--evidence (coerce memory-lines 'list)
                                                     (coerce fact-lines 'list))
              :priority 25 :lifetime ':turn :deduplication-key "related-memories")
             'memory-context-contribution :count (length memory-lines)
             :stale-count stale :unverified-count unverified :fact-count (length fact-lines)))
          (when (plusp change-count)
            (change-class
             (make-context-contribution
              :identifier        "knowledge-changes"
              :instruction       "Facts you were given earlier in this conversation changed. Revise any answer or plan that relied on the old values."
              :evidence changes :priority 30 :lifetime ':next-request
              :deduplication-key "knowledge-changes")
             'knowledge-change-contribution :count change-count))))))))

(-> agent-knowledge-context--pattern (t) t)
(defun agent-knowledge-context--pattern (data)
  "Decode portable pattern arrays without reading Lisp or interning text."
  (cond
    ((hash-table-p data)
     (cond
       ((gethash "term_keyword" data)
        (or (find (gethash "term_keyword" data)
                  '(:not :true :false :instant :date :duration :quantity)
                  :test #'equal :key (lambda (item) (string-downcase (symbol-name item))))
            (error 'agent-protocol-knowledge-error :code ':invalid-response)))
       ((gethash "term_ratio" data)
        (let ((ratio (gethash "term_ratio" data)))
          (unless (and (simple-vector-p ratio) (= (length ratio) 2)
                       (every #'integerp ratio) (plusp (aref ratio 1)))
            (error 'agent-protocol-knowledge-error :code ':invalid-response))
          (/ (aref ratio 0) (aref ratio 1))))
       (t
        (error 'agent-protocol-knowledge-error :code ':invalid-response))))
    ((and (vectorp data) (not (stringp data)))
     (map 'list #'agent-knowledge-context--pattern data))
    (t
     data)))

(-> agent-knowledge-expand-tool (tool-context hash-table) tool-result)
(defun agent-knowledge-expand-tool (context arguments)
  "Expand through the parent and save only the public plan in the workspace."
  (let* ((configuration (tool-context-configuration context))
         (scope         (agent-knowledge-context-scope configuration))
         (task          (agent-context--data (tool-argument arguments "task" :required t)))
         (data          (agent-knowledge-context--request scope ':expand (json-object "task" task)))
         (status        (gethash "status" data))
         (details       (gethash "details" data)))
    (unless (and (member status '("planned" "unknown-task" "no-plan" "exhausted") :test #'equal)
                 (hash-table-p details))
      (error 'agent-protocol-knowledge-error :code ':invalid-response))
    (cond
      ((equal status "planned")
       (let* ((raw  (gethash "steps" data))
              (steps
                (map 'list
                     (lambda (step)
                       (unless (and (hash-table-p step) (stringp (gethash "text" step))
                                    (equal (gethash "status" step) "pending")
                                    (stringp (gethash "id" step)))
                         (error 'agent-protocol-knowledge-error :code ':invalid-response))
                       (list :text (gethash "text" step) :status "pending" :id (gethash "id" step)
                             :requires (agent-knowledge-context--pattern (gethash "requires" step #()))
                             :verify   (agent-knowledge-context--pattern (gethash "verify" step #()))))
                     raw))
              (plan (plan-update configuration steps
                                 :explanation (format nil "Expanded ~A by its methods."
                                                      (knowledge-pattern-text (coerce task 'list))))))
          (multiple-value-bind (result installed-p)
              (knowledge-plan-tool-remote context (json-object) ':list)
            (if installed-p result (tool-success (plan-render plan))))))
      ((equal status "unknown-task")
       (tool-success
        (format nil "No method describes ~{~A~^, ~}; the plan is unchanged. Define one with method-define on memory:facts, or write the plan with plan.update."
                (coerce (gethash "tasks" details #()) 'list))))
      ((equal status "no-plan")
       (tool-success
        (format nil "No method for ~A applies to the current facts; the plan is unchanged.~{~%~A~}"
                (knowledge-pattern-text (coerce task 'list))
                (map 'list (lambda (entry)
                             (format nil "method ~A needs ~{~A~^ and ~}"
                                     (gethash "method" entry)
                                     (coerce (gethash "conditions" entry) 'list)))
                     (gethash "unmet" details #())))))
      (t
       (tool-success
        (format nil "Expanding ~A tried ~D tasks without finishing; the plan is unchanged. The methods may recurse or branch too widely."
                (knowledge-pattern-text (coerce task 'list)) (gethash "nodes" details)))))))

(defmethod memory-context-remote ((request request-context) (configuration configuration))
  "Use parent recall for an installed connection, including an empty response."
  (let ((scope (agent-knowledge-context-scope configuration)))
    (if scope
        (values (agent-knowledge-related-context request scope) t)
        (values nil nil))))

(defmethod knowledge-expand-tool-remote ((context tool-context) (arguments hash-table))
  "Use parent expansion for an installed connection without local graph reads."
  (if (agent-knowledge-context-scope (tool-context-configuration context))
      (values (agent-knowledge-expand-tool context arguments) t)
      (values nil nil)))
