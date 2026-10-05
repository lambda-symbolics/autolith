(in-package #:autolith)

;;;; -- Explicit Continuity Decisions --

(defclass task-continuity-tool (task-job-tool)
  ()
  (:documentation "Owned durable job classification and explicit recovery decisions."))

(-> task-continuity-parameters () json-object)
(defun task-continuity-parameters ()
  "Return the public job.continuity parameter schema."
  (json-object
   "type" "object"
   "properties"
   (json-object
    "action" (json-object "type" "string" "enum"
                          #("list" "get" "allow-restart" "checkpoint" "revive" "abandon"))
    "id" (json-object "type" "string")
    "offset" (json-object "type" "integer" "minimum" 0)
    "limit" (json-object "type" "integer" "minimum" 1 "maximum" 100)
    "safety" (json-object "type" "string" "maxLength" 4000)
    "image" (json-object "type" "string")
    "repl" (json-object "type" "string")
    "authorize" (json-object "type" "boolean"))
   "required" #("action") "additionalProperties" (json-false)))

(-> task-continuity-page
    (agent task-orchestrator &key (:offset integer) (:limit integer)) list)
(defun task-continuity-page (viewer orchestrator &key (offset 0) (limit 20))
  "Return a count- and character-bounded page of whole readable inventory rows.

Offsets refer to execution-ID order in the current inventory, not a snapshot."
  (unless (and (integerp offset) (<= 0 offset)
               (integerp limit) (<= 1 limit 100))
    (task-continuity--fail "Continuity offset or limit is outside its bounds."))
  (let* ((rows (stable-sort (task-continuity-inventory viewer orchestrator)
                            #'string< :key (lambda (row) (or (getf row :execution-id) ""))))
         (total (length rows))
         (start (min offset total))
         (selected nil))
    (labels ((page (entries)
               (let ((next (+ start (length entries))))
               (list :continuity-list t :offset start :next-offset next
                       :total total :more-p (< next total) :entries entries))))
      (loop for row in (nthcdr start rows)
            repeat limit
            for trial = (append selected (list row))
            if (<= (length (task--write-readable-sexp (page trial)))
                   *task-tool-content-limit*)
              do (setf selected trial)
            else do (when (null selected)
                      (task-continuity--fail
                       "One continuity row exceeds the native output bound; inspect its artifact."))
                    (return))
      (page selected))))

(defmethod tool-execute ((tool task-continuity-tool) (context tool-context) arguments)
  "Classify or decide recovery through the same owned job boundary."
  (let* ((viewer (tool-context-agent context))
         (orchestrator (task-job-tool-orchestrator tool))
         (action (tool-argument arguments "action" :required t))
         (identifier (tool-argument arguments "id")))
    (unless (typep viewer 'agent)
      (task-continuity--fail "Continuity requires an executing conversation owner."))
    (unless (or (equal action "list") (non-empty-string-p identifier))
      (task-continuity--fail "This action requires a job or execution ID."))
    (when (equal action "revive")
      (return-from tool-execute
        (task-continuity-revive
         context orchestrator identifier
         :authorized-p (eq t (tool-argument arguments "authorize")))))
    (let ((record
            (cond
              ((equal action "list")
               (task-continuity-page viewer orchestrator
                                     :offset (or (tool-argument arguments "offset") 0)
                                     :limit (or (tool-argument arguments "limit") 20)))
              ((equal action "get")
               (task-continuity-classify
                (task-continuity--find viewer identifier) viewer orchestrator))
              ((equal action "allow-restart")
               (unless (eq t (tool-argument arguments "authorize"))
                 (task-continuity--fail "Safety declaration requires explicit authority."))
               (task-continuity-declare
                viewer orchestrator identifier
                :safety (tool-argument arguments "safety" :required t)))
              ((equal action "checkpoint")
               (unless (eq t (tool-argument arguments "authorize"))
                 (task-continuity--fail "Checkpoint declaration requires explicit authority."))
               (task-continuity-declare
                viewer orchestrator identifier
                :image (tool-argument arguments "image" :required t)
                :repl (tool-argument arguments "repl" :required t)))
              ((equal action "abandon")
               (unless (eq t (tool-argument arguments "authorize"))
                 (task-continuity--fail "Abandonment requires explicit authority."))
               (task-continuity-abandon viewer orchestrator identifier))
              (t
               (task-continuity--fail "Unknown continuity action.")))))
      (task-tool-result
       (task--write-readable-sexp record) record))))
