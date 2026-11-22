(in-package #:autolith)

;;;; -- Procedure Offers --

;;; When the latest request fits a learned procedure's request pattern, the
;;; model is shown the one procedure.run call that makes the same tool calls
;;; the earlier turns made, with the parameters the request supplies. An
;;; admitted procedure the request fits runs before the model's first request,
;;; recorded as that procedure.run call, so the model answers from its results
;;; in one request; admission is the consent to run it this way. A request no
;;; pattern matches may still share its wording with a procedure; such related
;;; procedures are only listed as possible fits for the model to choose among,
;;; with their parameters to fill in, and never run on their own.

(defclass procedure-offer-contribution (context-contribution)
  ((names :initarg       :names
          :reader        procedure-offer-contribution-names
          :type          list
          :documentation "The names of the offered procedures.")
   (ran-p :initarg       :ran-p
          :initform      nil
          :reader        procedure-offer-contribution-ran-p
          :type          boolean
          :documentation "Whether the procedures already ran for this request.")
   (related-p :initarg       :related-p
              :initform      nil
              :reader        procedure-offer-contribution-related-p
              :type          boolean
              :documentation "Whether the procedures only may fit, matched by shared words."))
  (:documentation "Advice offering learned procedures that fit the request, or noting they ran."))

(defclass procedure-offer-event (provider-event)
  ((names :initarg       :names
          :reader        procedure-offer-event-names
          :type          list
          :documentation "The names of the procedures offered to the model.")
   (related-p :initarg       :related-p
              :initform      nil
              :reader        procedure-offer-event-related-p
              :type          boolean
              :documentation "Whether the procedures only may fit the request."))
  (:documentation "A presentation event for offered procedures."))

(defmethod context--copy-contribution :around
    ((contribution procedure-offer-contribution) contributor source)
  "Preserve the offered names when attaching contribution provenance."
  (change-class (call-next-method) 'procedure-offer-contribution
                :names     (procedure-offer-contribution-names contribution)
                :ran-p     (procedure-offer-contribution-ran-p contribution)
                :related-p (procedure-offer-contribution-related-p contribution)))

(defmethod context-contribution-provider-event ((contribution procedure-offer-contribution))
  "Project the offered procedure names; a procedure that already ran shows as its call."
  (unless (procedure-offer-contribution-ran-p contribution)
    (make-instance 'procedure-offer-event
                   :names     (procedure-offer-contribution-names contribution)
                   :related-p (procedure-offer-contribution-related-p contribution))))

(-> procedure-offer-line (list) string)
(defun procedure-offer-line (offer)
  "Return the line offering OFFER: its calls and the procedure.run arguments to use."
  (let ((procedure (getf offer :procedure)))
    (format nil "~A (~:[learned, not yet admitted; running it asks for admission~;admitted~]; calls ~{~A~^, ~}): procedure.run ~A"
            (getf offer :id) (eq (getf offer :status) ':admitted)
            (mapcar (lambda (step) (getf step :tool)) (getf (rest procedure) :steps))
            (procedure-describe-call procedure (getf offer :bindings)))))

(-> procedure-related-offer-line (list) string)
(defun procedure-related-offer-line (offer)
  "Return the line listing related OFFER: its calls, request pattern, and parameters to fill."
  (let* ((procedure  (getf offer :procedure))
         (parameters (getf (rest procedure) :parameters)))
    (format nil "~A (~:[learned, not yet admitted~;admitted~]; calls ~{~A~^, ~}; learned from requests like ~S~:[~;; parameters ~:*~{~A~^, ~}~]; shares ~{~A~^, ~}): procedure.run {\"id\":~S,\"parameters\":{...}}"
            (getf offer :id) (eq (getf offer :status) ':admitted)
            (mapcar (lambda (step) (getf step :tool)) (getf (rest procedure) :steps))
            (or (procedure-input-text procedure) "")
            (mapcar (lambda (parameter)
                      (format nil "~A (~(~A~))" (getf parameter :name) (getf parameter :type)))
                    parameters)
            (getf offer :shared)
            (getf (rest procedure) :id))))

(-> procedure-offer-context (request-context) (option context-contribution))
(defun procedure-offer-context (request)
  "Offer the learned procedures that fit the latest user input in REQUEST.

Once one ran for this request, the model is told so instead of offered it again."
  (let ((configuration (request-context-configuration request))
        (text          (request-context-latest-user-text request)))
    (when (and (not (request-context-compaction-p request))
               (knowledge-active-p configuration)
               (non-empty-string-p text))
      (if-let ((ran (procedure-context--ran-this-turn (request-context-conversation request))))
          (change-class
           (make-context-contribution
            :identifier        "procedure-offers"
            :instruction
            (format nil "Learned procedure~P ~{~A~^, ~} already ran for this request; the procedure.run result reports every call it made. Answer from it, or continue with ordinary calls where it stopped."
                    (length ran) ran)
            :priority          25
            :lifetime          ':turn
            :deduplication-key "procedure-offers")
           'procedure-offer-contribution
           :names ran
           :ran-p t)
          (let ((offers  (handler-case (procedure-offers configuration text)
                           (knowledge-error ()
                             nil)))
                (related nil))
            (unless offers
              (setf related (handler-case (procedure-related-offers configuration text)
                              (knowledge-error ()
                                nil))))
            (when related
              (return-from procedure-offer-context
                (change-class
                 (make-context-contribution
                  :identifier        "procedure-offers"
                  :instruction
                  "Procedures Autolith learned from earlier turns may fit this request, though its wording matched none of their request patterns. If one does the work this request needs, call procedure.run with its id and the parameters it lists, taken from the request; otherwise work normally."
                  :evidence          (format nil "~{~A~^~%~}"
                                             (mapcar #'procedure-related-offer-line related))
                  :priority          25
                  :lifetime          ':turn
                  :deduplication-key "procedure-offers")
                 'procedure-offer-contribution
                 :names     (mapcar (lambda (offer) (getf offer :id)) related)
                 :related-p t)))
            (when offers
              (change-class
               (make-context-contribution
                :identifier        "procedure-offers"
                :instruction
                "A procedure Autolith learned from earlier turns fits this request. One procedure.run call with the arguments shown makes the same tool calls those turns made; check its report and answer from it. Work normally instead if the request needs something different."
                :evidence          (format nil "~{~A~^~%~}" (mapcar #'procedure-offer-line offers))
                :priority          25
                :lifetime          ':turn
                :deduplication-key "procedure-offers")
               'procedure-offer-contribution
               :names (mapcar (lambda (offer) (getf offer :id)) offers))))))))

(register-context-contributor "procedure-offers" 'procedure-offer-context :source ':built-in)

(-> agent-learn-procedures (agent agent-observer integer) null)
(defun agent-learn-procedures (agent observer tool-calls)
  "Learn procedures after a primary AGENT's turn that made TOOL-CALLS calls, reporting new ones.

A turn with fewer than two calls teaches nothing, so it reads no traces. A
failure while learning is not the turn's failure: nothing is reported."
  (let ((child-class (find-class 'task-child-agent nil)))
    (when (and (>= tool-calls 2)
               (not (and child-class (typep agent child-class))))
      (when-let ((learned (handler-case (procedure-learn (agent-configuration agent))
                            (error ()
                              nil))))
        (agent-observer-status observer :procedures-learned
                               (list :procedures learned)))))
  nil)

(-> agent-run-fitting-procedure
    (agent agent-observer string &key (:automatic-p boolean) (:tools-p boolean)
                                      (:tool-restriction-p boolean))
    null)
(defun agent-run-fitting-procedure
    (agent observer text &key automatic-p tools-p tool-restriction-p)
  "Run the procedure the user's request TEXT exactly calls for, before the model's first request.

The run is recorded as the procedure.run call the model would have made and
executed through the ordinary tool path, so each call keeps its approval and
the model's first request already sees every result. Only an offer
PROCEDURE-AUTOMATIC-OFFER allows runs this way: admitted, matched word for
word apart from its parameters, and already shown to save requests. Nothing
runs for an automatic turn, a turn without tools or with restricted tools, or
a child agent."
  (let ((configuration (agent-configuration agent))
        (child-class   (find-class 'task-child-agent nil))
        (registry      (agent-tool-registry agent)))
    (when (and tools-p
               (not automatic-p)
               (not tool-restriction-p)
               (not (and child-class (typep agent child-class)))
               registry
               (tool-registry-find registry "procedure" "run")
               (knowledge-active-p configuration))
      (when-let ((offer (handler-case (procedure-automatic-offer configuration text)
                          (knowledge-error ()
                            nil))))
        (let* ((call  (json-object "type" "function_call"
                                   "call_id" (format nil "autolith-procedure-~A" (make-identifier))
                                   "namespace" "procedure"
                                   "name" "run"
                                   "arguments" (procedure-describe-call (getf offer :procedure)
                                                                        (getf offer :bindings))))
               (plans (agent--tool-call-plans agent (list call)))
               (plan  (first plans)))
          (conversation-reconcile-call-identifiers (agent-conversation agent) (list call))
          (agent--append-provider-item agent call
                                       :tool-label  (tool-display-name (getf plan :tool))
                                       :persistence (getf plan :persistence))
          (agent--execute-tool-calls agent plans
                                     (make-instance 'provider-result
                                                    :output-items (list call)
                                                    :tool-calls   (list call))
                                     :observer   observer
                                     :tool-round 0)))))
  nil)

(-> procedure-context--ran-this-turn (conversation) list)
(defun procedure-context--ran-this-turn (conversation)
  "Return the names of the procedures run since CONVERSATION's latest user message."
  (let* ((items (conversation-input-items conversation))
         (start (position-if (lambda (item)
                               (eq (item-role item) ':user))
                             items :from-end t)))
    (loop for item in (and start (nthcdr (1+ start) items))
          for arguments = (and (function-call-item-p item)
                               (equal (json-get item "namespace") "procedure")
                               (equal (json-get item "name") "run")
                               (handler-case (json-decode (json-get item "arguments"))
                                 ((or json-error type-error) ()
                                   nil)))
          when (and (json-object-p arguments) (stringp (json-get arguments "id")))
            collect (json-get arguments "id"))))
