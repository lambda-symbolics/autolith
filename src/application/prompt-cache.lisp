(in-package #:autolith)

;;;; -- Prompt-Cache Misses --

(defparameter *prompt-cache-lifetime-seconds* 300
  "The inactivity heuristic used to explain cache misses, not a provider expiry guarantee.")

(defparameter *prompt-cache-stall-minimum-characters* 16384
  "The minimum shared history growth behind non-advancing cache reads before a stall notice.")

(defparameter *prompt-cache-stall-ratio* 1/16
  "The reference history's share that shared growth must reach before a stall notice.")

(defparameter *prompt-cache-block-tokens* 128
  "The provider's cache granularity: the smallest drop in cache reads that counts as lost prefix.")

(deftype prompt-cache-anomaly-kind ()
  "How cache reads compare with the previous request's.

:SHRUNK means fewer tokens were read than before, so a cached prefix was lost.
:STALLED means reads did not advance while shared history grew substantially."
  '(member :shrunk :stalled))

(deftype prompt-cache-miss-cause ()
  "A local observation accompanying low cache reads, or an unknown cause."
  '(member :model-changed :idle :prompt-shrank :resumed :unknown))

(defclass prompt-cache-episode ()
  ((conversation
    :initarg       :conversation
    :reader        prompt-cache-episode-conversation
    :type          conversation
    :documentation "The exact conversation served by this request.")
   (provider
    :initarg       :provider
    :reader        prompt-cache-episode-provider
    :type          model-provider
    :documentation "The provider instance whose cache may contain the prompt.")
   (model
    :initarg       :model
    :reader        prompt-cache-episode-model
    :type          string
    :documentation "The model selected when the request started.")
   (process-id
    :initform      (current-process-id)
    :reader        prompt-cache-episode-process-id
    :type          integer
    :documentation "The process that established the local monotonic cache clock.")
   (started-at
    :initarg       :started-at
    :reader        prompt-cache-episode-started-at
    :type          real
    :documentation "The monotonic request start in seconds, before response generation.")
   (prompt-tokens
    :initform      nil
    :accessor      prompt-cache-episode-prompt-tokens
    :type          (option (integer 0))
    :documentation "The completed request's prompt size, or NIL while in flight.")
   (history-characters
    :initform      0
    :accessor      prompt-cache-episode-history-characters
    :type          (integer 0)
    :documentation "The bounded textual history size eligible for summarization.")
   (sequence
    :initform      nil
    :accessor      prompt-cache-episode-sequence
    :type          (option (integer 0))
    :documentation "The durable conversation boundary after request completion."))
  (:documentation "One process-local cache opportunity, consumed before idle compaction."))

(defclass prompt-cache-baseline ()
  ((prompt-tokens
    :initarg       :prompt-tokens
    :reader        prompt-cache-baseline-prompt-tokens
    :type          (integer 0)
    :documentation "The previous request's full prompt size, including temporary context.")
   (cached-tokens
    :initarg       :cached-tokens
    :initform      nil
    :reader        prompt-cache-baseline-cached-tokens
    :type          (option (integer 0))
    :documentation "How much of that prompt the cache already served, when reported.")
   (loss-reference-tokens
    :initarg       :loss-reference-tokens
    :initform      nil
    :reader        prompt-cache-baseline-loss-reference-tokens
    :type          (option (integer 0))
    :documentation "The cache-read reference for accumulating unreported losses, or NIL to use actual reads.")
   (prefix
    :initarg       :prefix
    :initform      nil
    :reader        prompt-cache-baseline-prefix
    :type          list
    :documentation "The previous request's reusable-prefix fingerprints, or NIL when unknown.")
   (stall-reference-prefix
    :initarg       :stall-reference-prefix
    :initform      nil
    :reader        prompt-cache-baseline-stall-reference-prefix
    :type          list
    :documentation "The reusable prefix when reads last advanced or a stall was reported.")
   (completed-at
    :initarg  :completed-at
    :initform nil
    :reader   prompt-cache-baseline-completed-at
    :type     (option timestamp)
    :documentation
    "When that request completed, or NIL when it belongs to a resumed conversation.")
   (model
    :initarg       :model
    :initform      nil
    :reader        prompt-cache-baseline-model
    :type          (option string)
    :documentation "The model that served that request, or NIL when unknown."))
  (:documentation "Previous request measurements, not proof of a reusable prefix."))

(-> prompt-cache-baseline-create
    (t &key (:completed-at (option timestamp)) (:model (option string)) (:prefix list))
    (option prompt-cache-baseline))
(defun prompt-cache-baseline-create (usage &key completed-at model prefix)
  "Return request measurements from USAGE and PREFIX, or NIL without a prompt size."
  (let ((prompt-tokens (conversation--usage-field usage "input_tokens")))
    (and prompt-tokens
         (make-instance 'prompt-cache-baseline
                        :prompt-tokens          prompt-tokens
                        :cached-tokens          (conversation--usage-field usage "cached_input_tokens")
                        :prefix                 prefix
                        :stall-reference-prefix prefix
                        :completed-at           completed-at
                        :model                  model))))

(-> prompt-cache-baseline-advance
    ((option prompt-cache-baseline) t
     &key (:completed-at (option timestamp)) (:model (option string))
          (:prefix list) (:reported-p boolean))
    (option prompt-cache-baseline))
(defun prompt-cache-baseline-advance
    (baseline usage &key completed-at model prefix reported-p)
  "Return the baseline following BASELINE after a request completing with USAGE.

Keep actual reads separate from accumulated small losses. Preserve the stall
reference only while reads fail to advance and that prefix is still present."
  (let* ((next            (prompt-cache-baseline-create usage :completed-at completed-at
                                                            :model model :prefix prefix))
         (previous-cached (and baseline (prompt-cache-baseline-cached-tokens baseline)))
         (next-cached     (and next (prompt-cache-baseline-cached-tokens next)))
         (reference       (and baseline (prompt-cache-baseline-stall-reference-prefix baseline))))
    (if (and next baseline (not reported-p) previous-cached next-cached
             (<= next-cached previous-cached))
        (make-instance 'prompt-cache-baseline
                       :prompt-tokens (prompt-cache-baseline-prompt-tokens next)
                       :cached-tokens next-cached
                       :loss-reference-tokens
                       (or (prompt-cache-baseline-loss-reference-tokens baseline)
                           previous-cached)
                       :prefix        prefix
                       :stall-reference-prefix
                       (if (provider-cache-prefix-extends-p reference prefix) reference prefix)
                       :completed-at  completed-at
                       :model         model)
        next)))

(-> prompt-cache-baseline-from-conversation
    (conversation)
    (option prompt-cache-baseline))
(defun prompt-cache-baseline-from-conversation (conversation)
  "Return the baseline from CONVERSATION's checkpointed provider usage.

Persisted usage carries neither time nor model. A report against this baseline
can identify a resumed session, but cannot establish why cache reads were low."
  (prompt-cache-baseline-create (conversation-prompt-cache-usage conversation)))

(-> prompt-cache--miss-cause
    (&key (:model-changed-p boolean)
          (:idle-seconds (option (integer 0)))
          (:shrunk-p boolean)
          (:completed-at (option timestamp)))
    prompt-cache-miss-cause)
(defun prompt-cache--miss-cause
    (&key model-changed-p idle-seconds shrunk-p completed-at)
  "Return a known local observation, never infer a prefix change from token counts."
  (cond
    (model-changed-p
     ':model-changed)
    ((and idle-seconds (> idle-seconds *prompt-cache-lifetime-seconds*))
     ':idle)
    (shrunk-p
     ':prompt-shrank)
    ((null completed-at)
     ':resumed)
    (t
     ':unknown)))

(-> prompt-cache--usage-entry (t string) t)
(defun prompt-cache--usage-entry (usage name)
  "Return the nested value NAME carries in portable or wire USAGE data."
  (cond
    ((json-object-p usage)
     (json-get usage name))
    ((listp usage)
     (second (assoc name usage :test #'equal)))
    (t
     nil)))

(-> prompt-cache--attribution-uncached (t string) (option (integer 0)))
(defun prompt-cache--attribution-uncached (usage field)
  "Return measured uncached tokens for request FIELD from server attribution.

Attribution separates instruction and tool usage when the provider reports it.
It does not establish why those tokens were uncached. NIL means no attribution."
  (let* ((attribution (prompt-cache--usage-entry usage "attribution"))
         (fields      (and attribution
                           (prompt-cache--usage-entry attribution "request_fields")))
         (entry       (and fields (prompt-cache--usage-entry fields field)))
         (input       (and entry (prompt-cache--usage-entry entry "input_tokens")))
         (cached      (and entry (prompt-cache--usage-entry entry "cached_tokens"))))
    (when (and (integerp input) (integerp cached))
      (max 0 (- input cached)))))

(-> prompt-cache--anomaly
    (prompt-cache-baseline (integer 0) list)
    (values (option prompt-cache-anomaly-kind) (option (integer 0)) (integer 0)))
(defun prompt-cache--anomaly (baseline cached-tokens prefix)
  "Return anomaly kind, lost cache reads, and shared history growth in characters.

A shrinking cache loses measured reads. A stall requires a preserved reference
prefix and growth shared by successive requests, excluding both temporary tails
and newly offered history. Token counts for that growth are unknown. Without
previous reads or prefix fingerprints, do not infer a loss from full input."
  (let* ((previous-cached      (prompt-cache-baseline-cached-tokens baseline))
         (loss-reference       (or (prompt-cache-baseline-loss-reference-tokens baseline)
                                   previous-cached))
         (reference            (prompt-cache-baseline-stall-reference-prefix baseline))
         (reference-characters (provider-cache-prefix-characters reference))
         (growth
           (max 0 (- (provider-cache-prefix-shared-characters
                      (prompt-cache-baseline-prefix baseline) prefix)
                     reference-characters))))
    (cond
      ((or (null previous-cached) (> cached-tokens previous-cached))
       (values nil nil 0))
      ((>= (- loss-reference cached-tokens) *prompt-cache-block-tokens*)
       (values ':shrunk (- loss-reference cached-tokens) 0))
      ((and (provider-cache-prefix-extends-p reference prefix)
            (>= growth (max *prompt-cache-stall-minimum-characters*
                            (floor (* reference-characters *prompt-cache-stall-ratio*)))))
       (values ':stalled nil growth))
      (t
       (values nil nil 0)))))

(-> prompt-cache-miss-detect
    ((option prompt-cache-baseline) t
     &key (:started-at (option timestamp)) (:model (option string)) (:prefix list))
    (option list))
(defun prompt-cache-miss-detect (baseline usage &key started-at model prefix)
  "Return comparative cache measurements for completed USAGE and projected PREFIX.

:LOST-TOKENS is measured lost cache reads for :SHRUNK, NIL for :STALLED.
:HISTORY-GROWTH-CHARACTERS measures newly shared history behind a stalled cache,
not uncached tokens. :CAUSE is a local observation, not a proven cache cause.
Requests without comparable counters or sufficient evidence return NIL.
:WIRE-CHANGE compares the final requests as sent, separately from usage."
  (block nil
    (unless baseline
      (return nil))
    (let* ((prompt-tokens (conversation--usage-field usage "input_tokens"))
           (cached-tokens (conversation--usage-field usage "cached_input_tokens"))
           (expected      (prompt-cache-baseline-prompt-tokens baseline)))
      (unless (and prompt-tokens cached-tokens)
        (return nil))
      (multiple-value-bind (kind lost-tokens history-growth-characters)
          (prompt-cache--anomaly baseline cached-tokens prefix)
        (unless kind
          (return nil))
        (let* ((completed-at   (prompt-cache-baseline-completed-at baseline))
               (baseline-model (prompt-cache-baseline-model baseline))
               (idle-seconds
                 (and completed-at
                      started-at
                      (max 0 (- started-at completed-at)))))
          (list :kind                      kind
                :lost-tokens               lost-tokens
                :history-growth-characters history-growth-characters
                :prompt-tokens             prompt-tokens
                :cached-tokens             cached-tokens
                :previous-cached-tokens    (prompt-cache-baseline-cached-tokens baseline)
                :cause                     (prompt-cache--miss-cause
                                            :model-changed-p (and model
                                                                  baseline-model
                                                                  (string/= model baseline-model)
                                                                  t)
                                            :idle-seconds    idle-seconds
                                            :shrunk-p        (< prompt-tokens expected)
                                            :completed-at    completed-at)
                :idle-seconds              idle-seconds
                :instructions-uncached
                (prompt-cache--attribution-uncached usage "instructions")
                :tools-uncached
                (prompt-cache--attribution-uncached usage "tools")
                :wire-change
                (provider-cache-wire-change
                 (getf (prompt-cache-baseline-prefix baseline) :wire)
                 (getf prefix :wire))))))))
