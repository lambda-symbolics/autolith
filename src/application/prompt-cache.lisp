(in-package #:autolith)

;;;; -- Prompt-Cache Misses --

(defparameter *prompt-cache-lifetime-seconds* 300
  "The idle gap after which provider prompt caches are assumed to have expired.")

(defparameter *prompt-cache-miss-minimum-tokens* 1024
  "The smallest previously cached prompt whose loss is worth reporting.")

(defparameter *prompt-cache-miss-ratio* 1/2
  "Cached tokens below this share of the previous prompt count as a miss.")

(defparameter *prompt-cache-stall-minimum-tokens* 4096
  "The smallest re-read of an unchanged prefix that counts as a stalled cache.")

(defparameter *prompt-cache-stall-ratio* 1/16
  "Re-reads at or above this share of the previous prompt count as a stall.")

(defparameter *prompt-cache-block-tokens* 128
  "The provider's cache granularity; smaller differences are rounding.")

(deftype prompt-cache-miss-cause ()
  "Why a provider request re-read context the prompt cache should have served."
  '(member :model-changed :idle :context-rewritten :resumed :prefix-changed
           :prefix-stalled))

(defclass prompt-cache-baseline ()
  ((prompt-tokens
    :initarg :prompt-tokens
    :reader prompt-cache-baseline-prompt-tokens
    :type (integer 0)
    :documentation "The full prompt size of the request that primed the cache.")
   (cached-tokens
    :initarg :cached-tokens
    :initform nil
    :reader prompt-cache-baseline-cached-tokens
    :type (option (integer 0))
    :documentation "How much of that prompt the cache already served, when reported.")
   (completed-at
    :initarg :completed-at
    :initform nil
    :reader prompt-cache-baseline-completed-at
    :type (option timestamp)
    :documentation
    "When that request completed, or NIL when it belongs to a resumed conversation.")
   (model
    :initarg :model
    :initform nil
    :reader prompt-cache-baseline-model
    :type (option string)
    :documentation "The model that served that request, or NIL when unknown."))
  (:documentation "What the next provider request should find in the prompt cache."))

(-> prompt-cache-baseline-create
    (t &key (:completed-at (option timestamp)) (:model (option string)))
    (option prompt-cache-baseline))
(defun prompt-cache-baseline-create (usage &key completed-at model)
  "Return the baseline primed by a request reporting USAGE, or NIL without a prompt size."
  (let ((prompt-tokens (conversation--usage-field usage "input_tokens")))
    (and prompt-tokens
         (make-instance 'prompt-cache-baseline
                        :prompt-tokens prompt-tokens
                        :cached-tokens (conversation--usage-field
                                        usage "cached_input_tokens")
                        :completed-at completed-at
                        :model model))))

(-> prompt-cache-baseline-from-conversation
    (conversation)
    (option prompt-cache-baseline))
(defun prompt-cache-baseline-from-conversation (conversation)
  "Return the baseline from CONVERSATION's checkpointed provider usage.

Persisted usage carries neither time nor model, so a miss against this baseline
is attributed to resuming the conversation."
  (prompt-cache-baseline-create (conversation-prompt-cache-usage conversation)))

(-> prompt-cache--miss-cause
    (&key (:model-changed-p boolean)
          (:idle-seconds (option (integer 0)))
          (:shrunk-p boolean)
          (:completed-at (option timestamp)))
    prompt-cache-miss-cause)
(defun prompt-cache--miss-cause
    (&key model-changed-p idle-seconds shrunk-p completed-at)
  "Return the most specific explanation the request evidence supports."
  (cond
    (model-changed-p
     ':model-changed)
    ((and idle-seconds (> idle-seconds *prompt-cache-lifetime-seconds*))
     ':idle)
    (shrunk-p
     ':context-rewritten)
    ((null completed-at)
     ':resumed)
    (t
     ':prefix-changed)))

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

(-> prompt-cache--attribution-re-read (t string) (option (integer 0)))
(defun prompt-cache--attribution-re-read (usage field)
  "Return the uncached tokens of request FIELD from USAGE's server attribution.

The Codex backend attributes cached and total tokens to the instructions and
tools fields of each request, which separates a changed prefix from history
the cache should have served. NIL means the request carried no attribution."
  (let* ((attribution (prompt-cache--usage-entry usage "attribution"))
         (fields (and attribution
                      (prompt-cache--usage-entry attribution "request_fields")))
         (entry (and fields (prompt-cache--usage-entry fields field)))
         (input (and entry (prompt-cache--usage-entry entry "input_tokens")))
         (cached (and entry (prompt-cache--usage-entry entry "cached_tokens"))))
    (when (and (integerp input) (integerp cached))
      (max 0 (- input cached)))))

(-> prompt-cache--stalled-p
    (prompt-cache-baseline (integer 0) (integer 0))
    boolean)
(defun prompt-cache--stalled-p (baseline prompt-tokens cached-tokens)
  "Return true when the cache stopped growing behind a growing prompt.

A healthy cache serves everything the previous request sent except its
request-local tail, so its reads grow with each request. Reads that stay at
the previous level while a substantial share of the previous prompt is re-read
mean the request reached a backend without the newest prefix."
  (let ((expected (prompt-cache-baseline-prompt-tokens baseline))
        (previous-cached (prompt-cache-baseline-cached-tokens baseline)))
    (and previous-cached
         (> prompt-tokens expected)
         (<= cached-tokens (+ previous-cached *prompt-cache-block-tokens*))
         (>= (- expected cached-tokens)
             (max *prompt-cache-stall-minimum-tokens*
                  (floor (* expected *prompt-cache-stall-ratio*))))
         t)))

(-> prompt-cache-miss-detect
    ((option prompt-cache-baseline) t
     &key (:started-at (option timestamp)) (:model (option string)))
    (option list))
(defun prompt-cache-miss-detect (baseline usage &key started-at model)
  "Return a miss plist when USAGE re-read context BASELINE's prompt had cached.

USAGE is the completed request's portable usage; STARTED-AT and MODEL describe
that request. A miss re-reads most of the previous prompt; a stall re-reads a
smaller share while the cache stops growing behind the prompt. The plist
carries :RE-READ-TOKENS, :PROMPT-TOKENS, :CACHED-TOKENS, :CAUSE,
:IDLE-SECONDS, and the attributed :INSTRUCTIONS-RE-READ and :TOOLS-RE-READ
when the provider reported them. Requests without cache counters, baselines
below *PROMPT-CACHE-MISS-MINIMUM-TOKENS*, and healthy cache reads report
nothing."
  (block nil
    (unless baseline
      (return nil))
    (let* ((prompt-tokens (conversation--usage-field usage "input_tokens"))
           (cached-tokens (conversation--usage-field usage "cached_input_tokens"))
           (expected (prompt-cache-baseline-prompt-tokens baseline))
           (miss-p (and prompt-tokens
                        cached-tokens
                        (>= expected *prompt-cache-miss-minimum-tokens*)
                        (< cached-tokens (* expected *prompt-cache-miss-ratio*))))
           (stall-p (and prompt-tokens
                         cached-tokens
                         (not miss-p)
                         (prompt-cache--stalled-p
                          baseline prompt-tokens cached-tokens))))
      (unless (or miss-p stall-p)
        (return nil))
      (let* ((completed-at (prompt-cache-baseline-completed-at baseline))
             (baseline-model (prompt-cache-baseline-model baseline))
             (idle-seconds
               (and completed-at
                    started-at
                    (max 0 (- started-at completed-at)))))
        (list :re-read-tokens
              (max 0 (- (min prompt-tokens expected) cached-tokens))
              :prompt-tokens prompt-tokens
              :cached-tokens cached-tokens
              :cause (if stall-p
                         ':prefix-stalled
                         (prompt-cache--miss-cause
                          :model-changed-p (and model
                                                baseline-model
                                                (string/= model baseline-model)
                                                t)
                          :idle-seconds idle-seconds
                          :shrunk-p (< prompt-tokens expected)
                          :completed-at completed-at))
              :idle-seconds idle-seconds
              :instructions-re-read
              (prompt-cache--attribution-re-read usage "instructions")
              :tools-re-read
              (prompt-cache--attribution-re-read usage "tools"))))))
