(in-package #:autolith)

;;;; -- Prompt-Cache Misses --

(defparameter *prompt-cache-lifetime-seconds* 300
  "The idle gap after which provider prompt caches are assumed to have expired.")

(defparameter *prompt-cache-miss-minimum-tokens* 1024
  "The smallest previously cached prompt whose loss is worth reporting.")

(defparameter *prompt-cache-miss-ratio* 1/2
  "Cached tokens below this share of the previous prompt count as a miss.")

(deftype prompt-cache-miss-cause ()
  "Why a provider request re-read context the prompt cache should have served."
  '(member :model-changed :idle :context-rewritten :resumed :prefix-changed))

(defclass prompt-cache-baseline ()
  ((prompt-tokens
    :initarg :prompt-tokens
    :reader prompt-cache-baseline-prompt-tokens
    :type (integer 0)
    :documentation "The full prompt size of the request that primed the cache.")
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
                        :completed-at completed-at
                        :model model))))

(-> prompt-cache-baseline-from-conversation
    (conversation)
    (option prompt-cache-baseline))
(defun prompt-cache-baseline-from-conversation (conversation)
  "Return the baseline primed by CONVERSATION's newest persisted provider request.

Persisted usage carries neither time nor model, so a miss against this baseline
is attributed to resuming the conversation."
  (let ((baseline nil))
    (conversation-map-records
     conversation
     (lambda (record)
       (when (eq (first record) :provider)
         (let ((candidate
                 (prompt-cache-baseline-create
                  (getf (getf (rest record) :metadata) :usage))))
           (when candidate
             (setf baseline candidate))))))
    baseline))

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

(-> prompt-cache-miss-detect
    ((option prompt-cache-baseline) t
     &key (:started-at (option timestamp)) (:model (option string)))
    (option list))
(defun prompt-cache-miss-detect (baseline usage &key started-at model)
  "Return a miss plist when USAGE re-read most of BASELINE's prompt uncached.

USAGE is the completed request's portable usage; STARTED-AT and MODEL describe
that request. The plist carries :RE-READ-TOKENS, :PROMPT-TOKENS,
:CACHED-TOKENS, :CAUSE, and :IDLE-SECONDS. Requests without cache counters,
baselines below *PROMPT-CACHE-MISS-MINIMUM-TOKENS*, and cache reads reaching
*PROMPT-CACHE-MISS-RATIO* of the previous prompt report nothing."
  (block nil
    (unless baseline
      (return nil))
    (let ((prompt-tokens (conversation--usage-field usage "input_tokens"))
          (cached-tokens (conversation--usage-field usage "cached_input_tokens"))
          (expected (prompt-cache-baseline-prompt-tokens baseline)))
      (unless (and prompt-tokens
                   cached-tokens
                   (>= expected *prompt-cache-miss-minimum-tokens*)
                   (< cached-tokens (* expected *prompt-cache-miss-ratio*)))
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
              :cause (prompt-cache--miss-cause
                      :model-changed-p (and model
                                            baseline-model
                                            (string/= model baseline-model)
                                            t)
                      :idle-seconds idle-seconds
                      :shrunk-p (< prompt-tokens expected)
                      :completed-at completed-at)
              :idle-seconds idle-seconds)))))
