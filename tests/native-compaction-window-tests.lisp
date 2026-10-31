(in-package #:autolith)

;;;; -- Native Compaction Window Contracts --

(-> native-compaction-tests--opaque (string) json-object)
(defun native-compaction-tests--opaque (content)
  "Return a native item containing CONTENT as opaque provider state."
  (json-object "type" "compaction" "encrypted_content" content))

(-> test-native-compaction-window-validation () null)
(defun test-native-compaction-window-validation ()
  "Preserve complete windows and account successful envelopes before rejecting bad items."
  (with-test-configuration (configuration)
    (let* ((provider (provider-create configuration))
           (usage (json-object "input_tokens" 100 "output_tokens" 10))
           (message (json-object "type" "message" "role" "user" "content"
                                 (json-array (json-object "type" "input_text" "text" "exact request"))))
           (call (json-object "type" "function_call" "call_id" "native-call"
                              "name" "probe" "arguments" "{}"))
           (output (function-call-output-item "native-call" "verified result"))
           (opaque (native-compaction-tests--opaque "opaque-one"))
           (window (list message opaque call output
                         (native-compaction-tests--opaque "opaque-two")))
           (events nil))
      (flet ((decode (items)
               "Decode ITEMS and capture each completed usage notification."
               (provider--decode-native-compaction-response
                provider (json-encode (json-object "id" "paid-native" "usage" usage
                                                  "output" (coerce items 'vector)))
                :status 200 :headers nil
                :event-callback (lambda (event) (push event events)))))
        (multiple-value-bind (decoded returned-usage) (decode window)
          (test-assert (= 5 (length decoded)) "the entire native output window survives decoding")
          (test-assert (equal '("message" "compaction" "function_call" "function_call_output" "compaction")
                              (mapcar (lambda (item) (json-get item "type")) decoded))
                       "native output order is exact, including both opaque items")
          (test-assert (= 100 (json-get returned-usage "input_tokens")) "paid usage is returned"))
        (test-assert (= 1 (length events)) "one successful native envelope emits one completion")
        (dolist (body '("" "not-json" "[]" "null"))
          (let ((before (length events)))
            (test-assert
             (handler-case
                 (progn
                   (provider--decode-native-compaction-response
                    provider body :status 200 :headers nil
                    :event-callback (lambda (event) (push event events)))
                   nil)
               (provider-error () t))
             "an undecodable HTTP success body is rejected")
            (test-assert (and (= (1+ before) (length events))
                              (null (native-compaction-completed-event-usage (first events))))
                         "an undecodable completion is metered once with unknown usage")))
        (multiple-value-bind (decoded returned-usage) (decode (list message))
          (test-assert (and (null decoded) (= 100 (json-get returned-usage "input_tokens")))
                       "an opaque-free successful response retains usage for portable fallback"))
        (dolist (bad (list (list opaque (json-object))
                          (list opaque (json-object "type" "message" "role" "user" "content" "bad"))
                          (list opaque (json-object "type" "message" "role" "assistant" "content"
                                                    (json-array (json-object "type" "refusal" "refusal" "no"))))
                          (list opaque output)
                          (list opaque call)
                          (list opaque call call output)))
          (let ((before (length events)))
            (test-assert
             (handler-case (progn (decode bad) nil) (provider-error () t))
             "malformed, refusal and invalid-correlation windows are rejected")
            (test-assert (= (1+ before) (length events))
                         "the rejected paid response was already metered")
            (test-assert (= 100 (json-get (native-compaction-completed-event-usage (first events))
                                         "input_tokens"))
                         "the notification preserves reported usage"))))))
  nil)

(-> test-native-compaction-window-replay () null)
(defun test-native-compaction-window-replay ()
  "Replay full native windows and project portable handoffs across provider families."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration))
           (window (list (native-compaction-tests--opaque "first")
                         (json-object "type" "reasoning" "summary" (json-array)
                                      "encrypted_content" "retained-reasoning")
                         (native-compaction-tests--opaque "last"))))
      (conversation-append-user-message conversation "original")
      (conversation-append-native-compaction conversation window
                                             :family ':responses :summary "portable handoff")
      (let ((loaded (conversation-load-by-id configuration (conversation-identifier conversation))))
        (dolist (family '(:responses :messages))
          (let ((before (conversation-input-items-for-family conversation family))
                (after (conversation-input-items-for-family loaded family)))
            (test-assert (equal (mapcar #'json-encode before) (mapcar #'json-encode after))
                         "reload reproduces the exact family projection")
            (test-assert
             (if (eq family ':responses)
                 (equal (mapcar #'json-encode window) (mapcar #'json-encode after))
                 (and (= 1 (length after))
                      (test-object-contains-string-p after "portable handoff")))
             "the source family receives the full ordered window and other families receive only the portable handoff")
            (test-assert (= (if (eq family ':responses) 2 0)
                            (count-if #'native-compaction-item-p after))
                         "all opaque items belong exclusively to their originating family"))))
      (let ((legacy (conversation-create configuration)))
        (conversation-append-record
         legacy (list :native-compaction :family ':responses
                      :through-seq (1- (conversation-next-sequence legacy))
                      :wire-json (json-encode (native-compaction-tests--opaque "legacy"))
                      :summary "legacy handoff"))
        (let ((loaded (conversation-load-by-id configuration (conversation-identifier legacy))))
          (test-assert (= 1 (count-if #'native-compaction-item-p
                                      (conversation-input-items-for-family loaded ':responses)))
                       "legacy one-object archives are readable")))))
  nil)
