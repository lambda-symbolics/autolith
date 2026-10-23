(in-package #:autolith)

;;;; -- Knowledge Usage Attribution --

;;; Structured knowledge makes no provider requests of its own; it costs
;;; tokens inside ordinary requests: recalled facts, change explanations, and
;;; the model's memory:facts tool calls. For each request where knowledge took
;;; part, a provider metadata record attributes that share and copies the
;;; request's usage, so the overhead can be measured instead of assumed.

(defparameter *knowledge-usage-characters-per-token* 4
  "Characters per token used to estimate the tokens of recalled knowledge text.")

(defvar *knowledge-usage-notes* (make-hash-table :test #'equal :synchronized t)
  "Each conversation's knowledge recall in the request being prepared.")

(-> knowledge-usage-note
    (string &key (:facts (integer 0)) (:fact-characters (integer 0))
                 (:changes (integer 0)) (:change-characters (integer 0)))
    null)
(defun knowledge-usage-note (conversation &key (facts 0) (fact-characters 0)
                                               (changes 0) (change-characters 0))
  "Remember what knowledge recall added to CONVERSATION's next request."
  (setf (gethash conversation *knowledge-usage-notes*)
        (list :facts facts :fact-characters fact-characters
              :changes changes :change-characters change-characters))
  nil)

(-> knowledge-usage--tool-call-p (t) boolean)
(defun knowledge-usage--tool-call-p (call)
  "Return true when function CALL reads or edits a structured knowledge view."
  (and (member (function-call-canonical-name call) '("resource.read" "resource.edit")
               :test #'equal)
       (let* ((arguments (ignore-errors (json-decode (or (json-get call "arguments") "{}"))))
              (uri (and (json-object-p arguments) (json-get arguments "uri"))))
         (and (stringp uri)
              (some (lambda (prefix) (uiop:string-prefix-p prefix uri))
                    '("memory:facts" "memory:claim/" "memory:predicates" "memory:rules"
                      "memory:changes"))))
       t))

(-> knowledge-usage-capture (conversation list (integer 1) t) null)
(defun knowledge-usage-capture (conversation calls request-number usage)
  "Attribute knowledge's share of one completed request to CONVERSATION durably.

CALLS are the request's function calls and USAGE its provider usage. Nothing
is recorded for a request knowledge did not take part in."
  (let* ((identifier (conversation-identifier conversation))
         (note (gethash identifier *knowledge-usage-notes*))
         (tool-calls (count-if #'knowledge-usage--tool-call-p calls)))
    (remhash identifier *knowledge-usage-notes*)
    (when (or (and note (or (plusp (getf note :facts)) (plusp (getf note :changes))))
              (plusp tool-calls))
      (conversation-append-provider-metadata
       conversation
       (list :request-number request-number
             :knowledge (append (or note
                                    (list :facts 0 :fact-characters 0
                                          :changes 0 :change-characters 0))
                                (list :tool-calls tool-calls
                                      :usage (agent--portable-value
                                              (provider-usage-normalize usage))))))))
  nil)

(-> knowledge-usage-report (conversation) list)
(defun knowledge-usage-report (conversation)
  "Return CONVERSATION's knowledge overhead summed over its attributed requests.

The plist counts :REQUESTS, recalled :FACTS and :CHANGES, model
:TOOL-CALLS, :ESTIMATED-RECALL-TOKENS from the recalled text, and the
:REQUEST-TOKENS those requests used in total."
  (let ((requests 0) (facts 0) (changes 0) (tool-calls 0) (characters 0) (tokens 0))
    (conversation-map-records
     conversation
     (lambda (record)
       (let ((knowledge (and (eq (first record) :provider)
                             (getf (getf (rest record) :metadata) :knowledge))))
         (when knowledge
           (incf requests)
           (incf facts (getf knowledge :facts 0))
           (incf changes (getf knowledge :changes 0))
           (incf tool-calls (getf knowledge :tool-calls 0))
           (incf characters (+ (getf knowledge :fact-characters 0)
                               (getf knowledge :change-characters 0)))
           (incf tokens (or (conversation--usage-total (getf knowledge :usage)) 0))))))
    (list :requests requests :facts facts :changes changes :tool-calls tool-calls
          :estimated-recall-tokens (ceiling characters *knowledge-usage-characters-per-token*)
          :request-tokens tokens)))
