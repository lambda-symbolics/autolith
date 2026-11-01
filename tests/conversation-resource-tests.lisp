(in-package #:autolith)

;;;; -- Conversation History Resource Tests --

(-> conversation-resource-tests--append (conversation function) (integer 1))
(defun conversation-resource-tests--append (conversation function)
  "Call FUNCTION to append one record to CONVERSATION and return its sequence."
  (let ((sequence (conversation-next-sequence conversation)))
    (funcall function)
    sequence))

(-> conversation-resource-tests--populate (conversation) list)
(defun conversation-resource-tests--populate (conversation)
  "Append a compacted history to CONVERSATION and return its sequences by name."
  (flet ((append-record (function)
           "Append one record through FUNCTION and return its sequence."
           (conversation-resource-tests--append conversation function)))
    (list
     :question
     (append-record
      (lambda ()
        (conversation-append-user-message
         conversation "Where did we put the argo decoder?")))
     :answer
     (append-record
      (lambda ()
        (conversation-append-provider-item
         conversation
         (json-object "type" "message" "role" "assistant"
                      "content" (vector (json-object
                                         "type" "output_text"
                                         "text" "The ARGO decoder lives in grammar.lisp."))))))
     :call
     (append-record
      (lambda ()
        (conversation-append-provider-item
         conversation
         (json-object "type" "function_call" "call_id" "call-1"
                      "namespace" "shell" "name" "run"
                      "arguments" "{\"command\":\"grep -rn needle-term src\"}"))))
     :result
     (append-record
      (lambda ()
        (conversation-append-tool-result
         conversation "call-1"
         :tool-name "shell.run"
         :output    "src/grammar.lisp:12: needle-term found"
         :success-p t)))
     :summary
     (append-record
      (lambda ()
        (conversation-append-summary
         conversation "The summary keeps the compaction-marker.")))
     :quoted
     (append-record
      (lambda ()
        (conversation-append-user-message
         conversation "After compaction: say \"quoted phrase\" please.")))
     :unicode
     (append-record
      (lambda ()
        (conversation-append-user-message
         conversation "Čeština with an emoji 🙂 at the end."))))))

(-> conversation-resource-tests--context (configuration conversation) tool-context)
(defun conversation-resource-tests--context (configuration conversation)
  "Return a primary tool context for CONVERSATION."
  (make-instance 'tool-context
                 :configuration configuration
                 :worker        nil
                 :conversation  conversation
                 :registry      (make-instance 'tool-registry)))

(-> conversation-resource-tests--read-tool () resource-read-tool)
(defun conversation-resource-tests--read-tool ()
  "Return a resource read tool for direct conversation resource reads."
  (make-instance 'resource-read-tool
                 :namespace         "resource"
                 :name              "read"
                 :description       "Test resource read."
                 :parameters        (tool-object-schema (json-object) '())
                 :resource-registry (make-resource-registry)))

(-> conversation-resource-tests--read (tool-context string &rest t) string)
(defun conversation-resource-tests--read (context identifier &rest arguments)
  "Read conversation IDENTIFIER under CONTEXT with ARGUMENTS and return its text."
  (let* ((resolver (make-instance 'conversation-resolver :scheme "conversation"))
         (resource (resource-resolver-resolve resolver identifier context))
         (result   (resource-tool-read resource
                                       (conversation-resource-tests--read-tool)
                                       context
                                       (apply #'json-object arguments))))
    (test-assert (tool-result-success-p result)
                 "conversation resource reads succeed")
    (tool-result-content result)))

(-> conversation-resource-tests--rejected-p (tool-context string &rest t) boolean)
(defun conversation-resource-tests--rejected-p (context identifier &rest arguments)
  "Return true when reading conversation IDENTIFIER with ARGUMENTS signals."
  (handler-case
      (progn
        (apply #'conversation-resource-tests--read context identifier arguments)
        nil)
    (autolith-error ()
      t)))

(-> conversation-resource-tests--heading (integer) string)
(defun conversation-resource-tests--heading (sequence)
  "Return the beginning of the rendered heading for record SEQUENCE."
  (format nil "[sequence ~D |" sequence))

(-> test-conversation-resource-search () null)
(defun test-conversation-resource-search ()
  "Test searching one conversation's records across compaction, newest first."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration :identifier "resource-search"))
           (sequences    (conversation-resource-tests--populate conversation))
           (context      (conversation-resource-tests--context configuration conversation)))
      (flet ((search-text (query &rest arguments)
               "Return the rendered search of the current conversation for QUERY."
               (apply #'conversation-resource-tests--read context "current"
                      "query" query arguments))
             (heading (name)
               "Return the heading prefix of the record appended as NAME."
               (conversation-resource-tests--heading (getf sequences name))))
        (let ((found (search-text "Argo  DECODER")))
          (test-assert
           (and (search "2 records" found)
                (< (search (heading :answer) found)
                   (search (heading :question) found)))
           "every term matches regardless of ASCII case, newest record first"))
        (let ((found (search-text "needle-term")))
          (test-assert
           (and (search (heading :call) found)
                (search (heading :result) found)
                (search "grep -rn needle-term src" found))
           "tool call arguments and tool output from before compaction are found"))
        (test-assert (search (heading :summary) (search-text "compaction-marker"))
                     "compaction summaries are searchable")
        (test-assert (search (heading :quoted) (search-text "\"quoted"))
                     "terms holding escaped characters still match")
        (test-assert (search (heading :unicode) (search-text "Čeština 🙂"))
                     "non-ASCII and astral terms match exactly")
        (test-assert (search "No records" (search-text "čeština"))
                     "case folding is limited to ASCII letters")
        (test-assert (search "No records" (search-text "argo absent-term"))
                     "a record must contain every term")
        (let ((found (search-text "decoder" "max-results" 1)))
          (test-assert
           (and (search "limited to 1" found)
                (search (heading :answer) found)
                (not (search (heading :question) found)))
           "max-results keeps only the newest matches"))
        (test-assert (search "Where did we put"
                             (conversation-resource-tests--read
                              context
                              (format nil "id/~A" (conversation-identifier conversation))
                              "query" "decoder"))
                     "a named conversation is searched by its identifier")
        (test-assert
         (and (conversation-resource-tests--rejected-p context "current" "query" " ")
              (conversation-resource-tests--rejected-p context "current"
                                                       "max-results" 3)
              (conversation-resource-tests--rejected-p context "current"
                                                       "query" "argo"
                                                       "max-results" 51))
         "blank queries and arguments that do not apply to search are rejected")
        (let ((*resource-tool-notes* nil))
          (conversation-resource-tests--read context "current" "query" "argo" "start-sequence" 1)
          (test-assert (search "ignores start-sequence" (format nil "~{~A~}" *resource-tool-notes*))
                       "a search ignores window arguments with a note instead of failing")))))
  nil)

(-> test-conversation-resource-windows () null)
(defun test-conversation-resource-windows ()
  "Test reading one conversation's records by sequence across compaction."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration :identifier "resource-windows"))
           (sequences    (conversation-resource-tests--populate conversation))
           (context      (conversation-resource-tests--context configuration conversation)))
      (flet ((heading (name)
               "Return the heading prefix of the record appended as NAME."
               (conversation-resource-tests--heading (getf sequences name))))
        (let ((newest (conversation-resource-tests--read context "current"
                                                         "record-count" 2)))
          (test-assert
           (and (search "newest records" newest)
                (search (heading :quoted) newest)
                (search (heading :unicode) newest)
                (not (search (heading :summary) newest))
                (search (format nil "Compacted 1 time; latest at sequence ~D."
                                (getf sequences :summary))
                        newest))
           "the default window ends at the newest record and names the compaction"))
        (let ((window (conversation-resource-tests--read
                       context "current"
                       "start-sequence" (getf sequences :result)
                       "record-count" 2)))
          (test-assert
           (and (search (heading :result) window)
                (search (heading :summary) window)
                (search "The summary keeps the compaction-marker." window)
                (not (search (heading :quoted) window))
                (search (format nil "continue with start-sequence ~D"
                                (1+ (getf sequences :summary)))
                        window))
           "a sequence window crosses the compaction boundary and offers the next page"))
        (let ((all (conversation-resource-tests--read context "current"
                                                      "start-sequence" 1
                                                      "record-count" 200)))
          (test-assert
           (and (search (heading :question) all)
                (search "tool shell.run" all)
                (search (heading :unicode) all)
                (not (search "continue with start-sequence" all))
                (not (search ":CONVERSATION" all)))
           "a full window holds every record once and no segment header"))
        (let ((long (make-string (* 2 *conversation-resource-record-characters*)
                                 :initial-element #\y)))
          (conversation-append-user-message conversation long)
          (test-assert
           (search "[record truncated after"
                   (conversation-resource-tests--read context "current"
                                                      "record-count" 2))
           "a long record is truncated inside a multi-record window")
          (test-assert
           (search long (conversation-resource-tests--read context "current"
                                                           "record-count" 1))
           "a record read alone is shown in full"))
        (test-assert
         (and (conversation-resource-tests--rejected-p context "current"
                                                       "start-line" 1)
              (conversation-resource-tests--rejected-p context "current"
                                                       "record-count" 201)
              (conversation-resource-tests--rejected-p context "current"
                                                       "start-sequence" 0))
         "line windows and out-of-range record windows are rejected")
        (test-assert
         (handler-case
             (progn
               (resource-resolver-resolve
                (make-instance 'conversation-resolver :scheme "conversation")
                "resource-windows" context)
               nil)
           (resource-operation-unsupported ()
             t))
         "a bare identifier is not a conversation resource")
        (test-assert
         (conversation-resource-tests--rejected-p context "id/missing-conversation")
         "an unknown named conversation is reported"))))
  nil)

(-> test-conversation-resource-segment-prefilter () null)
(defun test-conversation-resource-segment-prefilter ()
  "Test the segment prefilter finds folded fragments across buffer blocks."
  (with-test-configuration (configuration root)
    (declare (ignore configuration))
    (let ((pathname                            (merge-pathnames "segment.sexp" root))
          (*conversation-search-buffer-octets* 16))
      (with-open-file (stream pathname :direction       ':output
                                       :external-format ':utf-8)
        (write-string "0123456789abcDEFghijkl Žluťoučký" stream))
      (flet ((contains-p (term)
               "Return true when the segment holds TERM's longest literal run."
               (conversation-search--segment-contains-p
                pathname (conversation-search--fragment term))))
        (test-assert (contains-p "cdefgh")
                     "a fragment spanning two buffer blocks is found")
        (test-assert (contains-p "ABCdefGHI")
                     "ASCII letters are folded in both the fragment and segment")
        (test-assert (contains-p "Žluťoučký")
                     "non-ASCII fragments match their exact UTF-8 octets")
        (test-assert (not (contains-p "žluťoučký"))
                     "non-ASCII letters are not case folded")
        (test-assert (not (contains-p "absent"))
                     "a missing fragment is reported absent")
        (test-assert (null (conversation-search--fragment "\"\\"))
                     "a term of only escaped characters has no fragment")
        (test-assert (equalp (conversation-search--fragments
                              (list "ab" "\"Quoted\"" "\\"))
                             (list (utf8-string-to-octets "quoted")
                                   (utf8-string-to-octets "ab")))
                     "every term with a literal run contributes one folded fragment"))))
  nil)
