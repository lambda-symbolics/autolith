(in-package #:autolith)

;;;; -- Continuation Checkpoints --

;;; A compaction checkpoint is typed. The model supplies an intent, notes and
;;; a next step through one compaction.submit call, and each may cite
;;; references the harness offered beforehand. The stored checkpoint keeps the
;;; cited references, so a reader can tell what a note rests on.
;;;
;;; A reference is a property list. Notes cite its :HANDLE, its :KIND and
;;; :LABEL describe it, :OBSERVED is its state at capture when that state can
;;; change, and its other properties name the thing it refers to. A reference
;;; to a durable record holds the record's :CONVERSATION and :SEQUENCE.

(defparameter *continuation-format* 1
  "The version of the stored checkpoint layout and of its rendering.")

(defparameter *continuation-note-headings*
  '((:decision . "Decisions")
    (:constraint . "Constraints")
    (:completed . "Completed")
    (:open . "Open")
    (:uncertainty . "Uncertain"))
  "The kinds of note a checkpoint holds, with their headings, in rendering order.")

(defparameter *continuation-submit-tool* '("compaction" "submit")
  "The namespace and name of the one tool a compaction request offers.")

(defparameter *continuation-maximum-octets* 32768
  "Maximum UTF-8 octets of one stored checkpoint.")

(defparameter *continuation-submission-maximum-octets* 16384
  "Maximum UTF-8 octets of the arguments of one compaction.submit call.")

(defparameter *continuation-intent-maximum-characters* 1024
  "Maximum characters of a checkpoint's intent.")

(defparameter *continuation-next-maximum-characters* 512
  "Maximum characters of a checkpoint's next step.")

(defparameter *continuation-note-maximum-characters* 768
  "Maximum characters of one note.")

(defparameter *continuation-maximum-notes* 16
  "Maximum notes in one checkpoint.")

(defparameter *continuation-row-maximum-references* 8
  "Maximum references one note or the next step cites.")

(defparameter *continuation-maximum-references* 64
  "Maximum distinct references one checkpoint cites, and one catalog offers.")

(defparameter *continuation-submission-schema*
  (flet ((object (&rest properties)
           "Return a closed object schema that requires each of PROPERTIES."
           (list :type                  ':object
                 :properties            properties
                 :required              (mapcar #'first properties)
                 :additional-properties nil)))
    (let ((text '("text" (:type :string)))
          (refs '("refs" (:type :array :items (:type :string))))
          (kind (list "kind"
                      (list :type ':string
                            :enum (mapcar (lambda (heading) (string-downcase (first heading)))
                                          *continuation-note-headings*)))))
      (cl-llm-provider-api:output-schema-normalize
       (object '("intent" (:type :string))
               (list "notes" (list :type ':array :items (object kind text refs)))
               (list "next" (object text refs))))))
  "The arguments of a compaction.submit call, as a native output schema.")


;;;; -- Public Functions --

(-> continuation-tool-namespaces () vector)
(defun continuation-tool-namespaces ()
  "Return the provider tool namespaces of a compaction request: its one submit tool."
  (destructuring-bind (namespace name) *continuation-submit-tool*
    (json-array
     (json-object
      "type" "namespace"
      "name" namespace
      "description" "Checkpoint submission during compaction."
      "tools" (json-array
               (json-object
                "name" name
                "description" "Submit the checkpoint that replaces the conversation so far."
                "strict" t
                "parameters" (cl-llm-provider-api:output-schema->json
                              *continuation-submission-schema*)))))))

(-> make-continuation
    (&key (:intent (option string)) (:notes list) (:next list)
          (:references list) (:unsummarized list))
    list)
(defun make-continuation (&key intent notes next references unsummarized)
  "Return a checkpoint of INTENT, NOTES and the NEXT step with the REFERENCES they cite.

A note lists its :KIND, :TEXT and the handles of its :REFS; NEXT lists :TEXT
and :REFS. UNSUMMARIZED lists the record ranges nobody summarized, each as a
conversation identifier with its first and last sequence."
  (let ((cited (continuation--cited-handles notes next)))
    (list :format       *continuation-format*
          :intent       intent
          :notes        notes
          :next         next
          :references   (remove-if-not (lambda (reference)
                                         (member (getf reference :handle) cited :test #'string=))
                                       references)
          :unsummarized unsummarized)))

(-> continuation-without-summary ((option list) (option list)) list)
(defun continuation-without-summary (prior range)
  "Return a checkpoint for records nobody summarized.

It keeps what PRIOR, the previous checkpoint or NIL, holds and adds RANGE, a
conversation identifier with its first and last sequence, to the unsummarized
ranges. RANGE is NIL when no record was covered."
  (make-continuation :intent       (getf prior :intent)
                     :notes        (getf prior :notes)
                     :next         (getf prior :next)
                     :references   (getf prior :references)
                     :unsummarized (append (getf prior :unsummarized)
                                           (and range (list range)))))

(-> continuation-submission
    (string list &key (:unsummarized list)) (values list (option string)))
(defun continuation-submission (arguments references &key unsummarized)
  "Return the checkpoint ARGUMENTS describe, or NIL and why they are not usable.

ARGUMENTS is the JSON text of one compaction.submit call. REFERENCES are the
references it may cite. UNSUMMARIZED passes to MAKE-CONTINUATION."
  (let ((object (handler-case (cl-llm-provider-api:output-json-decode arguments)
                  (cl-llm-provider-api:output-value-error ()
                    nil))))
    (cond
      ((> (length (utf8-string-to-octets arguments)) *continuation-submission-maximum-octets*)
       (values nil (format nil "the arguments are larger than ~:D bytes"
                           *continuation-submission-maximum-octets*)))
      ((null object)
       (values nil "the arguments are not valid JSON"))
      ((not (cl-llm-provider-api:output-schema-valid-p object *continuation-submission-schema*))
       (values nil "the arguments do not match the schema of compaction.submit"))
      (t
       (let* ((continuation
                (make-continuation
                 :intent       (json-get object "intent")
                 :notes        (map 'list #'continuation--row (json-get object "notes"))
                 :next         (continuation--row (json-get object "next"))
                 :references   references
                 :unsummarized unsummarized))
              (problem      (continuation-problem continuation)))
         (if problem
             (values nil problem)
             (values continuation nil)))))))

(-> continuation-problem (list) (option string))
(defun continuation-problem (continuation)
  "Return why CONTINUATION breaks a bound or cites a reference it does not keep, or NIL."
  (destructuring-bind (&key intent notes next references &allow-other-keys) continuation
    (let* ((cited   (continuation--cited-handles notes next))
           (unknown (set-difference cited
                                    (mapcar (lambda (reference) (getf reference :handle))
                                            references)
                                    :test #'string=)))
      (flet ((note-text (note)
               "Return NOTE's text."
               (getf note :text))

             (reference-count (row)
               "Return how many references ROW cites."
               (length (getf row :refs))))
        (cond
          ;; Only a checkpoint without notes may leave the intent out.
          ((not (or (non-empty-string-p intent)
                    (and (null intent) (null notes))))
           "the intent is empty")
          ((> (length intent) *continuation-intent-maximum-characters*)
           (format nil "the intent has more than ~:D characters"
                   *continuation-intent-maximum-characters*))
          ((> (length (getf next :text)) *continuation-next-maximum-characters*)
           (format nil "the next step has more than ~:D characters"
                   *continuation-next-maximum-characters*))
          ((> (length notes) *continuation-maximum-notes*)
           (format nil "there are more than ~D notes" *continuation-maximum-notes*))
          ((notevery #'non-empty-string-p (mapcar #'note-text notes))
           "a note has no text")
          ((find *continuation-note-maximum-characters* notes
                 :key (lambda (note) (length (note-text note))) :test #'<)
           (format nil "a note has more than ~:D characters"
                   *continuation-note-maximum-characters*))
          ((find *continuation-row-maximum-references* (cons next notes)
                 :key #'reference-count :test #'<)
           (format nil "a note or the next step cites more than ~D references"
                   *continuation-row-maximum-references*))
          ((> (length cited) *continuation-maximum-references*)
           (format nil "the checkpoint cites more than ~D references"
                   *continuation-maximum-references*))
          (unknown
           (format nil "the reference catalog does not list ~{~A~^, ~}" unknown))
          ((> (length (utf8-string-to-octets
                       (with-standard-io-syntax (prin1-to-string continuation))))
              *continuation-maximum-octets*)
           (format nil "the checkpoint is larger than ~:D bytes"
                   *continuation-maximum-octets*)))))))

(-> continuation-p (t) boolean)
(defun continuation-p (value)
  "Return true when VALUE is a well-formed checkpoint within its bounds."
  (flet ((handles-p (handles)
           "Return true when HANDLES is a list of strings."
           (and (listp handles) (every #'stringp handles)))

         (properties-p (value &rest keys)
           "Return true when VALUE is a property list of exactly KEYS."
           (record-check value :properties-p t :allow-other-keys nil
                         :fields            (mapcar (lambda (key)
                                                      (list :indicator key :required t))
                                                    keys))))
    (handler-case
        (and (properties-p value :format :intent :notes :next :references :unsummarized)
             (eql (getf value :format) *continuation-format*)
             (typep (getf value :intent) '(option string))
             (every (lambda (note)
                      (and (properties-p note :kind :text :refs)
                           (assoc (getf note :kind) *continuation-note-headings*)
                           (stringp (getf note :text))
                           (handles-p (getf note :refs))))
                    (getf value :notes))
             (let ((next (getf value :next)))
               (or (null next)
                   (and (properties-p next :text :refs)
                        (stringp (getf next :text))
                        (handles-p (getf next :refs)))))
             (every (lambda (reference)
                      (and (non-empty-string-p (getf reference :handle))
                           (keywordp (getf reference :kind))
                           (stringp (getf reference :label))))
                    (getf value :references))
             (every (lambda (range)
                      (typep range '(cons string (cons (integer 1) (cons (integer 1) null)))))
                    (getf value :unsummarized))
             (null (continuation-problem value)))
      (type-error ()
        nil))))

(-> continuation-render (list) string)
(defun continuation-render (continuation)
  "Return CONTINUATION as the text the continuing model reads.

The text depends on CONTINUATION alone, so a stored checkpoint always renders
the same. A completed note that cites nothing is marked unsubstantiated."
  (destructuring-bind (&key intent notes next references unsummarized &allow-other-keys)
      continuation
    (with-output-to-string (stream)
      (format stream "~@[Intent: ~A~2%~]" intent)
      (loop for (kind . heading) in *continuation-note-headings*
            for rows = (remove kind notes
                               :key      (lambda (note) (getf note :kind))
                               :test-not #'eq)
            when rows
              do (format stream "~A:~%" heading)
                 (dolist (row rows)
                   (format stream "- ~A~@[ [~{~A~^, ~}]~]~:[~; (unsubstantiated)~]~%"
                           (getf row :text)
                           (getf row :refs)
                           (and (eq kind ':completed) (null (getf row :refs)))))
                 (terpri stream))
      (when (non-empty-string-p (getf next :text))
        (format stream "Next: ~A~@[ [~{~A~^, ~}]~]~2%" (getf next :text) (getf next :refs)))
      (loop for (conversation first last) in unsummarized
            do (format stream "Records ~D to ~D of conversation ~A are not summarized here.~%"
                       first last conversation))
      (format stream "~@[References:~%~{~A~%~}~]"
              (mapcar #'continuation-reference-line references)))))

(-> continuation-catalog (list) string)
(defun continuation-catalog (references)
  "Return the catalog of REFERENCES a compaction request shows the model."
  (format nil "Reference catalog:~%~:[(empty)~;~:*~{~A~%~}~]"
          (mapcar #'continuation-reference-line references)))

(-> continuation-reference-line (list) string)
(defun continuation-reference-line (reference)
  "Return the line that shows REFERENCE's handle, its label and its state at capture."
  (format nil "~A: ~A~@[ (~A)~]"
          (getf reference :handle) (getf reference :label) (getf reference :observed)))


;;;; -- Private Functions --

(-> continuation--cited-handles (list list) list)
(defun continuation--cited-handles (notes next)
  "Return the distinct handles NOTES and the NEXT step cite, in their order."
  (remove-duplicates (loop for row in (cons next notes)
                           append (getf row :refs))
                     :test #'string= :from-end t))

(-> continuation--row (hash-table) list)
(defun continuation--row (object)
  "Return a submitted note or next step OBJECT as a property list."
  (let ((kind (json-get object "kind")))
    (append (when kind
              (list :kind (first (assoc kind *continuation-note-headings*
                                        :test #'string-equal))))
            (list :text (json-get object "text")
                  :refs (coerce (json-get object "refs") 'list)))))
