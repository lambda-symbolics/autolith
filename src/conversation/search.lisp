(in-package #:autolith)

;;;; -- Durable Conversation Record Search --

(defparameter *conversation-search-buffer-octets* (* 1024 1024)
  "The octets read from one segment at once while looking for a query fragment.")

(defparameter *conversation-search-octet-fold*
  (let ((table (make-array 256 :element-type '(unsigned-byte 8))))
    (dotimes (octet 256 table)
      (setf (aref table octet)
            (if (<= (char-code #\A) octet (char-code #\Z))
                (+ octet (- (char-code #\a) (char-code #\A)))
                octet))))
  "Each octet mapped to itself with ASCII capital letters folded to lowercase.")

(defclass conversation-search-match ()
  ((record
    :initarg       :record
    :reader        conversation-search-match-record
    :type          list
    :documentation "The projected replay record that matched.")
   (text
    :initarg       :text
    :reader        conversation-search-match-text
    :type          string
    :documentation "The searchable text in which every query term occurs."))
  (:documentation "One durable conversation record matching a search."))


;;;; -- Searchable Text --

(-> conversation-search-terms (string) list)
(defun conversation-search-terms (query)
  "Return QUERY's whitespace-separated terms in order."
  (let ((terms nil)
        (start nil))
    (loop for index from 0 to (length query)
          for character = (and (< index (length query)) (char query index))
          do (if (and character
                      (not (member character
                                   '(#\Space #\Tab #\Newline #\Return #\Page))))
                 (unless start
                   (setf start index))
                 (when start
                   (push (subseq query start index) terms)
                   (setf start nil))))
    (nreverse terms)))

(-> conversation-record-search-text (list) (option string))
(defun conversation-record-search-text (record)
  "Return the text RECORD stores verbatim for search, or NIL when it has none."
  (let ((fields (conversation-search--record-fields record)))
    (and fields
         (format nil "~{~A~^~%~}" fields))))

(-> conversation-search-text-matches-p (string list) boolean)
(defun conversation-search-text-matches-p (text terms)
  "Return true when every one of TERMS occurs in TEXT, folding ASCII letters."
  (and (every (lambda (term)
                (conversation-search-text-position text term))
              terms)
       t))

(-> conversation-search-text-position (string string) (option (integer 0)))
(defun conversation-search-text-position (text term)
  "Return the first position of TERM in TEXT, folding ASCII letters, or NIL."
  (search term text :test #'conversation-search--character-equal))

(-> conversation-search--record-fields (list) list)
(defun conversation-search--record-fields (record)
  "Return the searchable strings RECORD stores verbatim, oldest field first."
  (let ((properties (rest record)))
    (flet ((strings (&rest names)
             "Return the string values of NAMES in RECORD's properties."
             (loop for name in names
                   for value = (getf properties name)
                   when (stringp value)
                     collect value)))
      (case (first record)
        ((:message :summary)
         (strings :content))
        (:native-compaction
         (strings :summary))
        (:tool-result
         (strings :tool :output))
        (:turn-aborted
         (strings :message))
        ((:async-lisp-event :user-operation)
         (strings :source :result))
        (:provider-item
         (conversation-search--provider-item-fields
          (getf properties :wire-json)))
        (otherwise
         nil)))))

(-> conversation-search--provider-item-fields (t) list)
(defun conversation-search--provider-item-fields (wire-json)
  "Return the visible strings of the provider item encoded in WIRE-JSON."
  (handler-case
      (let ((item (and (stringp wire-json) (json-decode wire-json))))
        (when (json-object-p item)
          (let ((type (json-get item "type")))
            (flet ((strings (object &rest names)
                     "Return the string values of NAMES in OBJECT."
                     (and (json-object-p object)
                          (loop for name in names
                                for value = (json-get object name)
                                when (stringp value)
                                  collect value))))
              (cond
                ((json-string= type "message")
                 (let ((text (response-item-assistant-text item)))
                   (and text (list text))))
                ((json-string= type "reasoning")
                 (let ((summary (response-item-reasoning-summary item)))
                   (and summary (list summary))))
                ((json-string= type "function_call")
                 (strings item "arguments"))
                ((json-string= type "web_search_call")
                 (let* ((action  (json-get item "action"))
                        (queries (and (json-object-p action)
                                      (json-get action "queries"))))
                   (append (strings action "query" "url" "pattern")
                           (and (vectorp queries)
                                (remove-if-not #'stringp
                                               (coerce queries 'list))))))
                (t
                 nil))))))
    (json-error ()
      nil)))

(-> conversation-search--character-equal (character character) boolean)
(defun conversation-search--character-equal (left right)
  "Return true when LEFT and RIGHT are equal after folding ASCII letters."
  (flet ((fold (character)
           "Return CHARACTER with an ASCII capital letter folded to lowercase."
           (if (char<= #\A character #\Z)
               (char-downcase character)
               character)))
    (char= (fold left) (fold right))))


;;;; -- Segment Prefiltering --

(-> conversation-search--literal-character-p (character) boolean)
(defun conversation-search--literal-character-p (character)
  "Return true when CHARACTER is stored unescaped in every segment encoding."
  (let ((code (char-code character)))
    (and (<= #x20 code #xFFFF)
         (char/= character #\")
         (char/= character #\\))))

(-> conversation-search--fragment (string) (option (simple-array (unsigned-byte 8) (*))))
(defun conversation-search--fragment (term)
  "Return the folded UTF-8 octets of TERM's longest literal run, or NIL."
  (let ((longest nil)
        (start   nil))
    (loop for index from 0 to (length term)
          for literal-p = (and (< index (length term))
                               (conversation-search--literal-character-p
                                (char term index)))
          do (cond
               (literal-p
                (unless start
                  (setf start index)))
               (start
                (when (> (- index start) (length longest))
                  (setf longest (subseq term start index)))
                (setf start nil))))
    (and longest
         (let ((octets (utf8-string-to-octets longest)))
           (map-into octets
                     (lambda (octet)
                       (aref *conversation-search-octet-fold* octet))
                     octets)
           (coerce octets '(simple-array (unsigned-byte 8) (*)))))))

(-> conversation-search--fragments (list) list)
(defun conversation-search--fragments (terms)
  "Return the fragments of TERMS that have one, longest and most selective first."
  (sort (remove nil (mapcar #'conversation-search--fragment terms))
        #'>
        :key #'length))

(-> conversation-search--segment-contains-p
    (pathname (simple-array (unsigned-byte 8) (*)))
    boolean)
(defun conversation-search--segment-contains-p (pathname fragment)
  "Return true when segment PATHNAME holds FRAGMENT, folding ASCII letters.

The segment streams through a fixed buffer that keeps the final octets of
each block, so a fragment spanning two blocks is still found."
  (let* ((length (length fragment))
         (buffer (make-array (max *conversation-search-buffer-octets*
                                  (* 2 length))
                             :element-type '(unsigned-byte 8)))
         (shift  (conversation-search--shift-table fragment)))
    (with-open-file (stream pathname :element-type '(unsigned-byte 8))
      (loop with kept = 0
            for end = (read-sequence buffer stream :start kept)
            while (> end kept)
            do (when (conversation-search--octets-position fragment shift buffer
                                                           :end end)
                 (return t))
               (setf kept (min (1- length) end))
               (replace buffer buffer :start2 (- end kept) :end2 end)
            finally (return nil)))))

(-> conversation-search--shift-table
    ((simple-array (unsigned-byte 8) (*)))
    (simple-array fixnum (256)))
(defun conversation-search--shift-table (fragment)
  "Return the Horspool shift for each octet that ends a window over FRAGMENT."
  (let* ((length (length fragment))
         (table  (make-array 256 :element-type 'fixnum :initial-element length)))
    (loop for index from 0 below (1- length)
          do (setf (aref table (aref fragment index)) (- length 1 index)))
    table))

(-> conversation-search--octets-position
    ((simple-array (unsigned-byte 8) (*))
     (simple-array fixnum (256))
     (simple-array (unsigned-byte 8) (*))
     &key (:end (integer 0)))
    (option (integer 0)))
(defun conversation-search--octets-position (fragment shift buffer &key end)
  "Return where folded FRAGMENT first occurs in BUFFER below END, or NIL."
  (declare (type (simple-array (unsigned-byte 8) (*)) fragment buffer)
           (type (simple-array fixnum (256)) shift)
           (type fixnum end)
           (optimize speed))
  (let ((fold  *conversation-search-octet-fold*)
        (final (1- (length fragment))))
    (declare (type (simple-array (unsigned-byte 8) (256)) fold)
             (type fixnum final))
    (loop with start of-type fixnum = 0
          while (< (+ start final) end)
          do (let ((tail (aref fold (aref buffer (+ start final)))))
               (when (and (= tail (aref fragment final))
                          (loop for index of-type fixnum from (1- final) downto 0
                                always (= (aref fold (aref buffer (+ start index)))
                                          (aref fragment index))))
                 (return start))
               (incf start (aref shift tail))))))


;;;; -- Search and Record Windows --

(-> conversation-search
    (conversation list &key (:limit (integer 1)))
    list)
(defun conversation-search (conversation terms &key (limit 20))
  "Return up to LIMIT matches of every term in TERMS in CONVERSATION, newest first."
  (let ((fragments (conversation-search--fragments terms))
        (matches   nil)
        (found     0))
    (dolist (segment (reverse (conversation-search--segments conversation)))
      (when (>= found limit)
        (return))
      (when (every (lambda (fragment)
                     (conversation-search--segment-contains-p segment fragment))
                   fragments)
        (let ((segment-matches nil))
          (conversation--map-records
           segment
           (lambda (record)
             (let ((text (conversation-record-search-text record)))
               (when (and text (conversation-search-text-matches-p text terms))
                 (let ((projected (conversation-replay-project-record record)))
                   (when projected
                     (push (make-instance 'conversation-search-match
                                          :record projected
                                          :text   text)
                           segment-matches)))))))
          (loop for match in segment-matches
                while (< found limit)
                do (push match matches)
                   (incf found)))))
    (nreverse matches)))

(-> conversation-records-from
    (conversation (integer 0) (integer 1))
    (values list boolean))
(defun conversation-records-from (conversation start-sequence count)
  "Return up to COUNT projected records of CONVERSATION from START-SEQUENCE on.

The second value is true when a further projected record follows them."
  (let* ((segments    (conversation-search--segments conversation))
         (first-index (or (position-if
                           (lambda (segment)
                             (<= (conversation-search--segment-start segment)
                                 start-sequence))
                           segments
                           :from-end t)
                          0))
         (records     nil)
         (collected   0)
         (more-p      nil))
    (block collect
      (dolist (segment (nthcdr first-index segments))
        (conversation-search--map-projections
         segment
         (lambda (projected)
           (let ((sequence (getf (rest projected) :seq)))
             (when (and (integerp sequence) (>= sequence start-sequence))
               (when (= collected count)
                 (setf more-p t)
                 (return-from collect))
               (push projected records)
               (incf collected)))))))
    (values (nreverse records) more-p)))

(-> conversation-records-newest (conversation (integer 1)) list)
(defun conversation-records-newest (conversation count)
  "Return CONVERSATION's newest COUNT projected records, oldest first."
  (let ((records nil))
    (dolist (segment (reverse (conversation-search--segments conversation)))
      (when (>= (length records) count)
        (return))
      (let ((segment-records nil))
        (conversation-search--map-projections
         segment
         (lambda (projected)
           (push projected segment-records)))
        (setf records (append (nreverse segment-records) records))))
    (last records count)))

(-> conversation-compaction-sequences (conversation) list)
(defun conversation-compaction-sequences (conversation)
  "Return the sequences at which CONVERSATION's compactions began, oldest first."
  (loop for segment in (conversation-search--segments conversation)
        for start = (conversation-chunk-start-sequence segment)
        when (and start (> start 1))
          collect start))

(-> conversation-search--segments (conversation) list)
(defun conversation-search--segments (conversation)
  "Return CONVERSATION's durable log segments in chronological order."
  (conversation-storage-pathnames (conversation-pathname conversation)))

(-> conversation-search--segment-start (pathname) (integer 0))
(defun conversation-search--segment-start (segment)
  "Return the first sequence SEGMENT can hold, zero for a legacy log."
  (or (conversation-chunk-start-sequence segment) 0))

(-> conversation-search--map-projections (pathname function) null)
(defun conversation-search--map-projections (segment function)
  "Call FUNCTION with the replay projection of each record in SEGMENT that has one.

Segment headers describe the conversation rather than its history, so they
are never projected."
  (conversation--map-records
   segment
   (lambda (record)
     (unless (eq (first record) ':conversation)
       (let ((projected (conversation-replay-project-record record)))
         (when projected
           (funcall function projected))))))
  nil)
