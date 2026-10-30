;;;; -- Legacy Session Import --

(in-package #:autolith-supervisor)

(define-condition parent-session-compatibility-error (error)
  ((code :initarg :code :reader parent-session-compatibility-error-code
         :documentation "A fixed code that contains no conversation data."))
  (:documentation "A session cannot be imported or selected safely.")
  (:report (lambda (condition stream)
             (format stream "Session compatibility operation failed: ~A"
                     (parent-session-compatibility-error-code condition)))))

(defparameter *parent-session-legacy-byte-limit* (* 64 1024 1024)
  "Maximum bytes in one legacy segment. Retire this reader after the v1/v2 migration window.")

(serapeum/types:-> parent-session--reject (keyword) nil)
(defun parent-session--reject (code)
  "Signal a fixed diagnostic without including private persisted values."
  (error 'parent-session-compatibility-error :code code))

(serapeum/types:-> parent-session--component (string) string)
(defun parent-session--component (identifier)
  "Validate one literal session path component, accepting the old display hyphen."
  (let ((value (if (and (= (length identifier) 8) (char= (char identifier 1) #\-))
                   (concatenate 'string (subseq identifier 0 1) (subseq identifier 2))
                   identifier)))
    (unless (and (< 0 (length value) 129)
                 (not (member value '("." "..") :test #'string=))
                 (every (lambda (character)
                          (or (alphanumericp character) (find character "-_"))) value))
      (parent-session--reject ':invalid-session-id))
    value))

(serapeum/types:-> parent-session-path (parent-configuration string string) pathname)
(defun parent-session-path (configuration identifier name)
  "Return a parent-private session file for an already validated file name."
  (unless (member name '("session.sexp" "migration.sexp" "replay.sexp" "migration.lock")
                  :test #'string=)
    (parent-session--reject ':invalid-session-file))
  (merge-pathnames (format nil "sessions/~A/~A" (parent-session--component identifier) name)
                   (uiop:ensure-directory-pathname
                    (parent-configuration-get configuration ':private-root))))

(serapeum/types:-> parent-session--portable-input (string) null)
(defun parent-session--portable-input (text)
  "Reject executable reader syntax and excessive nesting before calling READ.
Only the portable printer's lists, vectors, strings, keywords, numbers, NIL and T
are accepted. The scan also bounds individual tokens and strings."
  (let ((depth 0) (nodes 0) (length (length text)) (index 0))
    (labels ((space-p (character)
               (find character '(#\Space #\Tab #\Newline #\Return #\Page)))

             (token-p (token)
               (or (member token '("NIL" "T") :test #'string-equal)
                   (and (> (length token) 1) (char= (char token 0) #\:)
                        (every (lambda (character)
                                 (or (alphanumericp character) (find character "-+*/_<>=!?%&.")))
                               (subseq token 1)))
                   (and (find (char token 0) "+-0123456789.")
                        (every (lambda (character) (find character "+-0123456789.eEsSdDfFlL/")) token)))))
      (loop while (< index length)
            for character = (char text index)
            do
        (cond
          ((space-p character)
           (incf index))
          ((char= character #\;)
           (setf index (or (position #\Newline text :start index) length)))
          ((char= character #\")
           (incf index)
           (let ((start index) (closed nil))
             (loop while (< index length)
                   for next = (char text index)
                   do (incf index)
                      (cond
                        ((char= next #\\)
                         (incf index))
                        ((char= next #\")
                         (setf closed t) (return))))
             (when (> (- index start) 1048576) (parent-session--reject ':record-limit))
             ;; An incomplete final string is left for READ's crash-tail handling.
             (unless closed (return))))
          ((char= character #\#)
           (unless (and (< (1+ index) length)
                        (char= (char text (1+ index)) (code-char 40)))
             (parent-session--reject ':invalid-reader-syntax))
           (incf index))
          ((char= character (code-char 40))
           (when (> (incf depth) 32) (parent-session--reject ':record-limit))
           (incf index))
          ((char= character (code-char 41))
           (when (minusp (decf depth)) (parent-session--reject ':invalid-reader-syntax))
           (incf index))
          (t
           (let* ((start index)
                  (end   (or (position-if (lambda (next)
                                            (or (space-p next) (find next "();\"")))
                                          text :start index) length))
                  (token (subseq text start end)))
             (unless (and (< 0 (length token) 161) (token-p token))
               (parent-session--reject ':invalid-reader-syntax))
             (setf index end))))
        (when (> (incf nodes) 1000000) (parent-session--reject ':record-limit))))
   nil))

(serapeum/types:-> parent-session--normalize-base-strings (string) string)
(defun parent-session--normalize-base-strings (text)
  "Replace only SBCL's bounded readable BASE-CHAR string notation with ordinary strings.
No general array reader syntax or arbitrary element type is accepted."
  (let ((index 0) (size (length text)))
    (with-output-to-string (output)
      (labels ((spaces ()
                 (loop while (and (< index size) (find (char text index) '(#\Space #\Tab #\Newline #\Return)))
                       do (incf index)))

               (incomplete ()
                 (write-char #\" output)
                 (throw 'incomplete-base-string nil))

               (expect (character)
                 (when (>= index size) (incomplete))
                 (unless (and (< index size) (char= (char text index) character))
                   (parent-session--reject ':invalid-reader-syntax))
                 (incf index))

               (string-end ()
                 (expect #\")
                 (loop while (< index size) for character = (char text index) do
                   (incf index)
                   (cond ((char= character #\\)
                          (incf index))
                         ((char= character #\")
                          (return-from string-end index))))
                 nil))
       (catch 'incomplete-base-string
         (loop while (< index size) do
           (cond
             ((char= (char text index) #\")
              (let* ((start index) (end (or (string-end) size)))
                (write-string text output :start start :end end)))
             ((char= (char text index) #\;)
              (let ((end (or (position #\Newline text :start index) size)))
                (write-string text output :start index :end end)
                (setf index end)))
             ((and (< (- size index) 4)
                   (string-equal text "#A((" :start1 index :end2 (- size index)))
              (incomplete))
             ((and (<= (+ index 4) size) (string-equal text "#A((" :start1 index :end1 (+ index 4)))
              (incf index 4)
              (let* ((start index)
                     (end   (or (position-if-not #'digit-char-p text :start index) size)))
               (when (and (= start end) (= end size)) (incomplete))
               (unless (<= 1 (- end start) 7) (parent-session--reject ':invalid-reader-syntax))
               (let ((count (parse-integer text :start start :end end)))
                 (when (> count 1048576) (parent-session--reject ':record-limit))
                 (setf index end)
                 (expect #\)) (spaces)
                (when (< (- size index) 9)
                  (if (string-equal text "BASE-CHAR" :start1 index :end2 (- size index))
                      (incomplete)
                      (parent-session--reject ':invalid-reader-syntax)))
                (unless (and (<= (+ index 9) size)
                             (string-equal text "BASE-CHAR" :start1 index :end1 (+ index 9)))
                  (parent-session--reject ':invalid-reader-syntax))
                (incf index 9) (spaces) (expect #\.) (spaces)
                 (let* ((string-start index) (string-end (string-end)))
                  (unless string-end (incomplete))
                  (let ((*read-eval* nil) (*readtable* (copy-readtable nil)))
                    (unless (= count (length (read-from-string text nil nil :start string-start :end string-end)))
                      (parent-session--reject ':invalid-reader-syntax)))
                  (spaces) (expect #\))
                   (write-string text output :start string-start :end string-end)))))
             (t
              (write-char (char text index) output) (incf index)))))))))

(serapeum/types:-> parent-session--read-forms (pathname) (values list boolean))
(defun parent-session--read-forms (pathname)
  "Read bounded inert forms, retaining complete forms before a crash tail."
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (when (> (file-length stream) *parent-session-legacy-byte-limit*)
      (parent-session--reject ':segment-limit)))
  (let* ((text                        (parent-session--normalize-base-strings (uiop:read-file-string pathname)))
         (*read-eval*                 nil)
         (*readtable*                 (copy-readtable nil))
         (*package*                   (find-package '#:cl-user))
         (*read-default-float-format* 'double-float)
         (records nil) (eof (list nil)))
    (parent-session--portable-input text)
    (with-input-from-string (stream text)
      (loop
        (handler-case
            (let ((record (read stream nil eof)))
              (when (eq record eof) (return (values (nreverse records) nil)))
              (push record records))
          (end-of-file () (return (values (nreverse records) t)))
          (error () (parent-session--reject ':invalid-reader-syntax)))))))

(serapeum/types:-> parent-session--chunk-sequence (pathname) (or null integer))
(defun parent-session--chunk-sequence (pathname)
  "Return the positive sequence of a canonical legacy chunk filename."
  (let ((name (pathname-name pathname)))
    (when (and (stringp name) (>= (length name) 20) (every #'digit-char-p name))
      (let ((number (parse-integer name)))
        (when (and (plusp number) (string= name (format nil "~20,'0D" number))) number)))))

(serapeum/types:-> parent-session--legacy-segments (pathname string) list)
(defun parent-session--legacy-segments (root identifier)
  "Return the old top-level file followed by deterministic chunks."
  (let* ((directory (uiop:ensure-directory-pathname root))
         (identity  (merge-pathnames (format nil "~A.sexp" identifier) directory))
         (chunks    (merge-pathnames (format nil "~A/" identifier) directory)))
    (append (when (probe-file identity) (list identity))
            (when (uiop:directory-exists-p chunks)
              (sort (remove-if-not #'parent-session--chunk-sequence
                                   (uiop:directory-files chunks "*.sexp"))
                    #'< :key #'parent-session--chunk-sequence)))))

(serapeum/types:-> parent-session--tagged (t) vector)
(defun parent-session--tagged (value)
  "Encode legacy data in the existing bounded agent-record wire grammar."
  (let ((remaining 8192))
    (labels ((encode (item depth)
               (when (or (> depth 12) (minusp (decf remaining)))
                 (parent-session--reject ':record-limit))
               (typecase item
                 (null
                  (vector "nil"))
                 (string
                  (vector "string" item))
                 ((signed-byte 64)
                  (vector "integer" item))
                 (double-float
                  (vector "number" item))
                 (cons
                  (unless (ignore-errors (list-length item))
                    (parent-session--reject ':invalid-record))
                  (vector "list" (map 'vector (lambda (child) (encode child (1+ depth))) item)))
                 (vector
                  (vector "vector" (map 'vector (lambda (child) (encode child (1+ depth))) item)))
                 (symbol
                  (cond ((eq item t)
                         (vector "true"))
                        ((keywordp item)
                         (vector "keyword" (symbol-name item)))
                        (t
                         (parent-session--reject ':invalid-record))))
                 (t
                  (parent-session--reject ':invalid-record)))))
      (encode value 0))))

(serapeum/types:-> parent-session--legacy-record (list) list)
(defun parent-session--legacy-record (record)
  "Remove only the old sequence, preserving data and first-key precedence."
  (unless (sexp-store:record-shape-p record :duplicate-keys ':first)
    (parent-session--reject ':invalid-record))
  (cons (first record)
        (loop for (key value) on (rest record) by (lambda (tail) (rest (rest tail)))
              unless (eq key ':seq) append (list key value))))

(serapeum/types:-> parent-session-read-legacy (pathname string) (values list list boolean))
(defun parent-session-read-legacy (root identifier)
  "Validate legacy v1/v2 segments without loading AUTOLITH or private replay code.
Return the latest header, chronological records, and final crash-tail flag."
  (let* ((id       (parent-session--component identifier))
         (segments (parent-session--legacy-segments root id))
         (header nil) (records nil) (expected nil) (incomplete nil) (bytes 0))
    (unless segments (parent-session--reject ':unknown-session))
    (when (> (length segments) 4096) (parent-session--reject ':segment-limit))
    (loop for segment in segments for final-p = (eq segment (first (last segments))) do
      (with-open-file (stream segment :element-type '(unsigned-byte 8))
        (when (> (incf bytes (file-length stream)) (* 256 1024 1024))
          (parent-session--reject ':session-limit)))
      (multiple-value-bind (forms tail-p) (parent-session--read-forms segment)
        (when (and tail-p (not final-p)) (parent-session--reject ':incomplete-interior-segment))
        (setf incomplete tail-p)
        (let* ((candidate  (first forms))
               (properties (and (consp candidate) (rest candidate)))
               (version    (and (sexp-store:record-shape-p candidate :duplicate-keys ':first)
                                (eq (first candidate) ':conversation) (getf properties ':version)))
               (start      (if (eql version 1) 1 (getf properties ':chunk-start-sequence))))
          (unless (and (member version '(1 2)) (equal (getf properties ':id) id)
                       (typep start '(integer 1)) (or (null expected) (= start expected)))
            (parent-session--reject ':invalid-segment-header))
          (setf header candidate expected start)
          (dolist (record (rest forms))
            (unless (and (sexp-store:record-shape-p record :duplicate-keys ':first)
                         (eql (getf (rest record) ':seq) expected))
              (parent-session--reject ':invalid-record-sequence))
            (parent-session--tagged (parent-session--legacy-record record))
            (push record records)
            (incf expected)))))
    (values header (nreverse records) incomplete)))

(serapeum/types:-> parent-session--entry
                   (&key (:sequence integer) (:event string) (:turn string)
                    (:kind keyword) (:payload list) (:origin keyword)) list)
(defun parent-session--entry (&key sequence event turn kind payload origin)
  "Build one entry in the existing parent session log schema."
  (list ':session-entry :version 1 :record
        (list sequence event turn kind payload origin) :continuation nil))

(serapeum/types:-> parent-session--header-payload (list) list)
(defun parent-session--header-payload (header)
  "Project static legacy metadata before chronological records, excluding queues and counters."
  (let ((payload  (make-hash-table :test #'equal))
        (approved (make-hash-table :test #'equal)))
    (dolist (key '(:id :created-at :title :title-source :prompt-cache-key :model
                   :model-provider :reasoning-effort :artifact-root))
      (let ((value (getf (rest header) key)))
        (when (or (stringp value) (integerp value) (keywordp value))
          (setf (gethash (string-downcase key) approved)
                (if (keywordp value) (string-downcase value) value)))))
    (setf (gethash "text" payload) "Legacy conversation imported."
          (gethash "legacy_header" payload) approved)
    (session-store--value-data (protocol-data->value payload))))

(serapeum/types:-> parent-session-migrate
                   (parent-configuration string &key (:legacy-root (or null pathname))) session-store)
(defun parent-session-migrate (configuration identifier &key legacy-root)
  "Atomically publish a real parent log containing every complete historical record.
The original files are read-only. A failed publication can be retried. Historical
records retain agent origin, and never authorize a parent action."
  (let* ((id     (parent-session--component identifier))
         (path   (parent-session-path configuration id "session.sexp"))
         (marker (parent-session-path configuration id "migration.sexp"))
         (lock   (parent-session-path configuration id "migration.lock"))
         (root   (or legacy-root
                     (let ((value (parent-configuration-get configuration ':legacy-conversations-root)))
                       (and value (pathname value))))))
    (unless root (parent-session--reject ':unknown-session))
    (ensure-directories-exist path)
    (ls-flock:with-file-lock (lock)
      (when (probe-file path)
        (return-from parent-session-migrate (session-store-open path id :recover-interrupted nil)))
      (multiple-value-bind (header records incomplete) (parent-session-read-legacy root id)
        (multiple-value-setq (header records)
          (parent-session-project-artifacts configuration id :header header :records records))
        (let* ((turn     "legacy-import")
               (sequence 0)
               (entries  (list (list ':session-header :version 1 :id id))))
          (push (parent-session--entry
                 :sequence (incf sequence) :event "legacy/import-start" :turn turn :kind ':note
                 :payload (parent-session--header-payload header)
                 :origin  ':parent) entries)
          (dolist (record records)
            (let ((payload (make-hash-table :test 'equal)))
              (setf (gethash "version" payload) 1
                    (gethash "agent_record" payload)
                    (parent-session--tagged (parent-session--legacy-record record)))
              (push (parent-session--entry
                     :sequence (incf sequence) :event (format nil "legacy/~D" sequence)
                     :turn turn :kind ':note
                     :payload (session-store--value-data (protocol-data->value payload))
                     :origin  ':agent) entries)))
          (push (parent-session--entry
                 :sequence (incf sequence) :event "legacy/import-finish" :turn turn :kind ':turn-outcome
                 :payload (list ':map (list (list "outcome" (list ':string "completed"))))
                 :origin  ':parent) entries)
          ;; Publish metadata before the log. Only a valid log makes an import visible.
          (sexp-store:log-write marker (list (list ':legacy-migration :version 1 :id id
                                                  :header header :incomplete-tail-p incomplete
                                                  :record-count (length records))))
          (sexp-store:log-write path (nreverse entries) :require-absent t)
          (session-store-open path id :recover-interrupted nil))))))

(serapeum/types:-> parent-session-ensure
                   (parent-configuration string &key (:legacy-root (or null pathname)) (:read-only-p boolean)) session-store)
(defun parent-session-ensure (configuration identifier &key legacy-root read-only-p)
  "Open an existing parent session or migrate an existing legacy conversation."
  (let* ((id   (parent-session--component identifier))
         (path (parent-session-path configuration id "session.sexp")))
    (unless (probe-file path)
      (parent-session-migrate configuration id :legacy-root legacy-root))
    (session-store-open path id :recover-interrupted nil :read-only-p read-only-p)))

(serapeum/types:-> parent-session-legacy-header (session-store) (or null list))
(defun parent-session-legacy-header (store)
  "Return the inert legacy header retained beside an imported session log."
  (let ((marker (merge-pathnames "migration.sexp" (session-store-pathname store))))
    (when (probe-file marker)
      (multiple-value-bind (forms incomplete) (parent-session--read-forms marker)
        (let ((record (first forms)))
          (unless (and (not incomplete) (= (length forms) 1)
                       (sexp-store:record-shape-p record)
                       (eq (first record) ':legacy-migration)
                       (eql (getf (rest record) ':version) 1)
                       (equal (getf (rest record) ':id) (session-store-id store)))
            (parent-session--reject ':invalid-migration-metadata))
          (getf (rest record) ':header))))))


;;;; -- Session Snapshots and Replay --

(defclass parent-session-replay ()
  ((store :initarg :store :reader parent-session-replay-store
          :documentation "The private parent store from which this snapshot was read.")
   (records :initarg :records :reader parent-session-replay-records
            :documentation "The bounded snapshot of inert chronological records.")
   (turns :initarg :turns :reader parent-session-replay-turns
          :documentation "A user-turn number for each snapshot record.")
   (position :initform 0 :accessor parent-session-replay-position
             :documentation "The selected zero-based snapshot index."))
  (:documentation "Read-only navigation state. Opening it never runs the agent or a provider."))

(serapeum/types:-> parent-session-snapshot (session-store) list)
(defun parent-session-snapshot (store)
  "Copy validated session records under the same lock used by writers."
  (session-store--locked
   store (lambda (records active events turns calls)
           (declare (ignore active events turns calls))
           (labels ((copy-data (value)
                      (typecase value
                        (cons
                         (cons (copy-data (first value)) (copy-data (rest value))))
                        (string
                         (copy-seq value))
                        (t
                         value))))
             (map 'list #'copy-data records)))))

(serapeum/types:-> parent-session--untagged (t) t)
(defun parent-session--untagged (value)
  "Decode bounded tagged records, resolving only existing keyword symbols."
  (let ((remaining 8192))
    (labels ((decode (item depth)
               (unless (and (vectorp item) (not (stringp item))
                            (<= 1 (length item) 2) (<= depth 12)
                            (not (minusp (decf remaining))))
                 (parent-session--reject ':invalid-record))
               (let ((tag   (aref item 0))
                     (child (and (= (length item) 2) (aref item 1))))
                 (cond
                   ((and (equal tag "nil") (= (length item) 1))
                    nil)
                   ((and (equal tag "true") (= (length item) 1))
                    t)
                   ((and (equal tag "keyword") (stringp child) (<= (length child) 160))
                    (or (find-symbol child '#:keyword) (parent-session--reject ':invalid-record)))
                   ((and (equal tag "string") (stringp child) (<= (length child) 1048576))
                    child)
                   ((and (equal tag "integer") (typep child '(signed-byte 64)))
                    child)
                   ((and (equal tag "number") (typep child 'double-float))
                    child)
                   ((and (member tag '("list" "vector") :test #'equal)
                         (vectorp child) (not (stringp child)))
                    (map (if (equal tag "list") 'list 'vector)
                         (lambda (entry) (decode entry (1+ depth))) child))
                   (t
                    (parent-session--reject ':invalid-record))))))
      (decode value 0))))

(serapeum/types:-> parent-session-record->local (list) (or null list))
(defun parent-session-record->local (record)
  "Return the ordinary conversation record represented by a parent journal entry."
  (destructuring-bind (sequence event turn kind encoded origin) record
    (declare (ignore event turn))
    (let* ((payload (protocol-value->data (session-store--data-value encoded)))
           (agent   (if (eq kind ':tool-finished) (gethash "result" payload) payload)))
      (cond
        ((and (eq origin ':agent) (hash-table-p agent)
              (eql (gethash "version" agent) 1) (gethash "agent_record" agent))
         (let ((local (parent-session--untagged (gethash "agent_record" agent))))
           (unless (sexp-store:record-shape-p local :duplicate-keys ':first)
             (parent-session--reject ':invalid-record))
           (list* (first local) ':seq sequence (rest local))))
        ((and (eq origin ':parent) (eq kind ':user-input)
              (not (gethash "application_kind" payload)))
         (list ':message :seq sequence :role ':user :content (gethash "text" payload)
               :time (gethash "time" payload 0)))
        ((and (eq origin ':agent) (eq kind ':assistant-message))
         (list ':message :seq sequence :role ':assistant :content (gethash "text" payload)))
        (t
         nil)))))

(serapeum/types:-> parent-session-replay-open (session-store) parent-session-replay)
(defun parent-session-replay-open (store)
  "Capture all durable events and their user-turn positions without starting a child."
  (let* ((records (coerce (parent-session-snapshot store) 'vector))
         (turns   (make-array (length records)))
         (turn    0))
    (loop for record across records for index from 0
          for local = (parent-session-record->local record) do
      (when (and (eq (first local) ':message) (eq (getf (rest local) ':role) ':user)
                 (not (getf (rest local) ':automatic-p)))
        (incf turn))
      (setf (aref turns index) turn))
    (make-instance 'parent-session-replay :store store :records records :turns turns)))

(serapeum/types:-> parent-session--record-time (list) (or null integer))
(defun parent-session--record-time (record)
  "Return a positive universal time from an ordinary or parent result record."
  (let ((time (getf (rest (parent-session-record->local record)) ':time)))
    (and (typep time '(integer 1)) time)))

(serapeum/types:-> parent-session--date-time (string) integer)
(defun parent-session--date-time (text)
  "Parse a local calendar date, rejecting normalized nonexistent dates."
  (handler-case
      (let* ((year  (and (= (length text) 10) (char= (char text 4) #\-)
                        (char= (char text 7) #\-) (parse-integer text :end 4)))
             (month (parse-integer text :start 5 :end 7))
             (day   (parse-integer text :start 8))
             (time  (encode-universal-time 0 0 0 day month year)))
        (multiple-value-bind (second minute hour actual-day actual-month actual-year)
            (decode-universal-time time)
          (declare (ignore second minute hour))
          (unless (equal (list year month day) (list actual-year actual-month actual-day))
            (parent-session--reject ':invalid-date)))
        time)
    (parent-session-compatibility-error (condition) (error condition))
    (error () (parent-session--reject ':invalid-date))))

(serapeum/types:-> parent-session--clock-time (parent-session-replay string) integer)
(defun parent-session--clock-time (replay text)
  "Parse a local clock on the selected date, or a complete RFC3339 timestamp."
  (handler-case
      (if (find #\T text :test #'char-equal)
          (local-time:timestamp-to-universal (local-time:parse-timestring text))
          (let* ((parts  (uiop:split-string text :separator '(#\:)))
                 (hour   (parse-integer (first parts)))
                 (minute (parse-integer (second parts)))
                 (second (if (third parts) (parse-integer (third parts)) 0))
                 (record (aref (parent-session-replay-records replay) (parent-session-replay-position replay)))
                 (base   (or (parent-session--record-time record)
                             (loop for candidate across (parent-session-replay-records replay)
                                   thereis (parent-session--record-time candidate)))))
            (unless (and (<= 2 (length parts) 3) (<= 0 hour 23)
                         (<= 0 minute 59) (<= 0 second 59) base)
              (parent-session--reject ':invalid-time))
            (multiple-value-bind (ignored-second ignored-minute ignored-hour day month year)
                (decode-universal-time base)
              (declare (ignore ignored-second ignored-minute ignored-hour))
              (encode-universal-time second minute hour day month year))))
    (parent-session-compatibility-error (condition) (error condition))
    (error () (parent-session--reject ':invalid-time))))

(serapeum/types:-> parent-session-replay-select (parent-session-replay list) (or null list))
(defun parent-session-replay-select (replay selection)
  "Select the first event at a sequence, turn, date, or time and return that event."
  (let ((records (parent-session-replay-records replay)))
    (when (zerop (length records)) (return-from parent-session-replay-select nil))
    (when selection
      (unless (and (= (length selection) 2) (every #'stringp selection))
        (parent-session--reject ':invalid-selection))
      (let* ((kind (first selection)) (text (second selection))
             (number (when (member kind '("turn" "sequence" "record") :test #'string-equal)
                       (or (ignore-errors (parse-integer text :junk-allowed nil))
                           (parent-session--reject ':invalid-selection))))
             (time   (cond ((string-equal kind "date")
                            (parent-session--date-time text))
                           ((string-equal kind "time")
                            (parent-session--clock-time replay text))))
             (position
               (loop for record across records for index from 0
                     for candidate-time = (and time (parent-session--record-time record))
                     when (cond
                            ((string-equal kind "turn")
                             (= (aref (parent-session-replay-turns replay) index) number))
                            ((member kind '("sequence" "record") :test #'string-equal)
                             (>= (first record) number))
                            ((string-equal kind "date")
                             (and candidate-time (<= time candidate-time)
                                  (< candidate-time
                                     (multiple-value-bind (second minute hour day month year)
                                         (decode-universal-time time)
                                       (declare (ignore second minute hour))
                                       (encode-universal-time 0 0 0 (1+ day) month year)))))
                            ((string-equal kind "time")
                             (and candidate-time (>= candidate-time time)))
                            (t
                             (parent-session--reject ':invalid-selection))) return index)))
        (unless (and position (or (null number) (not (minusp number))))
          (parent-session--reject ':selection-not-found))
        (setf (parent-session-replay-position replay) position)))
    (aref records (parent-session-replay-position replay))))

(serapeum/types:-> parent-session-replay-move (parent-session-replay integer) (or null list))
(defun parent-session-replay-move (replay delta)
  "Move through the snapshot, clamping at either boundary."
  (let ((size (length (parent-session-replay-records replay))))
    (when (plusp size)
      (setf (parent-session-replay-position replay)
            (max 0 (min (1- size) (+ (parent-session-replay-position replay) delta)))))
    (parent-session-replay-select replay nil)))


;;;; -- Durable Historical Forks --

(serapeum/types:-> parent-session--fork-replay (session-store list string) list)
(defun parent-session--fork-replay (source records identifier)
  "Copy only provider requests visible in the selected session snapshot.
Keep opaque provider originals in the parent journal. Do not copy future or
unprojected results from the source session."
  (let ((requests (make-hash-table :test 'equal))
        (path     (merge-pathnames "replay.sexp" (session-store-pathname source))))
    (dolist (record records)
      (when (and (eq (fourth record) ':note) (eq (sixth record) ':parent))
        (let ((payload (protocol-value->data (session-store--data-value (fifth record)))))
          (when (gethash "model_result" payload)
            (setf (gethash (gethash "request_id" payload) requests) t)))))
    (when (probe-file path)
      (ls-flock:with-file-lock ((pathname (concatenate 'string (namestring path) ".lock")))
        (multiple-value-bind (forms incomplete) (parent-session--read-forms path)
          (declare (ignore incomplete))
          (unless (equal (first forms) (list ':model-replay :version 1 :session (session-store-id source)))
            (parent-session--reject ':invalid-replay-journal))
          (let ((selected
                  (remove-if-not (lambda (entry) (gethash (getf (rest entry) ':request) requests))
                                 (rest forms)))
                (state (model-replay-store--state)))
            (dolist (entry selected) (model-replay-store--apply state entry))
            (cons (list ':model-replay :version 1 :session identifier) selected)))))))

(serapeum/types:-> parent-session-fork
                   (parent-configuration string &key (:selection list) (:identifier (or null string))) session-store)
(defun parent-session-fork (configuration source-identifier &key selection identifier)
  "Fork actual durable content and scoped provider replay at an inclusive selector.
Read the source without recovery or tail repair. Publish the destination session
last, after all of its private sidecars are complete."
  (let* ((source      (parent-session-ensure configuration source-identifier :read-only-p t))
         (replay      (parent-session-replay-open source))
         (records     (parent-session-replay-records replay))
         (id          (parent-session--component (or identifier (parent-application--identifier))))
         (path        (parent-session-path configuration id "session.sexp"))
         (marker      (parent-session-path configuration id "migration.sexp"))
         (replay-path (parent-session-path configuration id "replay.sexp"))
         (lock        (parent-session-path configuration id "migration.lock")))
    (when (or (equal id (session-store-id source)) (zerop (length records)))
      (parent-session--reject ':invalid-fork))
    (ensure-directories-exist path)
    (ls-flock:with-file-lock (lock)
      (when (or (probe-file path)
                (let ((root (parent-configuration-get configuration ':legacy-conversations-root)))
                  (and root (parent-session--legacy-segments (pathname root) id))))
        (parent-session--reject ':session-exists))
     (if selection
         (parent-session-replay-select replay selection)
         (setf (parent-session-replay-position replay) (1- (length records))))
     (let* ((prefix          (coerce (subseq records 0 (1+ (parent-session-replay-position replay))) 'list))
            (head            (first (first (last prefix))))
            (provider-replay (parent-session--fork-replay source prefix id))
            (header          (or (copy-tree (parent-session-legacy-header source))
                                 (list ':conversation :version 2 :created-at (get-universal-time))))
            (state           (make-instance 'session-store-state))
            (entries         nil))
       (setf (getf (rest header) ':id) id
             (getf (rest header) ':chunk-start-sequence) 1
             (getf (rest header) ':prompt-cache-key)
             (or (getf (rest header) ':prompt-cache-key) (session-store-id source)))
       (multiple-value-setq (header prefix)
         (parent-session-project-fork configuration id :header header :records prefix))
       (dolist (record prefix)
         (when (and (eq (sixth record) ':parent) (eq (fourth record) ':note)
                    (session-store--field (fifth record) "legacy_header"))
           (setf record (copy-list record)
                 (fifth record) (parent-session--header-payload header)))
         (let ((entry (list ':session-entry :version 1 :record record :continuation nil)))
           ;; Preserve continuation metadata from the original private log below.
           (push entry entries)))
       (let ((continuations (make-hash-table)))
         (ls-flock:with-file-lock ((session-store-lock-pathname source))
           (multiple-value-bind (forms incomplete) (parent-session--read-forms (session-store-pathname source))
             (declare (ignore incomplete))
             (dolist (entry (rest forms))
               (let* ((data (getf (rest entry) ':record)) (sequence (first data)))
                 (when (and (integerp sequence) (<= sequence head))
                   (setf (gethash sequence continuations) (getf (rest entry) ':continuation)))))))
         (setf entries (nreverse entries))
         (dolist (entry entries)
           (setf (getf (rest entry) ':continuation)
                 (gethash (first (getf (rest entry) ':record)) continuations))
           (session-store--replay-entry state entry)))
       (let* ((active  (session-store-state-active state))
              (turn    (or active (format nil "fork/~A" id)))
              (payload (make-hash-table :test #'equal)))
         (labels ((append-entry (entry)
                    (session-store--replay-entry state entry)
                    (setf entries (append entries (list entry)))))
           (unless active
             (append-entry
              (parent-session--entry
               :sequence (incf head) :event (format nil "fork/~A/start" id)
               :turn turn :kind ':note :origin ':parent
               :payload (list ':map (list (list "text" (list ':string "Historical fork.")))))))
           (setf (gethash "version" payload) 1
                 (gethash "agent_record" payload)
                 (parent-session--tagged (list ':fork :source-id (session-store-id source)
                                              :source-sequence   (first (first (last prefix)))
                                              :selection selection :time (get-universal-time))))
           (append-entry
            (parent-session--entry
             :sequence (incf head) :event (format nil "fork/~A/provenance" id)
             :turn turn :kind ':note :origin ':agent
             :payload (session-store--value-data (protocol-data->value payload))))
           ;; Close a cut turn without retrying an unfinished external effect.
           (append-entry
            (parent-session--entry
             :sequence (incf head) :event (format nil "fork/~A/finish" id)
             :turn turn :kind ':turn-outcome :origin ':parent
             :payload (list ':map
                            (list (list "outcome" (list ':string (if active "interrupted" "completed")))
                                 (list "reason" (list ':string "Historical fork boundary."))))))))
       (when provider-replay (sexp-store:log-write replay-path provider-replay))
       (sexp-store:log-write marker
                             (list (list ':legacy-migration :version 1 :id id :header header
                                         :incomplete-tail-p nil :record-count (length prefix)
                                         :source-id       (session-store-id source)
                                         :source-sequence (first (first (last prefix)))
                                         :selection       selection)))
       (sexp-store:log-write path (cons (list ':session-header :version 1 :id id) entries)
                             :require-absent t)
       (session-store-open path id :recover-interrupted nil)))))


;;;; -- Read-only Replay Terminal --

(serapeum/types:-> parent-session-replay-write (parent-session-replay stream &key (:raw boolean)) null)
(defun parent-session-replay-write (replay output &key raw)
  "Render one inert event without exposing control sequences to the owner terminal."
  (let ((record (parent-session-replay-select replay nil)))
    (if record
        (let* ((position (parent-session-replay-position replay))
               (local    (parent-session-record->local record))
               (text     (let ((*print-readably* t) (*print-pretty* nil))
                           (write-to-string (if raw (or local record)
                                                (or local (list ':parent-event :kind (fourth record)
                                                                :sequence            (first record))))))))
          (format output "record ~D of ~D, turn ~D, sequence ~D~%~A~%"
                  (1+ position) (length (parent-session-replay-records replay))
                  (aref (parent-session-replay-turns replay) position) (first record)
                  (parent-terminal-escape text)))
        (format output "No durable records.~%")))
  (finish-output output)
  nil)

(serapeum/types:-> parent-session-replay-command (parent-session-replay string stream) boolean)
(defun parent-session-replay-command (replay line output)
  "Execute the current read-only replay navigation commands."
  (let* ((words   (remove "" (uiop:split-string line :separator '(#\Space #\Tab #\Newline #\Return))
                          :test #'string=))
         (command (string-downcase (or (first words) "next")))
         (value   (second words)))
    (when (> (length words) 2) (parent-session--reject ':invalid-replay-command))
    (cond
      ((member command '("q" "quit" "exit") :test #'string=)
       (when value (parent-session--reject ':invalid-replay-command))
       nil)
      ((member command '("h" "help" "?") :test #'string=)
       (when value (parent-session--reject ':invalid-replay-command))
       (format output "next [N], previous [N], turn N, sequence N, date YYYY-MM-DD, time TIME,~%first, last, print, raw, where, help, quit~%")
       t)
      ((member command '("n" "next" "p" "previous" "prev") :test #'string=)
       (let ((count (if value (ignore-errors (parse-integer value :junk-allowed nil)) 1)))
         (unless (typep count '(integer 1)) (parent-session--reject ':invalid-replay-command))
         (parent-session-replay-move replay (if (member command '("p" "previous" "prev") :test #'string=)
                                               (- count) count)))
       (parent-session-replay-write replay output)
       t)
      ((member command '("turn" "sequence" "record" "date" "time") :test #'string=)
       (unless value (parent-session--reject ':invalid-replay-command))
       (parent-session-replay-select replay (list command value))
       (parent-session-replay-write replay output)
       t)
      ((member command '("first" "last" "print" "raw" "where") :test #'string=)
       (when value (parent-session--reject ':invalid-replay-command))
       (when (equal command "first") (setf (parent-session-replay-position replay) 0))
       (when (equal command "last")
         (setf (parent-session-replay-position replay)
               (max 0 (1- (length (parent-session-replay-records replay))))))
       (parent-session-replay-write replay output :raw (equal command "raw"))
       t)
      (t
       (parent-session--reject ':unknown-replay-command)))))

(serapeum/types:-> parent-session-replay-run
                   (session-store &key (:selection list) (:input stream) (:output stream)) keyword)
(defun parent-session-replay-run (store &key selection (input *standard-input*) (output *standard-output*))
  "Inspect a durable snapshot without any child, provider, recovery, or tool execution."
  (let ((replay (parent-session-replay-open store)))
    (parent-session-replay-select replay selection)
    (parent-session-replay-write replay output)
    (loop
      (format output "replay> ")
      (finish-output output)
      (let ((line (read-line input nil nil)))
        (unless line (return ':completed))
        (handler-case
            (unless (parent-session-replay-command replay line output) (return ':completed))
          (parent-session-compatibility-error (condition)
            (format output "~A~%" condition)))))))
