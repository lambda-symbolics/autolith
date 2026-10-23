(in-package #:autolith)

;;;; -- Structured Knowledge Fixture Families --

;;; Each family runs ten scenarios in two variants through the same tool
;;; operations a model uses, against local files and replayed observations,
;;; with no provider. Expected values are computed independently here, so
;;; the engine is checked against plain arithmetic, and every scenario
;;; asserts that it made no provider request.

(defparameter *knowledge-fixture-day* (encode-universal-time 0 0 0 22 10 2026 0)
  "Midnight UTC of the day the fixtures' events take place.")

(defclass knowledge-fixture ()
  ((configuration
    :initarg :configuration
    :reader knowledge-fixture-configuration
    :documentation "The fixture's workspace configuration.")
   (workspace
    :initarg :workspace
    :reader knowledge-fixture-workspace
    :documentation "The fixture's workspace directory.")
   (conversation
    :initarg :conversation
    :reader knowledge-fixture-conversation
    :documentation "The conversation whose tool calls the fixture replays.")
   (context
    :initarg :context
    :reader knowledge-fixture-context
    :documentation "A tool context with full access, as an approving user grants.")
   (registry
    :initarg :registry
    :reader knowledge-fixture-registry
    :documentation "The default tool registry.")
   (label
    :initarg :label
    :reader knowledge-fixture-label
    :documentation "The scenario's name in assertion messages."))
  (:documentation "One fixture scenario's workspace, conversation, and tools."))


;;;; -- Running Scenarios --

(-> knowledge-fixture--call-with (string function) null)
(defun knowledge-fixture--call-with (label function)
  "Call FUNCTION with a fresh fixture named LABEL, asserting it makes no provider request."
  (with-test-configuration (base root)
    (let* ((workspace (merge-pathnames "workspace/" root))
           (configuration (progn
                            (ensure-directories-exist workspace)
                            (configuration-copy base :working-directory workspace)))
           (conversation (conversation-create configuration))
           (requests 0))
      (test-call-with-function-replacements
       (list (list 'provider-stream-turn
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (incf requests)
                     (error "Fixtures must not reach a provider."))))
       (lambda ()
         (funcall function
                  (make-instance 'knowledge-fixture
                                 :configuration configuration
                                 :workspace workspace
                                 :conversation conversation
                                 :registry (make-default-tool-registry
                                            :configuration configuration)
                                 :context (make-instance
                                           'tool-context
                                           :configuration configuration
                                           :worker nil
                                           :conversation conversation
                                           :command-authorization-function
                                           (lambda (command directory)
                                             (declare (ignore command directory))
                                             ':full-access))
                                 :label label))))
      (test-assert (zerop requests) (format nil "~A makes no provider request" label))))
  nil)

(-> knowledge-fixture--file (knowledge-fixture string string) pathname)
(defun knowledge-fixture--file (fixture name content)
  "Write workspace file NAME with CONTENT in FIXTURE."
  (memory-source-tests--write (merge-pathnames name (knowledge-fixture-workspace fixture)) content))

(-> knowledge-fixture--say (knowledge-fixture string) null)
(defun knowledge-fixture--say (fixture text)
  "Record that the user said TEXT in FIXTURE's conversation."
  (conversation-append-user-message (knowledge-fixture-conversation fixture) text)
  nil)

(-> knowledge-fixture--read (knowledge-fixture string &rest t) string)
(defun knowledge-fixture--read (fixture uri &rest arguments)
  "Read URI through FIXTURE's tools and return the result text."
  (tool-result-content
   (apply #'memory-resource-tests--call (knowledge-fixture-registry fixture)
          (knowledge-fixture-context fixture) "resource" "read" "uri" uri arguments)))

(-> knowledge-fixture--edit (knowledge-fixture &rest t) string)
(defun knowledge-fixture--edit (fixture &rest operations)
  "Apply OPERATIONS to memory:facts through FIXTURE's tools, asserting they succeed."
  (let ((result (memory-resource-tests--call
                 (knowledge-fixture-registry fixture) (knowledge-fixture-context fixture)
                 "resource" "edit"
                 "uri" "memory:facts"
                 "base-revision" (memory-resource-tests--field
                                  (knowledge-fixture--read fixture "memory:facts" "max-results" 1)
                                  "Revision: ")
                 "operations" (coerce operations 'vector))))
    (unless (tool-result-success-p result)
      (error "Fixture ~A edit failed: ~A" (knowledge-fixture-label fixture)
             (tool-result-content result)))
    (tool-result-content result)))

(-> knowledge-fixture--define (string list &key (:key list) (:template (option string)))
    json-object)
(defun knowledge-fixture--define (predicate arguments &key key template)
  "Return a predicate-define operation of PREDICATE with (NAME TYPE [UNIT]) ARGUMENTS."
  (apply #'json-object
         "op" "predicate-define" "predicate" predicate
         "arguments" (map 'vector (lambda (argument)
                                    (apply #'json-object "name" (first argument)
                                           "type" (second argument)
                                           (and (third argument)
                                                (list "unit" (third argument)))))
                          arguments)
         (append (and key (list "key" (coerce key 'vector)))
                 (and template (list "template" template)))))

(-> knowledge-fixture--assert (string list &rest t) json-object)
(defun knowledge-fixture--assert (predicate arguments &rest sources)
  "Return a fact-assert operation of PREDICATE with ARGUMENTS citing SOURCES."
  (apply #'json-object "op" "fact-assert" "predicate" predicate
         "arguments" (coerce arguments 'vector)
         (and sources (list "sources" (coerce sources 'vector)))))

(-> knowledge-fixture--view (knowledge-fixture) knowledge-view)
(defun knowledge-fixture--view (fixture)
  "Return FIXTURE's knowledge as its tools see it now."
  (memory-knowledge-view (knowledge-fixture-context fixture)))

(-> knowledge-fixture--holds (knowledge-fixture list) list)
(defun knowledge-fixture--holds (fixture pattern)
  "Return the sorted sentences of FIXTURE's facts matching PATTERN."
  (sort (mapcar #'knowledge-node-text
                (knowledge-view-holds (knowledge-fixture--view fixture) pattern))
        #'string<))

(-> knowledge-fixture--claim (knowledge-fixture list) string)
(defun knowledge-fixture--claim (fixture pattern)
  "Return the identifier of the one fact of FIXTURE matching PATTERN."
  (let ((nodes (knowledge-view-holds (knowledge-fixture--view fixture) pattern)))
    (unless (= (length nodes) 1)
      (error "Fixture ~A expected one fact matching ~S, found ~D."
             (knowledge-fixture-label fixture) pattern (length nodes)))
    (knowledge-node-identifier (first nodes))))

(-> knowledge-fixture--check (knowledge-fixture t string) null)
(defun knowledge-fixture--check (fixture value description)
  "Assert VALUE in FIXTURE, labelling DESCRIPTION with the scenario."
  (test-assert value (format nil "~A: ~A" (knowledge-fixture-label fixture) description))
  nil)

(-> knowledge-fixture--clock-text (integer) string)
(defun knowledge-fixture--clock-text (minutes)
  "Return MINUTES after midnight as HH:MM."
  (format nil "~2,'0D:~2,'0D" (floor minutes 60) (mod minutes 60)))

(-> knowledge-fixture--instant-text (integer) string)
(defun knowledge-fixture--instant-text (minutes)
  "Return the fixture day's instant MINUTES after midnight as displayed."
  (knowledge-term-display (knowledge-make-instant (+ *knowledge-fixture-day* (* 60 minutes)))))


;;;; -- Appointment Family --

(-> knowledge-fixture--appointment-vocabulary () list)
(defun knowledge-fixture--appointment-vocabulary ()
  "Return the operations defining the appointment vocabulary and departure rule."
  (list (knowledge-fixture--define "event.start" '(("event" "entity") ("time" "instant"))
                                   :key '("event") :template "{event} starts at {time}")
        (knowledge-fixture--define "event.travel" '(("event" "entity") ("duration" "duration"))
                                   :key '("event"))
        (knowledge-fixture--define "event.buffer" '(("event" "entity") ("duration" "duration"))
                                   :key '("event"))
        (knowledge-fixture--define "event.departure" '(("event" "entity") ("time" "instant"))
                                   :key '("event") :template "leave for {event} at {time}")
        (json-object "op" "rule-propose" "id" "departure"
                     "premises" #(#("event.start" "?event" "?start")
                                  #("event.travel" "?event" "?travel")
                                  #("event.buffer" "?event" "?buffer"))
                     "bindings" #(#("?leave" #("-" "?start" #("+" "?travel" "?buffer"))))
                     "conclusion" #("event.departure" "?event" "?leave"))))

(-> knowledge-fixture--appointment (integer integer) null)
(defun knowledge-fixture--appointment (index variant)
  "Run appointment scenario INDEX in VARIANT: derive a departure, change one input."
  (let* ((kind (mod index 5))
         (event (format nil "review-~D" index))
         (start (+ (* 8 60) (* 45 index) (* 15 variant)))
         (travel (+ 10 (* 5 index)))
         (buffer (* 5 (1+ variant)))
         (label (format nil "appointment ~D/~D" index variant)))
    (knowledge-fixture--call-with
     label
     (lambda (fixture)
       (flet ((schedule (minutes room)
                (knowledge-fixture--file
                 fixture "schedule.txt"
                 (format nil "Design ~A at ~A UTC.~%Room ~D.~%"
                         event (knowledge-fixture--clock-text minutes) room)))

              (departures ()
                (knowledge-fixture--holds fixture '("event.departure" "?e" "?t")))

              (departure (minutes)
                (format nil "leave for ~A at ~A" event (knowledge-fixture--instant-text minutes)))

              (start-fact (minutes)
                (knowledge-fixture--assert
                 "event.start"
                 (list event (format nil "2026-10-22T~A:00Z" (knowledge-fixture--clock-text minutes)))
                 (json-object "file" "schedule.txt"
                              "quote" (format nil "~A at ~A" event
                                              (knowledge-fixture--clock-text minutes))))))
         (schedule start 4)
         (knowledge-fixture--say fixture (format nil "Travel takes ~D minutes, and I want ~D minutes of buffer."
                                                 travel buffer))
         (apply #'knowledge-fixture--edit fixture (knowledge-fixture--appointment-vocabulary))
         (knowledge-fixture--edit
          fixture
          (start-fact start)
          (knowledge-fixture--assert
           "event.travel" (list event (format nil "PT~DM" travel))
           (if (= kind 2)
               (json-object "url" (format nil "https://maps.example.com/~A" event)
                            "quote" (format nil "route takes ~D min" travel))
               (json-object "user" (format nil "Travel takes ~D minutes" travel))))
          (knowledge-fixture--assert
           "event.buffer" (list event (format nil "PT~DM" buffer))
           (json-object "user" (format nil "~D minutes of buffer" buffer))))
         (knowledge-fixture--check fixture
                                   (equal (departures) (list (departure (- start travel buffer))))
                                   "the departure is start minus travel and buffer")
         (knowledge-fixture--read fixture "memory:changes")
         (ecase kind
           (0
            (schedule (+ start 60) 4)
            (knowledge-fixture--edit fixture (start-fact (+ start 60)))
            (knowledge-fixture--check fixture
                                      (equal (departures)
                                             (list (departure (- (+ start 60) travel buffer))))
                                      "a moved event recomputes the departure")
            (knowledge-fixture--check fixture
                                      (search (format nil "changed: ~A, was ~A"
                                                      (departure (- (+ start 60) travel buffer))
                                                      (departure (- start travel buffer)))
                                              (knowledge-fixture--read fixture "memory:changes"))
                                      "memory:changes reports the moved departure"))
           (1
            (knowledge-fixture--edit
             fixture
             (json-object "op" "fact-retract"
                          "id" (knowledge-fixture--claim fixture (list "event.travel" event "?d"))))
            (knowledge-fixture--check fixture (null (departures))
                                      "a retracted travel time withdraws the departure")
            (knowledge-fixture--check fixture
                                      (search "was retracted"
                                              (knowledge-fixture--read fixture "memory:changes"))
                                      "the withdrawal names the retraction"))
           (2
            (let ((*knowledge-clock*
                    (let ((later (+ (get-universal-time) *knowledge-url-lifetime* 60)))
                      (lambda () later))))
              (knowledge-fixture--check fixture (null (departures))
                                        "an expired route estimate withdraws the departure")
              (knowledge-fixture--check fixture
                                        (search "expired"
                                                (knowledge-fixture--read fixture "memory:changes"))
                                        "the withdrawal names the expiry")))
           (3
            (knowledge-fixture--say fixture (format nil "Make it ~D minutes of buffer." (* 2 buffer)))
            (let ((old (knowledge-fixture--claim fixture (list "event.buffer" event "?b"))))
              (knowledge-fixture--edit
               fixture
               (knowledge-fixture--assert
                "event.buffer" (list event (format nil "PT~DM" (* 2 buffer)))
                (json-object "user" (format nil "~D minutes of buffer." (* 2 buffer)))))
              (let ((view (knowledge-fixture--view fixture)))
                (knowledge-fixture--check
                 fixture
                 (and (= 2 (length (departures)))
                      (every (lambda (node) (eq (knowledge-node-status view node) ':conflicted))
                             (knowledge-view-holds view '("event.departure" "?e" "?t"))))
                 "two buffers yield two conflicting departures, no winner chosen"))
              (knowledge-fixture--edit fixture (json-object "op" "fact-retract" "id" old))
              (knowledge-fixture--check fixture
                                        (equal (departures)
                                               (list (departure (- start travel (* 2 buffer)))))
                                        "retracting the old buffer settles the departure")))
           (4
            (schedule start 5)
            (knowledge-fixture--edit fixture (start-fact start))
            (knowledge-fixture--check fixture
                                      (equal (departures) (list (departure (- start travel buffer))))
                                      "reobserving the schedule yields the same departure")
            (knowledge-fixture--check fixture
                                      (search "Nothing changed"
                                              (knowledge-fixture--read fixture "memory:changes"))
                                      "reobserving the room edit reports no fact change")))))))
  nil)

(-> test-knowledge-appointment-fixtures () null)
(defun test-knowledge-appointment-fixtures ()
  "Run the appointment family: ten scenarios in two variants."
  (dotimes (index 10)
    (dotimes (variant 2)
      (knowledge-fixture--appointment index variant)))
  nil)


;;;; -- Document Family --

(-> knowledge-fixture--document-vocabulary () list)
(defun knowledge-fixture--document-vocabulary ()
  "Return the operations defining service limits, usage, and the over-limit rule."
  (list (knowledge-fixture--define "service.limit" '(("service" "entity") ("limit" "quantity" "MB"))
                                   :key '("service") :template "{service} may use {limit}")
        (knowledge-fixture--define "service.usage" '(("service" "entity") ("usage" "quantity" "MB"))
                                   :key '("service") :template "{service} uses {usage}")
        (knowledge-fixture--define "service.over" '(("service" "entity") ("excess" "quantity" "MB"))
                                   :key '("service") :template "{service} is {excess} over its limit")
        (json-object "op" "rule-propose" "id" "over-limit"
                     "premises" #(#("service.limit" "?service" "?limit")
                                  #("service.usage" "?service" "?usage"))
                     "guards" #(#(">" "?usage" "?limit"))
                     "bindings" #(#("?excess" #("-" "?usage" "?limit")))
                     "conclusion" #("service.over" "?service" "?excess"))))

(-> knowledge-fixture--document (integer integer) null)
(defun knowledge-fixture--document (index variant)
  "Run document scenario INDEX in VARIANT: limits from two documents, usage from metrics."
  (let* ((kind (mod index 5))
         (service (format nil "svc-~D" index))
         (limit (+ 100 (* 10 index)))
         (usage (+ limit (if (zerop variant) 20 -20)))
         (label (format nil "document ~D/~D" index variant)))
    (knowledge-fixture--call-with
     label
     (lambda (fixture)
       (labels ((write-readme (value)
                  (knowledge-fixture--file fixture "README.md"
                                           (format nil "# ~A~%~A memory limit: ~D MB.~%" service service value)))

                (write-spec (value)
                  (knowledge-fixture--file fixture "SPEC.md"
                                           (format nil "The ~A limit is ~D MB.~%" service value)))

                (write-metrics (value)
                  (knowledge-fixture--file fixture "metrics.txt"
                                           (format nil "~A usage: ~D MB~%" service value)))

                (limit-fact (value &rest files)
                  (apply #'knowledge-fixture--assert "service.limit"
                         (list service (json-object "magnitude" value "unit" "MB"))
                         (mapcar (lambda (file)
                                   (json-object "file" file
                                                "quote" (if (string= file "README.md")
                                                            (format nil "~A memory limit: ~D MB"
                                                                    service value)
                                                            (format nil "~A limit is ~D MB"
                                                                    service value))))
                                 files)))

                (usage-fact (value)
                  (knowledge-fixture--assert "service.usage"
                                             (list service (json-object "magnitude" value "unit" "MB"))
                                             (json-object "file" "metrics.txt"
                                                          "quote" (format nil "~A usage: ~D MB"
                                                                          service value))))

                (overs ()
                  (knowledge-fixture--holds fixture '("service.over" "?s" "?e")))

                (over (excess)
                  (format nil "~A is ~D MB over its limit" service excess)))
         (write-readme limit)
         (write-spec limit)
         (write-metrics usage)
         (apply #'knowledge-fixture--edit fixture (knowledge-fixture--document-vocabulary))
         (knowledge-fixture--edit fixture
                                  (limit-fact limit "README.md" "SPEC.md")
                                  (usage-fact usage))
         (knowledge-fixture--check fixture
                                   (equal (overs) (and (> usage limit) (list (over (- usage limit)))))
                                   "the excess is derived only for usage over the limit")
         (knowledge-fixture--read fixture "memory:changes")
         (ecase kind
           (0
            (write-readme (+ limit 50))
            (knowledge-fixture--check fixture
                                      (equal (overs) (and (> usage limit) (list (over (- usage limit)))))
                                      "one revised document leaves the limit supported by the other")
            (knowledge-fixture--check fixture
                                      (search "Nothing changed"
                                              (knowledge-fixture--read fixture "memory:changes"))
                                      "an alternative source keeps the facts unchanged"))
           (1
            (write-readme (+ limit 40))
            (write-spec (+ limit 40))
            (knowledge-fixture--edit fixture (limit-fact (+ limit 40) "README.md" "SPEC.md"))
            (knowledge-fixture--check fixture (null (overs))
                                      "a raised limit in both documents clears the excess")
            (when (> usage limit)
              (knowledge-fixture--check
               fixture
               (search (format nil "no longer holds: ~A" (over (- usage limit)))
                       (knowledge-fixture--read fixture "memory:changes"))
               "the cleared excess is reported")))
           (2
            (write-spec (+ limit 30))
            (knowledge-fixture--edit fixture (limit-fact (+ limit 30) "SPEC.md"))
            (let* ((view (knowledge-fixture--view fixture))
                   (limits (knowledge-view-holds view '("service.limit" "?s" "?l"))))
              (knowledge-fixture--check
               fixture
               (and (= 2 (length limits))
                    (every (lambda (node) (eq (knowledge-node-status view node) ':conflicted))
                           limits))
               "documents stating different limits conflict, both kept")))
           (3
            (write-metrics (+ usage 50))
            (knowledge-fixture--edit fixture (usage-fact (+ usage 50)))
            (knowledge-fixture--check fixture
                                      (equal (overs) (list (over (- (+ usage 50) limit))))
                                      "new usage recomputes the excess"))
           (4
            (delete-file (merge-pathnames "metrics.txt" (knowledge-fixture-workspace fixture)))
            (knowledge-fixture--check fixture (null (overs))
                                      "deleted metrics leave no excess")
            (knowledge-fixture--check
             fixture
             (eq (knowledge-node-applicability
                  (knowledge-view-node
                   (knowledge-fixture--view fixture)
                   (knowledge-claim-identifier
                    (list "service.usage" service (list :quantity usage "MB"))
                    :scope ':workspace
                    :workspace (namestring (knowledge-fixture-workspace fixture)))))
                 ':stale)
             "the usage fact is stale once its file is gone")))))))
  nil)

(-> test-knowledge-document-fixtures () null)
(defun test-knowledge-document-fixtures ()
  "Run the document family: ten scenarios in two variants."
  (dotimes (index 10)
    (dotimes (variant 2)
      (knowledge-fixture--document index variant)))
  nil)


;;;; -- CSV Report Family --

(-> knowledge-fixture--sales-vocabulary () list)
(defun knowledge-fixture--sales-vocabulary ()
  "Return the operations defining sales facts and the margin and loss rules."
  (list (knowledge-fixture--define "sale.amount" '(("order" "entity") ("amount" "quantity" "EUR"))
                                   :key '("order"))
        (knowledge-fixture--define "sale.cost" '(("order" "entity") ("cost" "quantity" "EUR"))
                                   :key '("order"))
        (knowledge-fixture--define "sale.margin" '(("order" "entity") ("margin" "quantity" "EUR"))
                                   :key '("order") :template "{order} margin {margin}")
        (knowledge-fixture--define "sale.loss" '(("order" "entity"))
                                   :template "{order} loses money")
        (json-object "op" "rule-propose" "id" "margin"
                     "premises" #(#("sale.amount" "?order" "?amount")
                                  #("sale.cost" "?order" "?cost"))
                     "bindings" #(#("?margin" #("-" "?amount" "?cost")))
                     "conclusion" #("sale.margin" "?order" "?margin"))
        (json-object "op" "rule-propose" "id" "loss"
                     "premises" #(#("sale.margin" "?order" "?margin"))
                     "guards" #(#("<" "?margin" #.(json-object "magnitude" 0 "unit" "EUR")))
                     "conclusion" #("sale.loss" "?order"))))

(-> knowledge-fixture--sales (integer integer) null)
(defun knowledge-fixture--sales (index variant)
  "Run CSV scenario INDEX in VARIANT: per-row margins, one source line changing at a time."
  (let* ((kind (mod index 5))
         (label (format nil "sales ~D/~D" index variant))
         (rows (loop for row from 1 to 3
                     collect (list (format nil "o-~D-~D" index row)
                                   (+ 100 (* 10 row) index)
                                   (+ 80 (* 5 row) (if (and (= variant 1) (= row 3)) 60 0))))))
    (knowledge-fixture--call-with
     label
     (lambda (fixture)
       (labels ((line (row)
                  (format nil "~A,~D,~D" (first row) (second row) (third row)))

                (write-csv (rows &optional header)
                  (knowledge-fixture--file fixture "sales.csv"
                                           (format nil "~@[~A~%~]order,amount,cost~%~{~A~%~}"
                                                   header (mapcar #'line rows))))

                (row-facts (row)
                  (let ((source (json-object "file" "sales.csv" "quote" (line row))))
                    (list (knowledge-fixture--assert
                           "sale.amount"
                           (list (first row) (json-object "magnitude" (second row) "unit" "EUR"))
                           source)
                          (knowledge-fixture--assert
                           "sale.cost"
                           (list (first row) (json-object "magnitude" (third row) "unit" "EUR"))
                           source))))

                (margins ()
                  (knowledge-fixture--holds fixture '("sale.margin" "?o" "?m")))

                (expected (rows)
                  (sort (mapcar (lambda (row)
                                  (format nil "~A margin ~D EUR" (first row)
                                          (- (second row) (third row))))
                                rows)
                        #'string<))

                (losses ()
                  (knowledge-fixture--holds fixture '("sale.loss" "?o"))))
         (write-csv rows)
         (apply #'knowledge-fixture--edit fixture (knowledge-fixture--sales-vocabulary))
         (apply #'knowledge-fixture--edit fixture (mapcan #'row-facts rows))
         (knowledge-fixture--check fixture (equal (margins) (expected rows))
                                   "each row's margin is amount minus cost")
         (knowledge-fixture--check fixture
                                   (equal (losses)
                                          (and (= variant 1)
                                               (list (format nil "~A loses money"
                                                             (first (third rows))))))
                                   "only a negative margin is a loss")
         (knowledge-fixture--read fixture "memory:changes")
         (ecase kind
           (0
            (let ((changed (list (list (first (second rows)) (second (second rows))
                                       (+ 7 (third (second rows)))))))
              (setf rows (list (first rows) (first changed) (third rows)))
              (write-csv rows)
              (apply #'knowledge-fixture--edit fixture (mapcan #'row-facts rows))
              (knowledge-fixture--check fixture (equal (margins) (expected rows))
                                        "an edited row recomputes only its margin")
              (let ((changes (knowledge-fixture--read fixture "memory:changes")))
                (knowledge-fixture--check
                 fixture
                 (and (search (first (first changed)) changes)
                      (not (search (first (first rows)) changes))
                      (not (search (first (third rows)) changes)))
                 "changes name only the edited row"))))
           (1
            (delete-file (merge-pathnames "sales.csv" (knowledge-fixture-workspace fixture)))
            (knowledge-fixture--check fixture (null (margins))
                                      "a deleted report withdraws every margin"))
           (2
            (let ((added (list (format nil "o-~D-4" index) 200 150)))
              (write-csv (append rows (list added)))
              (apply #'knowledge-fixture--edit fixture (mapcan #'row-facts (append rows (list added))))
              (knowledge-fixture--check fixture
                                        (equal (margins) (expected (append rows (list added))))
                                        "an added row derives its own margin")))
           (3
            (write-csv rows "# exported nightly")
            (apply #'knowledge-fixture--edit fixture (mapcan #'row-facts rows))
            (knowledge-fixture--check fixture (equal (margins) (expected rows))
                                      "reobserving the rows yields the same margins")
            (knowledge-fixture--check fixture
                                      (search "Nothing changed"
                                              (knowledge-fixture--read fixture "memory:changes"))
                                      "reobserving the header edit reports no fact change"))
           (4
            (let ((loss (list (first (first rows)) (second (first rows))
                              (+ 15 (second (first rows))))))
              (setf rows (cons loss (rest rows)))
              (write-csv rows)
              (apply #'knowledge-fixture--edit fixture (mapcan #'row-facts rows))
              (knowledge-fixture--check fixture
                                        (member (format nil "~A loses money" (first loss)) (losses)
                                                :test #'string=)
                                        "a cost above the amount derives a loss"))))))))
  nil)

(-> test-knowledge-sales-fixtures () null)
(defun test-knowledge-sales-fixtures ()
  "Run the CSV report family: ten scenarios in two variants."
  (dotimes (index 10)
    (dotimes (variant 2)
      (knowledge-fixture--sales index variant)))
  nil)


;;;; -- Scale --

(-> knowledge-benchmark--populate (configuration integer integer) null)
(defun knowledge-benchmark--populate (configuration events supports)
  "Write EVENTS appointments of three claims each, SUPPORTS user statements per claim.

Records are appended in transactions of twenty, as interactive writes would."
  (let* ((workspace (knowledge--workspace configuration))
         (time (get-universal-time))
         (records nil))
    (dotimes (index events)
      (let ((event (format nil "event-~D" index)))
        (dolist (proposition
                 (list (list "event.start" event (knowledge-make-instant (+ time (* 60 index))))
                       (list "event.travel" event (list :duration (* 60 (mod index 50))))
                       (list "event.buffer" event (list :duration 600))))
          (let ((claim (knowledge-claim-identifier proposition :scope ':workspace
                                                               :workspace workspace)))
            (dotimes (k supports)
              (let ((evidence (knowledge-observe
                               :source (list :user :quote (format nil "statement ~D about ~A"
                                                                  k event))
                               :adapter "user/1" :time time)))
                (push evidence records)
                (push (list :claim-asserted :version *knowledge-format-version*
                            :support (make-identifier) :claim claim
                            :scope ':workspace :workspace workspace :proposition proposition
                            :evidence (getf (rest evidence) :id) :basis ':user-assertion
                            :valid-from nil :valid-until nil :time time
                            :source-conversation nil)
                      records)))))))
    (let* ((pathname (configuration-knowledge-path configuration))
           (ordered (nreverse records)))
      (sexp-store:log-write
       pathname
       (append (or (sexp-store:log-read pathname) (list (knowledge--header)))
               (loop while ordered
                     collect (list :transaction :version *knowledge-format-version*
                                   :records (loop repeat 20 while ordered
                                                  collect (pop ordered))))))))
  nil)

(-> knowledge-benchmark (&key (:events integer) (:supports integer)) list)
(defun knowledge-benchmark (&key (events 3334) (supports 5))
  "Measure knowledge at scale: by default 10,002 claims with 50,010 supports.

Return seconds for a cold read, an evaluation, a reused evaluation, ranking
facts for recall, one more assertion, and the evaluation after it, with log
bytes, heap bytes, and the machine measured on."
  (let ((result nil))
    (with-test-configuration (base root)
      (multiple-value-bind (configuration workspace)
          (memory-source-tests--workspace base root)
        (declare (ignore workspace))
        (knowledge-tests--define-appointments configuration)
        (knowledge-tests--admit-departure configuration)
        (knowledge-benchmark--populate configuration events supports)
        (flet ((seconds (function)
                 (let ((start (get-internal-real-time)))
                   (funcall function)
                   (/ (- (get-internal-real-time) start)
                      (float internal-time-units-per-second 1d0)))))
          (clrhash *knowledge-base-cache*)
          (clrhash *knowledge-view-cache*)
          (let* ((cold (seconds (lambda () (knowledge-base-read configuration))))
                 (view nil)
                 (evaluate (seconds (lambda () (setf view (knowledge-view-current configuration)))))
                 (reuse (seconds (lambda () (knowledge-view-current configuration))))
                 (recall (seconds (lambda ()
                                    (memory-context--relevant-facts view "leave for event-17"))))
                 (assert (seconds (lambda ()
                                    (knowledge-tests--said configuration "event.buffer"
                                                           '("event-1" "PT20M")
                                                           "a later buffer statement"))))
                 (reevaluate (seconds (lambda () (setf view (knowledge-view-current configuration))))))
            (sb-ext:gc :full t)
            (setf result
                  (list :claims (hash-table-count
                                 (knowledge-base-claims (knowledge-base-read configuration)))
                        :supports (hash-table-count
                                   (knowledge-base-supports (knowledge-base-read configuration)))
                        :derived (length (knowledge-view-holds view '("event.departure" "?e" "?t")))
                        :incomplete (knowledge-view-incomplete view)
                        :cold-read cold :evaluate evaluate :reuse reuse :recall recall
                        :assert assert :reevaluate reevaluate
                        :log-bytes (with-open-file (stream (configuration-knowledge-path configuration)
                                                           :element-type '(unsigned-byte 8))
                                     (file-length stream))
                        :heap-bytes (sb-kernel:dynamic-usage)
                        :machine (list (machine-type) (machine-version)
                                       (lisp-implementation-type)
                                       (lisp-implementation-version))))))))
    result))

(-> test-knowledge-scale () null)
(defun test-knowledge-scale ()
  "Test a thousand-claim knowledge base: incremental appends, complete evaluation, work caps."
  (let ((result (knowledge-benchmark :events 334 :supports 3)))
    (test-assert (and (= (getf result :claims) 1003)
                      (= (getf result :supports) 3007)
                      (= (getf result :derived) 335)
                      (null (getf result :incomplete)))
                 "every derivation completes within the default limits"))
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (declare (ignore workspace))
      (knowledge-tests--define-appointments configuration)
      (knowledge-tests--admit-departure configuration)
      (knowledge-benchmark--populate configuration 334 1)
      (let* ((pathname (configuration-knowledge-path configuration))
             (before (platform-path-status *platform* pathname :follow-links-p t))
             (revision (knowledge-base-revision (knowledge-base-read configuration))))
        (knowledge-tests--said configuration "event.buffer" '("event-1" "PT20M")
                               "a later buffer statement")
        (test-assert (and (platform-file-status-same-object-p
                           before (platform-path-status *platform* pathname :follow-links-p t))
                          (= (knowledge-base-revision (knowledge-base-read configuration))
                             (+ revision 2)))
                     "a write appends to the same log and the read folds only what it added"))
      (let ((*knowledge-match-limit* 100))
        (clrhash *knowledge-view-cache*)
        (let ((view (knowledge-view-current configuration)))
          (test-assert (and (equal (knowledge-view-incomplete view) '("departure"))
                            (= 100 (length (knowledge-view-holds view '("event.departure" "?e" "?t")))))
                       "a reached work limit is reported instead of evaluating further")))))
  nil)
