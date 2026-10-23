(in-package #:autolith)

;;;; -- Structured Knowledge Tests --

(-> knowledge-tests--error-code (function) (option keyword))
(defun knowledge-tests--error-code (function)
  "Return the KNOWLEDGE-ERROR code FUNCTION signals, or NIL when it returns."
  (handler-case
      (progn
        (funcall function)
        nil)
    (knowledge-error (condition)
      (knowledge-error-code condition))))


(-> test-knowledge-terms () null)
(defun test-knowledge-terms ()
  "Test term parsing, canonical forms, comparison, and presentation."
  (dolist (case '(("2026-10-22T13:00:00Z" :instant (:instant "2026-10-22T13:00:00Z"))
                  ("2026-10-22T15:00+02:00" :instant (:instant "2026-10-22T13:00:00Z"))
                  ("2026-10-22T07:30:15.250-0530" :instant (:instant "2026-10-22T13:00:15Z"))
                  ("2026-10-22" :date (:date "2026-10-22"))
                  ("PT30M" :duration (:duration 1800))
                  ("P1DT2H" :duration (:duration 93600))
                  ("-PT5M" :duration (:duration -300))
                  (90 :duration (:duration 90))
                  ("2.5" :number 5/2)
                  (7 :number 7)
                  ("true" :boolean :true)
                  ("event-42" :entity "event-42")
                  (("30" "km") :quantity :invalid)
                  ((30 "km") :quantity (:quantity 30 "km"))
                  ((12 "EUR") :quantity (:quantity 12 "EUR"))))
    (destructuring-bind (input type expected) case
      (test-assert (if (eq expected :invalid)
                       (eq (knowledge-tests--error-code
                            (lambda () (knowledge-term-from-input input type)))
                           ':invalid-term)
                       (equal (knowledge-term-from-input input type) expected))
                   (format nil "~S as ~A reads as ~S" input type expected))))
  (dolist (case '(("2026-10-22T13:00" :instant)
                  ("2026-02-30" :date)
                  ("P1M" :duration)
                  ("PT" :duration)
                  ("" :entity)
                  ((3 "furlongs") :quantity)
                  ("maybe" :boolean)))
    (destructuring-bind (input type) case
      (test-assert (eq (knowledge-tests--error-code
                        (lambda () (knowledge-term-from-input input type)))
                       ':invalid-term)
                   (format nil "~S is rejected as ~A" input type))))
  (test-assert (and (= (knowledge-term-compare '(:quantity 1 "km") '(:quantity 999 "m")) 1)
                    (knowledge-term= '(:quantity 1 "km") '(:quantity 1000 "m"))
                    (= (knowledge-term-compare '(:instant "2026-10-22T13:00:00Z")
                                               '(:instant "2026-10-22T14:00:00Z"))
                       -1)
                    (= (knowledge-term-compare '(:duration 60) '(:duration 60)) 0))
               "terms compare within their class, converting units")
  (test-assert (eq (knowledge-tests--error-code
                    (lambda ()
                      (knowledge-term-compare '(:quantity 1 "km") '(:quantity 1 "kg"))))
                   ':incomparable)
               "different dimensions do not compare")
  (test-assert (not (knowledge-term= '(:quantity 1 "EUR") '(:quantity 1 "USD")))
               "different currencies are different values")
  (test-assert (equal (mapcar #'knowledge-term-display
                              '((:instant "2026-10-22T13:00:00Z") (:duration 5430)
                                (:quantity 5/2 "km") 1/3 :false))
                      '("2026-10-22 13:00 UTC" "1h 30m 30s" "2.5 km" "0.333333" "false"))
               "terms display compactly for people")
  (test-assert (and (knowledge-term-p '(:instant "2026-10-22T13:00:00Z"))
                    (not (knowledge-term-p '(:instant "2026-10-22T15:00:00+02:00")))
                    (not (knowledge-term-p '(:duration 1.5)))
                    (not (knowledge-term-p '(:quantity 3 "furlongs"))))
               "only canonical forms are terms")
  nil)

(-> knowledge-tests--event-schema () list)
(defun knowledge-tests--event-schema ()
  "Return a schema keyed by its event, so one event has one start."
  (knowledge-make-schema :predicate "event.start"
                         :arguments '((:name "event" :type :entity)
                                      (:name "time" :type :instant))
                         :key '("event")
                         :template "{event} starts at {time}"))


(-> test-knowledge-schemas () null)
(defun test-knowledge-schemas ()
  "Test schemas, canonical propositions, claim identity, and conflict keys."
  (let* ((schema (knowledge-tests--event-schema))
         (start (knowledge-make-proposition
                 schema '("review" "2026-10-22T15:00+02:00")))
         (same (knowledge-make-proposition
                schema '("review" "2026-10-22T13:00:00Z")))
         (later (knowledge-make-proposition
                 schema '("review" "2026-10-22T14:00:00Z")))
         (denied (knowledge-make-proposition
                  schema '("review" "2026-10-22T13:00:00Z") :negated t)))
    (test-assert (equal start '("event.start" "review" (:instant "2026-10-22T13:00:00Z")))
                 "propositions are canonical whatever the input zone")
    (test-assert (and (string= (knowledge-claim-identifier start :scope ':workspace
                                                                  :workspace "/w/")
                               (knowledge-claim-identifier same :scope ':workspace
                                                                :workspace "/w/"))
                      (string/= (knowledge-claim-identifier start :scope ':workspace
                                                                  :workspace "/w/")
                                (knowledge-claim-identifier start :scope ':global))
                      (string/= (knowledge-claim-identifier start :scope ':global)
                                (knowledge-claim-identifier denied :scope ':global)))
                 "one proposition in one scope is one claim")
    (test-assert (and (equal (knowledge-proposition-key start schema)
                             (knowledge-proposition-key later schema))
                      (not (knowledge-propositions-equal-p start later))
                      (null (knowledge-proposition-key denied schema)))
                 "claims keyed by the same event with other times conflict")
    (test-assert (equal (list (knowledge-proposition-display start schema)
                              (knowledge-proposition-display denied nil))
                        '("review starts at 2026-10-22 13:00 UTC"
                          "not: event.start(review, 2026-10-22 13:00 UTC)"))
                 "propositions read as sentences through their template")
    (dolist (case (list (list "a wrong arity"
                              (lambda () (knowledge-make-proposition schema '("review"))))
                        (list "a rule variable"
                              (lambda () (knowledge-make-proposition
                                          schema '("?event" "2026-10-22T13:00:00Z"))))))
      (test-assert (eq (knowledge-tests--error-code (second case)) ':invalid-claim)
                   (format nil "~A is not a claim" (first case))))
    (test-assert (handler-case (progn (knowledge-make-proposition schema '("review" "1pm")) nil)
                   (knowledge-error (condition)
                     (and (eq (knowledge-error-code condition) ':invalid-term)
                          (search "Argument time of event.start"
                                  (autolith-error-message condition))
                          t)))
                 "a mistyped argument names the argument it belongs to")
    (dolist (case (list (list "an unqualified predicate"
                              (lambda () (knowledge-make-schema
                                          :predicate "start"
                                          :arguments '((:name "x" :type :entity)))))
                        (list "an unknown type"
                              (lambda () (knowledge-make-schema
                                          :predicate "a.b"
                                          :arguments '((:name "x" :type :colour)))))
                        (list "a duplicate argument"
                              (lambda () (knowledge-make-schema
                                          :predicate "a.b"
                                          :arguments '((:name "x" :type :entity)
                                                       (:name "x" :type :number)))))
                        (list "a key naming no argument"
                              (lambda () (knowledge-make-schema
                                          :predicate "a.b"
                                          :arguments '((:name "x" :type :entity)
                                                       (:name "y" :type :number))
                                          :key '("z"))))
                        (list "a unit on a number"
                              (lambda () (knowledge-make-schema
                                          :predicate "a.b"
                                          :arguments '((:name "x" :type :number :unit "km")))))))
      (test-assert (eq (knowledge-tests--error-code (second case)) ':invalid-schema)
                   (format nil "~A is not a schema" (first case))))
    (let ((distance (knowledge-make-schema
                     :predicate "route.distance"
                     :arguments '((:name "route" :type :entity)
                                  (:name "length" :type :quantity :unit "km")))))
      (test-assert (and (knowledge-make-proposition distance '("r1" (12 "mi")))
                        (eq (knowledge-tests--error-code
                             (lambda ()
                               (knowledge-make-proposition distance '("r1" (12 "kg")))))
                            ':invalid-claim))
                   "a quantity argument accepts only units of its dimension")))
  nil)

(-> knowledge-tests--statement (string) list)
(defun knowledge-tests--statement (quote)
  "Return evidence that the user said QUOTE in a test conversation."
  (knowledge-make-evidence :source (list :user :quote quote :conversation "test")
                           :observed-at 1
                           :adapter "test/1"))

(-> knowledge-tests--reread (configuration) knowledge-base)
(defun knowledge-tests--reread (configuration)
  "Return CONFIGURATION's knowledge base folded again from its log, as after a restart."
  (clrhash *knowledge-base-cache*)
  (knowledge-base-read configuration))


(-> test-knowledge-store () null)
(defun test-knowledge-store ()
  "Test knowledge persistence, support identity, retraction, schemas, and log tolerance."
  (with-test-configuration (configuration)
    (let* ((schema (knowledge-tests--event-schema))
           (thirteen (knowledge-make-proposition schema '("review" "2026-10-22T13:00:00Z"))))
      (test-assert (eq (knowledge-tests--error-code
                        (lambda ()
                          (knowledge-assert configuration thirteen
                                            :evidence (knowledge-tests--statement "it is at one")
                                            :basis ':user-assertion)))
                       ':unknown-predicate)
                   "claims need a defined predicate")
      (knowledge-define-schema configuration schema)
      (test-assert (eq (knowledge-tests--error-code
                        (lambda ()
                          (knowledge-assert configuration thirteen
                                            :scope ':global
                                            :evidence (knowledge-tests--statement "it is at one")
                                            :basis ':user-assertion)))
                       ':unknown-predicate)
                   "a global claim needs a global predicate")
      (multiple-value-bind (claim support)
          (knowledge-assert configuration thirteen
                            :evidence (knowledge-tests--statement "the review is at one")
                            :basis ':user-assertion)
        (let ((revision (knowledge-base-revision (knowledge-base-read configuration))))
          (multiple-value-bind (same-claim same-support)
              (knowledge-assert configuration thirteen
                                :evidence (knowledge-tests--statement "the review is at one")
                                :basis ':user-assertion)
            (test-assert (and (string= claim same-claim)
                              (string= support same-support)
                              (= revision (knowledge-base-revision
                                           (knowledge-base-read configuration))))
                         "asserting the same support twice writes nothing")))
        (multiple-value-bind (second-claim second-support)
            (knowledge-assert configuration thirteen
                              :evidence (knowledge-tests--statement "review moved to one")
                              :basis ':user-assertion)
          (let ((entry (knowledge-base-claim (knowledge-tests--reread configuration) claim)))
            (test-assert (and (string= claim second-claim)
                              (string/= support second-support)
                              (= 2 (length (getf entry :supports))))
                         "independent evidence adds a second support to the same claim after a restart"))
          (test-assert (equal (knowledge-retract configuration second-support
                                                 :reason "misheard")
                              (list second-support))
                       "one support can be retracted")
          (let ((base (knowledge-tests--reread configuration)))
            (test-assert (and (getf (knowledge-base-support base second-support) :retracted-at)
                              (equal (getf (knowledge-base-support base second-support)
                                           :retraction-reason)
                                     "misheard")
                              (null (getf (knowledge-base-support base support) :retracted-at)))
                         "a retracted support stays inspectable beside the active one"))
          (test-assert (equal (knowledge-retract configuration claim) (list support))
                       "retracting a claim retracts its remaining active supports")))
      (multiple-value-bind (assumption-claim assumption-support)
          (knowledge-assert configuration
                            (knowledge-make-proposition schema '("lunch" "2026-10-22T12:00:00Z"))
                            :basis ':assumption)
        (declare (ignore assumption-claim))
        (test-assert (null (getf (knowledge-base-support (knowledge-base-read configuration)
                                                         assumption-support)
                                 :evidence))
                     "an assumption needs no evidence"))
      (test-assert (eq (knowledge-tests--error-code
                        (lambda ()
                          (knowledge-assert configuration thirteen :basis ':observation)))
                       ':invalid-claim)
                   "an observation needs evidence")
      (let ((retemplated (knowledge-make-schema :predicate "event.start"
                                                :arguments '((:name "event" :type :entity)
                                                             (:name "time" :type :instant))
                                                :key '("event")
                                                :template "{event} begins {time}")))
        (test-assert (= 2 (nth-value 1 (knowledge-define-schema configuration retemplated)))
                     "changing a template redefines the schema as version 2")
        (test-assert (eq (knowledge-tests--error-code
                          (lambda ()
                            (knowledge-define-schema
                             configuration
                             (knowledge-make-schema :predicate "event.start"
                                                    :arguments '((:name "event" :type :entity)
                                                                 (:name "day" :type :date))))))
                         ':schema-in-use)
                     "changing arguments of a predicate in use is refused"))
      (let ((base (knowledge-base-read configuration)))
        (test-assert (eq base (knowledge-base-read configuration))
                     "an unchanged log is read once"))
      (let* ((pathname (configuration-knowledge-path configuration))
             (forms (sexp-store:log-read pathname))
             (revision (knowledge-base-revision (knowledge-tests--reread configuration))))
        (sexp-store:log-write pathname (append forms (list '(:future-kind :version 9 :x 1))))
        (with-open-file (stream pathname :direction ':output :if-exists ':append)
          (write-string "(:claim-asserted :version 1 :support" stream))
        (let ((base (knowledge-tests--reread configuration)))
          (test-assert (= (knowledge-base-revision base) (1+ revision))
                       "unknown record kinds and an interrupted final record are tolerated"))
        (knowledge-tests--said configuration "event.start" '("standup" "2026-10-22T09:00:00Z")
                               "standup is at nine sharp")
        (test-assert (and (null (nth-value 1 (sexp-store:log-read pathname)))
                          (knowledge-view-holds (knowledge-tests--view configuration)
                                                '("event.start" "standup" "?t")))
                     "the next write repairs the interrupted record before appending"))))
  nil)

(-> knowledge-tests--evidence-state (evidence-check list) keyword)
(defun knowledge-tests--evidence-state (check evidence)
  "Return EVIDENCE's applicability under CHECK, from its plist."
  (values (evidence-check-evidence check (rest evidence))))


(-> test-knowledge-applicability () null)
(defun test-knowledge-applicability ()
  "Test when evidence and supports apply: files, expiry, memories, retraction, validity."
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (declare (ignore configuration))
      (let* ((schedule (memory-source-tests--write
                        (merge-pathnames "schedule.txt" workspace)
                        (format nil "Review at 13:00 in room 4.~%")))
             (quoted (knowledge-observe-file schedule :quote "Review at 13:00"
                                                      :workspace (namestring workspace)
                                                      :time 1000))
             (whole (knowledge-observe-file schedule :workspace (namestring workspace)
                                                     :time 1000))
             (page (knowledge-observe :source '(:url :url "https://example.com/times")
                                      :adapter "web/1" :time 1000))
             (said (knowledge-observe :source '(:user :quote "the review is at one")
                                      :adapter "user/1" :time 1000))
             (memory (memory-remember base :title "Room" :content "Room 4."))
             (cited (knowledge-observe
                     :source (list :memory :id (memory-identifier memory)
                                   :digest (memory-content-digest "Room" "Room 4."))
                     :adapter "memory/1" :time 1000)))
        (test-assert (and (equal (getf (rest (getf (rest quoted) :source)) :path)
                                 "schedule.txt")
                          (getf (rest (getf (rest quoted) :source)) :relative))
                     "file evidence inside the workspace is recorded relatively")
        (flet ((check (now)
                 (evidence-check-create :now now
                                        :memories (memory-list base :visibility ':all))))
          (let ((check (check 2000)))
            (test-assert (every (lambda (evidence)
                                  (eq (knowledge-tests--evidence-state check evidence)
                                      ':current))
                                (list quoted whole page said cited))
                         "fresh evidence of every kind applies"))
          (test-assert (and (eq (knowledge-tests--evidence-state
                                 (check (+ 1001 *knowledge-url-lifetime*)) page)
                                ':expired)
                            (eq (knowledge-tests--evidence-state
                                 (check (+ 1001 *knowledge-url-lifetime*)) said)
                                ':current))
                       "a web observation expires while a statement does not")
          (memory-source-tests--write schedule (format nil "Review at 13:00 in room 5.~%"))
          (let ((check (check 2000)))
            (test-assert (and (eq (knowledge-tests--evidence-state check quoted) ':changed)
                              (eq (knowledge-tests--evidence-state check whole) ':changed))
                         "a surviving quotation cannot establish applicability after a source edit"))
          (delete-file schedule)
          (test-assert (eq (knowledge-tests--evidence-state (check 2000) quoted) ':missing)
                       "a deleted file no longer supports anything")
          (memory-remember base :identifier (memory-identifier memory)
                                :title "Room" :content "Room 5.")
          (test-assert (eq (knowledge-tests--evidence-state (check 2000) cited) ':changed)
                       "a replaced memory no longer supports what cited it")
          (let ((store (make-instance 'knowledge-base)))
            (setf (gethash (getf (rest said) :id) (knowledge-base-evidence store))
                  (rest said))
            (dolist (case (list (list (list :evidence (getf (rest said) :id)) ':current)
                                (list (list :evidence (getf (rest said) :id)
                                            :retracted-at 1500)
                                      ':retracted)
                                (list (list :evidence (getf (rest said) :id)
                                            :valid-from 3000)
                                      ':pending)
                                (list (list :evidence (getf (rest said) :id)
                                            :valid-until 1500)
                                      ':expired)
                                (list (list :basis :assumption) ':current)))
              (test-assert (eq (evidence-check-support (check 2000) store (first case))
                               (second case))
                           (format nil "a support ~S is ~A" (first case) (second case)))))))))
  nil)

(-> test-knowledge-quotation-context () null)
(defun test-knowledge-quotation-context ()
  "Test that an obsolete section cannot support current facts through a surviving quote."
  (with-test-configuration (base root)
    (let* ((configuration (configuration-copy base :working-directory root))
           (context (make-instance 'tool-context :configuration configuration :worker nil))
           (quote "The report is ready for release.")
           (file (memory-source-tests--write
                  (merge-pathnames "status.txt" root) (format nil "Current status~%~A~%" quote)))
           (observed (knowledge-observe-file file :quote quote :workspace (namestring root))))
      (dolist (predicate '("report.ready" "report.sendable"))
        (knowledge-define-schema
         configuration (knowledge-make-schema :predicate predicate
                                              :arguments '((:name "report" :type :entity)))))
      (knowledge-propose-rule configuration :id "sendable"
                              :premises '(("report.ready" "?r"))
                              :conclusion '("report.sendable" "?r"))
      (knowledge-admit-rule configuration "sendable")
      (knowledge-assert configuration '("report.ready" "q3")
                        :evidence observed :basis ':observation)
      (flet ((gate (pattern)
               (mission--fact-gate-support (list :kind :fact :pattern pattern
                                                :accepted '(:observation :user-assertion)) context)))
        (test-assert (gate '("report.sendable" "q3"))
                     "the original current section supports a derived fact gate")
        (memory-source-tests--write
         file (format nil "Obsolete status~%~A~%Current status~%The report still needs review.~%" quote))
        (let* ((view (knowledge-view-current configuration))
               (node (knowledge-view-node
                      view (knowledge-claim-identifier '("report.ready" "q3")
                                                       :scope ':workspace
                                                       :workspace (namestring root)))))
          (test-assert (eq (knowledge-node-applicability node) ':stale)
                       "changing the context invalidates the original observation")
          (test-assert (not (gate '("report.sendable" "q3")))
                       "a surviving obsolete quote cannot justify a derived fact gate"))
        (let ((refreshed (knowledge-observe-file file :quote "The report still needs review."
                                                    :workspace (namestring root))))
          (knowledge-assert configuration '(:not ("report.ready" "q3"))
                            :evidence refreshed :basis ':observation))
        (test-assert (gate '(:not ("report.ready" "q3")))
                     "an explicit observation of the new status supports the corrected fact")
        (let* ((store (knowledge-tests--reread configuration))
               (historical (knowledge-base-evidence-record store (getf (rest observed) :id))))
          (test-assert (equal historical (rest observed))
                       "replay preserves the original quotation, source digest, and time")
          (test-assert (eq (evidence-check-evidence (evidence-check-create) historical) ':changed)
                       "replaying provenance does not refresh its applicability")))))
  nil)

(-> knowledge-tests--define-appointments (configuration) null)
(defun knowledge-tests--define-appointments (configuration)
  "Define the appointment vocabulary used by rule and evaluation tests."
  (dolist (definition
           '(("event.start" ((:name "event" :type :entity) (:name "time" :type :instant))
              ("event") "{event} starts at {time}")
             ("event.travel" ((:name "event" :type :entity) (:name "duration" :type :duration))
              ("event") "travel to {event} takes {duration}")
             ("event.buffer" ((:name "event" :type :entity) (:name "duration" :type :duration))
              ("event") "arrive {duration} early for {event}")
             ("event.cancelled" ((:name "event" :type :entity))
              nil "{event} is cancelled")
             ("event.departure" ((:name "event" :type :entity) (:name "time" :type :instant))
              ("event") "leave for {event} at {time}")))
    (destructuring-bind (predicate arguments key template) definition
      (knowledge-define-schema configuration
                               (knowledge-make-schema :predicate predicate
                                                      :arguments arguments
                                                      :key key
                                                      :template template))))
  nil)

(-> knowledge-tests--departure-rule (&key (:id string)) list)
(defun knowledge-tests--departure-rule (&key (id "departure"))
  "Return the departure rule's input fields as keyword arguments."
  (list :id id
        :premises '(("event.start" "?event" "?start")
                    ("event.travel" "?event" "?travel")
                    ("event.buffer" "?event" "?buffer")
                    (:not ("event.cancelled" "?event")))
        :bindings '(("?leave" ("-" "?start" ("+" "?travel" "?buffer"))))
        :conclusion '("event.departure" "?event" "?leave")))

(-> knowledge-tests--schema-function (configuration) function)
(defun knowledge-tests--schema-function (configuration)
  "Return a schema lookup over CONFIGURATION's current knowledge."
  (let ((base (knowledge-base-read configuration)))
    (lambda (predicate)
      (knowledge-base-schema base predicate
                             :workspace (namestring (config :working-directory configuration))))))


(-> test-knowledge-rule-validation () null)
(defun test-knowledge-rule-validation ()
  "Test that rules are range restricted, well typed, bounded, and acyclic."
  (with-test-configuration (configuration)
    (knowledge-tests--define-appointments configuration)
    (let ((schemas (knowledge-tests--schema-function configuration)))
      (test-assert (knowledge-rule-p (apply #'knowledge-make-rule
                                            :schema-function schemas
                                            (knowledge-tests--departure-rule)))
                   "the departure rule is valid")
      (dolist (case
               `(("an unbound conclusion variable"
                  :invalid-rule
                  (:premises (("event.start" "?event" "?start"))
                   :conclusion ("event.departure" "?event" "?leave")))
                 ("a computation on the wrong types"
                  :invalid-rule
                  (:premises (("event.start" "?event" "?start"))
                   :bindings (("?leave" ("-" "?start" 5)))
                   :conclusion ("event.departure" "?event" "?leave")))
                 ("an unknown computation"
                  :invalid-rule
                  (:premises (("event.start" "?event" "?start"))
                   :bindings (("?leave" ("later" "?start")))
                   :conclusion ("event.departure" "?event" "?leave")))
                 ("a negation of an unbound variable"
                  :invalid-rule
                  (:premises (("event.start" "?event" "?start")
                              (:not ("event.cancelled" "?other")))
                   :conclusion ("event.departure" "?event" "?start")))
                 ("a guard across types"
                  :invalid-rule
                  (:premises (("event.start" "?event" "?start")
                              ("event.travel" "?event" "?travel"))
                   :guards (("<" "?start" "?travel"))
                   :conclusion ("event.departure" "?event" "?start")))
                 ("a conclusion of a premise predicate"
                  :rule-cycle
                  (:premises (("event.start" "?event" "?start"))
                   :conclusion ("event.start" "?event" "?start")))
                 ("an undefined predicate"
                  :unknown-predicate
                  (:premises (("event.room" "?event" "?room"))
                   :conclusion ("event.departure" "?event" "?room")))
                 ("a literal of the wrong type"
                  :invalid-term
                  (:premises (("event.start" "review" "one o'clock"))
                   :conclusion ("event.departure" "review" "2026-10-22T12:00:00Z")))))
        (destructuring-bind (label code fields) case
          (test-assert (eq (knowledge-tests--error-code
                            (lambda ()
                              (apply #'knowledge-make-rule :id "r" :schema-function schemas
                                     fields)))
                           code)
                       (format nil "~A is refused as ~(~A~)" label code))))))
  nil)

(-> test-knowledge-rule-computations () null)
(defun test-knowledge-rule-computations ()
  "Test rule computations and guards over typed values."
  (let ((environment (make-hash-table :test #'equal)))
    (setf (gethash "?start" environment) '(:instant "2026-10-22T13:00:00Z")
          (gethash "?travel" environment) '(:duration 1800)
          (gethash "?day" environment) '(:date "2026-10-22")
          (gethash "?far" environment) '(:quantity 2 "km")
          (gethash "?near" environment) '(:quantity 500 "m"))
    (dolist (case '((("-" "?start" ("+" "?travel" (:duration 600))) (:instant "2026-10-22T12:20:00Z"))
                    (("-" "?start" (:instant "2026-10-22T12:00:00Z")) (:duration 3600))
                    (("+" "?day" ("days" 10)) (:date "2026-11-01"))
                    (("+" "?far" "?near") (:quantity 5/2 "km"))
                    (("in-unit" "?far" "m") (:quantity 2000 "m"))
                    (("/" "?travel" (:duration 600)) 3)
                    (("max" "?near" "?far") (:quantity 2 "km"))
                    (("weekday" "?day") 4)
                    (("date" "?start") (:date "2026-10-22"))))
      (destructuring-bind (expression expected) case
        (test-assert (equal (knowledge-evaluate-expression expression environment) expected)
                     (format nil "~S computes ~S" expression expected))))
    (dolist (expression '(("/" "?travel" 0) ("+" "?day" ("hours" 5)) ("+" "?far" (:quantity 1 "kg"))))
      (test-assert (eq (knowledge-tests--error-code
                        (lambda () (knowledge-evaluate-expression expression environment)))
                       ':evaluation)
                   (format nil "~S is an evaluation error" expression)))
    (test-assert (and (knowledge-guard-holds-p '("<" "?near" "?far") environment)
                      (not (knowledge-guard-holds-p '(">=" "?travel" (:duration 3600)) environment))
                      (knowledge-guard-holds-p '("=" "?far" (:quantity 2000 "m")) environment))
                 "guards compare converted values")
    (test-assert (equal (knowledge-expression-from-input #("-" "?start" "PT10M"))
                        '("-" "?start" (:duration 600)))
                 "durations written as text become duration literals"))
  nil)

(-> test-knowledge-rule-lifecycle () null)
(defun test-knowledge-rule-lifecycle ()
  "Test proposing, versioning, admitting, retiring, and cycle refusal of stored rules."
  (with-test-configuration (configuration)
    (knowledge-tests--define-appointments configuration)
    (let ((workspace (namestring (config :working-directory configuration))))
      (multiple-value-bind (id version new-p)
          (apply #'knowledge-propose-rule configuration (knowledge-tests--departure-rule))
        (test-assert (and (string= id "departure") (= version 1) new-p
                          (null (knowledge-base-active-rules (knowledge-base-read configuration)
                                                             :workspace workspace)))
                     "a proposed rule is recorded inactive"))
      (test-assert (null (nth-value 2 (apply #'knowledge-propose-rule configuration
                                             (knowledge-tests--departure-rule))))
                   "proposing the same rule again adds no version")
      (test-assert (= 1 (knowledge-admit-rule configuration "departure" :by "test"))
                   "admitting activates the latest version")
      (apply #'knowledge-propose-rule configuration
             :description "Leave earlier." (knowledge-tests--departure-rule))
      (let ((active (knowledge-base-active-rules (knowledge-tests--reread configuration)
                                                 :workspace workspace)))
        (test-assert (and (= 1 (length active)) (= 1 (second (first active))))
                     "a new proposal leaves the admitted version active until admitted itself"))
      (knowledge-admit-rule configuration "departure")
      (test-assert (= 2 (second (first (knowledge-base-active-rules
                                        (knowledge-base-read configuration)
                                        :workspace workspace))))
                   "admitting the new version replaces the old one")
      (knowledge-define-schema
       configuration
       (knowledge-make-schema :predicate "event.reminder"
                              :arguments '((:name "event" :type :entity)
                                           (:name "time" :type :instant))))
      (knowledge-propose-rule configuration
                              :id "reminder"
                              :premises '(("event.departure" "?event" "?leave"))
                              :bindings '(("?ping" ("-" "?leave" "PT10M")))
                              :conclusion '("event.reminder" "?event" "?ping"))
      (knowledge-admit-rule configuration "reminder")
      (test-assert (eq (knowledge-tests--error-code
                        (lambda ()
                          (knowledge-propose-rule configuration
                                                  :id "loop"
                                                  :premises '(("event.reminder" "?event" "?t"))
                                                  :conclusion '("event.start" "?event" "?t"))))
                       ':rule-cycle)
                   "a rule closing a cycle through admitted rules is refused")
      (knowledge-retire-rule configuration "departure")
      (test-assert (equal (mapcar (lambda (item) (getf (first item) :id))
                                  (knowledge-base-active-rules (knowledge-tests--reread configuration)
                                                               :workspace workspace))
                          '("reminder"))
                   "a retired rule is no longer active after a restart")))
  nil)

(-> knowledge-tests--view (configuration &key (:now timestamp)) knowledge-view)
(defun knowledge-tests--view (configuration &key (now (get-universal-time)))
  "Return CONFIGURATION's knowledge evaluated from its workspace at NOW."
  (knowledge-evaluate (knowledge-base-read configuration)
                      :workspace (namestring (config :working-directory configuration))
                      :check (evidence-check-create
                              :now now
                              :memories (memory-list configuration :visibility ':all))))

(-> knowledge-tests--claim (configuration string list &key (:negated boolean)) list)
(defun knowledge-tests--claim (configuration predicate inputs &key negated)
  "Return the proposition PREDICATE makes of INPUTS under CONFIGURATION's schema."
  (knowledge-make-proposition
   (knowledge-base-schema (knowledge-base-read configuration) predicate
                          :workspace (namestring (config :working-directory configuration)))
   inputs
   :negated negated))

(-> knowledge-tests--said (configuration string list string &key (:negated boolean))
    (values string string))
(defun knowledge-tests--said (configuration predicate inputs quote &key negated)
  "Assert PREDICATE of INPUTS as something the user said in QUOTE."
  (knowledge-assert configuration
                    (knowledge-tests--claim configuration predicate inputs :negated negated)
                    :evidence (knowledge-observe :source (list :user :quote quote)
                                                 :adapter "user/1")
                    :basis ':user-assertion))

(-> knowledge-tests--departures (knowledge-view) list)
(defun knowledge-tests--departures (view)
  "Return the departure times VIEW supports, as canonical instant text."
  (mapcar (lambda (node)
            (second (third (knowledge-node-proposition node))))
          (knowledge-view-holds view '("event.departure" "?event" "?time"))))


(-> test-knowledge-derivation () null)
(defun test-knowledge-derivation ()
  "Test the appointment fixture: a changed source recomputes the departure locally."
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (knowledge-tests--define-appointments configuration)
      (knowledge-propose-rule configuration
                              :id "departure"
                              :premises '(("event.start" "?event" "?start")
                                          ("event.travel" "?event" "?travel")
                                          ("event.buffer" "?event" "?buffer"))
                              :bindings '(("?leave" ("-" "?start" ("+" "?travel" "?buffer"))))
                              :conclusion '("event.departure" "?event" "?leave"))
      (knowledge-admit-rule configuration "departure")
      (let ((schedule (memory-source-tests--write
                       (merge-pathnames "schedule.txt" workspace)
                       (format nil "Design review at 13:00 UTC on 22 October.~%"))))
        (knowledge-assert configuration
                          (knowledge-tests--claim configuration "event.start"
                                                  '("review" "2026-10-22T13:00:00Z"))
                          :evidence (knowledge-observe-file schedule
                                                            :quote "review at 13:00"
                                                            :workspace (namestring workspace))
                          :basis ':observation)
        (knowledge-tests--said configuration "event.travel" '("review" "PT30M")
                               "it takes half an hour to get there")
        (knowledge-tests--said configuration "event.buffer" '("review" "PT10M")
                               "be there ten minutes early")
        (let* ((view (knowledge-tests--view configuration))
               (node (first (knowledge-view-holds view '("event.departure" "review" "?t"))))
               (justification (first (knowledge-node-justifications node))))
          (test-assert (equal (knowledge-tests--departures view) '("2026-10-22T12:20:00Z"))
                       "the departure is derived from start, travel, and buffer")
          (test-assert (and (equal (getf justification :rule) "departure")
                            (= (getf justification :version) 1)
                            (= 3 (length (getf justification :premises)))
                            (equal (rest (assoc "?leave" (getf justification :bindings)
                                                :test #'string=))
                                   '(:instant "2026-10-22T12:20:00Z")))
                       "the justification names the rule version, premises, and binding")
          (test-assert (and (equal (knowledge-node-bases node) '((:observation :user-assertion)))
                            (knowledge-node-observed-p node))
                       "the derived claim keeps the bases of its premises"))
        (memory-source-tests--write schedule
                                    (format nil "Design review at 14:00 UTC on 22 October.~%"))
        (let ((view (knowledge-tests--view configuration)))
          (test-assert (null (knowledge-tests--departures view))
                       "a changed schedule withdraws the departure before the new time is known")
          (test-assert (eq (knowledge-node-applicability
                            (knowledge-view-node
                             view
                             (knowledge-claim-identifier
                              (knowledge-tests--claim configuration "event.start"
                                                      '("review" "2026-10-22T13:00:00Z"))
                              :scope ':workspace :workspace (namestring workspace))))
                           ':stale)
                       "the old start is stale because its source changed"))
        (knowledge-assert configuration
                          (knowledge-tests--claim configuration "event.start"
                                                  '("review" "2026-10-22T14:00:00Z"))
                          :evidence (knowledge-observe-file schedule
                                                            :quote "review at 14:00"
                                                            :workspace (namestring workspace))
                          :basis ':observation)
        (test-assert (equal (knowledge-tests--departures (knowledge-tests--view configuration))
                            '("2026-10-22T13:20:00Z"))
                     "recording the new start recomputes the departure without a model")
        (multiple-value-bind (old-buffer)
            (knowledge-tests--said configuration "event.buffer" '("review" "PT10M")
                                   "be there ten minutes early")
          (knowledge-tests--said configuration "event.buffer" '("review" "PT20M")
                                 "make it twenty minutes early")
          (let ((view (knowledge-tests--view configuration)))
            (test-assert (and (= 2 (length (knowledge-view-holds
                                           view '("event.buffer" "review" "?b"))))
                              (every (lambda (node)
                                       (eq (knowledge-node-status view node) ':conflicted))
                                     (knowledge-view-holds view '("event.departure" "review" "?t"))))
                         "two buffers both hold and their departures conflict, with no winner chosen"))
          (knowledge-retract configuration old-buffer :reason "superseded")
          (test-assert (equal (knowledge-tests--departures (knowledge-tests--view configuration))
                              '("2026-10-22T13:10:00Z"))
                       "retracting the old buffer leaves one departure")))))
  nil)

(-> knowledge-tests--admit-departure (configuration &key (:negation boolean)) null)
(defun knowledge-tests--admit-departure (configuration &key negation)
  "Admit the departure rule, requiring an explicit not-cancelled claim when NEGATION."
  (apply #'knowledge-propose-rule configuration
         (if negation
             (knowledge-tests--departure-rule)
             (list :id "departure"
                   :premises '(("event.start" "?event" "?start")
                               ("event.travel" "?event" "?travel")
                               ("event.buffer" "?event" "?buffer"))
                   :bindings '(("?leave" ("-" "?start" ("+" "?travel" "?buffer"))))
                   :conclusion '("event.departure" "?event" "?leave"))))
  (knowledge-admit-rule configuration "departure")
  nil)

(-> knowledge-tests--review (configuration &key (:travel-basis knowledge-basis)) null)
(defun knowledge-tests--review (configuration &key (travel-basis ':user-assertion))
  "Assert the review's start, buffer, and travel, the travel with TRAVEL-BASIS."
  (knowledge-tests--said configuration "event.start" '("review" "2026-10-22T13:00:00Z")
                         "the review starts at one")
  (knowledge-tests--said configuration "event.buffer" '("review" "PT10M")
                         "be there ten minutes early")
  (if (eq travel-basis ':assumption)
      (knowledge-assert configuration
                        (knowledge-tests--claim configuration "event.travel" '("review" "PT30M"))
                        :basis ':assumption)
      (knowledge-tests--said configuration "event.travel" '("review" "PT30M")
                             "half an hour to get there"))
  nil)


(-> test-knowledge-justifications-and-conflicts () null)
(defun test-knowledge-justifications-and-conflicts ()
  "Test alternative support paths, explicit negation, conflicts, and assumption bases."
  (with-test-configuration (configuration)
    (knowledge-tests--define-appointments configuration)
    (knowledge-tests--admit-departure configuration :negation t)
    (knowledge-tests--review configuration)
    (test-assert (null (knowledge-tests--departures (knowledge-tests--view configuration)))
                 "a missing cancellation claim does not satisfy a negated premise")
    (knowledge-tests--said configuration "event.cancelled" '("review")
                           "the review is definitely still on" :negated t)
    (test-assert (equal (knowledge-tests--departures (knowledge-tests--view configuration))
                        '("2026-10-22T12:20:00Z"))
                 "an explicit not-cancelled claim satisfies it")
    (multiple-value-bind (stated)
        (knowledge-tests--said configuration "event.departure" '("review" "2026-10-22T12:20:00Z")
                               "I will leave at twenty past twelve")
      (let ((node (knowledge-view-node (knowledge-tests--view configuration) stated)))
        (test-assert (and (knowledge-node-derived-p node)
                          (find ':current (knowledge-node-supports node)
                                :key (lambda (support) (getf support :state))))
                     "a stated departure the rule also derives has two independent paths"))
      (knowledge-retract configuration stated)
      (test-assert (knowledge-node-supported-p
                    (knowledge-view-node (knowledge-tests--view configuration) stated))
                   "retracting the statement leaves the derivation supporting it"))
    (multiple-value-bind (cancelled)
        (knowledge-tests--said configuration "event.cancelled" '("review")
                               "the review got cancelled after all")
      (let ((view (knowledge-tests--view configuration)))
        (test-assert (eq (knowledge-node-status view (knowledge-view-node view cancelled))
                         ':conflicted)
                     "a claim and its explicit negation both supported conflict")
        (test-assert (equal (knowledge-tests--departures view) '("2026-10-22T12:20:00Z"))
                     "conflicting evidence is shown, not resolved by picking a winner"))
      (knowledge-retract configuration cancelled))
    (knowledge-tests--said configuration "event.departure" '("review" "2026-10-22T12:00:00Z")
                           "I am leaving at noon")
    (let* ((view (knowledge-tests--view configuration))
           (departures (knowledge-view-holds view '("event.departure" "review" "?t"))))
      (test-assert (and (= 2 (length departures))
                        (every (lambda (node) (eq (knowledge-node-status view node) ':conflicted))
                               departures))
                   "two departure times for one event conflict through the schema key")))
  (with-test-configuration (configuration)
    (knowledge-tests--define-appointments configuration)
    (knowledge-tests--admit-departure configuration)
    (knowledge-tests--review configuration :travel-basis ':assumption)
    (let ((node (first (knowledge-view-holds (knowledge-tests--view configuration)
                                             '("event.departure" "review" "?t")))))
      (test-assert (and (equal (knowledge-node-bases node) '((:user-assertion :assumption)))
                        (not (knowledge-node-observed-p node)))
                   "deriving from an assumption does not make an independent observation")))
  nil)

(-> test-knowledge-evaluation-bounds () null)
(defun test-knowledge-evaluation-bounds ()
  "Test expiry, visibility, refused computations, goals, and incomplete evaluation."
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (declare (ignore workspace))
      (knowledge-tests--define-appointments configuration)
      (knowledge-tests--admit-departure configuration)
      (knowledge-tests--said configuration "event.start" '("review" "2026-10-22T13:00:00Z")
                             "the review starts at one")
      (knowledge-tests--said configuration "event.buffer" '("review" "PT10M")
                             "be there ten minutes early")
      (knowledge-assert configuration
                        (knowledge-tests--claim configuration "event.travel" '("review" "PT30M"))
                        :evidence (knowledge-observe
                                   :source '(:url :url "https://maps.example.com/route")
                                   :adapter "web/1" :time 1000)
                        :basis ':observation)
      (test-assert (knowledge-tests--departures
                    (knowledge-tests--view configuration :now 2000))
                   "a fresh route estimate supports the departure")
      (let ((view (knowledge-tests--view configuration
                                         :now (+ 1001 *knowledge-url-lifetime*))))
        (test-assert (and (null (knowledge-tests--departures view))
                          (eq (knowledge-node-applicability
                               (knowledge-view-node
                                view
                                (knowledge-claim-identifier
                                 (knowledge-tests--claim configuration "event.travel"
                                                         '("review" "PT30M"))
                                 :scope ':workspace
                                 :workspace (namestring (config :working-directory
                                                                configuration)))))
                              ':expired))
                     "an expired route estimate no longer supports the departure"))
      (let ((other (memory-tests--configuration-in-workspace
                    base (merge-pathnames "other/" root))))
        (ensure-directories-exist (merge-pathnames "other/" root))
        (test-assert (null (knowledge-view-holds (knowledge-tests--view other)
                                                 '("event.start" "?e" "?t")))
                     "another workspace sees none of this workspace's claims"))
      (knowledge-tests--said configuration "event.start" '("standup" "2026-10-22T09:00:00Z")
                             "standup is at nine")
      (knowledge-tests--said configuration "event.buffer" '("standup" "PT0S")
                             "no need to be early for standup")
      (knowledge-tests--said configuration "event.travel" '("standup" "PT5M")
                             "standup is five minutes away")
      (let ((*knowledge-match-limit* 1))
        (let ((view (knowledge-tests--view configuration :now 2000)))
          (test-assert (and (= 1 (length (knowledge-tests--departures view)))
                            (equal (knowledge-view-incomplete view) '("departure")))
                       "a reached work limit marks the rule incomplete instead of denying the rest")))
      (test-assert (null (knowledge-tests--departures
                          (knowledge-evaluate (knowledge-base-read configuration)
                                              :workspace (namestring (config :working-directory
                                                                             configuration))
                                              :check (evidence-check-create :now 2000)
                                              :goals '("event.cancelled"))))
                   "goals skip rules that cannot contribute to them")
      (knowledge-define-schema configuration
                               (knowledge-make-schema
                                :predicate "event.pace"
                                :arguments '((:name "event" :type :entity)
                                             (:name "ratio" :type :number))))
      (knowledge-propose-rule configuration
                              :id "pace"
                              :premises '(("event.travel" "?event" "?travel")
                                          ("event.buffer" "?event" "?buffer"))
                              :bindings '(("?ratio" ("/" "?travel" "?buffer")))
                              :conclusion '("event.pace" "?event" "?ratio"))
      (knowledge-admit-rule configuration "pace")
      (let ((view (knowledge-tests--view configuration :now 2000)))
        (test-assert (and (= 1 (length (knowledge-view-holds view '("event.pace" "?e" "?r"))))
                          (search "Division by zero"
                                  (getf (first (knowledge-view-errors view)) :message)))
                     "a computation refused for one match is reported and the others still derive"))))
  nil)

(-> knowledge-tests--schedule-review (configuration pathname string) pathname)
(defun knowledge-tests--schedule-review (configuration workspace time)
  "Write the schedule with the review at TIME and record the start it states."
  (let ((schedule (memory-source-tests--write
                   (merge-pathnames "schedule.txt" workspace)
                   (format nil "Design review at ~A UTC on 22 October.~%" time))))
    (knowledge-assert configuration
                      (knowledge-tests--claim configuration "event.start"
                                              (list "review"
                                                    (format nil "2026-10-22T~A:00Z" time)))
                      :evidence (knowledge-observe-file
                                 schedule
                                 :quote (format nil "review at ~A" time)
                                 :workspace (namestring workspace))
                      :basis ':observation)
    schedule))


(-> test-knowledge-explanations () null)
(defun test-knowledge-explanations ()
  "Test bounded, paged explanation trees of derived claims."
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (knowledge-tests--define-appointments configuration)
      (knowledge-tests--admit-departure configuration)
      (knowledge-tests--schedule-review configuration workspace "13:00")
      (knowledge-tests--said configuration "event.buffer" '("review" "PT10M")
                             "be there ten minutes early")
      (knowledge-tests--said configuration "event.travel" '("review" "PT30M")
                             "half an hour to get there")
      (let* ((view (knowledge-tests--view configuration))
             (departure (knowledge-node-identifier
                         (first (knowledge-view-holds view '("event.departure" "?e" "?t")))))
             (tree (first (knowledge-explain view departure)))
             (justification (first (getf tree :justifications))))
        (test-assert (and (equal (getf tree :text) "leave for review at 2026-10-22 12:20 UTC")
                          (eq (getf tree :status) ':supported)
                          (equal (getf justification :rule) "departure")
                          (= 3 (length (getf justification :premises))))
                     "the explanation names the conclusion, its rule, and its premises")
        (test-assert (find-if (lambda (premise)
                                (let ((support (first (getf premise :supports))))
                                  (and (equal (getf support :text)
                                              "schedule.txt \"review at 13:00\"")
                                       (eq (getf support :state) ':current)
                                       (eq (getf support :basis) ':observation))))
                              (getf justification :premises))
                     "each premise shows the evidence it rests on")
        (multiple-value-bind (trees cursor) (knowledge-explain view departure :limit 2)
          (test-assert (and (= 2 (length cursor))
                            (= 2 (count-if (lambda (premise) (getf premise :more))
                                           (getf (first (getf (first trees) :justifications))
                                                 :premises))))
                       "a page past its limit marks unexpanded premises and returns a cursor")
          (test-assert (= 2 (count-if (lambda (tree) (getf tree :supports))
                                      (knowledge-explain view cursor :limit 2)))
                       "the cursor continues the explanation")))))
  nil)

(-> test-knowledge-changes () null)
(defun test-knowledge-changes ()
  "Test what changed between a snapshot and a later view, with causes."
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (knowledge-tests--define-appointments configuration)
      (knowledge-tests--admit-departure configuration)
      (knowledge-tests--schedule-review configuration workspace "13:00")
      (knowledge-tests--said configuration "event.buffer" '("review" "PT10M")
                             "be there ten minutes early")
      (multiple-value-bind (travel)
          (knowledge-tests--said configuration "event.travel" '("review" "PT30M")
                                 "half an hour to get there")
        (let ((snapshot (knowledge-view-snapshot (knowledge-tests--view configuration))))
          (test-assert (null (knowledge-view-changes snapshot (knowledge-tests--view configuration)))
                       "an unchanged view reports no changes")
          (knowledge-tests--schedule-review configuration workspace "14:00")
          (let* ((changes (knowledge-view-changes snapshot (knowledge-tests--view configuration)))
                 (departure (find "leave for review at 2026-10-22 13:20 UTC" changes
                                  :key (lambda (change) (getf change :to)) :test #'equal)))
            (test-assert (and departure
                              (eq (getf departure :kind) ':changed)
                              (equal (getf departure :from)
                                     "leave for review at 2026-10-22 12:20 UTC"))
                         "the departure is reported as moved from its old to its new time")
            (test-assert (find-if (lambda (cause)
                                    (search "schedule.txt \"review at 13:00\" changed" cause))
                                  (getf departure :causes))
                         "the move names the changed file behind it"))
          (let ((moved (knowledge-view-snapshot (knowledge-tests--view configuration))))
            (knowledge-retract configuration travel :reason "taking a taxi")
            (let ((withdrawn (find ':withdrawn (knowledge-view-changes
                                                moved (knowledge-tests--view configuration))
                                   :key (lambda (change) (getf change :kind)))))
              (test-assert (and withdrawn
                                (search "you said \"half an hour to get there\" was retracted"
                                        (first (getf withdrawn :causes))))
                           "a withdrawn departure names the retracted premise"))))))
    nil))

(-> test-knowledge-memory-premises () null)
(defun test-knowledge-memory-premises ()
  "Test prose memories derived from structured claims, stored and derived."
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (knowledge-tests--define-appointments configuration)
      (knowledge-tests--admit-departure configuration)
      (knowledge-tests--schedule-review configuration workspace "13:00")
      (knowledge-tests--said configuration "event.buffer" '("review" "PT10M")
                             "be there ten minutes early")
      (knowledge-tests--said configuration "event.travel" '("review" "PT30M")
                             "half an hour to get there")
      (let* ((departure (knowledge-node-identifier
                         (first (knowledge-view-holds (knowledge-tests--view configuration)
                                                      '("event.departure" "?e" "?t")))))
             (note (memory-remember configuration
                                    :title "Review logistics"
                                    :content "Book the 12:15 bus to the review."
                                    :premises (list departure))))
        (test-assert (and (eq (getf (first (memory-premises note)) :kind) ':claim)
                          (eq (memory-source-tests--status configuration note) ':current))
                     "a memory derived from a derived fact is current while the fact holds")
        (test-assert (eq (memory-source-tests--error-p
                          (lambda ()
                            (memory-remember configuration
                                             :scope ':global
                                             :title "Global logistics"
                                             :content "Too broad."
                                             :premises (list departure))))
                         t)
                     "a global memory cannot rely on a workspace fact")
        (knowledge-tests--schedule-review configuration workspace "14:00")
        (multiple-value-bind (status reasons) (memory-source-tests--status configuration note)
          (test-assert (and (eq status ':stale) (equal reasons '(:premise-claim)))
                       "the memory goes stale when its fact is no longer derived"))
        (test-assert (equal (memory-assessment-summary
                             (memory-assess (memory-assessor-create configuration)
                                            (memory-find configuration (memory-identifier note))))
                            "\"leave for review at 2026-10-22 12:20 UTC\" no longer holds")
                     "the reason names the recorded fact even after it disappears"))))
  nil)

(-> knowledge-tests--recall (configuration conversation) list)
(defun knowledge-tests--recall (configuration conversation)
  "Return the contributions memory recall makes for CONVERSATION's latest input."
  (let ((*context-contributors* nil)
        (*context-resolver* (cl-llm-provider-api:make-context-resolver))
        (*context-last-deliveries* (make-hash-table :test #'equal))
        (*context-last-delivery-order* nil))
    (register-context-contributor "related-memories" 'memory-related-context
                                  :source ':built-in)
    (context-delivery-contributions (context-resolve-request configuration conversation #()))))

(-> knowledge-tests--contribution (list string) (option context-contribution))
(defun knowledge-tests--contribution (contributions identifier)
  "Return the contribution IDENTIFIER among CONTRIBUTIONS."
  (find identifier contributions :key #'context-contribution-identifier :test #'string=))


(-> test-knowledge-recall () null)
(defun test-knowledge-recall ()
  "Test recall of complete current facts and once-only explanation of their changes."
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (knowledge-tests--define-appointments configuration)
      (knowledge-tests--admit-departure configuration)
      (knowledge-tests--schedule-review configuration workspace "13:00")
      (knowledge-tests--said configuration "event.buffer" '("review" "PT10M")
                             "be there ten minutes early")
      (knowledge-tests--said configuration "event.travel" '("review" "PT30M")
                             "half an hour to get there")
      (let ((conversation (conversation-create configuration)))
        (conversation-append-user-message conversation "When should I leave for the review?")
        (let* ((contributions (knowledge-tests--recall configuration conversation))
               (advice (knowledge-tests--contribution contributions "related-memories")))
          (test-assert (and advice
                            (search "\"fact\":\"leave for review at 2026-10-22 12:20 UTC\""
                                    (context-contribution-evidence advice))
                            (search "\"derived\":\"departure v1\""
                                    (context-contribution-evidence advice))
                            (search "use them as they are" (context-contribution-instruction advice))
                            (not (search "persistent memor" (context-contribution-instruction advice)))
                            (null (knowledge-tests--contribution contributions "knowledge-changes")))
                       "recall offers the derived departure as a complete current fact")
          (test-assert (equal (application--related-memories-notice 0 :facts 3)
                              "@ 3 relevant facts")
                       "the notice counts offered facts"))
        (knowledge-tests--schedule-review configuration workspace "14:00")
        (test-assert (null (knowledge-tests--contribution
                            (knowledge-tests--recall configuration conversation)
                            "knowledge-changes"))
                     "changes wait for the next turn")
        (conversation-append-user-message conversation "Ok, anything else?")
        (let* ((changes (knowledge-tests--contribution
                         (knowledge-tests--recall configuration conversation)
                         "knowledge-changes")))
          (test-assert (and changes
                            (search "changed: leave for review at 2026-10-22 13:20 UTC, was leave for review at 2026-10-22 12:20 UTC"
                                    (context-contribution-evidence changes))
                            (eq (context-contribution-lifetime changes) ':next-request))
                       "the next turn explains how a fact the model was given changed")
          (test-assert (equal (context-contribution-evidence changes)
                              (context-contribution-evidence
                               (knowledge-tests--contribution
                                (knowledge-tests--recall configuration conversation)
                                "knowledge-changes")))
                       "later requests in the same turn offer the same explanation"))
        (conversation-append-user-message conversation "Thanks.")
        (test-assert (null (knowledge-tests--contribution
                            (knowledge-tests--recall configuration conversation)
                            "knowledge-changes"))
                     "each change is explained once")
        (test-assert (equal (application--knowledge-changes-notice 1)
                            "@ 1 fact given earlier changed")
                     "the person sees that a fact the model relied on changed"))))
  nil)

(-> test-knowledge-commands () null)
(defun test-knowledge-commands ()
  "Test facts in (memories), explaining and forgetting facts, (what-changed), and rules."
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (let ((application (make-instance 'application
                                        :configuration configuration
                                        :conversation (conversation-create configuration))))
        (flet ((text (entry)
                 "Return ENTRY's plain text."
                 (memory-source-tests--text entry)))
          (knowledge-tests--define-appointments configuration)
          (knowledge-tests--admit-departure configuration)
          (knowledge-tests--schedule-review configuration workspace "13:00")
          (knowledge-tests--said configuration "event.buffer" '("review" "PT10M")
                                 "be there ten minutes early")
          (multiple-value-bind (travel)
              (knowledge-tests--said configuration "event.travel" '("review" "PT30M")
                                     "half an hour to get there")
            (test-assert (search "facts  4" (text (application-memories-entry application)))
                         "(memories) lists facts even without prose memories")
            (test-assert (search "4 facts hold now"
                                 (text (application-knowledge-changes-entry application)))
                         "the first (what-changed) records what the person saw")
            (knowledge-tests--schedule-review configuration workspace "14:00")
            (let ((listing (text (application-memories-entry application))))
              (test-assert (and (search "facts  5 · 1 no longer holding" listing)
                                (< (search "stale: schedule.txt" listing)
                                   (search "leave for review at 2026-10-22 13:20 UTC" listing)))
                           "a fact that stopped holding is listed first with its cause"))
            (let ((changes (text (application-knowledge-changes-entry application))))
              (test-assert (and (search "~ leave for review at 2026-10-22 13:20 UTC" changes)
                                (search "was leave for review at 2026-10-22 12:20 UTC" changes))
                           "(what-changed) shows the departure moving with its cause"))
            (test-assert (search "Nothing changed since"
                                 (text (application-knowledge-changes-entry application)))
                         "(what-changed) reports each change once")
            (let* ((view (application-knowledge--view application))
                   (departure (first (knowledge-view-holds view '("event.departure" "?e" "?t"))))
                   (why (text (application-memory-entry
                               application
                               (subseq (knowledge-node-identifier departure) 0 7)))))
              (test-assert (and (search "fact  supported" why)
                                (search "derived by departure v1" why)
                                (search "observation: schedule.txt \"review at 14:00\"" why))
                           "(memory) explains a fact selected by an ID prefix"))
            (let ((forgotten (text (application-memory-forget-entry application travel))))
              (test-assert (and (search "forgot  travel to review takes 30m" forgotten)
                                (search "1 derived fact changed or stopped holding" forgotten))
                           "(forget) retracts a fact and names what stopped holding with it")))
          (test-assert (search "✓ departure v1" (text (application-knowledge-rules-entry application)))
                       "(rules) lists the admitted rule")
          (test-assert (search "retired  departure"
                               (text (application-knowledge-rule-transition-entry
                                      application "departure" ':retire)))
                       "(retire-rule) retires a rule")
          (test-assert (search "awaiting (admit-rule \"departure\")"
                               (text (application-knowledge-rules-entry application)))
                       "a retired rule waits for admission")
          (test-assert (search "admitted  departure v1"
                               (text (application-knowledge-rule-transition-entry
                                      application "departure" ':admit)))
                       "(admit-rule) admits it again")
          (test-assert (search "No rule \"nothing\" exists"
                               (text (application-knowledge-rule-transition-entry
                                      application "nothing" ':admit)))
                       "an unknown rule points to (rules)")))))
  nil)

(-> knowledge-tests--tool-surface (configuration) string)
(defun knowledge-tests--tool-surface (configuration)
  "Return the resource tools' provider schemas under CONFIGURATION as JSON text."
  (let ((registry (make-default-tool-registry :configuration configuration)))
    (json-encode
     (vector (tool-provider-schema (tool-registry-find registry "resource" "read"))
             (tool-provider-schema (tool-registry-find registry "resource" "edit"))))))


(-> test-knowledge-activation () null)
(defun test-knowledge-activation ()
  "Test that passive and off modes leave the model's requests as they were."
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (let ((off (configuration-copy configuration :knowledge-mode "off"))
            (passive (configuration-copy configuration :knowledge-mode "passive")))
        (knowledge-tests--define-appointments configuration)
        (knowledge-tests--admit-departure configuration)
        (knowledge-tests--schedule-review configuration workspace "13:00")
        (knowledge-tests--said configuration "event.buffer" '("review" "PT10M")
                               "be there ten minutes early")
        (knowledge-tests--said configuration "event.travel" '("review" "PT30M")
                               "half an hour to get there")
        (let ((active-surface (knowledge-tests--tool-surface configuration))
              (off-surface (knowledge-tests--tool-surface off)))
          (test-assert (and (search "fact-assert" active-surface)
                            (search "memory:facts" active-surface))
                       "active mode offers knowledge operations and views")
          (test-assert (and (not (search "fact-assert" off-surface))
                            (not (search "memory:facts" off-surface))
                            (string= off-surface (knowledge-tests--tool-surface passive)))
                       "passive and off modes offer the earlier tool surface"))
        (dolist (mode (list off passive))
          (let ((conversation (conversation-create mode)))
            (conversation-append-user-message conversation "When should I leave for the review?")
            (test-assert (null (knowledge-tests--recall mode conversation))
                         (format nil "~A mode recalls no facts" (config :knowledge-mode mode)))))
        (test-assert (and (knowledge-capture-p passive)
                          (not (knowledge-capture-p off))
                          (not (knowledge-active-p passive)))
                     "passive mode records observations without offering them"))))
  nil)

(-> knowledge-tests--mission-facts (configuration) list)
(defun knowledge-tests--mission-facts (configuration)
  "Return the sentences of mission facts holding under CONFIGURATION, sorted."
  (sort (mapcar #'knowledge-node-text
                (append (knowledge-view-holds (knowledge-tests--view configuration)
                                              '("autolith.mission.status" "?m" "?s"))
                        (knowledge-view-holds (knowledge-tests--view configuration)
                                              '("autolith.mission.gate" "?m" "?g" "?s"))))
        #'string<))


(-> test-knowledge-mission-observations () null)
(defun test-knowledge-mission-observations ()
  "Test that mission status and gate results become facts, each report replacing the last."
  (dolist (mode '("active" "off"))
    (with-test-configuration (base root)
      (let* ((configuration (configuration-copy base :working-directory root :knowledge-mode mode))
             (application (mission-test--application configuration))
             (gates '((:id "first" :kind :artifact :path "first")
                      (:id "second" :kind :artifact :path "second" :attempt-limit 1))))
        (application-mission-start application (mission-test--specification :gates gates))
        (test-call-with-function-replacements
         (list (list 'application-tool-context (lambda (app) (mission-test--context app)))
               (list 'mission-gate-execute
                     (lambda (gate context)
                       (declare (ignore context))
                       (values (equal (getf gate :id) "first") "bounded test evidence"))))
         (lambda ()
           (application-mission-verify application)))
        (let ((facts (knowledge-tests--mission-facts configuration))
              (mission (format nil "mission/~A/~D"
                               (conversation-identifier (application-conversation application))
                               (getf (application-goal application) :created-at))))
          (if (string= mode "off")
              (test-assert (null facts) "off mode records no mission facts")
              (test-assert (equal facts
                                  (sort (list (format nil "gate first of mission ~A passed" mission)
                                              (format nil "gate second of mission ~A failed" mission)
                                              (format nil "mission ~A is exhausted" mission))
                                        #'string<))
                           "each gate result and only the latest mission status hold"))))))
  nil)

(-> test-knowledge-transfer () null)
(defun test-knowledge-transfer ()
  "Test exporting a workspace's knowledge and importing it into another directory."
  (with-test-configuration (base root)
    (with-test-configuration (target target-root)
      (let* ((source (test-data-transfer--workspace base (merge-pathnames "project/" root)))
             (workspace (merge-pathnames "project/" root))
             (path (merge-pathnames "workspace.sexp" root))
             (destination (merge-pathnames "moved/" target-root)))
        (knowledge-tests--define-appointments source)
        (knowledge-tests--admit-departure source)
        (knowledge-tests--schedule-review source workspace "13:00")
        (knowledge-tests--said source "event.buffer" '("review" "PT10M")
                               "be there ten minutes early")
        (multiple-value-bind (travel)
            (knowledge-tests--said source "event.travel" '("review" "PT30M")
                                   "half an hour to get there")
          (memory-remember source :title "Travel note" :content "Half an hour by tram."
                                  :premises (list travel)))
        (test-assert (plusp (getf (data-export path
                                               :workspace (config :working-directory source)
                                               :configuration source)
                                  :facts))
                     "a workspace export carries its facts")
        (memory-source-tests--write (merge-pathnames "schedule.txt" destination)
                                    (format nil "Design review at 13:00 UTC on 22 October.~%"))
        (data-import path :workspace (namestring destination) :configuration target)
        (let* ((moved (test-data-transfer--workspace target destination))
               (view (knowledge-tests--view moved))
               (travel (knowledge-claim-identifier
                        (knowledge-tests--claim moved "event.travel" '("review" "PT30M"))
                        :scope ':workspace :workspace (namestring destination)))
               (note (find "Travel note" (memory-list moved) :key #'memory-title
                                                             :test #'string=)))
          (test-assert (equal (knowledge-tests--departures view) '("2026-10-22T12:20:00Z"))
                       "the imported facts and rule derive the departure in the new directory")
          (test-assert (knowledge-node-supported-p (knowledge-view-node view travel))
                       "claim identities are recomputed for the new workspace")
          (test-assert (and note
                            (equal (getf (first (memory-premises note)) :id) travel)
                            (eq (memory-source-tests--status moved note) ':current))
                       "a memory premise naming a claim follows the recomputed identity"))
        (test-assert (zerop (getf (data-import path :workspace (namestring destination)
                                               :configuration target)
                                  :installed-files))
                     "importing the same archive again writes nothing"))))
  nil)

(-> test-knowledge-usage-attribution () null)
(defun test-knowledge-usage-attribution ()
  "Test that requests where knowledge took part record its share and their usage."
  (with-test-configuration (configuration)
    (knowledge-tests--define-appointments configuration)
    (knowledge-tests--said configuration "event.start" '("review" "2026-10-22T13:00:00Z")
                           "the review starts at one")
    (let* ((conversation (conversation-create configuration))
           (provider
             (make-instance
              'scripted-provider
              :results (list (agent-test-result
                              "read-facts"
                              (list (agent-test-call
                                     :call-id "facts" :namespace "resource" :name "read"
                                     :arguments "{\"uri\":\"memory:facts\"}"))
                              :usage (json-object "input_tokens" 900 "output_tokens" 100
                                                  "total_tokens" 1000))
                             (agent-test-result
                              "answer" (list (agent-test-message "Leave at noon."))
                              :usage (json-object "input_tokens" 1100 "output_tokens" 20
                                                  "total_tokens" 1120)))))
           (agent (agent-create :configuration configuration :conversation conversation
                                :provider provider
                                :tool-registry (make-default-tool-registry
                                                :configuration configuration)
                                :worker :unused)))
      (agent-run-user-turn agent "When does the review start?")
      (test-assert (equal (knowledge-usage-report conversation)
                          '(:requests 1 :facts 0 :changes 0 :tool-calls 1
                            :estimated-recall-tokens 0 :request-tokens 1000))
                   "a request whose model read memory:facts is attributed with its usage")
      (knowledge-usage-note (conversation-identifier conversation)
                            :facts 2 :fact-characters 400)
      (knowledge-usage-capture conversation nil 3
                               (json-object "input_tokens" 1500 "output_tokens" 30
                                            "total_tokens" 1530))
      (knowledge-usage-capture conversation nil 4
                               (json-object "input_tokens" 1500 "output_tokens" 30
                                            "total_tokens" 1530))
      (test-assert (equal (knowledge-usage-report conversation)
                          '(:requests 2 :facts 2 :changes 0 :tool-calls 1
                            :estimated-recall-tokens 100 :request-tokens 2530))
                   "recalled facts are attributed once, and requests without knowledge are not")))
  nil)

(-> test-knowledge-view-cache () null)
(defun test-knowledge-view-cache ()
  "Test that current views are reused only while nothing they depend on changed."
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (knowledge-tests--define-appointments configuration)
      (let ((schedule (knowledge-tests--schedule-review configuration workspace "13:00")))
        (knowledge-assert configuration
                          (knowledge-tests--claim configuration "event.travel" '("review" "PT30M"))
                          :evidence (knowledge-observe
                                     :source '(:url :url "https://maps.example.com/route")
                                     :adapter "web/1")
                          :basis ':observation)
        (let ((view (knowledge-view-current configuration)))
          (test-assert (eq view (knowledge-view-current configuration))
                       "an unchanged state reuses the evaluated view")
          (memory-source-tests--write schedule
                                      (format nil "Design review at 14:00 UTC on 22 October.~%"))
          (let ((edited (knowledge-view-current configuration)))
            (test-assert (and (not (eq view edited))
                              (null (knowledge-view-holds edited '("event.start" "?e" "?t"))))
                         "an edit in the same second at the same size is still noticed"))
          (let ((*knowledge-clock*
                  (let ((later (+ (get-universal-time) *knowledge-url-lifetime* 60)))
                    (lambda () later))))
            (test-assert (null (knowledge-view-holds (knowledge-view-current configuration)
                                                     '("event.travel" "?e" "?d")))
                         "passing an expiry boundary evaluates again"))
          (knowledge-tests--said configuration "event.buffer" '("review" "PT10M")
                                 "be there ten minutes early")
          (test-assert (knowledge-view-holds (knowledge-view-current configuration)
                                             '("event.buffer" "?e" "?d"))
                       "a new assertion evaluates again")))))
  nil)

(-> test-knowledge-plan-steps () null)
(defun test-knowledge-plan-steps ()
  "Test plan steps that depend on facts: filled text, blocking, repair, and receipts."
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (knowledge-tests--define-appointments configuration)
      (knowledge-define-schema configuration
                               (knowledge-make-schema
                                :predicate "event.room"
                                :arguments '((:name "event" :type :entity)
                                             (:name "room" :type :string))
                                :key '("event")))
      (knowledge-tests--admit-departure configuration)
      (knowledge-tests--schedule-review configuration workspace "13:00")
      (knowledge-tests--said configuration "event.buffer" '("review" "PT10M")
                             "be there ten minutes early")
      (knowledge-tests--said configuration "event.travel" '("review" "PT30M")
                             "half an hour to get there")
      (let* ((registry (make-default-tool-registry :configuration configuration))
             (context (make-instance 'tool-context
                                     :configuration configuration
                                     :worker nil
                                     :conversation (conversation-create configuration))))
        (flet ((update (&rest steps)
                 (tool-result-content
                  (memory-resource-tests--call registry context "plan" "update"
                                               "steps" (coerce steps 'vector))))

               (listing ()
                 (tool-result-content
                  (memory-resource-tests--call registry context "plan" "list")))

               (leave (status)
                 (json-object "step" "Leave at {?leave}" "status" status "id" "leave"
                              "requires" #(#("event.departure" "review" "?leave"))))

               (room-step ()
                 (json-object "step" "Note room {?room}" "status" "pending"
                              "requires" #(#("event.room" "review" "?room")))))
          (let ((planned (update (leave "pending") (room-step)
                                 (json-object "step" "Book a taxi" "status" "pending"))))
            (test-assert (and (search "1. [pending] Leave at 2026-10-22 12:20 UTC" planned)
                              (search "2. [pending] Note room {?room} (blocked: nothing satisfies event.room(review, ?room))"
                                      planned)
                              (search "3. [pending] Book a taxi" planned))
                         "steps fill their text from facts and show what blocks them"))
          (knowledge-tests--schedule-review configuration workspace "14:00")
          (test-assert (search "1. [pending] Leave at 2026-10-22 13:20 UTC" (listing))
                       "a pending step follows a recomputed fact without replanning")
          (update (leave "done") (room-step))
          (knowledge-tests--schedule-review configuration workspace "15:00")
          (test-assert (search "1. [done] Leave at 2026-10-22 13:20 UTC (done; it relied on leave for review at 2026-10-22 13:20 UTC, which no longer holds)"
                               (listing))
                       "a done step keeps what it was done with and reports the change")
          (update (leave "done") (room-step))
          (test-assert (search "(done; it relied on" (listing))
                       "restating a done step keeps its receipt")
          (let ((failed (memory-resource-tests--call
                         registry context "plan" "update"
                         "steps" (vector (json-object "step" "Pack" "status" "pending"
                                                      "requires" #(#("trip.bag" "?b")))))))
            (test-assert (and (not (tool-result-success-p failed))
                              (search "Define predicate trip.bag" (tool-result-content failed)))
                         "an undefined predicate in a step is refused correctively"))))))
  nil)

(-> knowledge-tests--plan-advice (configuration conversation) (option context-contribution))
(defun knowledge-tests--plan-advice (configuration conversation)
  "Return the plan change advice a request for CONVERSATION carries, if any."
  (let ((*context-contributors* nil)
        (*context-resolver* (cl-llm-provider-api:make-context-resolver))
        (*context-last-deliveries* (make-hash-table :test #'equal))
        (*context-last-delivery-order* nil))
    (register-context-contributor "plan-changes" 'plan-changes-context :source ':built-in)
    (knowledge-tests--contribution
     (context-delivery-contributions (context-resolve-request configuration conversation #()))
     "plan-changes")))


(-> test-knowledge-plan-changes () null)
(defun test-knowledge-plan-changes ()
  "Test that changes to fact-dependent plan steps are explained once, after delivery."
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (knowledge-tests--define-appointments configuration)
      (knowledge-tests--admit-departure configuration)
      (knowledge-tests--schedule-review configuration workspace "13:00")
      (knowledge-tests--said configuration "event.buffer" '("review" "PT10M")
                             "be there ten minutes early")
      (knowledge-tests--said configuration "event.travel" '("review" "PT30M")
                             "half an hour to get there")
      (let* ((conversation (conversation-create configuration))
             (registry (make-default-tool-registry :configuration configuration))
             (context (make-instance 'tool-context :configuration configuration :worker nil
                                                   :conversation conversation)))
        (conversation-append-user-message conversation "Plan my afternoon.")
        (memory-resource-tests--call
         registry context "plan" "update"
         "steps" (vector (json-object "step" "Leave at {?leave}" "status" "pending" "id" "leave"
                                      "requires" #(#("event.departure" "review" "?leave")))))
        (conversation-append-user-message conversation "Anything to adjust?")
        (test-assert (null (knowledge-tests--plan-advice configuration conversation))
                     "a plan the model just saw needs no advice")
        (knowledge-tests--schedule-review configuration workspace "14:00")
        (conversation-append-user-message conversation "And now?")
        (let ((advice (knowledge-tests--plan-advice configuration conversation)))
          (test-assert (and advice
                            (search "step 1 now reads \"Leave at 2026-10-22 13:20 UTC\" and is ready, was \"Leave at 2026-10-22 12:20 UTC\""
                                    (context-contribution-evidence advice)))
                       "the next turn explains how the step changed with its facts"))
        (conversation-append-user-message conversation "Thanks.")
        (test-assert (null (knowledge-tests--plan-advice configuration conversation))
                     "each plan change is explained once"))))
  nil)

(-> test-knowledge-task-methods () null)
(defun test-knowledge-task-methods ()
  "Test task methods: local expansion into a checked plan and each failure outcome."
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (knowledge-tests--define-appointments configuration)
      (knowledge-tests--admit-departure configuration)
      (knowledge-tests--schedule-review configuration workspace "13:00")
      (knowledge-tests--said configuration "event.buffer" '("review" "PT10M")
                             "be there ten minutes early")
      (let* ((registry (make-default-tool-registry :configuration configuration))
             (context (make-instance 'tool-context :configuration configuration :worker nil
                                                   :conversation (conversation-create configuration)))
             (travel (nth-value 0 (knowledge-tests--said configuration "event.travel"
                                                         '("review" "PT30M")
                                                         "half an hour to get there"))))
        (flet ((define (&rest operations)
                 (tool-result-content
                  (memory-resource-tests--call
                   registry context "resource" "edit"
                   "uri" "memory:facts"
                   "base-revision" (memory-resource-tests--field
                                    (tool-result-content
                                     (memory-resource-tests--call registry context "resource" "read"
                                                                  "uri" "memory:facts"))
                                    "Revision: ")
                   "operations" (coerce operations 'vector))))

               (expand (&rest task)
                 (tool-result-content
                  (memory-resource-tests--call registry context "plan" "expand"
                                               "task" (coerce task 'vector)))))
          (test-assert
           (search "Defined method prepare-appointment."
                   (define (json-object "op" "method-define" "id" "prepare-appointment"
                                        "task" #("prepare" "?event")
                                        "when" #(#("event.start" "?event" "?start"))
                                        "steps" (vector (json-object "task" #("check-route" "?event"))
                                                        (json-object "step" "Leave for {?event} at {?leave}"
                                                                     "requires" #(#("event.departure" "?event" "?leave")))))
                           (json-object "op" "method-define" "id" "check-route-known"
                                        "task" #("check-route" "?event")
                                        "when" #(#("event.travel" "?event" "?travel"))
                                        "steps" (vector (json-object "step" "Travel takes {?travel}")))
                           (json-object "op" "method-define" "id" "check-route-unknown"
                                        "task" #("check-route" "?event")
                                        "steps" (vector (json-object "step" "Look up the route to {?event}")))
                           (json-object "op" "method-define" "id" "spin"
                                        "task" #("spin" "?x")
                                        "steps" (vector (json-object "task" #("spin" "?x"))))))
           "methods are defined through memory:facts")
          (let ((plan (expand "prepare" "review")))
            (test-assert (and (search "1. [pending] Travel takes 30m" plan)
                              (search "2. [pending] Leave for review at 2026-10-22 12:20 UTC" plan))
                         "a task expands locally into steps filled from facts"))
          (knowledge-retract configuration travel)
          (let ((plan (expand "prepare" "review")))
            (test-assert (and (search "1. [pending] Look up the route to review" plan)
                              (search "(blocked: nothing satisfies event.departure(review, ?leave))"
                                      plan))
                         "without the travel fact the alternative method applies and the departure is blocked"))
          (test-assert (search "No method describes celebrate" (expand "celebrate" "review"))
                       "a task without methods is reported as unmodeled")
          (test-assert (search "method prepare-appointment needs event.start(standup, ?start)"
                               (expand "prepare" "standup"))
                       "methods that do not apply report their unmet conditions")
          (test-assert (search "without finishing" (expand "spin" "top"))
                       "a recursive method exhausts its budget instead of looping")
          (test-assert (search "is not a task parameter or bound by a condition"
                               (define (json-object "op" "method-define" "id" "broken"
                                                    "task" #("broken")
                                                    "steps" (vector (json-object "task" #("spin" "?free"))))))
                       "a subtask passing an unbound variable is refused")
          (test-assert (search "prepare-appointment workspace: prepare(?event) when event.start(?event, ?start)"
                               (tool-result-content
                                (memory-resource-tests--call registry context "resource" "read"
                                                             "uri" "memory:methods")))
                       "memory:methods lists the methods")))))
  nil)

(-> test-knowledge-fact-gates () null)
(defun test-knowledge-fact-gates ()
  "Test mission gates that pass only on facts resting on accepted evidence."
  (dolist (case '((:user-assertion :passed) (:assumption :failed)))
    (destructuring-bind (travel-basis expected) case
      (with-test-configuration (base root)
        (let* ((configuration (configuration-copy base :working-directory root))
               (application (mission-test--application configuration)))
          (knowledge-tests--define-appointments configuration)
          (knowledge-tests--admit-departure configuration)
          (knowledge-tests--review configuration :travel-basis travel-basis)
          (application-mission-start
           application
           (mission-test--specification
            :gates '((:id "departure-known" :kind :fact
                      :pattern ("event.departure" "review" "?t")))))
          (test-call-with-function-replacements
           (list (list 'application-tool-context (lambda (app) (mission-test--context app))))
           (lambda () (application-mission-verify application)))
          (let ((gate (first (getf (application-goal application) :gates))))
            (test-assert (eq (getf gate :status) expected)
                         (format nil "a departure derived from ~(~A~) ~(~A~) the gate"
                                 travel-basis expected))
            (when (eq expected ':failed)
              (test-assert (search "No fact resting on observation or user-assertion evidence"
                                   (getf gate :evidence))
                           "the failed gate says which evidence it accepts"))))))
    nil))

(-> test-knowledge-expiry-lifecycle () null)
(defun test-knowledge-expiry-lifecycle ()
  "Test that a fact expires on time after a cached evaluation and applies again when reobserved."
  (with-test-configuration (base root)
    (let* ((configuration (configuration-copy base :working-directory root))
           (now 100000)
           (*knowledge-clock* (lambda () now))
           (until (+ now *knowledge-url-lifetime*)))
      (knowledge-define-schema configuration
                               (knowledge-make-schema
                                :predicate "price.listed"
                                :arguments '((:name "item" :type :entity)
                                             (:name "price" :type :number))
                                :key '("item")))
      (flet ((observe ()
               (knowledge-assert configuration '("price.listed" "lamp" 40)
                                 :evidence (knowledge-observe
                                            :source '(:url :url "https://example.com/lamp")
                                            :digest (memory-source-text-digest "Lamp: 40")
                                            :adapter "web/1")
                                 :basis ':observation))

             (state ()
               (knowledge-node-applicability
                (knowledge-view-node (knowledge-view-current configuration)
                                     (knowledge-claim-identifier
                                      '("price.listed" "lamp" 40)
                                      :scope ':workspace
                                      :workspace (knowledge--workspace configuration))))))
        (observe)
        (setf now until)
        (test-assert (eq (state) ':current) "an observation applies until its last second")
        (setf now (1+ until))
        (test-assert (eq (state) ':expired)
                     "the view evaluated at that second does not keep it current afterwards")
        (setf now (+ 100 until))
        (test-assert (eq (state) ':expired) "an expired observation stays expired")
        (observe)
        (test-assert (eq (state) ':current)
                     "observing the same content again renews it")
        (setf now (+ 200 until))
        (test-assert (eq (state) ':current)
                     "the renewed observation expires a lifetime after it was made"))))
  nil)

(-> test-knowledge-cross-scope-conflicts () null)
(defun test-knowledge-cross-scope-conflicts ()
  "Test visible opposites across scopes at fact gates and action dispatch."
  (dolist (global-negated '(nil t))
    (with-test-configuration (base root)
      (let* ((configuration (configuration-copy base :working-directory root))
             (other (configuration-copy
                     base :working-directory
                     (ensure-directories-exist (merge-pathnames "other/" root))))
             (context (make-instance 'tool-context :configuration configuration :worker nil
                                                  :conversation (conversation-create configuration)))
             (other-context (make-instance 'tool-context :configuration other :worker nil))
             (registry (make-default-tool-registry :configuration configuration))
             (mail (make-instance 'knowledge-tests--mail-tool
                                  :namespace "mail" :name "send" :description "Send a report."
                                  :parameters (tool-object-schema
                                               (json-object "report" (tool-string-property "Report."))
                                               '("report"))))
             (positive '("report.ready" "q3"))
             (negative '(:not ("report.ready" "q3")))
             (global (if global-negated negative positive)))
        (tool-registry-register registry mail)
        (dolist (predicate '("report.ready" "report.eligible" "report.blocked" "report.known"))
          (knowledge-define-schema
           configuration
           (knowledge-make-schema :predicate predicate
                                  :arguments '((:name "report" :type :entity)))
           :scope ':global))
        (knowledge-propose-rule configuration :id "eligible"
                                :premises '(("report.ready" "?r"))
                                :conclusion '("report.eligible" "?r"))
        (knowledge-admit-rule configuration "eligible")
        (knowledge-tests--said configuration "report.known" '("q3") "q3 is known")
        (knowledge-propose-rule configuration :id "blocked"
                                :premises '(("report.known" "?r") (:not ("report.ready" "?r")))
                                :conclusion '("report.blocked" "?r"))
        (knowledge-admit-rule configuration "blocked")
        (knowledge-assert configuration global :scope ':global
                          :evidence (knowledge-observe :source '(:user :quote "global report status")
                                                       :adapter "user/1")
                          :basis ':user-assertion)
        (knowledge-assert configuration (if global-negated positive negative)
                          :evidence (knowledge-observe :source '(:user :quote "local opposite status")
                                                       :adapter "user/1")
                          :basis ':user-assertion)
        (let* ((view (knowledge-view-current configuration))
               (nodes (append (knowledge-view-holds view positive)
                              (knowledge-view-holds view negative))))
          (test-assert (= 2 (length nodes)) "both scoped identities are visible")
          (dolist (node nodes)
            (test-assert (and (eq (knowledge-node-status view node) ':conflicted)
                              (member (knowledge-node-identifier
                                       (find-if (lambda (other) (not (eq other node))) nodes))
                                      (knowledge-node-conflicts node) :test #'string=)
                              (not (knowledge-node-admissible-p view node)))
                         "each visible opposite has a symmetric conflict edge")))
        (dolist (pattern (list positive negative '("report.eligible" "q3")
                              '("report.blocked" "q3")))
          (test-assert (not (mission--fact-gate-support
                            (list :kind :fact :pattern pattern
                                  :accepted '(:observation :user-assertion)) context))
                       "neither disputed premise nor its conclusion satisfies a fact gate")
          (knowledge-define-contract configuration :id "send-report" :tool "mail.send"
                                     :requires (list pattern))
          (test-assert (and (not (tool-result-success-p
                                 (memory-resource-tests--call registry context "mail" "send"
                                                              "report" "q3")))
                            (zerop (knowledge-tests--mail-tool-calls mail)))
                       "a contract refuses the disputed requirement before the action"))
        (test-assert (mission--fact-gate-support
                      (list :kind :fact :pattern global
                            :accepted '(:observation :user-assertion)) other-context)
                     "the other workspace can use the unopposed global claim")
        (dolist (scope '(:global :workspace))
          (knowledge-assert configuration '("report.ready" "clean") :scope scope
                            :evidence (knowledge-observe :source '(:user :quote "clean is ready")
                                                         :adapter "user/1")
                            :basis ':user-assertion))
        (let* ((view (knowledge-view-current configuration))
               (nodes (knowledge-view-holds view '("report.ready" "clean"))))
          (test-assert (and (= 2 (length nodes))
                            (every (lambda (node) (knowledge-node-admissible-p view node)) nodes))
                       "equal positives in different scopes are independent clean alternatives")))))
  nil)

(-> test-knowledge-admissible-support () null)
(defun test-knowledge-admissible-support ()
  "Test that gates use only undisputed facts on accepted bases, negated premises included."
  (with-test-configuration (base root)
    (let* ((configuration (configuration-copy base :working-directory root))
           (context (make-instance 'tool-context :configuration configuration :worker nil)))
      (dolist (predicate '("item.exists" "item.blocked" "item.ready" "item.paid"))
        (knowledge-define-schema configuration
                                 (knowledge-make-schema
                                  :predicate predicate
                                  :arguments '((:name "item" :type :entity)))))
      (knowledge-propose-rule configuration
                              :id "readiness"
                              :premises '(("item.exists" "?x") (:not ("item.blocked" "?x")))
                              :conclusion '("item.ready" "?x"))
      (knowledge-admit-rule configuration "readiness")
      (knowledge-tests--said configuration "item.exists" '("a") "a exists")
      (knowledge-assert configuration
                        (knowledge-tests--claim configuration "item.blocked" '("a") :negated t)
                        :basis ':assumption)
      (knowledge-tests--said configuration "item.exists" '("b") "b exists")
      (knowledge-tests--said configuration "item.blocked" '("b") "b is not blocked" :negated t)
      (knowledge-tests--said configuration "item.paid" '("c") "c is paid")
      (knowledge-tests--said configuration "item.paid" '("c") "c is not paid" :negated t)
      (knowledge-tests--said configuration "item.exists" '("d") "d exists")
      (knowledge-tests--said configuration "item.blocked" '("d") "d is not blocked" :negated t)
      (knowledge-tests--said configuration "item.blocked" '("d") "d is blocked")
      (let* ((view (knowledge-view-current configuration))
             (ready-a (first (knowledge-view-holds view '("item.ready" "a")))))
        (test-assert (and ready-a
                          (= 2 (length (getf (first (knowledge-node-justifications ready-a))
                                             :premises)))
                          (equal (knowledge-node-bases ready-a)
                                 '((:user-assertion :assumption))))
                     "a negated premise joins the derivation's premises and basis"))
      (loop for (pattern expected description) in
            '((("item.ready" "a") nil "a conclusion resting on an assumed negation fails the gate")
              (("item.ready" "b") t "a conclusion resting on stated facts passes the gate")
              ((:not ("item.blocked" "b")) t "a stated negation passes a negated gate")
              ((:not ("item.blocked" "a")) nil "an assumed negation fails a negated gate")
              (("item.paid" "c") nil "a disputed fact fails the gate")
              (("item.ready" "d") nil "a conclusion with a conflicted negative premise fails the gate")
              ((:not ("item.paid" "c")) nil "a disputed negation fails the gate"))
            do (test-assert (eq expected
                                (and (mission--fact-gate-support
                                      (list :kind :fact :pattern pattern
                                            :accepted '(:observation :user-assertion))
                                      context)
                                     t))
                            description))))
  (with-test-configuration (base root)
    (let* ((configuration (configuration-copy base :working-directory root))
           (context (make-instance 'tool-context :configuration configuration :worker nil)))
      (dolist (predicate '("item.exists" "item.ready" "item.sent"))
        (knowledge-define-schema configuration
                                 (knowledge-make-schema
                                  :predicate predicate
                                  :arguments '((:name "item" :type :entity)))))
      (knowledge-propose-rule configuration :id "ready"
                              :premises '(("item.exists" "?x"))
                              :conclusion '("item.ready" "?x"))
      (knowledge-admit-rule configuration "ready")
      (knowledge-propose-rule configuration :id "send"
                              :premises '(("item.ready" "?x"))
                              :conclusion '("item.sent" "?x"))
      (knowledge-admit-rule configuration "send")
      (knowledge-tests--said configuration "item.exists" '("a") "a exists")
      (knowledge-tests--said configuration "item.exists" '("a") "a does not exist" :negated t)
      (knowledge-tests--said configuration "item.exists" '("b") "b exists")
      (knowledge-assert configuration '("item.exists" "b") :basis ':assumption)
      (let* ((view (knowledge-view-current configuration))
             (node (first (knowledge-view-holds view '("item.sent" "b")))))
        (test-assert (and (member '(:assumption) (knowledge-node-bases node) :test #'equal)
                          (member '(:user-assertion) (knowledge-node-bases node) :test #'equal))
                     "an indirect derivation preserves incomparable alternative bases")
        (dolist (basis '(:user-assertion :assumption))
          (test-assert (mission--fact-gate-support
                        (list :kind :fact :pattern '("item.sent" "b")
                              :accepted (list basis)) context)
                       "either independent basis can satisfy a policy explicitly accepting it")))
      (flet ((passes (predicate)
               (and (mission--fact-gate-support
                     (list :kind :fact :pattern (list predicate "a")
                           :accepted '(:observation :user-assertion))
                     context)
                    t)))
        (let* ((view (knowledge-view-current configuration))
               (node (first (knowledge-view-holds view '("item.sent" "a")))))
          (test-assert (and node (knowledge-node-observed-p node))
                       "conflicted premises retain their derived evidence for inspection")
          (test-assert (not (passes "item.ready"))
                       "a conclusion from a conflicted premise fails the gate")
          (test-assert (not (passes "item.sent"))
                       "conflict in an indirect premise also fails the gate"))
        (knowledge-tests--said configuration "item.ready" '("a") "a is independently ready")
        (test-assert (passes "item.sent")
                     "an independent clean support path can justify the conclusion"))))
  (dolist (bases '((:assumption :user-assertion) (:user-assertion :assumption)))
    (with-test-configuration (base root)
      (let* ((configuration (configuration-copy base :working-directory root))
             (context (make-instance 'tool-context :configuration configuration :worker nil)))
        (knowledge-tests--define-appointments configuration)
        (knowledge-tests--admit-departure configuration)
        (knowledge-tests--said configuration "event.start" '("review" "2026-10-22T13:00:00Z")
                               "the review starts at one")
        (knowledge-tests--said configuration "event.buffer" '("review" "PT10M")
                               "be there ten minutes early")
        (dolist (basis bases)
          (if (eq basis ':user-assertion)
              (knowledge-tests--said configuration "event.travel" '("review" "PT30M")
                                     "half an hour to get there")
              (knowledge-assert configuration
                                (knowledge-tests--claim configuration "event.travel"
                                                        '("review" "PT30M"))
                                :basis basis)))
        (let* ((view (knowledge-view-current configuration))
               (departure (first (knowledge-view-holds view '("event.departure" "review" "?t")))))
          (test-assert (and (member '(:user-assertion) (knowledge-node-bases departure) :test #'equal)
                            (knowledge-node-observed-p departure))
                       "the derivation preserves its independent stated support path")
          (test-assert (mission--fact-gate-support
                        '(:kind :fact :pattern ("event.departure" "review" "?t")
                          :accepted (:user-assertion)) context)
                       "the clean path passes regardless of support insertion order"))
        (let* ((*knowledge-match-limit* 0)
               (view (knowledge-tests--view configuration)))
          (test-assert (knowledge-view-incomplete view) "a bounded view reports unfinished inference")
          (test-call-with-function-replacements
           (list (list 'knowledge-view-current (lambda (&rest arguments)
                                                (declare (ignore arguments))
                                                view)))
           (lambda ()
             (test-assert (null (mission--fact-gate-support
                                 '(:kind :fact :pattern ("event.start" "review" "?t")
                                   :accepted (:user-assertion)) context))
                          "even stored evidence cannot satisfy a gate in an incomplete view")))))))
  (with-test-configuration (base root)
    (let ((configuration (configuration-copy base :working-directory root)))
      (dolist (predicate '("item.exists" "item.ready"))
        (knowledge-define-schema
         configuration (knowledge-make-schema
                        :predicate predicate
                        :arguments '((:name "item" :type :entity)))))
      (knowledge-propose-rule configuration :id "failed-contradiction"
                              :premises '(("item.exists" "?x"))
                              :bindings '(("?unused" ("/" 1 0)))
                              :conclusion '(:not ("item.ready" "?x")))
      (knowledge-admit-rule configuration "failed-contradiction")
      (knowledge-tests--said configuration "item.exists" '("a") "a exists")
      (let ((source (memory-source-tests--write (merge-pathnames "ready.txt" root) "a is ready")))
        (knowledge-assert configuration '("item.ready" "a") :basis ':observation
                          :evidence (knowledge-observe-file source :quote "a is ready"
                                                            :workspace (namestring root))))
      (let* ((view (knowledge-tests--view configuration))
             (node (first (knowledge-view-holds view '("item.ready" "a")))))
        (test-assert (and node (knowledge-node-observed-p node :accepted '(:observation)))
                     "the decision node retains its independent observed support")
        (test-assert (and (knowledge-view-errors view)
                          (not (knowledge-view-incomplete view)))
                     "a failed contradiction derivation records an inference error")
        (test-assert (not (knowledge-node-admissible-p view node))
                     "stored evidence cannot justify a decision after inference errors"))))
  (with-test-configuration (base root)
    (let ((configuration (configuration-copy base :working-directory root)))
      (dolist (predicate '("item.exists" "item.blocked" "item.ready"))
        (knowledge-define-schema
         configuration (knowledge-make-schema
                        :predicate predicate
                        :arguments '((:name "item" :type :entity)))))
      (knowledge-propose-rule configuration :id "negative-alternatives"
                              :premises '(("item.exists" "?x") (:not ("item.blocked" "?x")))
                              :conclusion '("item.ready" "?x"))
      (knowledge-admit-rule configuration "negative-alternatives")
      (knowledge-tests--said configuration "item.exists" '("a") "a exists")
      ;; Distinct visible scopes provide independent proofs of the same negation.
      (dolist (reverse-p '(nil t))
        (let* ((view (knowledge-tests--view configuration))
               (negations
                 (loop for (scope basis) in '((:global :observation) (:workspace :model-proposal))
                       collect (let ((node (knowledge--view-node
                                            view (symbol-name scope)
                                            '(:not ("item.blocked" "a")) scope
                                            (and (eq scope ':workspace)
                                                 (knowledge-view-workspace view)))))
                                 (setf (knowledge-node-supports node)
                                       (list (list :state :current :basis basis))
                                       (knowledge-node-bases node) (list (list basis)))
                                 node))))
          (setf (gethash "item.blocked" (knowledge-view-negated view))
                (if reverse-p (reverse negations) negations))
          (dolist (item (knowledge-base-active-rules
                        (knowledge-view-base view) :workspace (knowledge-view-workspace view)))
            (knowledge--run-rule view item))
          (let ((ready (first (knowledge-view-holds view '("item.ready" "a")))))
            (test-assert (= 2 (length (knowledge-node-justifications ready)))
                         "each matching negative fact supplies an independent justification")
            (test-assert (knowledge-node-admissible-p view ready)
                         "a clean negative proof justifies a decision in either index order")
            (test-assert (member '(:observation :user-assertion)
                                 (knowledge-node-bases ready) :test #'equal)
                         "negative basis alternatives survive rule matching"))
          (dolist (patterns '((("item.exists" "?x") (:not ("item.blocked" "?x")))
                              ((:not ("item.blocked" "a")))))
            (multiple-value-bind (solutions cut-p) (knowledge-view-solve view patterns)
              (test-assert (and (= 2 (length solutions)) (not cut-p))
                           "queries preserve every matching negative alternative"))
            (multiple-value-bind (solutions cut-p) (knowledge-view-solve view patterns :limit 1)
              (test-assert (and (= 1 (length solutions)) cut-p)
                           "negative alternatives obey the solution limit")))))))
  nil)

(defclass knowledge-tests--mail-tool (tool)
  ((calls
    :initform 0
    :accessor knowledge-tests--mail-tool-calls
    :documentation "How many times the tool ran."))
  (:documentation "A mail tool answering with a JSON receipt, for contract tests."))

(defmethod tool-execute ((tool knowledge-tests--mail-tool) (context tool-context) (arguments hash-table))
  "Count the call and answer with a JSON receipt."
  (declare (ignore context arguments))
  (incf (knowledge-tests--mail-tool-calls tool))
  (tool-success (json-encode (json-object "id" "msg-1" "status" "queued"))))


(-> test-knowledge-action-contracts () null)
(defun test-knowledge-action-contracts ()
  "Test contracts refusing calls on unmet requirements and recording predictions and observations."
  (dolist (mode '("active" "passive"))
    (with-test-configuration (base root)
      (let* ((configuration (configuration-copy base :working-directory root :knowledge-mode mode))
             (registry (make-default-tool-registry :configuration configuration))
             (mail (make-instance 'knowledge-tests--mail-tool
                                  :namespace "mail" :name "send" :description "Send a report."
                                  :parameters (tool-object-schema
                                               (json-object "report" (tool-string-property "Report."))
                                               '("report"))))
             (context (make-instance 'tool-context :configuration configuration :worker nil
                                                   :conversation (conversation-create configuration))))
        (tool-registry-register registry mail)
        (dolist (definition '(("report.ready" (("report" :entity)) nil)
                              ("report.sent" (("report" :entity)) nil)
                              ("mail.message" (("report" :entity) ("message" :string)) ("report"))))
          (destructuring-bind (predicate arguments key) definition
            (knowledge-define-schema
             configuration
             (knowledge-make-schema :predicate predicate
                                    :arguments (mapcar (lambda (argument)
                                                         (list :name (first argument)
                                                               :type (second argument)))
                                                       arguments)
                                    :key key))))
        (knowledge-define-contract
         configuration
         :id "send-report" :tool "mail.send"
         :when (json-object "report" "?report")
         :requires #(#("report.ready" "?report"))
         :effects #(#("report.sent" "?report"))
         :observes (vector (json-object "predicate" "mail.message"
                                        "arguments" #("$arg.report" "$result.id"))))
        (flet ((send ()
                 (memory-resource-tests--call registry context "mail" "send" "report" "q3"))

               (holding (pattern)
                 (knowledge-view-holds (knowledge-view-current configuration) pattern)))
          (let ((first-call (send)))
            (if (string= mode "active")
                (test-assert (and (not (tool-result-success-p first-call))
                                  (search "Contract send-report requires report.ready(q3), which does not hold. The call was not made."
                                          (tool-result-content first-call))
                                  (zerop (knowledge-tests--mail-tool-calls mail)))
                             "an unmet requirement refuses the call before it runs")
                (test-assert (and (tool-result-success-p first-call)
                                  (= 1 (knowledge-tests--mail-tool-calls mail)))
                             "passive mode never refuses a call")))
          (knowledge-assert configuration '("report.ready" "q3") :basis ':model-proposal)
          (when (string= mode "active")
            (let ((refused (send)))
              (test-assert (and (not (tool-result-success-p refused))
                                (search "holds only on disputed facts or on bases it does not accept (it accepts observation, user-assertion)"
                                        (tool-result-content refused)))
                           "a requirement resting only on the model's proposal does not admit the call")))
          (knowledge-define-schema configuration
                                   (knowledge-make-schema
                                    :predicate "report.drafted"
                                    :arguments '((:name "report" :type :entity))))
          (knowledge-propose-rule configuration :id "draft-ready"
                                  :premises '(("report.drafted" "?report"))
                                  :conclusion '("report.ready" "?report"))
          (knowledge-admit-rule configuration "draft-ready")
          (knowledge-tests--said configuration "report.drafted" '("q3") "q3 is drafted")
          (knowledge-tests--said configuration "report.drafted" '("q3") "q3 is not drafted" :negated t)
          (when (string= mode "active")
            (test-assert (and (not (tool-result-success-p (send)))
                              (zerop (knowledge-tests--mail-tool-calls mail)))
                         "a conflicted premise cannot satisfy the contract through a derivation"))
          (knowledge-tests--said configuration "report.ready" '("q3") "the q3 report is ready")
          (when (string= mode "active")
            (test-assert (tool-result-success-p (send))
                         "the call runs once its requirement holds on the user's statement"))
          (let ((sent (first (holding '("report.sent" "q3"))))
                (message (first (holding '("mail.message" "q3" "?m")))))
            (test-assert (and sent (not (knowledge-node-observed-p sent)))
                         "the predicted effect is recorded as a prediction, not an observation")
            (test-assert (and message
                              (equal (third (knowledge-node-proposition message)) "msg-1")
                              (knowledge-node-observed-p message))
                         "the result's id is observed through the contract")))
        (when (string= mode "active")
          (let ((listing (memory-resource-tests--call registry context "resource" "read"
                                                     "uri" "memory:contracts")))
            (test-assert (and (tool-result-success-p listing)
                              (search "send-report" (tool-result-content listing)))
                         "memory:contracts lists the contract"))))))
  nil)
