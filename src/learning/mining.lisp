(in-package #:autolith)

;;;; -- Procedure Mining --

;;; Turns that called the same tools in the same order are generalized into
;;; one procedure by anti-unification: an argument equal in every turn stays a
;;; constant, one equal to a field of an earlier call's JSON result becomes a
;;; reference to it, and one that varies becomes a parameter. Values that vary
;;; together share a parameter, and a value that contains another parameter's
;;; value at a fixed position becomes text around it. Words that vary in the
;;; requests that started the turns become the procedure's request pattern.
;;;
;;; A procedure is kept only when every source turn is explained by the
;;; procedure learned from the others, so it generalizes instead of copying
;;; one turn. Mining is local and deterministic; no model takes part.

(defparameter *procedure-minimum-examples* 3
  "How many turns must share a tool sequence before it is learned.")

(defparameter *procedure-example-limit* 8
  "The most recent turns one procedure is learned from.")

(defparameter *procedure-output-leaf-limit* 256
  "The most result fields searched for values later calls reuse.")

(defparameter *procedure-output-depth-limit* 4
  "The deepest result field searched for values later calls reuse.")

(defparameter *procedure-name-stopwords*
  '("a" "an" "the" "for" "of" "to" "in" "on" "at" "with" "and" "or" "please"
    "my" "our" "your" "this" "that" "it" "me" "us" "now" "again")
  "Request words left out of a learned procedure's name.")


;;;; -- Learning --

(-> procedure-mine (list) (option list))
(defun procedure-mine (traces)
  "Return a procedure generalizing TRACES, which share one tool sequence, or NIL.

NIL means the calls cannot be described by one procedure: their arguments
differ in shape or type."
  (let* ((input (mapcar #'action-trace-input traces))
         (outputs (mapcar (lambda (trace)
                            (mapcar (lambda (call) (procedure--decode-output (getf call :output)))
                                    (action-trace-calls trace)))
                          traces))
         (parameters nil)
         (hole-parameters nil))
    (multiple-value-bind (pattern holes) (procedure--input-pattern input)
      (labels ((parameter-name (key)
                 (let ((base (procedure--parameter-base-name key)))
                   (loop for index from 1
                         for name = (if (= index 1) base (format nil "~A-~D" base index))
                         unless (find name parameters
                                      :key (lambda (parameter) (getf parameter :name))
                                      :test #'string=)
                           return name)))

               (add-parameter (key values)
                 (let ((name (parameter-name key))
                       (type (procedure--values-type values)))
                   (unless type
                     (return-from procedure-mine nil))
                   (setf parameters
                         (append parameters
                                 (list (append (list :name name :type type)
                                               (let ((suffix (and (eq type ':string)
                                                                  (procedure--common-extension
                                                                   values))))
                                                 (and suffix (list :suffix suffix)))
                                               (list :values values)))))
                   name))

               (parameter-with-values (values)
                 (find-if (lambda (parameter)
                            (every #'procedure-json-equal-p (getf parameter :values) values))
                          parameters))

               (hole-with-values (values)
                 (loop for hole in holes
                       for index from 0
                       when (procedure--hole-values-agree-p hole values)
                         return index))

               (hole-parameter (index key values)
                 (or (rest (assoc index hole-parameters))
                     (let ((name (add-parameter key values)))
                       (push (cons index name) hole-parameters)
                       name)))

               (wrapped (values key)
                 (or (loop for parameter in parameters
                           for wrap = (and (eq (getf parameter :type) ':string)
                                           (procedure--common-wrap values
                                                                   (getf parameter :values)))
                           when wrap
                             return (list :text (first wrap) (getf parameter :name) (second wrap)))
                     (loop for hole in holes
                           for index from 0
                           for wrap = (procedure--common-wrap values hole)
                           when wrap
                             return (list :text (first wrap)
                                          (hole-parameter index key hole)
                                          (second wrap)))))

               (generalize-object (values step)
                 (let ((keys (procedure--object-keys (first values))))
                   (unless (every (lambda (value)
                                    (and (json-object-p value)
                                         (equal (sort (procedure--object-keys value) #'string<)
                                                (sort (copy-list keys) #'string<))))
                                  values)
                     (return-from procedure-mine nil))
                   (cons :object
                         (mapcar (lambda (name)
                                   (list name
                                         (generalize (mapcar (lambda (value) (gethash name value))
                                                             values)
                                                     name step)))
                                 keys))))

               (generalize (values key step)
                 (let ((first-value (first values)))
                   (cond
                     ((every (lambda (value) (procedure-json-equal-p value first-value)) values)
                      (list :json (json-encode first-value)))
                     ((every #'json-object-p values)
                      (generalize-object values step))
                     ((every (lambda (value) (and (vectorp value) (not (stringp value))))
                             values)
                      (unless (every (lambda (value) (= (length value) (length first-value)))
                                     values)
                        (return-from procedure-mine nil))
                      (cons :array
                            (loop for index below (length first-value)
                                  collect (generalize (mapcar (lambda (value) (aref value index))
                                                              values)
                                                      key step))))
                     ((procedure--values-type values)
                      (let ((from (procedure--output-reference values outputs step))
                            (parameter (parameter-with-values values))
                            (hole (hole-with-values values)))
                        (cond
                          (from from)
                          (parameter (list :param (getf parameter :name)))
                          (hole (list :param (hole-parameter hole key (procedure--hole-values
                                                                      (nth hole holes)
                                                                      values))))
                          ((and (every #'stringp values) (wrapped values key)))
                          (t (list :param (add-parameter key values))))))
                     (t
                      (list :param (add-parameter key values)))))))
        (let* ((steps
                 (loop for index from 0
                       for step-calls in (apply #'mapcar #'list
                                                (mapcar #'action-trace-calls traces))
                       collect (list :tool (getf (first step-calls) :tool)
                                     :arguments (generalize-object
                                                 (mapcar (lambda (call) (getf call :arguments))
                                                         step-calls)
                                                 index))))
               (procedure
                 (list :procedure
                       :id (procedure--name pattern (mapcar (lambda (step) (getf step :tool))
                                                            steps))
                       :parameters (mapcar (lambda (parameter)
                                             (let ((copy (copy-list parameter)))
                                               (remf copy :values)
                                               copy))
                                           parameters)
                       :input (and pattern
                                   (mapcar (lambda (element)
                                             (if (stringp element)
                                                 element
                                                 (destructuring-bind (index prefix suffix) element
                                                   (let ((name (rest (assoc index hole-parameters))))
                                                     (if name
                                                         (list :param name prefix suffix)
                                                         (list :any prefix suffix))))))
                                           pattern))
                       :steps steps)))
          (and (<= (length parameters) *procedure-parameter-limit*)
               (procedure-p procedure)
               procedure))))))

(-> procedure-explains-trace-p (list action-trace) boolean)
(defun procedure-explains-trace-p (procedure trace)
  "Return true when PROCEDURE reproduces TRACE's calls with one consistent set of parameters.

The request pattern, when there is one, must match TRACE's request and agree
with the arguments; every parameter must be bound and suit its type."
  (let* ((properties (rest procedure))
         (steps (getf properties :steps))
         (calls (action-trace-calls trace))
         (bindings nil)
         (outputs nil))
    (block explain
      (unless (equal (mapcar (lambda (step) (getf step :tool)) steps)
                     (action-trace-skeleton trace))
        (return-from explain nil))
      (when (getf properties :input)
        (multiple-value-bind (found matched-p)
            (procedure-match-input procedure (action-trace-input trace))
          (unless matched-p
            (return-from explain nil))
          (setf bindings found)))
      (loop for step in steps
            for call in calls
            do (multiple-value-bind (next matched-p)
                   (procedure-template-match (getf step :arguments) (getf call :arguments)
                                             bindings outputs)
                 (unless matched-p
                   (return-from explain nil))
                 (setf bindings next
                       outputs (append outputs
                                       (list (procedure--decode-output (getf call :output)))))))
      (and (every (lambda (parameter)
                    (let ((binding (assoc (getf parameter :name) bindings :test #'string=)))
                      (and binding (procedure-parameter-accepts-p parameter (rest binding)))))
                  (getf properties :parameters))
           t))))

(-> procedure-learn-candidates (list) list)
(defun procedure-learn-candidates (traces)
  "Return (PROCEDURE EXAMPLES EVALUATION) for each tool sequence enough of TRACES share.

TRACES are accepted traces, newest first. A sequence needs two or more calls,
none of them a procedure run, and at least *PROCEDURE-MINIMUM-EXAMPLES* turns.
A procedure is returned only when leaving out any one of its turns still
learns a procedure that explains that turn. When another of TRACES fits the
request pattern but did something else, other than running a procedure, the
pattern does not tell when the procedure applies, so it is dropped and the
procedure is never offered.
EVALUATION records the turns checked and those counterexamples."
  (let ((groups nil))
    (dolist (trace traces)
      (let ((skeleton (action-trace-skeleton trace)))
        (when (and (rest skeleton)
                   (notany (lambda (tool) (procedure--own-tool-p tool)) skeleton))
          (let ((group (assoc skeleton groups :test #'equal)))
            (if group
                (when (< (length (rest group)) *procedure-example-limit*)
                  (setf (rest group) (append (rest group) (list trace))))
                (push (list skeleton trace) groups))))))
    (loop for (nil . examples) in (reverse groups)
          for procedure = (and (>= (length examples) *procedure-minimum-examples*)
                               (procedure--validated examples))
          when procedure
            collect (let ((counterexamples
                            (count-if (lambda (trace)
                                        (and (not (member trace examples))
                                             (notany #'procedure--own-tool-p
                                                     (action-trace-skeleton trace))
                                             (nth-value 1 (procedure-match-input
                                                           procedure (action-trace-input trace)))
                                             (not (procedure-explains-trace-p procedure trace))))
                                      traces)))
                      (when (plusp counterexamples)
                        (setf procedure (copy-list procedure)
                              (getf (rest procedure) :input) nil))
                      (list procedure examples
                            (list :method ':leave-one-out
                                  :examples (length examples)
                                  :counterexamples counterexamples))))))

(-> procedure-learn (configuration) list)
(defun procedure-learn (configuration)
  "Learn procedures from CONFIGURATION's recent turns and record new ones or new versions.

A sequence of a retired procedure is not learned again, and a sequence
already learned keeps its name. Return (NAME VERSION EXAMPLE-COUNT) for each
recorded version. Nothing is learned when knowledge is off."
  (if (not (knowledge-capture-p configuration))
      nil
      (let* ((workspace (knowledge--workspace configuration))
             (entries (knowledge-base-procedures (knowledge-base-read configuration) workspace))
             (learned nil))
        (loop for (procedure examples evaluation) in (procedure-learn-candidates
                                                       (action-traces configuration))
              for skeleton = (mapcar (lambda (step) (getf step :tool))
                                     (getf (rest procedure) :steps))
              for entry = (find skeleton entries :key #'procedure--entry-skeleton :test #'equal)
              unless (and entry (getf entry :retired-p))
                do (let ((procedure (copy-list procedure)))
                     (setf (getf (rest procedure) :id)
                           (if entry
                               (getf entry :id)
                               (procedure--unique-name (getf (rest procedure) :id) entries)))
                     (multiple-value-bind (name version added-p)
                         (procedure-define
                          configuration procedure
                          :examples (mapcar (lambda (trace)
                                              (list :conversation (action-trace-conversation trace)
                                                    :sequence (action-trace-sequence trace)))
                                            examples)
                          :baseline (procedure--baseline examples)
                          :evaluation evaluation)
                       (when added-p
                         (push (list name version (length examples)) learned)))))
        (nreverse learned))))


;;;; -- Private Functions --

(-> procedure--validated (list) (option list))
(defun procedure--validated (examples)
  "Return the procedure EXAMPLES teach when each one is explained by the others' procedure."
  (let ((procedure (procedure-mine examples)))
    (and procedure
         (every (lambda (held-out)
                  (let ((others (procedure-mine (remove held-out examples))))
                    (and others (procedure-explains-trace-p others held-out))))
                examples)
         (every (lambda (example) (procedure-explains-trace-p procedure example)) examples)
         procedure)))

(-> procedure--own-tool-p (string) boolean)
(defun procedure--own-tool-p (tool)
  "Return true when TOOL runs procedures, so a sequence using it is never learned."
  (and (>= (length tool) 10) (string= "procedure." tool :end2 10)))

(-> procedure--entry-skeleton (list) list)
(defun procedure--entry-skeleton (entry)
  "Return the tool names of ENTRY's newest version in order."
  (mapcar (lambda (step) (getf step :tool))
          (getf (rest (getf (procedure-entry-version entry) :procedure)) :steps)))

(-> procedure--unique-name (string list) string)
(defun procedure--unique-name (name entries)
  "Return NAME, numbered when one of ENTRIES already uses it."
  (loop for index from 1
        for candidate = (if (= index 1) name (format nil "~A-~D" name index))
        unless (find candidate entries :key (lambda (entry) (getf entry :id)) :test #'string=)
          return candidate))

(-> procedure--baseline (list) list)
(defun procedure--baseline (examples)
  "Return the average model requests and tokens of the turns EXAMPLES."
  (let ((count (length examples)))
    (list :requests (round (reduce #'+ examples :key #'action-trace-requests) count)
          :tokens (round (reduce #'+ examples :key #'action-trace-tokens) count))))

(-> procedure--decode-output (t) t)
(defun procedure--decode-output (output)
  "Return tool OUTPUT decoded as JSON, or NIL when it is not a JSON object or array."
  (let ((value (and (stringp output)
                    (let ((start (position-if-not (lambda (character)
                                                    (find character '(#\Space #\Tab #\Newline
                                                                      #\Return)))
                                                  output)))
                      (and start (find (char output start) "{[")))
                    (ignore-errors (json-decode output)))))
    (and (or (json-object-p value) (and (vectorp value) (not (stringp value))))
         value)))

(-> procedure--output-leaves (t) list)
(defun procedure--output-leaves (value)
  "Return (PATH . LEAF) for the text and number fields of decoded JSON VALUE, outermost first."
  (let ((leaves nil)
        (count 0))
    (labels ((walk (value path depth)
               (cond
                 ((>= count *procedure-output-leaf-limit*))
                 ((or (stringp value) (realp value))
                  (incf count)
                  (push (cons (reverse path) value) leaves))
                 ((>= depth *procedure-output-depth-limit*))
                 ((json-object-p value)
                  (dolist (key (procedure--object-keys value))
                    (walk (gethash key value) (cons key path) (1+ depth))))
                 ((and (vectorp value) (not (stringp value)))
                  (loop for item across value
                        for index from 0
                        do (walk item (cons index path) (1+ depth)))))))
      (walk value nil 0))
    (nreverse leaves)))

(-> procedure--output-reference (list list integer) (option list))
(defun procedure--output-reference (values outputs step)
  "Return (:FROM STEP PATH) when every one of VALUES is the same field of an earlier result.

OUTPUTS holds each example's decoded results; only steps before STEP count."
  (loop for earlier below step
        do (loop for (path . leaf) in (procedure--output-leaves (nth earlier (first outputs)))
                 when (and (procedure-json-equal-p leaf (first values))
                           (every (lambda (value example-outputs)
                                    (multiple-value-bind (found present-p)
                                        (procedure-json-path-value (nth earlier example-outputs)
                                                                   path)
                                      (and present-p (procedure-json-equal-p found value))))
                                  values outputs))
                   do (return-from procedure--output-reference (list :from earlier path))))
  nil)

(-> procedure--object-keys (hash-table) list)
(defun procedure--object-keys (object)
  "Return OBJECT's keys in their stored order."
  (loop for key being the hash-keys of object collect key))

(-> procedure--values-type (list) (option procedure-parameter-type))
(defun procedure--values-type (values)
  "Return the parameter type all VALUES share, or NIL when they share none."
  (cond
    ((every #'non-empty-string-p values) ':string)
    ((every #'integerp values) ':integer)
    ((every #'realp values) ':number)
    ((every (lambda (value) (or (eq value t) (eq value :json-false))) values) ':boolean)
    (t nil)))

(-> procedure--common-extension (list) (option string))
(defun procedure--common-extension (values)
  "Return the file extension, such as \".csv\", every string in VALUES ends with, or NIL."
  (let* ((first-value (first values))
         (dot (position #\. first-value :from-end t)))
    (and dot
         (< 1 (- (length first-value) dot) 10)
         (every #'alphanumericp (subseq first-value (1+ dot)))
         (let ((extension (subseq first-value dot)))
           (and (every (lambda (value)
                         (and (> (length value) (length extension))
                              (string= extension value
                                       :start2 (- (length value) (length extension)))))
                       values)
                extension)))))

(-> procedure--common-wrap (list list) (option list))
(defun procedure--common-wrap (values parts)
  "Return (PREFIX SUFFIX) when each of VALUES is PREFIX, its PARTS element, and SUFFIX.

PARTS are strings; PREFIX and SUFFIX are the same for every value and not both empty."
  (let ((prefix nil)
        (suffix nil))
    (loop for value in values
          for part in parts
          for position = (and (stringp part) (plusp (length part)) (search part value))
          do (unless position
               (return-from procedure--common-wrap nil))
             (let ((before (subseq value 0 position))
                   (after (subseq value (+ position (length part)))))
               (if prefix
                   (unless (and (string= before prefix) (string= after suffix))
                     (return-from procedure--common-wrap nil))
                   (setf prefix before
                         suffix after))))
    (and (plusp (+ (length prefix) (length suffix)))
         (list prefix suffix))))

(-> procedure--input-pattern (list) (values list list))
(defun procedure--input-pattern (inputs)
  "Return the request pattern INPUTS share and the varying words' values.

The pattern lists literal words and (INDEX PREFIX SUFFIX) holes; the second
value lists each hole's middles across INPUTS. Requests with different word
counts share no pattern."
  (let ((word-lists (mapcar #'procedure-words inputs)))
    (if (or (null (first word-lists))
            (notevery (lambda (words) (= (length words) (length (first word-lists))))
                      word-lists))
        (values nil nil)
        (let ((pattern nil)
              (holes nil))
          (loop for column in (apply #'mapcar #'list word-lists)
                do (if (every (lambda (word) (string= word (first column))) column)
                       (push (first column) pattern)
                       (multiple-value-bind (prefix suffix) (procedure--punctuation-ends column)
                         (push (list (length holes) prefix suffix) pattern)
                         (setf holes
                               (append holes
                                       (list (mapcar (lambda (word)
                                                       (subseq word (length prefix)
                                                               (- (length word) (length suffix))))
                                                     column)))))))
          (values (nreverse pattern) holes)))))

(-> procedure--punctuation-ends (list) (values string string))
(defun procedure--punctuation-ends (words)
  "Return shared punctuation around WORDS, preserving numeric signs in their values."
  (flet ((common (reader)
           (let ((shortest (reduce #'min words :key #'length)))
             (coerce (loop for offset below shortest
                           for character = (funcall reader (first words) offset)
                           while (and (not (alphanumericp character))
                                      (not (find character "+-"))
                                      (every (lambda (word)
                                               (char= (funcall reader word offset) character))
                                             words))
                           collect character)
                     'string))))
    (let* ((prefix (common (lambda (word offset) (char word offset))))
           (suffix (reverse (common (lambda (word offset)
                                      (char word (- (length word) offset 1)))))))
      (if (every (lambda (word) (> (length word) (+ (length prefix) (length suffix)))) words)
          (values prefix suffix)
          (values "" "")))))

(-> procedure--hole-values-agree-p (list list) boolean)
(defun procedure--hole-values-agree-p (hole values)
  "Return true when request words HOLE decode as the JSON-typed argument VALUES."
  (let ((type (procedure--values-type values)))
    (and type
         (every (lambda (word value)
                  (procedure-json-equal-p (procedure--parse-value word type) value))
                hole values)
         t)))

(-> procedure--hole-values (list list) list)
(defun procedure--hole-values (hole values)
  "Return the parameter values request words HOLE stand for, typed like argument VALUES."
  (if (eq (procedure--values-type values) ':string) hole values))

(-> procedure--parameter-base-name (string) string)
(defun procedure--parameter-base-name (key)
  "Return a lowercase hyphenated parameter name made from argument KEY."
  (let* ((words (with-output-to-string (stream)
                  (loop for character across key
                        for previous = nil then current
                        for current = character
                        do (cond
                             ((and (upper-case-p character) previous (lower-case-p previous))
                              (write-char #\- stream)
                              (write-char (char-downcase character) stream))
                             ((alphanumericp character)
                              (write-char (char-downcase character) stream))
                             (t
                              (write-char #\- stream))))))
         (trimmed (string-trim "-" (procedure--collapse-hyphens words))))
    (if (and (plusp (length trimmed)) (alpha-char-p (char trimmed 0)))
        trimmed
        (format nil "value~@[-~A~]" (and (plusp (length trimmed)) trimmed)))))

(-> procedure--collapse-hyphens (string) string)
(defun procedure--collapse-hyphens (text)
  "Return TEXT with each run of hyphens reduced to one."
  (with-output-to-string (stream)
    (loop for character across text
          for previous = nil then current
          for current = character
          unless (and (char= character #\-) (eql previous #\-))
            do (write-char character stream))))

(-> procedure--name (list list) string)
(defun procedure--name (pattern tools)
  "Return a procedure name from request PATTERN's leading words, or from TOOLS without one."
  (let* ((words (loop for element in pattern
                      while (stringp element)
                      for word = (string-downcase (remove-if-not #'alphanumericp element))
                      when (and (plusp (length word))
                                (not (member word *procedure-name-stopwords* :test #'string=)))
                        collect word))
         (words (or (subseq words 0 (min 4 (length words)))
                    (mapcar (lambda (tool)
                              (let ((dot (position #\. tool :from-end t)))
                                (string-downcase (remove-if-not #'alphanumericp
                                                                (if dot (subseq tool (1+ dot)) tool)))))
                            (subseq tools 0 (min 3 (length tools))))))
         (name (procedure--parameter-base-name (format nil "~{~A~^-~}" words))))
    (subseq name 0 (min 60 (length name)))))
