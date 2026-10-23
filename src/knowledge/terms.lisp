(in-package #:autolith)

;;;; -- Knowledge Terms --

;;; Arguments of structured claims are portable terms: strings, integers and
;;; rationals, :TRUE and :FALSE, and tagged forms for instants, dates,
;;; durations, and quantities. Every term has one canonical form, so equal
;;; values print identically and claim identities stay stable.

(defparameter *knowledge-units*
  '(("m" :length 1) ("km" :length 1000) ("cm" :length 1/100) ("mm" :length 1/1000)
    ("mi" :length 201168/125) ("ft" :length 381/1250)
    ("g" :mass 1) ("kg" :mass 1000) ("mg" :mass 1/1000) ("lb" :mass 45359237/100000)
    ("s" :time 1) ("min" :time 60) ("h" :time 3600) ("d" :time 86400)
    ("B" :data 1) ("KB" :data 1000) ("MB" :data 1000000) ("GB" :data 1000000000)
    ("KiB" :data 1024) ("MiB" :data 1048576) ("GiB" :data 1073741824)
    ("%" :ratio 1/100))
  "Known units as (NAME DIMENSION FACTOR), FACTOR converting to the base unit.

Three uppercase letters name a currency, a dimension of its own that converts
only to itself.")

(defparameter *knowledge-string-limit* 2000
  "The maximum characters in one string term.")

(deftype knowledge-term-type ()
  "The value types a schema argument may require."
  '(member :entity :string :number :boolean :instant :date :duration :quantity))


;;;; -- Errors --

(-> knowledge--fail (keyword t string &rest t) nil)
(defun knowledge--fail (code subject control &rest arguments)
  "Signal KNOWLEDGE-ERROR with CODE about SUBJECT and a formatted message."
  (error 'knowledge-error
         :code    code
         :subject subject
         :message (apply #'format nil control arguments)))


;;;; -- Units --

(-> knowledge-unit-dimension (t) (option t))
(defun knowledge-unit-dimension (unit)
  "Return UNIT's dimension, or NIL when UNIT is not a known unit."
  (cond
    ((not (stringp unit))
     nil)
    ((assoc unit *knowledge-units* :test #'string=)
     (second (assoc unit *knowledge-units* :test #'string=)))
    ((and (= (length unit) 3) (every #'upper-case-p unit))
     (list :currency unit))
    (t
     nil)))

(-> knowledge-unit-factor (string) rational)
(defun knowledge-unit-factor (unit)
  "Return the factor converting UNIT to its dimension's base unit."
  (let ((entry (assoc unit *knowledge-units* :test #'string=)))
    (if entry (third entry) 1)))


;;;; -- Time Parsing --

(-> knowledge--digits (string integer integer) (option integer))
(defun knowledge--digits (text start end)
  "Return the decimal integer in TEXT from START below END, or NIL."
  (and (<= end (length text))
       (< start end)
       (loop for index from start below end
             always (digit-char-p (char text index)))
       (parse-integer text :start start :end end)))

(-> knowledge--date-parts (string) (values (option integer) (option integer) (option integer)))
(defun knowledge--date-parts (text)
  "Return the year, month, and day of TEXT's leading YYYY-MM-DD, or NIL."
  (let ((year (knowledge--digits text 0 4))
        (month (knowledge--digits text 5 7))
        (day (knowledge--digits text 8 10)))
    (if (and year month day
             (char= (char text 4) #\-)
             (char= (char text 7) #\-)
             (<= 1 month 12)
             (<= 1 day (knowledge--month-days year month)))
        (values year month day)
        (values nil nil nil))))

(-> knowledge--month-days (integer integer) integer)
(defun knowledge--month-days (year month)
  "Return the number of days in MONTH of YEAR."
  (case month
    ((4 6 9 11) 30)
    (2 (if (and (zerop (mod year 4))
                (or (plusp (mod year 100)) (zerop (mod year 400))))
           29
           28))
    (otherwise 31)))

(-> knowledge-parse-instant (string) (option integer))
(defun knowledge-parse-instant (text)
  "Return the universal time of ISO-8601 TEXT with an explicit zone, or NIL.

Accept YYYY-MM-DDTHH:MM, optional :SS and fractional seconds, and a zone of Z
or an offset such as +02:00 or -0530. Fractional seconds are truncated."
  (multiple-value-bind (year month day) (knowledge--date-parts text)
    (block nil
      (unless (and year
                   (> (length text) 16)
                   (member (char text 10) '(#\T #\t #\Space))
                   (char= (char text 13) #\:))
        (return nil))
      (let ((hour (knowledge--digits text 11 13))
            (minute (knowledge--digits text 14 16))
            (second 0)
            (position 16))
        (unless (and hour minute (<= hour 23) (<= minute 59))
          (return nil))
        (when (and (< position (length text)) (char= (char text position) #\:))
          (setf second (knowledge--digits text (1+ position) (+ position 3)))
          (unless (and second (<= second 59))
            (return nil))
          (incf position 3)
          (when (and (< position (length text)) (char= (char text position) #\.))
            (incf position)
            (loop while (and (< position (length text))
                             (digit-char-p (char text position)))
                  do (incf position))))
        (let ((zone (subseq text position))
              (offset nil))
          (cond
            ((member zone '("Z" "z") :test #'string=)
             (setf offset 0))
            ((and (member (length zone) '(5 6))
                  (find (char zone 0) "+-"))
             (let* ((colon-p (= (length zone) 6))
                    (zone-hours (knowledge--digits zone 1 3))
                    (zone-minutes (knowledge--digits zone
                                                     (if colon-p 4 3)
                                                     (if colon-p 6 5))))
               (when (and zone-hours zone-minutes
                          (or (not colon-p) (char= (char zone 3) #\:))
                          (<= zone-hours 14) (<= zone-minutes 59))
                 (setf offset (* (if (char= (char zone 0) #\-) -1 1)
                                 (+ (* 3600 zone-hours) (* 60 zone-minutes))))))))
          (when offset
            (- (encode-universal-time second minute hour day month year 0)
               offset)))))))

(-> knowledge-format-instant (integer) string)
(defun knowledge-format-instant (universal-time)
  "Return UNIVERSAL-TIME as canonical ISO-8601 UTC text."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour minute second)))

(-> knowledge-parse-date (string) (option string))
(defun knowledge-parse-date (text)
  "Return canonical YYYY-MM-DD when TEXT is exactly a valid date, or NIL."
  (and (= (length text) 10)
       (knowledge--date-parts text)
       text))

(-> knowledge-date-universal-time (string) integer)
(defun knowledge-date-universal-time (date)
  "Return the universal time at the start of canonical DATE in UTC."
  (multiple-value-bind (year month day) (knowledge--date-parts date)
    (encode-universal-time 0 0 0 day month year 0)))

(-> knowledge-parse-duration (t) (option integer))
(defun knowledge-parse-duration (value)
  "Return the seconds of VALUE, an integer or ISO-8601 PnWnDTnHnMnS text, or NIL.

Years and months have no fixed length and are rejected."
  (cond
    ((integerp value)
     value)
    ((not (stringp value))
     nil)
    (t
     (let* ((negative-p (and (plusp (length value)) (char= (char value 0) #\-)))
            (text (string-upcase (if negative-p (subseq value 1) value)))
            (total 0)
            (number nil)
            (time-p nil)
            (seen-p nil))
       (block nil
         (unless (and (> (length text) 1) (char= (char text 0) #\P))
           (return nil))
         (loop for index from 1 below (length text)
               for character = (char text index)
               do (cond
                    ((digit-char-p character)
                     (setf number (+ (* 10 (or number 0)) (digit-char-p character))))
                    ((char= character #\T)
                     (when (or time-p number)
                       (return nil))
                     (setf time-p t))
                    (t
                     (unless number
                       (return nil))
                     (let ((factor
                             (cond
                               ((and (not time-p) (char= character #\W)) 604800)
                               ((and (not time-p) (char= character #\D)) 86400)
                               ((and time-p (char= character #\H)) 3600)
                               ((and time-p (char= character #\M)) 60)
                               ((and time-p (char= character #\S)) 1))))
                       (unless factor
                         (return nil))
                       (incf total (* number factor))
                       (setf number nil
                             seen-p t)))))
         (when (or number (not seen-p))
           (return nil))
         (if negative-p (- total) total))))))

(-> knowledge-format-duration (integer) string)
(defun knowledge-format-duration (seconds)
  "Return SECONDS as compact text such as 1d 2h 30m."
  (if (zerop seconds)
      "0s"
      (let ((parts nil)
            (remaining (abs seconds)))
        (loop for (unit size) in '(("d" 86400) ("h" 3600) ("m" 60) ("s" 1))
              do (multiple-value-bind (count rest) (floor remaining size)
                   (when (plusp count)
                     (push (format nil "~D~A" count unit) parts))
                   (setf remaining rest)))
        (format nil "~:[~;-~]~{~A~^ ~}" (minusp seconds) (nreverse parts)))))


;;;; -- Terms --

(-> knowledge-term-type-of (t) (option keyword))
(defun knowledge-term-type-of (term)
  "Return the value type of canonical TERM, or NIL when TERM is not a term.

Strings report :STRING; whether a string is an entity is the schema's choice."
  (cond
    ((stringp term)
     (and (<= (length term) *knowledge-string-limit*) ':string))
    ((rationalp term)
     ':number)
    ((member term '(:true :false))
     ':boolean)
    ((not (proper-list-p term))
     nil)
    ((and (eq (first term) :instant) (= (length term) 2)
          (stringp (second term))
          (let ((time (knowledge-parse-instant (second term))))
            (and time (string= (knowledge-format-instant time) (second term)))))
     ':instant)
    ((and (eq (first term) :date) (= (length term) 2)
          (stringp (second term))
          (knowledge-parse-date (second term)))
     ':date)
    ((and (eq (first term) :duration) (= (length term) 2)
          (integerp (second term)))
     ':duration)
    ((and (eq (first term) :quantity) (= (length term) 3)
          (rationalp (second term))
          (knowledge-unit-dimension (third term)))
     ':quantity)
    (t
     nil)))

(-> knowledge-term-p (t) boolean)
(defun knowledge-term-p (value)
  "Return true when VALUE is one canonical knowledge term."
  (and (knowledge-term-type-of value) t))

(-> knowledge-term-matches-type-p (t keyword) boolean)
(defun knowledge-term-matches-type-p (term type)
  "Return true when canonical TERM is a value of schema TYPE."
  (let ((actual (knowledge-term-type-of term)))
    (and actual
         (if (eq type ':entity)
             (and (eq actual ':string) (non-empty-string-p term))
             (eq actual type))
         t)))

(-> knowledge-instant-time (list) integer)
(defun knowledge-instant-time (term)
  "Return the universal time of instant TERM."
  (knowledge-parse-instant (second term)))

(-> knowledge-make-instant (integer) list)
(defun knowledge-make-instant (universal-time)
  "Return the canonical instant term for UNIVERSAL-TIME."
  (list :instant (knowledge-format-instant universal-time)))

(-> knowledge--number (t) (option rational))
(defun knowledge--number (value)
  "Return VALUE as an exact rational, or NIL when it is not a real number."
  (typecase value
    (rational value)
    (float (rationalize value))
    (otherwise nil)))

(-> knowledge-term-from-input (t keyword) t)
(defun knowledge-term-from-input (value type)
  "Return the canonical term of TYPE for VALUE as written by a model or person.

Strings are parsed according to TYPE, so an instant may be given as ISO text
and a duration as PT30M or seconds. Quantities are written as a list or object
of magnitude and unit. Signal KNOWLEDGE-ERROR with a corrective message when
VALUE cannot be a TYPE."
  (flet ((fail (expected)
           (knowledge--fail ':invalid-term value
                            "Expected ~A, not ~S." expected value)))
    (ecase type
      ((:entity :string)
       (unless (and (stringp value)
                    (<= (length value) *knowledge-string-limit*)
                    (or (eq type ':string) (non-empty-string-p value)))
         (fail (if (eq type ':entity) "an entity identifier string" "a string")))
       value)
      (:number
       (or (knowledge--number value)
           (and (stringp value)
                (let ((parsed (ignore-errors
                               (let ((*read-eval* nil))
                                 (read-from-string value)))))
                  (knowledge--number parsed)))
           (fail "a number")))
      (:boolean
       (cond
         ((member value '(t :true)) ':true)
         ((or (member value '(nil :false)) (json-false-p value)) ':false)
         ((and (stringp value) (string-equal value "true")) ':true)
         ((and (stringp value) (string-equal value "false")) ':false)
         (t (fail "true or false"))))
      (:instant
       (cond
         ((eq (knowledge-term-type-of value) ':instant)
          value)
         ((and (stringp value) (knowledge-parse-instant value))
          (knowledge-make-instant (knowledge-parse-instant value)))
         (t
          (fail "an ISO-8601 instant with a zone, such as 2026-10-22T13:00:00+02:00"))))
      (:date
       (cond
         ((eq (knowledge-term-type-of value) ':date)
          value)
         ((and (stringp value) (knowledge-parse-date value))
          (list :date value))
         (t
          (fail "a date such as 2026-10-22"))))
      (:duration
       (cond
         ((eq (knowledge-term-type-of value) ':duration)
          value)
         ((knowledge-parse-duration value)
          (list :duration (knowledge-parse-duration value)))
         (t
          (fail "a duration in seconds or ISO-8601 text such as PT30M"))))
      (:quantity
       (let* ((magnitude
                (cond
                  ((eq (knowledge-term-type-of value) ':quantity) (second value))
                  ((hash-table-p value) (knowledge--number (gethash "magnitude" value)))
                  ((and (proper-list-p value) (= (length value) 2))
                   (knowledge--number (first value)))))
              (unit
                (cond
                  ((eq (knowledge-term-type-of value) ':quantity) (third value))
                  ((hash-table-p value) (gethash "unit" value))
                  ((and (proper-list-p value) (= (length value) 2))
                   (second value)))))
         (unless (and magnitude (knowledge-unit-dimension unit))
           (fail "a quantity such as {\"magnitude\": 30, \"unit\": \"km\"} with a known unit"))
         (list :quantity magnitude unit))))))


;;;; -- Comparison --

(-> knowledge--quantity-base (list) rational)
(defun knowledge--quantity-base (term)
  "Return quantity TERM's magnitude in its dimension's base unit."
  (* (second term) (knowledge-unit-factor (third term))))

(-> knowledge-term-comparable-key (t) (values t t))
(defun knowledge-term-comparable-key (term)
  "Return TERM's ordering class and a real number or string ordering it.

Terms compare only within one class: numbers, one quantity dimension,
instants, dates, durations, or strings."
  (ecase (knowledge-term-type-of term)
    (:number (values ':number term))
    (:string (values ':string term))
    (:boolean (values ':boolean (if (eq term :true) 1 0)))
    (:instant (values ':instant (knowledge-instant-time term)))
    (:date (values ':date (knowledge-date-universal-time (second term))))
    (:duration (values ':duration (second term)))
    (:quantity (values (list :quantity (knowledge-unit-dimension (third term)))
                       (knowledge--quantity-base term)))))

(-> knowledge-term-compare (t t) (integer -1 1))
(defun knowledge-term-compare (left right)
  "Return -1, 0, or 1 ordering LEFT against RIGHT.

Signal KNOWLEDGE-ERROR when the terms belong to different ordering classes."
  (multiple-value-bind (left-class left-key) (knowledge-term-comparable-key left)
    (multiple-value-bind (right-class right-key) (knowledge-term-comparable-key right)
      (unless (equal left-class right-class)
        (knowledge--fail ':incomparable (list left right)
                         "Cannot compare ~A with ~A."
                         (knowledge-term-display left)
                         (knowledge-term-display right)))
      (cond
        ((stringp left-key)
         (cond ((string< left-key right-key) -1)
               ((string= left-key right-key) 0)
               (t 1)))
        ((< left-key right-key) -1)
        ((= left-key right-key) 0)
        (t 1)))))

(-> knowledge-term= (t t) boolean)
(defun knowledge-term= (left right)
  "Return true when LEFT and RIGHT denote the same value.

Quantities in different units of one dimension are equal when their base
magnitudes are; terms of different classes are never equal."
  (and (handler-case (zerop (knowledge-term-compare left right))
         (knowledge-error () nil))
       t))


;;;; -- Presentation --

(-> knowledge--format-number (rational) string)
(defun knowledge--format-number (number)
  "Return NUMBER as an integer or a decimal rounded to six places."
  (if (integerp number)
      (format nil "~D" number)
      (let ((text (format nil "~,6F" (float number 1d0))))
        (string-right-trim "." (string-right-trim "0" text)))))

(-> knowledge-term-display (t) string)
(defun knowledge-term-display (term)
  "Return TERM as short text for people."
  (case (knowledge-term-type-of term)
    (:string term)
    (:number (knowledge--format-number term))
    (:boolean (if (eq term :true) "true" "false"))
    (:instant (let ((text (second term)))
                (if (string= (subseq text 16 19) ":00")
                    (format nil "~A ~A UTC" (subseq text 0 10) (subseq text 11 16))
                    (format nil "~A ~A UTC" (subseq text 0 10) (subseq text 11 19)))))
    (:date (second term))
    (:duration (knowledge-format-duration (second term)))
    (:quantity (format nil "~A ~A" (knowledge--format-number (second term)) (third term)))
    (otherwise (format nil "~S" term))))

(-> knowledge-term->json (t) t)
(defun knowledge-term->json (term)
  "Return TERM as the JSON value a model reads and can write back."
  (case (knowledge-term-type-of term)
    (:string term)
    (:number (knowledge--number-json term))
    (:boolean (if (eq term :true) t (json-false)))
    ((:instant :date) (second term))
    (:duration (format nil "PT~DS" (second term)))
    (:quantity (json-object "magnitude" (knowledge--number-json (second term))
                            "unit" (third term)))
    (otherwise (format nil "~S" term))))

(-> knowledge--number-json (rational) real)
(defun knowledge--number-json (number)
  "Return NUMBER as an integer or double float for JSON."
  (if (integerp number) number (float number 1d0)))
