(in-package #:autolith)

;;;; -- Timestamp Parsing and Formatting --

(-> timestamp--digits (string integer integer) (option integer))
(defun timestamp--digits (text start end)
  "Return the decimal field of TEXT from START below END, or NIL."
  (and (<= 0 start end (length text))
       (< start end)
       (loop for index from start below end
             always (digit-char-p (char text index)))
       (parse-integer text :start start :end end)))

(-> timestamp--month-days (integer integer) integer)
(defun timestamp--month-days (year month)
  "Return the Gregorian number of days in MONTH of YEAR."
  (case month
    ((4 6 9 11)
     30)
    (2
     (if (and (zerop (mod year 4))
              (or (plusp (mod year 100)) (zerop (mod year 400))))
         29
         28))
    (otherwise
     31)))

(-> timestamp--date-parts (string) (values (option integer) (option integer) (option integer)))
(defun timestamp--date-parts (text)
  "Return the Gregorian year, month and day of leading YYYY-MM-DD, or NIL values.

Year and instant range policies belong to callers, including date-only terms."
  (let ((year  (timestamp--digits text 0 4))
        (month (timestamp--digits text 5 7))
        (day   (timestamp--digits text 8 10)))
    (if (and year month day
             (char= (char text 4) #\-)
             (char= (char text 7) #\-)
             (<= 1 month 12)
             (<= 1 day (timestamp--month-days year month)))
        (values year month day)
        (values nil nil nil))))

(-> timestamp--offset-seconds
    (string &key (:start integer) (:profile (member :external :knowledge)))
    (option integer))
(defun timestamp--offset-seconds (text &key start profile)
  "Return TEXT's zone at START in seconds east of UTC, or NIL for PROFILE."
  (let ((length (length text)))
    (when (< start length)
      (let ((designator (char text start)))
        (cond
          ((and (find designator "Zz") (= (1+ start) length))
           0)
          ((find designator "+-")
           (let* ((size    (- length start))
                  (colon-p (= size 6))
                  (hours   (timestamp--digits text (1+ start) (+ start 3)))
                  (minutes (timestamp--digits text (+ start (if colon-p 4 3))
                                                   (+ start (if colon-p 6 5)))))
             (when (and (or colon-p (and (eq profile ':knowledge) (= size 5)))
                        (or (not colon-p) (char= (char text (+ start 3)) #\:))
                        hours minutes
                        (<= hours (if (eq profile ':knowledge) 14 23))
                        (<= minutes 59))
               (* (if (char= designator #\-) -1 1)
                  (+ (* hours 3600) (* minutes 60))))))
          (t
           nil))))))

(-> parse-utc-timestamp (string &key (:profile (member :external :knowledge))) (option timestamp))
(defun parse-utc-timestamp (text &key (profile ':external))
  "Return TEXT as universal time, or NIL for malformed or unsupported instants.

Both profiles require a zone and truncate nonempty fractional seconds. The
external profile requires seconds and colon-separated offsets through 23:59;
knowledge permits omitted seconds and basic offsets, bounded by 14:59.
Years 0001 through 9999 must resolve to nonnegative universal time."
  (block nil
    (multiple-value-bind (year month day) (timestamp--date-parts text)
      (unless (and year (plusp year) (> (length text) 16)
                   (find (char text 10) "Tt ")
                   (char= (char text 13) #\:))
        (return nil))
      (let ((hour     (timestamp--digits text 11 13))
            (minute   (timestamp--digits text 14 16))
            (second   0)
            (position 16))
        (unless (and hour minute (<= hour 23) (<= minute 59))
          (return nil))
        (cond
          ((char= (char text position) #\:)
           (setf second (timestamp--digits text (1+ position) (+ position 3)))
           (unless (and second (<= second 59))
             (return nil))
           (incf position 3)
           (when (and (< position (length text)) (char= (char text position) #\.))
             (incf position)
             (let ((start position))
               (loop while (and (< position (length text))
                                (digit-char-p (char text position)))
                     do (incf position))
               (when (= start position)
                 (return nil)))))
          ((eq profile ':external)
           (return nil)))
        (let ((offset (timestamp--offset-seconds text :start position :profile profile)))
          (when offset
            ;; Gregorian cycles preserve leap days, avoid two-digit year inference,
            ;; and allow local dates before 1900 whose offset reaches the epoch.
            (let* ((cycles (max 0 (ceiling (- 1900 year) 400)))
                   (local  (- (encode-universal-time second minute hour day month
                                                     (+ year (* cycles 400)) 0)
                              (* cycles 146097 86400)))
                   (utc    (- local offset)))
              (when (typep utc 'timestamp)
                utc))))))))

(-> rfc3339->universal-time (string) (option timestamp))
(defun rfc3339->universal-time (text)
  "Return external RFC 3339 TEXT as universal time, or NIL when invalid.

Fractional seconds are truncated. Unsupported UTC instants return NIL."
  (parse-utc-timestamp text))

(-> format-utc-timestamp (integer) string)
(defun format-utc-timestamp (universal-time)
  "Return UNIVERSAL-TIME as second-precision ISO-8601 UTC text."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour minute second)))
