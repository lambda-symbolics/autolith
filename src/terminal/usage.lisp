(in-package #:autolith)

;;;; -- Usage Report Formatting --

(-> usage--text (t) string)
(defun usage--text (value)
  "Return VALUE as sanitized single-line display text."
  (sanitize-text (princ-to-string (or value "")) :single-line-p t))

(-> usage--count (t) string)
(defun usage--count (value)
  "Format VALUE as a count, preserving unavailable values."
  (if (integerp value) (format nil "~:D" value) "unavailable"))

(-> usage--bytes (t) string)
(defun usage--bytes (value)
  "Format VALUE bytes for a human reader, preserving unavailable values."
  (if (not (integerp value))
      "unavailable"
      (let ((units  '("B" "KiB" "MiB" "GiB" "TiB"))
            (number (float value))
            (index  0))
        (loop while (and (>= number 1024.0) (< index (1- (length units))))
              do (incf index) (setf number (/ number 1024.0)))
        (if (zerop index)
            (format nil "~:D B" value)
            (format nil "~,1F ~A (~:D B)" number (nth index units) value)))))

(-> usage--wrap (string integer) list)
(defun usage--wrap (text columns)
  "Wrap TEXT at spaces or the display width, omitting spaces at line breaks."
  (let ((lines  nil)
        (start  0)
        (length (length text)))
    (loop while (< start length)
          do (let ((end   start)
                   (space nil))
               (loop while (and (< end length)
                                (<= (text-cell-width (subseq text start (1+ end))) columns))
                     do (when (char= (char text end) #\Space)
                          (setf space end))
                        (incf end))
               (when (= end start)
                 (incf end))
               (when (and (< end length) space (> space start))
                 (setf end space))
               (push (subseq text start end) lines)
               (setf start end)
               (loop while (and (< start length) (char= (char text start) #\Space))
                     do (incf start))))
    (or (nreverse lines) (list ""))))

(-> usage--line (stream list boolean) t)
(defun usage--line (stream spans styled-p)
  "Write SPANS as one optionally styled line."
  (localgroup--write-styled-line stream spans styled-p))

(-> usage--paragraph (stream string &key (:columns integer) (:styled-p boolean) (:style t)) null)
(defun usage--paragraph (stream text &key columns styled-p (style ':plain))
  "Write sanitized TEXT wrapped to COLUMNS."
  (dolist (line (usage--wrap (usage--text text) columns))
    (usage--line stream (list (terminal-span style line)) styled-p))
  nil)

(-> usage--field (stream string string &key (:columns integer) (:styled-p boolean) (:style t)) null)
(defun usage--field (stream label value &key columns styled-p (style ':plain))
  "Write a compact label and value, wrapping without truncation."
  (let* ((label (concatenate 'string (usage--text label) ": "))
         (value (usage--text value)))
    (if (<= (text-cell-width (concatenate 'string label value)) columns)
        (usage--line stream (list (terminal-span ':dim label)
                                 (terminal-span style value)) styled-p)
        (progn
          (usage--paragraph stream label :columns columns :styled-p styled-p :style ':dim)
          (usage--paragraph stream value :columns columns :styled-p styled-p :style style))))
  nil)

(-> usage--fields (stream list &key (:columns integer) (:styled-p boolean)) null)
(defun usage--fields (stream fields &key columns styled-p)
  "Pack label/value FIELDS into up to three naturally sized columns."
  (when fields
    (let* ((fields      (loop for (label value style) in fields
                              collect (list (usage--text label) (usage--text value) style)))
           (label-width (loop for field in fields maximize (text-cell-width (first field))))
           (value-width (loop for field in fields maximize (text-cell-width (second field))))
           (cell-width  (+ label-width 2 value-width))
           (count       (max 1 (min 3 (floor (+ columns 3) (+ cell-width 3))))))
      (if (> cell-width columns)
          (dolist (field fields)
            (usage--field stream (first field) (second field)
                          :columns columns :styled-p styled-p :style (or (third field) ':plain)))
          (loop for remaining on fields by (lambda (tail) (nthcdr count tail))
                do (let ((spans nil))
                     (loop for tail on remaining
                           for field = (first tail)
                           for index below count
                           do (when (plusp index)
                                (push (terminal-span ':plain "   ") spans))
                              (push (terminal-span ':dim
                                                   (concatenate 'string
                                                                (layout-fit-text (first field) label-width)
                                                                "  ")) spans)
                              (push (terminal-span (or (third field) ':strong)
                                                   (if (and (< index (1- count)) (rest tail))
                                                       (layout-fit-text (second field) value-width)
                                                       (second field))) spans))
                     (usage--line stream (nreverse spans) styled-p))))))
  nil)

(-> usage--bar (integer integer) string)
(defun usage--bar (bytes largest)
  "Show BYTES relative to the largest root, with an empty bar for measured zero."
  (let ((filled (if (plusp largest)
                    (min 12 (round (* 12 bytes) largest))
                    0)))
    (format nil "[~A~A]" (make-string filled :initial-element #\#)
            (make-string (- 12 filled) :initial-element #\.))))

(-> usage--storage-table (stream list &key (:columns integer) (:styled-p boolean)) boolean)
(defun usage--storage-table (stream storage &key columns styled-p)
  "Print naturally sized root columns when they fit; return whether printed."
  (let* ((largest (reduce #'max storage :key #'usage-storage-bytes :initial-value 0))
         (rows    (cons '("Root" "Size" "Files" "Links" "Relative size")
                        (loop for root in storage
                              collect (list (usage--text (usage-storage-name root))
                                            (usage--bytes (usage-storage-bytes root))
                                            (usage--count (usage-storage-files root))
                                            (usage--count (usage-storage-links root))
                                            (usage--bar (usage-storage-bytes root) largest)))))
         (widths  (loop for index below 5
                        collect (loop for row in rows maximize (text-cell-width (nth index row))))))
    (when (<= (+ (reduce #'+ widths) 12) columns)
      (loop for row in rows
            for header-p = t then nil
            do (let ((spans nil))
                 (loop for cell in row
                       for width in widths
                       for index from 0
                       do (when (plusp index)
                            (push (terminal-span ':plain "   ") spans))
                          (push (terminal-span (cond
                                                 (header-p
                                                  ':dim)
                                                 ((= index 4)
                                                  ':brand)
                                                 (t
                                                  ':plain))
                                               (layout-fit-text cell width
                                                                :alignment (if (<= 1 index 3)
                                                                               ':right ':left))) spans))
                 (usage--line stream (nreverse spans) styled-p)))
      t)))

(-> usage--storage (stream list &key (:columns integer) (:styled-p boolean)) null)
(defun usage--storage (stream storage &key columns styled-p)
  "Print disk roots as a table or stacked metrics, followed by their full paths."
  (unless (usage--storage-table stream storage :columns columns :styled-p styled-p)
    (let ((largest (reduce #'max storage :key #'usage-storage-bytes :initial-value 0)))
      (dolist (root storage)
        (usage--paragraph stream (usage-storage-name root)
                          :columns columns :styled-p styled-p :style ':strong)
        (usage--fields stream (list (list "Size" (usage--bytes (usage-storage-bytes root)))
                                   (list "Files" (usage--count (usage-storage-files root)))
                                   (list "Links" (usage--count (usage-storage-links root))))
                       :columns columns :styled-p styled-p)
        (when (>= columns 14)
          (usage--paragraph stream (usage--bar (usage-storage-bytes root) largest)
                            :columns columns :styled-p styled-p :style ':brand)))))
  (usage--paragraph stream "Bars compare roots with the largest root, not disk capacity."
                    :columns columns :styled-p styled-p :style ':dim)
  (dolist (root storage)
    (usage--field stream (usage-storage-name root) (namestring (usage-storage-pathname root))
                  :columns columns :styled-p styled-p :style ':dim)
    (when (plusp (usage-storage-errors root))
      (usage--field stream "Errors (partial root)" (usage--count (usage-storage-errors root))
                    :columns columns :styled-p styled-p :style ':notice)))
  nil)

(-> usage-print-report (usage-report &key (:stream stream) (:styled-p boolean) (:columns integer)) null)
(defun usage-print-report
    (report &key (stream *standard-output*)
                 (styled-p (localgroup--status-output-styled-p stream))
                 (columns (localgroup--status-output-columns stream)))
  "Print REPORT as a compact, colored operating-system usage summary."
  (let ((columns (max 1 (min 104 columns))))
    (flet ((paragraph (text &optional (style ':plain))
             (usage--paragraph stream text :columns columns :styled-p styled-p :style style))

           (field (label value &optional (style ':plain))
             (usage--field stream label value :columns columns :styled-p styled-p :style style))

           (fields (entries)
             (usage--fields stream entries :columns columns :styled-p styled-p)))
      (paragraph "Autolith usage" ':brand)
      (terpri stream)
      (paragraph "Stored data" ':strong)
      (fields (loop for (label . count) in (usage-report-counts report)
                    collect (list label (usage--count count) (if count ':strong ':notice))))
      (terpri stream)
      (paragraph "Disk" ':strong)
      (field "Logical bytes, union" (usage--bytes (usage-report-disk-bytes report)))
      (when (usage-report-disk-partial-p report)
        (paragraph "Disk totals are partial: some entries were inaccessible." ':notice))
      (usage--storage stream (usage-report-storage report) :columns columns :styled-p styled-p)
      (paragraph "Root sizes can overlap; the union deduplicates files." ':dim)
      (terpri stream)
      (paragraph "RAM" ':strong)
      (fields (list (list "Live sessions" (usage--count (usage-report-sessions report)))
                    (list "Session processes" (usage--count (usage-report-processes report)))))
      (fields (list (list "Session RSS" (usage--bytes (usage-report-resident-bytes report)))
                    (list "Reporter RSS" (usage--bytes (usage-report-reporter-bytes report)))))
      (paragraph "Session hosts and descendants; shared pages count per process." ':dim)
      (when (plusp (usage-report-resident-missing report))
        (field "RSS measurements missing" (usage--count (usage-report-resident-missing report)) ':notice))
      (field "Descendant discovery"
             (if (usage-report-descendants-available-p report) "available" "unavailable or partial")
             (if (usage-report-descendants-available-p report) ':plain ':notice))
      (dolist (warning (usage-report-warnings report))
        (field "Warning" (usage--text warning) ':notice))))
  nil)
