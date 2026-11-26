(in-package #:autolith)

;;;; -- Lisp Value Display Tests --

(defclass lisp-values-tests--broken-printer ()
  ((calls :initform 0 :accessor lisp-values-tests--printer-calls
          :documentation "The number of calls to the failing printer."))
  (:documentation "An object whose printer fails for error-boundary tests."))

(defmethod print-object ((value lisp-values-tests--broken-printer) stream)
  "Fail without giving the formatter a printable object or condition."
  (declare (ignore stream))
  (incf (lisp-values-tests--printer-calls value))
  (error 'type-error :datum value :expected-type 'string))

(-> test-application-lisp-value-roundtrips () null)
(defun test-application-lisp-value-roundtrips ()
  "Preserve readable Lisp values at each supported sample width."
  (let ((*package*   (find-package '#:autolith))
        (*read-eval* nil))
    (dolist (columns '(60 80 120))
      (dolist (value (list nil t 123 -2/3 #\Space
                           "plain string" "(:looks :like :lisp)"
                           (format nil "quotes \" and backslash \\~%next line")
                           :|Mixed case| :|has space| 'application-lisp-render-value
                           '(1 . 2) '(a b (c . d))
                           '(:name "scout" :source :bundled :models ("@smol")
                             :tools ("resource.read" "search.*" "web_search"))
                           '(:outer (:first "value" :second (one two three))
                             :other "another value")))
        (let ((text (application-lisp-render-value value :columns columns)))
          (multiple-value-bind (restored end) (read-from-string text)
            (test-assert (equal value restored)
                         "Displayed Lisp values must retain reader semantics.")
            (test-assert (= end (length text))
                         "A displayed value must have no trailing unread content.")))))))

(-> test-application-lisp-value-layout () null)
(defun test-application-lisp-value-layout ()
  "Use structural line breaks and aligned fields within the supplied width."
  (let* ((value  '(:name "scout" :source :bundled :models ("@smol")
                   :tools ("resource.read" "search.*" "web_search")))
         (narrow (application-lisp-render-value value :columns 60))
         (wide   (application-lisp-render-value value :columns 120)))
    (test-assert (find #\Newline narrow)
                 "Long property lists need structural line breaks.")
    (test-assert (not (find #\Newline (application-lisp-render-value '(:a 1 :b 2))))
                 "Short property lists must fit on one line.")
    (dolist (columns '(60 80 120))
      (let* ((text  (application-lisp-render-value value :columns columns))
             (lines (uiop:split-string text :separator '(#\Newline))))
        (test-assert (every (lambda (line) (<= (length line) columns)) lines)
                     "Breakable forms must fit within their available columns.")))
    (test-assert (<= (count #\Newline wide) (count #\Newline narrow))
                 "More available columns must not require more lines.")
    (let* ((lines        (uiop:split-string narrow :separator '(#\Newline)))
           (first-value  (position #\" (first lines)))
           (second-value (position #\: (second lines) :start 2)))
      (test-assert (= first-value second-value)
                   "Related keyword values must align on broken rows."))))

(-> test-application-lisp-value-graphs () null)
(defun test-application-lisp-value-graphs ()
  "Preserve dotted lists, circular lists, and shared substructure."
  (let ((*read-eval* nil))
    (let* ((value    (list :self))
           (ignored  (setf (rest value) value))
           (restored (read-from-string (application-lisp-render-value value))))
      (declare (ignore ignored))
      (test-assert (eq (rest restored) restored)
                   "Circular list structure must survive display and readback."))
    (let* ((tail     (list "shared"))
           (value    (list tail tail))
           (restored (read-from-string (application-lisp-render-value value))))
      (test-assert (eq (first restored) (second restored))
                   "Shared list tails must retain identity."))
    (let* ((shared   (list :name "shared" :models (list "@smol")))
           (value    (list :first shared :second shared))
           (restored (read-from-string (application-lisp-render-value value :columns 60))))
      (test-assert (and (eq (getf restored :first) (getf restored :second))
                        (equal shared (getf restored :first)))
                   "Aligned keyword plists preserve shared values and their contents."))
    (let* ((value    (list :a 1 :tail (cons :x :y)))
           (restored (read-from-string (application-lisp-render-value value))))
      (test-assert (equal value restored)
                   "Nested dotted data must retain reader syntax."))))

(-> test-application-lisp-value-bounds () null)
(defun test-application-lisp-value-bounds ()
  "Mark omitted depth, length, and output without excessive retained text."
  (let ((deep :leaf))
    (dotimes (index 30)
      (setf deep (list deep)))
    (test-assert (search "#" (application-lisp-render-value deep))
                 "Deep data must use the standard depth marker."))
  (test-assert (search "..." (application-lisp-render-value
                              (loop for index below 200 collect index)))
               "Long sequences must use the standard length marker.")
  (let ((text (application-lisp-render-value
               (make-string 100000 :initial-element #\x))))
    (test-assert (<= (length text) 16100)
                 "Large atoms must have a bounded display prefix.")
    (test-assert (search "[output truncated]" text)
                 "An incomplete prefix needs an explicit truncation notice.")))

(-> test-application-lisp-value-printer-failure () null)
(defun test-application-lisp-value-printer-failure ()
  "Represent printer errors without trying to print the failing datum again."
  (let* ((value (make-instance 'lisp-values-tests--broken-printer))
         (text  (application-lisp-render-value value)))
    (test-assert (plusp (length text)) "Printer failures need a visible result.")
    (test-assert (= 1 (lisp-values-tests--printer-calls value))
                 "Error presentation must not invoke a failed object printer again.")))

(-> test-application-lisp-value-evaluation-safety () null)
(defun test-application-lisp-value-evaluation-safety ()
  "Keep successful side effects and readable values when presentation fails."
  (let* ((evaluation
           (application-lisp-evaluate
            "(values (make-instance 'lisp-values-tests--broken-printer) :completed)"))
         (values (application-lisp-evaluation-values evaluation)))
    (test-assert (eq ':ok (application-lisp-evaluation-status evaluation))
                 "A broken object printer must not turn successful evaluation into failure.")
    (test-assert (and (search "printing failed" (first values))
                      (eq :completed (read-from-string (second values))))
                 "Other values remain visible after a printer failure."))
  (let* ((value      (format nil "first~%second\"\\"))
         (evaluation (application-lisp-evaluate (write-to-string value))))
    (test-assert (string= value (read-from-string
                                 (first (application-lisp-evaluation-values evaluation))))
                 "Prefix-aware printing must preserve multiline string contents."))
  nil)
