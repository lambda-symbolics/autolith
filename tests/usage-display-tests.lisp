(in-package #:autolith)

;;;; -- Usage Display Tests --

(-> test-usage-display--report (&key (:resident-bytes t) (:partial-p boolean) (:path string)) usage-report)
(defun test-usage-display--report (&key (resident-bytes 4096) (partial-p nil)
                                       (path "/data/autolith"))
  "Build a representative usage report for display tests."
  (make-usage-report
   :counts           '(("conversations" . 2) ("memory records" . nil))
   :storage          (list (make-usage-storage :name "data" :pathname (pathname path)
                                               :bytes 2048 :files 3 :links 1
                                               :errors (if partial-p 1 0)))
   :disk-bytes 2048 :disk-partial-p partial-p :sessions 1 :processes 2
   :resident-bytes   resident-bytes
   :resident-missing (if partial-p 1 0)
   :descendants-available-p (not partial-p) :reporter-bytes 1024
   :warnings         (and partial-p '("one warning"))))

(-> test-usage-display-visible-sections () null)
(defun test-usage-display-visible-sections ()
  "Verify the report exposes its sections and qualifications."
  (let ((output (with-output-to-string (stream)
                  (usage-print-report (test-usage-display--report)
                                      :stream stream :styled-p nil :columns 80))))
    (dolist (text '("Autolith usage" "Stored data" "Disk" "RAM" "Live sessions"
                    "Session hosts and descendants" "shared pages" "Reporter RSS"
                    "Logical bytes" "Root sizes can overlap"))
      (test-assert (search text output) text)))
  nil)

(-> test-usage-display-unavailable-is-not-zero () null)
(defun test-usage-display-unavailable-is-not-zero ()
  "Verify unavailable and partial measurements differ from measured zero."
  (let ((output (with-output-to-string (stream)
                  (usage-print-report
                   (test-usage-display--report :resident-bytes nil :partial-p t)
                   :stream stream :styled-p nil :columns 80)))
        (zero   (with-output-to-string (stream)
                  (usage-print-report (test-usage-display--report :resident-bytes 0)
                                      :stream stream :styled-p nil :columns 80))))
    (test-assert (search "unavailable" output) "unavailable is shown")
    (test-assert (search "partial" output) "partial observations are shown")
    (test-assert (search "0 B" zero) "measured zero is shown"))
  nil)

(-> test-usage-display-sanitizes-controls-and-fits-width () null)
(defun test-usage-display-sanitizes-controls-and-fits-width ()
  "Verify path controls are sanitized and output fits a range of terminals."
  (let ((path (format nil "/tmp/x~C~Csecret" #\Newline #\Escape)))
    (dolist (width '(1 2 8 16 24 40 52 80 104 240))
      (let ((output (with-output-to-string (stream)
                      (usage-print-report (test-usage-display--report :path path)
                                          :stream stream :styled-p nil :columns width))))
        (test-assert (not (search (string #\Escape) output)) "no escape in path")
        (dolist (line (uiop:split-string output :separator (string #\Newline)))
          (test-assert (<= (text-cell-width line) width) "line fits terminal")))))
  nil)

(-> test-usage-display-compact-columns () null)
(defun test-usage-display-compact-columns ()
  "Verify adjacent metrics share rows and wide screens do not pull them apart."
  (let* ((report (test-usage-display--report))
         (render (lambda (width)
                   (with-output-to-string (stream)
                     (usage-print-report report :stream stream :styled-p nil :columns width)))))
    (setf (usage-report-counts report)
          '(("Conversations" . 307) ("Task conversations" . 91) ("Memories" . 46)
            ("Agenda items" . 8) ("Retained checkpoints" . 12) ("Fact assertions" . 1538)))
    (dolist (width '(80 104 240))
      (let* ((output (funcall render width))
             (lines  (uiop:split-string output :separator (string #\Newline)))
             (row    (find-if (lambda (line) (search "Conversations" line)) lines)))
        (test-assert (and row (search "Task conversations" row)) "metrics share a row")
        (test-assert (< (- (search "307" row) (length "Conversations")) 12)
                     "value sits near its label")
        (dolist (line lines)
          (test-assert (<= (text-cell-width line) 104) "wide layout stays bounded"))))
    (test-assert (string= (funcall render 104) (funcall render 240))
                 "layout stops expanding at its content limit"))
  nil)

(-> test-usage-display-root-comparisons () null)
(defun test-usage-display-root-comparisons ()
  "Verify bars compare independent roots and handle empty storage."
  (let ((report (test-usage-display--report)))
    (setf (usage-report-disk-bytes report) 4096
          (usage-report-storage report)
          (loop for (name bytes) in '(("Empty" 0) ("Half" 2048) ("Full" 4096))
                collect (make-usage-storage :name name :pathname #P"/data/autolith/"
                                            :bytes bytes :files 1)))
    (dolist (width '(40 104))
      (let ((output (with-output-to-string (stream)
                      (usage-print-report report :stream stream :styled-p nil :columns width))))
        (dolist (bar '("[............]" "[######......]" "[############]"))
          (test-assert (search bar output) "root-relative bar, including zero"))
        (when (= width 104)
          (dolist (entry '(("Empty" "[............]") ("Half" "[######......]")
                           ("Full" "[############]")))
            (let ((row (find-if (lambda (line) (search (first entry) line))
                                (uiop:split-string output :separator (string #\Newline)))))
              (test-assert (and row (search (second entry) row))
                           "root metrics and proportional bar share a row"))))))
    (dolist (root (usage-report-storage report))
      (setf (usage-storage-bytes root) 0))
    (let ((output (with-output-to-string (stream)
                    (usage-print-report report :stream stream :styled-p nil :columns 104))))
      (test-assert (search "[............]" output) "all-zero roots have empty bars")))
  nil)

(-> test-usage-display-styling-gate () null)
(defun test-usage-display-styling-gate ()
  "Verify plain output has no ANSI controls and styled output has styles."
  (let ((plain   (with-output-to-string (stream)
                   (usage-print-report (test-usage-display--report)
                                       :stream stream :styled-p nil :columns 80)))
        (styled  (with-output-to-string (stream)
                   (usage-print-report (test-usage-display--report :partial-p t)
                                       :stream stream :styled-p t :columns 80)))
        (default (with-output-to-string (stream)
                   (usage-print-report (test-usage-display--report)
                                       :stream stream :columns 80))))
    (test-assert (not (search (string #\Escape) plain)) "plain output")
    (test-assert (not (search (string #\Escape) default)) "default noninteractive output")
    (test-assert (search (string #\Escape) styled) "styled output"))
  nil)

(-> test-usage-display-command () null)
(defun test-usage-display-command ()
  "Verify the command defers provider validation and rejects positional arguments."
  (let ((collected nil)
        (deferred  nil)
        (printed   nil)
        (arguments nil))
    (test-call-with-function-replacements
     (list (list 'command-arguments (lambda (command) (declare (ignore command)) arguments))
           (list 'configuration-create
                 (lambda (&key defer-provider-validation-p &allow-other-keys)
                   (setf deferred defer-provider-validation-p)
                   ':test-configuration))
           (list 'usage-collect
                 (lambda (configuration)
                   (setf collected configuration)
                   (test-usage-display--report)))
           (list 'usage-print-report
                 (lambda (report &key &allow-other-keys)
                   (setf printed (usage-report-p report)))))
     (lambda ()
       (let* ((command (main--usage-command))
              (handler (slot-value command 'clingon.command::handler)))
         (funcall handler command)
         (test-assert deferred "provider validation deferred")
         (test-assert (eq collected ':test-configuration) "configuration passed to collector")
         (test-assert printed "report printed")
         (setf arguments '("unexpected") collected nil)
         (test-assert (handler-case (progn (funcall handler command) nil)
                        (configuration-error () t))
                      "positional arguments rejected")
         (test-assert (null collected) "rejected before collection")))))
  nil)
