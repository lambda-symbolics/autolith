(in-package #:autolith)

;;;; -- Source Check Runner --

;;; The runner applies every rule to every tracked file, and with repairs
;;; requested rewrites files until the repairable rules find nothing. Before
;;; writing, it verifies that layout repairs changed only whitespace outside
;;; string literals, comparing the file's text with all whitespace removed and
;;; its string literals; a repair failing that comparison is not written.

(defparameter *source-check-rule-classes*
  '(source-final-newline-rule
    source-trailing-whitespace-rule
    source-tab-rule
    source-em-dash-rule
    source-section-header-rule
    source-package-rule
    source-constant-rule
    source-todo-rule
    source-documentation-rule
    source-generic-declaration-rule
    source-function-declaration-rule
    source-declaration-uniqueness-rule
    source-positional-parameters-rule
    source-list-accessor-rule
    source-format-percent-rule
    source-top-level-spacing-rule
    source-local-function-spacing-rule
    source-clause-layout-rule
    source-alignment-rule
    source-mallet-rule)
  "The source rules every check applies, in the order repairs run.")

(defparameter *source-check-repair-rounds* 12
  "The most times repairs run over one file before the runner stops.")


;;;; -- Public Functions --

(-> source-check-rules (&key (:parinfer-library (option pathname))) list)
(defun source-check-rules (&key parinfer-library)
  "Return instances of every source rule, parinfer's when PARINFER-LIBRARY is given."
  (append (mapcar #'make-instance *source-check-rule-classes*)
          (and parinfer-library
               (list (make-instance 'source-parinfer-rule :library parinfer-library)))))

(-> source-check-paths (pathname) list)
(defun source-check-paths (root)
  "Return repository-relative source paths from ROOT's Git or Jujutsu file list."
  (let ((command (if (probe-file (merge-pathnames ".jj/" root))
                     '("jj" "--ignore-working-copy" "file" "list" "-T" "path ++ \"\\0\"")
                     '("git" "ls-files" "-z"))))
    (remove-if-not (lambda (path)
                     (member (pathname-type path) *source-text-file-types* :test #'equal))
                   (remove ""
                           (uiop:split-string
                            (uiop:run-program command :directory root :output ':string)
                            :separator (list (code-char 0)))
                           :test #'equal))))

(-> source-check-system-paths (pathname) hash-table)
(defun source-check-system-paths (root)
  "Return the repository-relative source paths compiled by ROOT's systems."
  (let ((paths       (make-hash-table :test #'equal))
        (definitions (loop for name in '("autolith.asd" "autolith-agent.asd"
                                         "autolith-supervisor.asd")
                           for path = (merge-pathnames name root)
                           when (probe-file path) collect path)))
    (dolist (definition definitions)
      (let ((system (asdf:find-system (pathname-name definition) nil)))
        (unless (and system (uiop:pathname-equal (asdf:system-source-file system) definition))
          (asdf:load-asd definition))))
    (labels ((visit (component)
               (typecase component
                 (asdf:parent-component
                  (mapc #'visit (asdf:component-children component)))
                 (asdf:cl-source-file
                  (setf (gethash (uiop:unix-namestring
                                  (uiop:enough-pathname (asdf:component-pathname component) root))
                                 paths)
                        t)))))
      (dolist (name (asdf:registered-systems))
        (let ((system (asdf:find-system name nil)))
          (when (and system
                     (member (asdf:system-source-file system) definitions
                             :test #'uiop:pathname-equal))
            (visit system)))))
    paths))

(-> source-check-file (source-file list) list)
(defun source-check-file (file rules)
  "Return the findings every applicable rule of RULES reports in FILE."
  (loop for rule in rules
        when (source-rule-applies-p rule file)
          append (source-rule-findings rule file)))

(-> source-check-repair (source-file list) (values string list))
(defun source-check-repair (file rules)
  "Return FILE's text after every repair RULES can make, and the rules that changed it.

Rules that rename or remove forms run first, then layout rules until they find
nothing or the round limit passes. Signal SOURCE-CHECK-UNSAFE-REPAIR when the
layout repairs changed anything but whitespace outside string literals."
  (let* ((applicable (remove-if-not (lambda (rule) (source-rule-applies-p rule file)) rules))
         (changed    nil)
         (current    file))
    (flet ((repair (rule)
             (let ((text (source-rule-fix rule current)))
               (when (and text (string/= text (source-file-text current)))
                 (pushnew (source-rule-name rule) changed :test #'equal)
                 (setf current (source-file-create (source-file-path file) text))
                 t))))
      (dolist (rule (remove-if #'source-rule-layout-only-p applicable))
        (repair rule))
      (let ((signature (source-check--signature current)))
        (loop repeat *source-check-repair-rounds*
              while (some (lambda (rule)
                            (and (source-rule-applies-p rule current)
                                 (repair rule)))
                          (remove-if-not #'source-rule-layout-only-p applicable)))
        (unless (equal signature (source-check--signature current))
          (error 'source-check-unsafe-repair
                 :path    (source-file-path file)
                 :message (format nil "Repairing ~A would change more than its layout; it was left unchanged."
                                  (source-file-path file))))))
    (values (source-file-text current) (nreverse changed))))

(-> source-check-run
    (pathname &key (:fix boolean) (:parinfer-library (option pathname))
                   (:paths (option list)) (:stream stream))
    list)
(defun source-check-run (root &key fix parinfer-library paths (stream *standard-output*))
  "Check ROOT's tracked files, or PATHS among them, and return the remaining findings.

With FIX, repairable findings are repaired in place first, and each repaired
file is reported to STREAM. SOURCE-CHECK-REPORT prints the findings."
  (let* ((rules                       (source-check-rules :parinfer-library parinfer-library))
         (*source-check-system-paths* (let ((system-paths (source-check-system-paths root)))
                                        (and (plusp (hash-table-count system-paths))
                                             system-paths)))
         (paths                       (or paths (source-check-paths root))))
    (when fix
      (dolist (path paths)
        (source-check--repair-path root path :rules rules :stream stream)))
    (let ((files (mapcar (lambda (path) (source-check--read root path)) paths)))
      (dolist (file files)
        (dolist (rule rules)
          (when (source-rule-applies-p rule file)
            (source-rule-prepare rule file)))
        (source-file-release file))
      (loop for file in files
            append (prog1 (source-check-file file rules)
                     (source-file-release file))))))

(-> source-check-report (list stream) null)
(defun source-check-report (findings stream)
  "Print FINDINGS to STREAM one per line, then a count per rule."
  (dolist (finding findings)
    (format stream "~&~A:~D:~D: [~A] ~A~%"
            (source-finding-path finding) (source-finding-line finding)
            (source-finding-column finding) (source-finding-rule finding)
            (source-finding-message finding)))
  (let ((counts nil))
    (dolist (finding findings)
      (let ((entry (assoc (source-finding-rule finding) counts :test #'equal)))
        (if entry
            (incf (rest entry))
            (push (cons (source-finding-rule finding) 1) counts))))
    (if counts
        (format stream "~&Source checks found ~D problem~:P: ~{~A~^, ~}.~%"
                (length findings)
                (mapcar (lambda (entry) (format nil "~A ~D" (first entry) (rest entry)))
                        (sort counts #'> :key #'rest)))
        (format stream "~&Source checks passed.~%")))
  nil)


;;;; -- Private Functions --

(-> source-check--read (pathname string) source-file)
(defun source-check--read (root path)
  "Return the source file at repository-relative PATH under ROOT."
  (source-file-create path (uiop:read-file-string (merge-pathnames path root)
                                                  :external-format ':utf-8)))

(-> source-check--repair-path (pathname string &key (:rules list) (:stream stream)) null)
(defun source-check--repair-path (root path &key rules stream)
  "Repair the file at PATH under ROOT with RULES, reporting to STREAM what changed."
  (handler-case
      (multiple-value-bind (text changed)
          (source-check-repair (source-check--read root path) rules)
        (when changed
          (source-check--write (merge-pathnames path root) text)
          (format stream "~&repaired ~A (~{~A~^, ~})~%" path changed)))
    (source-check-unsafe-repair (condition)
      (format stream "~&~A~%" (autolith-error-message condition))))
  nil)

(-> source-check--signature (source-file) list)
(defun source-check--signature (file)
  "Return what a layout repair must preserve: FILE's text without whitespace, and its strings."
  (let ((strings nil))
    (when (source-file-lisp-p file)
      (dolist (node (source-file-nodes file))
        (source-node-walk node
                          (lambda (node)
                            (when (source-node-string-p node)
                              (push (source-node-text node) strings))))))
    (list (remove-if #'source-text--blank-character-p (source-file-text file))
          (nreverse strings))))

(-> source-check--write (pathname string) null)
(defun source-check--write (pathname text)
  "Replace the file at PATHNAME with TEXT."
  (with-open-file (stream pathname :direction ':output :if-exists ':supersede
                                   :external-format ':utf-8)
    (write-string text stream))
  nil)


;;;; -- Conditions --

(define-condition source-check-unsafe-repair (autolith-error)
  ((path
    :initarg       :path
    :reader        source-check-unsafe-repair-path
    :type          string
    :documentation "The file whose repair changed more than its layout."))
  (:documentation "A layout repair changed something other than whitespace outside strings."))
