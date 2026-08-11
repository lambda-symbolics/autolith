(in-package #:autolith)

;;;; -- Skill Policy --

(defparameter *skill-scan-depth-limit* 6
  "The maximum directory depth traversed below one skill root.")

(defparameter *skill-scan-directory-limit* 2000
  "The maximum directories inspected across all skill roots.")

(defparameter *skill-scan-entry-limit* 20000
  "The maximum filesystem entries inspected across all skill roots.")

(defparameter *skill-file-character-limit* (* 64 1024)
  "The maximum characters read from one SKILL.sexp.")

(defparameter *skill-discovery-character-limit* (* 8 1024 1024)
  "The maximum SKILL.sexp characters read during one catalog discovery.")

(defparameter *skill-form-depth-limit* 32
  "The maximum structural depth accepted in one SKILL.sexp form.")

(defparameter *skill-form-node-limit* 128
  "The maximum conses and atoms accepted in one SKILL.sexp form.")

(defparameter *skill-instruction-character-limit* (* 64 1024)
  "The maximum instruction characters accepted from one selected skill.")

(defparameter *skill-workflow-character-limit* (* 256 1024)
  "The maximum characters read from one executable Skill workflow.")

(defparameter *skill-workflow-depth-limit* 16
  "The maximum nested Skill workflow executions in one dynamic call chain.")

(defparameter *skill-selection-character-limit* (* 128 1024)
  "The maximum selected skill instruction characters injected in one request.")

(defparameter *skill-selection-count-limit* 32
  "The maximum distinct skills selected during one logical user turn.")

(defparameter *skill-name-character-limit* 64
  "The maximum characters in a skill name.")

(defparameter *skill-description-character-limit* 1024
  "The maximum characters in a skill description.")

(defparameter *skill-catalog-character-budget* 3500
  "The default maximum characters rendered into a model-visible skill catalog.")

(defparameter *skill-status-entry-limit* 100
  "The maximum skills or diagnostics rendered by one /skills invocation.")

(defparameter *skill-warning-character-limit* 1000
  "The maximum characters in one request-local skill warning.")

(defparameter *skill-warning-aggregate-character-limit* 4000
  "The maximum selected-skill warning characters injected in one request.")

(defparameter *skill-native-keywords*
  '(:autolith-skill :version :name :description :instructions :workflow
    :workflow-self-tools)
  "The complete keyword vocabulary accepted by native skill forms.")

(defparameter *skill-workflow-filename* "WORKFLOW.lisp"
  "The exact relative workflow filename accepted by SKILL.sexp.")

(defvar *skill-workflow-stack* nil
  "Skill names currently executing in the dynamic workflow call chain.")

(defvar *skill-logical-turn-active-p* nil
  "True while one logical user turn accepts explicit skill selections.")

(defvar *skill-logical-turn-selection-names* nil
  "Exact skill names selected during the current logical user turn.")

(defvar *skill-logical-turn-selection-metadata* nil
  "Definition identities selected during the current logical user turn.")

(defvar *skill-definition-source-character-count* 0
  "Characters read while validating the dynamically active skill definition.")


;;;; -- Skill Values and Diagnostics --

(deftype skill-diagnostic-kind ()
  "A structured reason why skill discovery did not select one path."
  '(member :missing-root
           :scan-error
           :scan-depth-limit
           :scan-directory-limit
           :scan-entry-limit
           :scan-character-limit
           :outside-root
           :not-regular-file
           :identity-changed
           :read-error
           :file-too-large
           :data-too-deep
           :data-too-large
           :invalid-syntax
           :invalid-structure
           :missing-field
           :unknown-field
           :duplicate-field
           :invalid-version
           :invalid-name
           :invalid-description
           :invalid-instructions
           :invalid-workflow
           :name-directory-mismatch
           :shadowed))

(defclass skill-metadata ()
  ((name
    :initarg :name
    :reader skill-metadata-name
    :type non-empty-string
    :documentation "The validated, case-sensitive skill name.")
   (description
    :initarg :description
    :reader skill-metadata-description
    :type non-empty-string
    :documentation "The bounded single-line description used for selection.")
   (workflow
    :initarg :workflow
    :initform nil
    :reader skill-metadata-workflow
    :type (option non-empty-string)
    :documentation "The validated relative executable workflow filename.")
   (workflow-self-tools-p
    :initarg :workflow-self-tools-p
    :initform t
    :reader skill-metadata-workflow-self-tools-p
    :type boolean
    :documentation
    "Whether the workflow receives active-image inspection and mutation tools.")
   (pathname
    :initarg :pathname
    :reader skill-metadata-pathname
    :type pathname
    :documentation "The exact absolute discovered SKILL.sexp pathname.")
   (canonical-pathname
    :initarg :canonical-pathname
    :reader skill-metadata-canonical-pathname
    :type pathname
    :documentation "The canonical regular file read during discovery.")
   (device
    :initarg :device
    :reader skill-metadata-device
    :type (integer 0)
    :documentation "The discovered regular file's device identity.")
   (inode
    :initarg :inode
    :reader skill-metadata-inode
    :type (integer 0)
    :documentation "The discovered regular file's inode identity.")
   (root
    :initarg :root
    :reader skill-metadata-root
    :type pathname
    :documentation "The ordered discovery root that supplied this skill.")
   (root-index
    :initarg :root-index
    :reader skill-metadata-root-index
    :type (integer 0)
    :documentation "The zero-based precedence position of the discovery root."))
  (:documentation
   "Validated skill catalog metadata without retained instruction text."))

(defclass skill-diagnostic ()
  ((kind
    :initarg :kind
    :reader skill-diagnostic-kind
    :type skill-diagnostic-kind
    :documentation "The machine-readable discovery or validation outcome.")
   (pathname
    :initarg :pathname
    :reader skill-diagnostic-pathname
    :type pathname
    :documentation "The file or directory associated with the outcome.")
   (root-index
    :initarg :root-index
    :reader skill-diagnostic-root-index
    :type (integer 0)
    :documentation "The zero-based discovery-root position.")
   (message
    :initarg :message
    :reader skill-diagnostic-message
    :type non-empty-string
    :documentation "A concise human-readable explanation.")
   (reservation-name
    :initarg :reservation-name
    :initform nil
    :reader skill-diagnostic-reservation-name
    :type (option string)
    :documentation
    "The lexical skill name this diagnostic reserves, when any."))
  (:documentation
   "One typed skill discovery result that does not abort the remaining scan."))

(defclass skill-catalog ()
  ((skills
    :initarg :skills
    :reader skill-catalog-skills
    :type list
    :documentation "Selected metadata in deterministic precedence order.")
   (diagnostics
    :initarg :diagnostics
    :reader skill-catalog-diagnostics
    :type list
    :documentation "Non-fatal scan, parse, validation, and shadowing outcomes."))
  (:documentation
   "An immutable skill metadata snapshot assembled from ordered roots."))

(define-condition skill-read-error (autolith-error)
  ((pathname
    :initarg :pathname
    :reader skill-read-error-pathname
    :type pathname
    :documentation "The selected SKILL.sexp that could not be read.")
   (cause
    :initarg :cause
    :initform nil
    :reader skill-read-error-cause
    :type t
    :documentation "The underlying filesystem, syntax, or validation failure."))
  (:documentation "Reading selected skill instructions failed."))

(define-condition skill-body-too-large (skill-read-error)
  ((character-limit
    :initarg :character-limit
    :reader skill-body-too-large-character-limit
    :type (integer 1)
    :documentation "The maximum selected instruction size in characters."))
  (:documentation "Selected skill instructions exceed the request input bound."))

(define-condition skill-catalog-render-error (autolith-error)
  ((character-budget
    :initarg :character-budget
    :reader skill-catalog-render-error-character-budget
    :type (integer 1)
    :documentation "The requested maximum rendered character count.")
   (minimum-required
    :initarg :minimum-required
    :reader skill-catalog-render-error-minimum-required
    :type (integer 1)
    :documentation "The characters needed for the catalog protocol itself."))
  (:documentation "A skill catalog budget cannot hold its required guidance."))

(define-condition skill-selection-error (autolith-error)
  ((name
    :initarg :name
    :reader skill-selection-error-name
    :type string
    :documentation "The exact case-sensitive skill name that was requested.")
   (reason
    :initarg :reason
    :reader skill-selection-error-reason
    :type (member :inactive-turn :unknown-skill :selection-limit)
    :documentation "The machine-readable reason selection could not proceed."))
  (:documentation "A skill could not be selected for the active logical turn."))

(define-condition skill--definition-error (error)
  ((kind
    :initarg :kind
    :reader skill--definition-error-kind
    :type skill-diagnostic-kind
    :documentation "The diagnostic kind produced for this file.")
   (message
    :initarg :message
    :reader skill--definition-error-message
    :type non-empty-string
    :documentation "The validation failure explanation.")
   (source-character-count
    :initarg :source-character-count
    :initform 0
    :reader skill--definition-error-source-character-count
    :type (integer 0)
    :documentation "Characters read before this definition failed."))
  (:documentation "An internal non-fatal SKILL.sexp validation failure.")
  (:report (lambda (condition stream)
             (write-string (skill--definition-error-message condition)
                           stream))))


;;;; -- Filesystem Discovery --

(-> skill--diagnostic
    (&key (:kind skill-diagnostic-kind)
          (:pathname pathname)
          (:root-index (integer 0))
          (:message string)
          (:reservation-name (option string)))
    skill-diagnostic)
(defun skill--diagnostic
    (&key kind pathname root-index message reservation-name)
  "Return one structured diagnostic for PATHNAME in ROOT-INDEX."
  (make-instance 'skill-diagnostic
                 :kind kind
                 :pathname pathname
                 :root-index root-index
                 :message message
                 :reservation-name reservation-name))

(-> skill--pathname< (pathname pathname) boolean)
(defun skill--pathname< (left right)
  "Return true when LEFT sorts before RIGHT by its namestring."
  (not (null (string< (namestring left) (namestring right)))))

(-> skill--skill-pathname-p (pathname) boolean)
(defun skill--skill-pathname-p (pathname)
  "Return true when PATHNAME names a case-sensitive SKILL.sexp file."
  (string= (file-namestring pathname) "SKILL.sexp"))

(-> skill--canonical-subpath-p (pathname pathname) boolean)
(defun skill--canonical-subpath-p (pathname root)
  "Return true when canonical PATHNAME is ROOT or lies beneath it."
  (or (uiop:pathname-equal pathname root)
      (not (null (uiop:subpathp pathname root)))))

(-> skill--directory-reservation-name (pathname) (option string))
(defun skill--directory-reservation-name (pathname)
  "Return PATHNAME's final textual directory component, when any."
  (let ((component
          (first
           (last
            (pathname-directory
             (uiop:ensure-directory-pathname pathname))))))
    (and (stringp component) component)))

(-> skill--unresolved-link-reservation-name (pathname) (option string))
(defun skill--unresolved-link-reservation-name (pathname)
  "Return the lexical entry name reserved by unresolved symbolic link PATHNAME."
  (let ((name (file-namestring pathname)))
    (and (plusp (length name)) name)))

(-> skill--directory-entries-bounded
    (pathname (integer 0))
    (values list list boolean (integer 0) list))
(defun skill--directory-entries-bounded (directory entry-limit)
  "Return bounded files and subdirectories directly beneath DIRECTORY.

The third value is true when more than ENTRY-LIMIT entries exist. The fourth
value is the number of entries retained toward the aggregate scan budget. The
fifth value contains unresolved symbolic links.
Enumeration stops after the first excess entry and never retains an unbounded
directory listing."
  (let ((handle (sb-posix:opendir (namestring directory)))
        (files nil)
        (subdirectories nil)
        (unresolved-links nil)
        (entry-count 0)
        (exceeded-p nil))
    (unwind-protect
         (loop for entry = (sb-posix:readdir handle)
               until (sb-alien:null-alien entry)
               for name = (sb-posix:dirent-name entry)
               unless (member name '("." "..") :test #'string=)
                 do
                    (incf entry-count)
                    (when (> entry-count entry-limit)
                      (setf entry-count entry-limit
                            exceeded-p t)
                      (loop-finish))
                    (let* ((pathname (merge-pathnames name directory))
                           (status (sb-posix:lstat (namestring pathname)))
                           (mode (sb-posix:stat-mode status)))
                      (cond
                        ((sb-posix:s-isdir mode)
                         (push (uiop:ensure-directory-pathname pathname)
                               subdirectories))
                        ((sb-posix:s-islnk mode)
                         (handler-case
                             (let* ((target-status
                                      (sb-posix:stat (namestring pathname)))
                                    (target-mode
                                      (sb-posix:stat-mode target-status)))
                               (if (sb-posix:s-isdir target-mode)
                                   (push
                                    (uiop:ensure-directory-pathname pathname)
                                    subdirectories)
                                   (when (skill--skill-pathname-p pathname)
                                     (push pathname files))))
                           (error ()
                             (push pathname unresolved-links))))
                        ((or (sb-posix:s-isreg mode)
                             (skill--skill-pathname-p pathname))
                         (push pathname files)))))
      (sb-posix:closedir handle))
    (if exceeded-p
        (values nil nil t entry-count nil)
        (values (sort files #'skill--pathname<)
                (sort subdirectories #'skill--pathname<)
                nil
                entry-count
                (sort unresolved-links #'skill--pathname<)))))

(-> skill--scan-root
    (pathname (integer 0)
     &key (:max-depth (integer 0))
          (:max-directories (integer 1))
          (:max-entries (integer 1)))
    (values list list (integer 0) (integer 0)))
(defun skill--scan-root
    (root root-index
     &key
       (max-depth *skill-scan-depth-limit*)
       (max-directories *skill-scan-directory-limit*)
       (max-entries *skill-scan-entry-limit*))
  "Return sorted SKILL.sexp paths and diagnostics found beneath ROOT."
  (let ((root (uiop:ensure-directory-pathname root))
        (canonical-root nil)
        (paths nil)
        (diagnostics nil)
        (visited (make-hash-table :test #'equal))
        (directory-count 0)
        (entry-count 0)
        (stopped-p nil))
    (labels
        ((record-diagnostic
             (kind pathname message &key reservation-name)
           (push (skill--diagnostic
                  :kind kind
                  :pathname pathname
                  :root-index root-index
                  :message message
                  :reservation-name reservation-name)
                 diagnostics))

         (stop-scan (kind pathname message)
           (unless stopped-p
             (setf stopped-p t)
             (record-diagnostic kind pathname message)))

         (walk (directory depth)
           (block nil
             (when stopped-p
               (return))
             (when (>= entry-count max-entries)
               (stop-scan :scan-entry-limit
                          directory
                          (format nil
                                  "Skill scan reached its ~D entry limit."
                                  max-entries))
               (return))
             (when (>= directory-count max-directories)
               (stop-scan :scan-directory-limit
                          directory
                          (format nil
                                  "Skill scan reached its ~D directory limit."
                                  max-directories))
               (return))
             (incf directory-count)
             (let ((canonical
                     (handler-case
                         (truename directory)
                       (error (condition)
                         (record-diagnostic
                          :scan-error
                          directory
                          (format nil
                                  "Could not resolve skill directory: ~A"
                                  condition))
                         nil))))
               (unless canonical
                 (return))
               (unless (skill--canonical-subpath-p canonical canonical-root)
                 (record-diagnostic
                  :outside-root
                  directory
                  "Skill discovery did not follow a directory outside its canonical root."
                  :reservation-name
                  (skill--directory-reservation-name directory))
                 (return))
               (let ((identity (namestring canonical)))
                 (when (gethash identity visited)
                   (return))
                 (setf (gethash identity visited) t))
               (multiple-value-bind
                     (files subdirectories exceeded-p entries unresolved-links)
                   (handler-case
                       (skill--directory-entries-bounded
                        directory
                        (- max-entries entry-count))
                     (error (condition)
                       (record-diagnostic
                        :scan-error
                        directory
                        (format nil
                                "Could not inspect skill directory: ~A"
                                condition))
                       (values nil nil nil 0 nil)))
                 (incf entry-count entries)
                 (when exceeded-p
                   (stop-scan
                    :scan-entry-limit
                    directory
                    (format nil
                            "Skill scan reached its ~D entry limit."
                            max-entries))
                   (return))
                 (dolist (link unresolved-links)
                   (if (skill--skill-pathname-p link)
                       (push link paths)
                       (record-diagnostic
                        :scan-error
                        link
                        "Could not resolve symbolic link during skill discovery."
                        :reservation-name
                        (skill--unresolved-link-reservation-name link))))
                 (dolist (file files)
                   (when (skill--skill-pathname-p file)
                     (push file paths)))
                 (cond
                   ((< depth max-depth)
                    (dolist (subdirectory subdirectories)
                      (walk subdirectory (1+ depth))))
                   (subdirectories
                    (record-diagnostic
                     :scan-depth-limit
                     directory
                     (format nil
                             "Skill scan did not descend beyond depth ~D."
                             max-depth)))))))))
      (if (uiop:directory-exists-p root)
          (let ((resolved-root
                  (handler-case
                      (truename root)
                    (error (condition)
                      (record-diagnostic
                       :scan-error
                       root
                       (format nil
                               "Could not resolve skill root: ~A"
                               condition))
                      nil))))
            (when resolved-root
              (setf canonical-root
                    (uiop:ensure-directory-pathname resolved-root))
              (walk root 0)))
          (record-diagnostic :missing-root
                             root
                             "Skill root does not exist.")))
    (values (sort (remove-duplicates paths :test #'equal)
                  #'skill--pathname<)
            (nreverse diagnostics)
            directory-count
            entry-count)))


;;;; -- Native Skill Form --

(-> skill--definition-fail (skill-diagnostic-kind string &rest t) t)
(defun skill--definition-fail (kind control &rest arguments)
  "Signal one internal skill definition failure of KIND."
  (error 'skill--definition-error
         :kind kind
         :source-character-count *skill-definition-source-character-count*
         :message (apply #'format nil control arguments)))

(-> skill--read-file-bounded
    (pathname (integer 1) &key (:root (option pathname)) (:label string))
    (values string pathname (integer 0) (integer 0)))
(defun skill--read-file-bounded
    (pathname character-limit &key root (label "SKILL.sexp"))
  "Read one root-confined regular PATHNAME with a stable filesystem identity.

Return the bounded UTF-8 source, canonical pathname, device, and inode. Opening
uses a nonblocking descriptor so a FIFO or other non-regular candidate cannot
stall discovery."
  (handler-case
      (let* ((canonical (truename pathname))
             (canonical-root
               (and root
                    (uiop:ensure-directory-pathname (truename root)))))
        (when (and canonical-root
                   (not
                    (skill--canonical-subpath-p canonical canonical-root)))
          (skill--definition-fail
           :outside-root
           "~A resolves outside its canonical root."
           label))
        (let* ((expected-status (sb-posix:stat (namestring pathname)))
               (expected-mode (sb-posix:stat-mode expected-status))
               (expected-device (sb-posix:stat-dev expected-status))
               (expected-inode (sb-posix:stat-ino expected-status))
               (descriptor nil))
          (unless (sb-posix:s-isreg expected-mode)
            (skill--definition-fail
             :not-regular-file
             "~A must resolve to a regular file."
             label))
          (unwind-protect
               (progn
                 (setf descriptor
                       (sb-posix:open
                        (namestring pathname)
                        (logior sb-posix:o-rdonly sb-posix:o-nonblock)))
                 (let* ((opened-status (sb-posix:fstat descriptor))
                        (opened-mode (sb-posix:stat-mode opened-status))
                        (opened-device (sb-posix:stat-dev opened-status))
                        (opened-inode (sb-posix:stat-ino opened-status))
                        (current-canonical (truename pathname))
                        (current-status
                          (sb-posix:stat (namestring pathname))))
                   (unless (sb-posix:s-isreg opened-mode)
                     (skill--definition-fail
                      :not-regular-file
                      "~A must resolve to a regular file."
                      label))
                   (unless (and (= opened-device expected-device)
                                (= opened-inode expected-inode)
                                (= opened-device
                                   (sb-posix:stat-dev current-status))
                                (= opened-inode
                                   (sb-posix:stat-ino current-status)))
                     (skill--definition-fail
                      :identity-changed
                      "~A changed identity while it was being opened."
                      label))
                   (when (and canonical-root
                              (not
                               (skill--canonical-subpath-p
                                current-canonical
                                canonical-root)))
                     (skill--definition-fail
                      :outside-root
                      "~A resolves outside its canonical root."
                      label))
                   (let ((stream
                           (sb-sys:make-fd-stream
                            descriptor
                            :input t
                            :element-type 'character
                            :external-format :utf-8
                            :pathname pathname
                            :auto-close t)))
                     (setf descriptor nil)
                     (with-open-stream (stream stream)
                       ;; A decoding or stream failure does not report its
                       ;; partial progress. Charge the complete allowance
                       ;; until a successful read supplies the exact count.
                       (setf *skill-definition-source-character-count*
                             character-limit)
                       (let* ((buffer (make-string (1+ character-limit)))
                              (count (read-character-sequence buffer stream)))
                         (setf *skill-definition-source-character-count* count)
                         (when (> count character-limit)
                           (skill--definition-fail
                            :file-too-large
                            "~A exceeds the ~D-character file limit."
                            label
                            character-limit))
                         (values (subseq buffer 0 count)
                                 current-canonical
                                 opened-device
                                 opened-inode))))))
            (when descriptor
              (ignore-errors (sb-posix:close descriptor))))))
    (skill--definition-error (condition)
      (error condition))
    (error (condition)
      (skill--definition-fail
       :read-error
       "Could not read ~A: ~A"
       label
       condition))))

(-> skill--source-grammar () source-grammar)
(defun skill--source-grammar ()
  "Return the bounded native data grammar one SKILL.sexp may use.

The grammar is rebuilt for every read so that a live change to the keyword or
bound policy takes effect without reloading this file. Charging list length to
the depth budget keeps one bound over both nesting and length, and withholding
COMMON-LISP from the reader package keeps a bare symbol from naming anything."
  (make-source-grammar
   :label "SKILL.sexp"
   :keywords *skill-native-keywords*
   :maximum-depth *skill-form-depth-limit*
   :maximum-nodes *skill-form-node-limit*
   :list-tails-increase-depth-p t
   :common-lisp-symbols-permitted-p nil))

(-> skill--source-diagnostic-kind (sexp-config-error) skill-diagnostic-kind)
(defun skill--source-diagnostic-kind (condition)
  "Return the skill diagnostic kind reporting CONDITION."
  (case (sexp-config-error-kind condition)
    (:unknown-field ':unknown-field)
    (:data-too-deep ':data-too-deep)
    (:data-too-large ':data-too-large)
    ((:invalid-structure :invalid-value) ':invalid-structure)
    (otherwise ':invalid-syntax)))

(-> skill--read-one-form (string) t)
(defun skill--read-one-form (source)
  "Read and return exactly one native form from bounded SOURCE."
  (handler-case
      (read-source source (skill--source-grammar))
    (sexp-config-error (condition)
      (skill--definition-fail (skill--source-diagnostic-kind condition)
                              "~A"
                              (sexp-config-error-message condition)))))

(-> skill--validate-tree (t) null)
(defun skill--validate-tree (form)
  "Reject improper, circular, shared, deep, or oversized FORM structure."
  (handler-case
      (progn (validate-tree form (skill--source-grammar)) nil)
    (sexp-config-error (condition)
      (skill--definition-fail (skill--source-diagnostic-kind condition)
                              "~A"
                              (sexp-config-error-message condition)))))

(-> skill--normalize-description (string) string)
(defun skill--normalize-description (description)
  "Return DESCRIPTION with whitespace runs collapsed for catalog display."
  (string-trim
   '(#\Space #\Tab #\Newline #\Return #\Page)
   (with-output-to-string (stream)
     (let ((pending-space-p nil)
           (wrote-p nil))
       (loop for character across description
             do (if (find character
                          '(#\Space #\Tab #\Newline #\Return #\Page))
                    (when wrote-p
                      (setf pending-space-p t))
                    (progn
                      (when pending-space-p
                        (write-char #\Space stream))
                      (write-char character stream)
                      (setf pending-space-p nil
                            wrote-p t))))))))

(-> skill--name-character-p (character) boolean)
(defun skill--name-character-p (character)
  "Return true when CHARACTER is valid inside an Autolith skill name."
  (or (and (char>= character #\a)
           (char<= character #\z))
      (not (null (digit-char-p character)))
      (char= character #\-)))

(-> skill--valid-name-p (t) boolean)
(defun skill--valid-name-p (name)
  "Return true when NAME is a valid Autolith skill name."
  (and (stringp name)
       (plusp (length name))
       (<= (length name) *skill-name-character-limit*)
       (every #'skill--name-character-p name)
       (char/= (char name 0) #\-)
       (char/= (char name (1- (length name))) #\-)
       (null (search "--" name))))

(-> skill--validate-name (t) string)
(defun skill--validate-name (name)
  "Return validated skill NAME."
  (unless (stringp name)
    (skill--definition-fail
     :invalid-name
     "The :name value must be a string."))
  (when (zerop (length name))
    (skill--definition-fail
     :invalid-name
     "The :name value must not be empty."))
  (when (> (length name) *skill-name-character-limit*)
    (skill--definition-fail
     :invalid-name
     "Skill name exceeds ~D characters."
     *skill-name-character-limit*))
  (unless (every #'skill--name-character-p name)
    (skill--definition-fail
     :invalid-name
     "Skill name must contain only lowercase ASCII letters, digits, and hyphens."))
  (when (or (char= (char name 0) #\-)
            (char= (char name (1- (length name))) #\-)
            (search "--" name))
    (skill--definition-fail
     :invalid-name
     "Skill name cannot begin or end with a hyphen or contain consecutive hyphens."))
  name)

(-> skill--validate-description (t) string)
(defun skill--validate-description (description)
  "Return a validated single-line skill DESCRIPTION."
  (unless (stringp description)
    (skill--definition-fail
     :invalid-description
     "The :description value must be a string."))
  (let ((description (skill--normalize-description description)))
    (when (zerop (length description))
      (skill--definition-fail
       :invalid-description
       "The :description value must not be empty."))
    (when (> (length description) *skill-description-character-limit*)
      (skill--definition-fail
       :invalid-description
       "Skill description exceeds ~D characters."
       *skill-description-character-limit*))
    description))

(-> skill--validate-instructions (t (integer 1)) string)
(defun skill--validate-instructions (instructions character-limit)
  "Return validated skill INSTRUCTIONS without modifying their contents."
  (unless (stringp instructions)
    (skill--definition-fail
     :invalid-instructions
     "The :instructions value must be a string."))
  (when (zerop
         (length
          (string-trim
           '(#\Space #\Tab #\Newline #\Return #\Page)
           instructions)))
    (skill--definition-fail
     :invalid-instructions
     "The :instructions value must not be empty."))
  (when (> (length instructions) character-limit)
    (skill--definition-fail
     :file-too-large
     "Skill instructions exceed the ~D-character limit."
     character-limit))
  instructions)

(-> skill--validate-workflow (t) (option non-empty-string))
(defun skill--validate-workflow (workflow)
  "Return the exact supported relative WORKFLOW filename, or NIL."
  (when workflow
    (unless (and (stringp workflow)
                 (string= workflow *skill-workflow-filename*))
      (skill--definition-fail
       :invalid-workflow
       "The optional :workflow value must be exactly ~S."
       *skill-workflow-filename*)))
  workflow)

(-> skill--validate-workflow-self-tools (t boolean (option string)) boolean)
(defun skill--validate-workflow-self-tools (value present-p workflow)
  "Return the workflow self-tool flag after validating its declaration."
  (when (and present-p (null workflow))
    (skill--definition-fail
     :invalid-workflow
     ":workflow-self-tools requires :workflow."))
  (when (and present-p (not (or (eq value t) (null value))))
    (skill--definition-fail
     :invalid-workflow
     "The optional :workflow-self-tools value must be T or NIL."))
  (if present-p (and value t) t))

(-> skill--directory-name (pathname) (option string))
(defun skill--directory-name (pathname)
  "Return the final directory name containing PATHNAME, when it is textual."
  (let ((component
          (first
           (last
            (pathname-directory
             (uiop:pathname-directory-pathname pathname))))))
    (and (stringp component) component)))

(-> skill--parse-definition
    (pathname &key (:instruction-character-limit (integer 1))
                   (:file-character-limit (integer 1))
                   (:root (option pathname)))
    (values string string string (option non-empty-string) boolean pathname
            (integer 0) (integer 0) (integer 0)))
(defun skill--parse-definition
    (pathname
     &key
       (instruction-character-limit *skill-instruction-character-limit*)
       (file-character-limit *skill-file-character-limit*)
       root)
  "Read and validate PATHNAME as one native Autolith skill definition."
  (let ((*skill-definition-source-character-count* 0))
    (multiple-value-bind (source canonical-pathname device inode)
        (skill--read-file-bounded
         pathname
         file-character-limit
         :root root)
      (let ((form (skill--read-one-form source)))
        (unless (and (consp form)
                     (eq (first form) ':autolith-skill))
          (skill--definition-fail
           :invalid-structure
           "SKILL.sexp must begin with :autolith-skill."))
        (let ((fields (rest form))
              (values (make-hash-table :test #'eq)))
          (loop while fields
                do
                   (unless (rest fields)
                     (skill--definition-fail
                      :invalid-structure
                      "SKILL.sexp contains a field without a value."))
                   (let ((key (first fields))
                         (value (second fields)))
                     (unless (member key
                                     '(:version
                                       :name
                                       :description
                                       :instructions
                                       :workflow
                                       :workflow-self-tools)
                                     :test #'eq)
                       (skill--definition-fail
                        :unknown-field
                        "SKILL.sexp contains unknown field ~S."
                        key))
                     (multiple-value-bind (present-value present-p)
                         (gethash key values)
                       (declare (ignore present-value))
                       (when present-p
                         (skill--definition-fail
                          :duplicate-field
                          "SKILL.sexp contains duplicate field ~S."
                          key)))
                     (setf (gethash key values) value))
                   (setf fields (rest (rest fields))))
          (dolist (key '(:version :name :description :instructions))
            (multiple-value-bind (value present-p)
                (gethash key values)
              (declare (ignore value))
              (unless present-p
                (skill--definition-fail
                 :missing-field
                 "SKILL.sexp requires field ~S."
                 key))))
          (let ((version (gethash ':version values))
                (name (skill--validate-name (gethash ':name values)))
                (description
                  (skill--validate-description
                   (gethash ':description values)))
                (instructions
                  (skill--validate-instructions
                   (gethash ':instructions values)
                   instruction-character-limit))
                (workflow
                  (skill--validate-workflow
                   (gethash ':workflow values)))
                (workflow-self-tools-p nil))
            (setf workflow-self-tools-p
                  (skill--validate-workflow-self-tools
                   (gethash ':workflow-self-tools values)
                   (nth-value 1
                              (gethash ':workflow-self-tools values))
                   workflow))
            (unless (eql version 1)
              (skill--definition-fail
               :invalid-version
               "SKILL.sexp :version must be the integer 1."))
            (unless (string= name (or (skill--directory-name pathname) ""))
              (skill--definition-fail
               :name-directory-mismatch
               "Skill name ~S must match its containing directory name."
               name))
            (values name
                    description
                    instructions
                    workflow
                    workflow-self-tools-p
                    canonical-pathname
                    device
                    inode
                    *skill-definition-source-character-count*)))))))

(-> skill--load-metadata
    (pathname pathname (integer 0)
     &key (:file-character-limit (integer 1))
          (:aggregate-limit-p boolean))
    (values (option skill-metadata)
            (option skill-diagnostic)
            (integer 0)))
(defun skill--load-metadata
    (pathname root root-index
     &key
       (file-character-limit *skill-file-character-limit*)
       aggregate-limit-p)
  "Return metadata or one typed diagnostic for PATHNAME."
  (handler-case
      (multiple-value-bind
            (name description instructions workflow workflow-self-tools-p
             canonical-pathname device inode source-character-count)
          (skill--parse-definition
           pathname
           :instruction-character-limit *skill-file-character-limit*
           :file-character-limit file-character-limit
           :root root)
        (declare (ignore instructions))
        (values
         (make-instance 'skill-metadata
                        :name name
                        :description description
                        :workflow workflow
                        :workflow-self-tools-p workflow-self-tools-p
                        :pathname pathname
                        :canonical-pathname canonical-pathname
                        :device device
                        :inode inode
                        :root root
                        :root-index root-index)
         nil
         source-character-count))
    (skill--definition-error (condition)
      (values
       nil
       (skill--diagnostic
        :kind
        (if (and aggregate-limit-p
                 (eq (skill--definition-error-kind condition)
                     ':file-too-large))
            ':scan-character-limit
            (skill--definition-error-kind condition))
        :pathname pathname
        :root-index root-index
        :message
        (if (and aggregate-limit-p
                 (eq (skill--definition-error-kind condition)
                     ':file-too-large))
            "The aggregate skill discovery character budget was exhausted."
            (skill--definition-error-message condition)))
       (skill--definition-error-source-character-count condition)))))


;;;; -- Catalog Assembly and Fresh Reads --

(-> skill-catalog-discover
    (list
     &key (:max-depth (integer 0))
          (:max-directories (integer 1))
          (:max-entries (integer 1))
          (:max-characters (integer 1)))
    skill-catalog)
(defun skill-catalog-discover
    (roots
     &key
       (max-depth *skill-scan-depth-limit*)
       (max-directories *skill-scan-directory-limit*)
       (max-entries *skill-scan-entry-limit*)
       (max-characters *skill-discovery-character-limit*))
  "Discover skills beneath ordered ROOTS, with earlier roots taking precedence."
  (let ((skills nil)
        (diagnostics nil)
        (reserved (make-hash-table :test #'equal))
        (remaining-directories max-directories)
        (remaining-entries max-entries)
        (remaining-characters max-characters)
        (character-budget-exhausted-p nil))
    (loop for root-designator in roots
          for root-index from 0
          for root = (uiop:ensure-directory-pathname
                      (pathname root-designator))
          do
             (when (or (zerop remaining-directories)
                       (zerop remaining-entries)
                       (zerop remaining-characters))
               (push
                (skill--diagnostic
                 :kind
                 (cond
                   ((zerop remaining-directories)
                    ':scan-directory-limit)
                   ((zerop remaining-entries)
                    ':scan-entry-limit)
                   (t
                    ':scan-character-limit))
                 :pathname root
                 :root-index root-index
                 :message
                 "The aggregate skill discovery budget was exhausted before this root.")
                diagnostics)
               (loop-finish))
             (multiple-value-bind
                   (pathnames scan-diagnostics directories entries)
                 (skill--scan-root
                  root
                  root-index
                  :max-depth max-depth
                  :max-directories remaining-directories
                  :max-entries remaining-entries)
               (decf remaining-directories directories)
               (decf remaining-entries entries)
               (dolist (diagnostic scan-diagnostics)
                 (push diagnostic diagnostics))
               (let ((events
                       (append
                        (mapcar
                         (lambda (pathname)
                           (cons ':pathname pathname))
                         pathnames)
                        (loop for diagnostic in scan-diagnostics
                              when
                              (skill-diagnostic-reservation-name diagnostic)
                                collect (cons ':diagnostic diagnostic)))))
                 (setf events
                       (sort
                        events
                        #'skill--pathname<
                        :key
                        (lambda (event)
                          (if (eq (first event) ':pathname)
                              (rest event)
                              (skill-diagnostic-pathname (rest event))))))
                 (dolist (event events)
                   (when (zerop remaining-characters)
                     (push
                      (skill--diagnostic
                       :kind ':scan-character-limit
                       :pathname
                       (if (eq (first event) ':pathname)
                           (rest event)
                           (skill-diagnostic-pathname (rest event)))
                       :root-index root-index
                       :message
                       "The aggregate skill discovery character budget was exhausted.")
                      diagnostics)
                     (setf character-budget-exhausted-p t)
                     (loop-finish))
                   (if (eq (first event) ':diagnostic)
                       (let* ((diagnostic (rest event))
                              (name
                                (skill-diagnostic-reservation-name
                                 diagnostic)))
                         (when (and (skill--valid-name-p name)
                                    (null (gethash name reserved)))
                           (setf (gethash name reserved)
                                 (skill-diagnostic-pathname diagnostic))))
                       (let* ((pathname (rest event))
                              (directory-name
                                (skill--directory-name pathname))
                              (reservation
                                (and
                                 (skill--valid-name-p directory-name)
                                 (gethash directory-name reserved)))
                              (file-character-limit
                                (min *skill-file-character-limit*
                                     remaining-characters)))
                         (multiple-value-bind
                               (metadata diagnostic source-character-count)
                             (skill--load-metadata
                              pathname
                              root
                              root-index
                              :file-character-limit file-character-limit
                              :aggregate-limit-p
                              (<= remaining-characters
                                  *skill-file-character-limit*))
                           (decf remaining-characters
                                 (min remaining-characters
                                      source-character-count))
                           (when (and diagnostic
                                      (eq
                                       (skill-diagnostic-kind diagnostic)
                                       ':scan-character-limit))
                             (setf character-budget-exhausted-p t))
                           (cond
                             (diagnostic
                              (push diagnostic diagnostics)
                              (when
                                  (and
                                   (skill--valid-name-p directory-name)
                                   (null reservation))
                                (setf
                                 (gethash directory-name reserved)
                                 pathname)))
                             (reservation
                              (push
                               (skill--diagnostic
                                :kind ':shadowed
                                :pathname pathname
                                :root-index root-index
                                :message
                                (format nil
                                        "Skill ~A is blocked by earlier ~A."
                                        (skill-metadata-name metadata)
                                        (namestring reservation)))
                               diagnostics))
                             (t
                              (setf
                               (gethash
                                (skill-metadata-name metadata)
                                reserved)
                               pathname)
                              (push metadata skills))))))))
               (when character-budget-exhausted-p
                 (loop-finish))))
    (make-instance 'skill-catalog
                   :skills (nreverse skills)
                   :diagnostics (nreverse diagnostics))))

(-> skill-catalog-find (skill-catalog string) (option skill-metadata))
(defun skill-catalog-find (catalog name)
  "Return the selected skill named NAME from CATALOG, if present."
  (find name
        (skill-catalog-skills catalog)
        :key #'skill-metadata-name
        :test #'string=))

(-> skill-metadata-same-definition-p
    (skill-metadata skill-metadata)
    boolean)
(defun skill-metadata-same-definition-p (left right)
  "Return true when LEFT and RIGHT identify the same discovered definition."
  (and (= (skill-metadata-root-index left)
          (skill-metadata-root-index right))
       (uiop:pathname-equal
        (skill-metadata-pathname left)
        (skill-metadata-pathname right))
       (uiop:pathname-equal
        (skill-metadata-canonical-pathname left)
        (skill-metadata-canonical-pathname right))
       (= (skill-metadata-device left)
          (skill-metadata-device right))
       (= (skill-metadata-inode left)
          (skill-metadata-inode right))
       t))

(-> skill-metadata-read (skill-metadata) string)
(defun skill-metadata-read (metadata)
  "Read and return METADATA's complete current instruction string."
  (let ((pathname (skill-metadata-pathname metadata)))
    (handler-case
        (multiple-value-bind
              (name description instructions workflow workflow-self-tools-p
               canonical-pathname device inode)
            (skill--parse-definition
             pathname
             :root (skill-metadata-root metadata))
          (declare (ignore description))
          (declare (ignore workflow workflow-self-tools-p))
          (unless (string= name (skill-metadata-name metadata))
            (skill--definition-fail
             :invalid-name
             "Selected skill changed its name from ~S to ~S."
             (skill-metadata-name metadata)
             name))
          (unless
              (and
               (uiop:pathname-equal
                canonical-pathname
                (skill-metadata-canonical-pathname metadata))
               (= device (skill-metadata-device metadata))
               (= inode (skill-metadata-inode metadata)))
            (skill--definition-fail
             :identity-changed
             "Selected SKILL.sexp changed filesystem identity."))
          instructions)
      (skill--definition-error (condition)
        (if (eq (skill--definition-error-kind condition) ':file-too-large)
            (error 'skill-body-too-large
                   :message (skill--definition-error-message condition)
                   :pathname pathname
                   :cause condition
                   :character-limit
                   (min *skill-file-character-limit*
                        *skill-instruction-character-limit*))
            (error 'skill-read-error
                   :message (skill--definition-error-message condition)
                   :pathname pathname
                   :cause condition)))
      (error (condition)
        (error 'skill-read-error
               :message (format nil
                                "Could not read selected skill ~A: ~A"
                                (namestring pathname)
                                condition)
               :pathname pathname
               :cause condition)))))

(-> skill-metadata-current-workflow
    (skill-metadata)
    (option non-empty-string))
(defun skill-metadata-current-workflow (metadata)
  "Return METADATA's current workflow after revalidating its definition identity."
  (let ((pathname (skill-metadata-pathname metadata)))
    (handler-case
        (multiple-value-bind
              (name description instructions workflow workflow-self-tools-p
               canonical-pathname device inode)
            (skill--parse-definition
             pathname
             :root (skill-metadata-root metadata))
          (declare (ignore description instructions workflow-self-tools-p))
          (unless (and (string= name (skill-metadata-name metadata))
                       (uiop:pathname-equal
                        canonical-pathname
                        (skill-metadata-canonical-pathname metadata))
                       (= device (skill-metadata-device metadata))
                       (= inode (skill-metadata-inode metadata)))
            (skill--definition-fail
             :identity-changed
             "Workflow Skill definition changed filesystem identity."))
          workflow)
      (skill--definition-error (condition)
        (error 'skill-read-error
               :message (skill--definition-error-message condition)
               :pathname pathname
               :cause condition)))))


;;;; -- Logical-Turn Selection --

(-> call-with-skill-logical-turn (user-message-input function) t)
(defun call-with-skill-logical-turn (input function)
  "Call FUNCTION with an empty, dynamically scoped skill selection."
  (declare (ignore input))
  (let ((*skill-logical-turn-active-p* t)
        (*skill-logical-turn-selection-names* nil)
        (*skill-logical-turn-selection-metadata* nil))
    (funcall function)))

(-> skill--logical-turn-record (skill-metadata) boolean)
(defun skill--logical-turn-record (metadata)
  "Record exact skill METADATA in the active logical turn.

Return true only when its name was newly added. Signal SKILL-SELECTION-ERROR
when there is no active logical turn in which request-local selection can
survive."
  (unless *skill-logical-turn-active-p*
    (error 'skill-selection-error
           :message
           "A skill can be selected only while an agent turn is active."
           :name (skill-metadata-name metadata)
           :reason ':inactive-turn))
  (let ((name (skill-metadata-name metadata)))
    (if (member name *skill-logical-turn-selection-names* :test #'string=)
        nil
        (progn
          (when (>= (length *skill-logical-turn-selection-names*)
                    *skill-selection-count-limit*)
            (error 'skill-selection-error
                   :message
                   (format nil
                           "A logical turn may select at most ~D distinct skills."
                           *skill-selection-count-limit*)
                   :name name
                   :reason ':selection-limit))
          (setf *skill-logical-turn-selection-names*
                (append *skill-logical-turn-selection-names* (list name))
                *skill-logical-turn-selection-metadata*
                (append
                 *skill-logical-turn-selection-metadata*
                 (list metadata)))
          t))))

(-> skill-record-steering-input ((or string user-message-input)) null)
(defun skill-record-steering-input (input)
  "Leave skill selection unchanged when steering input arrives."
  (declare (ignore input))
  nil)

(-> skill-select-for-logical-turn
    (configuration string)
    (values skill-metadata boolean))
(defun skill-select-for-logical-turn (configuration name)
  "Select exact discovered skill NAME for CONFIGURATION's active logical turn.

Return the selected metadata and true when this call newly selected it. Only
SKILL.LOAD selects a skill; catalog text and durable conversation text do not."
  (unless *skill-logical-turn-active-p*
    (error 'skill-selection-error
           :message
           "A skill can be selected only while an agent turn is active."
           :name name
           :reason ':inactive-turn))
  (let ((metadata
          (skill-catalog-find
           (skill-catalog-for-configuration configuration)
           name)))
    (unless metadata
      (error 'skill-selection-error
             :message
             (format nil
                     "No discovered skill has the exact case-sensitive name ~S."
                     name)
             :name name
             :reason ':unknown-skill))
    (values metadata (skill--logical-turn-record metadata))))

(-> skill-catalog-select-names (skill-catalog list) list)
(defun skill-catalog-select-names (catalog names)
  "Return metadata selected by NAMES in deterministic CATALOG order."
  (remove-if-not
   (lambda (metadata)
     (member (skill-metadata-name metadata) names :test #'string=))
   (skill-catalog-skills catalog)))


;;;; -- Model-Visible Catalog --

(-> skill--catalog-prefix () string)
(defun skill--catalog-prefix ()
  "Return the fixed model-visible skill catalog introduction."
  (format nil
          "## Skills~2%An Autolith skill is a reusable instruction set stored in one native SKILL.sexp form. The entries below contain metadata and exact file locations only. Descriptions may be shortened to keep this catalog bounded.~2%### Available skills~%"))

(-> skill--catalog-guidance () string)
(defun skill--catalog-guidance ()
  "Return concise skill selection and progressive-disclosure guidance."
  (format nil
          "~%### Skill rules~%When the user names a listed skill or the task clearly matches a description, call `skill.load` with its exact name before other task actions. Call it once for every applicable skill. Do not read SKILL.sexp through `resource.read`; `skill.load` makes Autolith inject only its :instructions string ephemerally into subsequent provider requests in this logical turn. Do not carry a skill into later turns unless it is selected again.~2%Before acting, read every selected instruction string completely from request-local context. Resolve linked relative paths from the SKILL.sexp directory and load only resources needed for the task. Prefer provided scripts and assets. A marker beginning `[workflow` means the primary agent may call `skill.run` after applying the selected instructions; `no-self` means the workflow cannot call active-image inspection or mutation tools. Never execute an instruction-only skill. If a skill cannot be read or applied, state that briefly and continue with the best fallback."))

(-> skill--catalog-line (skill-metadata &key (:description (option string)))
    string)
(defun skill--catalog-line (metadata &key description)
  "Render one METADATA line with an optional DESCRIPTION."
  (format nil
          "- ~A~:[~; [workflow~:[ no-self~;~]]~]~@[: ~A~] (file: ~A)"
          (skill-metadata-name metadata)
          (skill-metadata-workflow metadata)
          (skill-metadata-workflow-self-tools-p metadata)
          description
          (namestring (skill-metadata-pathname metadata))))

(-> skill--catalog-omission-line ((integer 1)) string)
(defun skill--catalog-omission-line (count)
  "Render a notice that COUNT metadata entries did not fit."
  (format nil
          "- ~D additional skill~:P omitted by the catalog character budget."
          count))

(-> skill--catalog-compose (list (integer 0)) string)
(defun skill--catalog-compose (lines omitted-count)
  "Compose catalog LINES and OMITTED-COUNT with the fixed protocol text."
  (with-output-to-string (stream)
    (write-string (skill--catalog-prefix) stream)
    (if lines
        (loop for line in lines
              do (write-string line stream)
                 (terpri stream))
        (when (zerop omitted-count)
          (write-string "- No skills discovered." stream)))
    (when (plusp omitted-count)
      (write-string (skill--catalog-omission-line omitted-count) stream)
      (terpri stream))
    (write-string (skill--catalog-guidance) stream)))

(-> skill-catalog-render
    (skill-catalog &key (:character-budget (integer 1)))
    (values string (integer 0) (integer 0)))
(defun skill-catalog-render
    (catalog &key (character-budget *skill-catalog-character-budget*))
  "Render bounded CATALOG metadata.

Return the rendered text, included metadata count, and omitted metadata count.
The function never retains a skill instruction string."
  (let* ((skills (skill-catalog-skills catalog))
         (minimum
           (skill--catalog-compose
            nil
            (if skills (length skills) 0))))
    (when (> (length minimum) character-budget)
      (error 'skill-catalog-render-error
             :message
             (format nil
                     "Skill catalog budget ~D is below the required ~D characters."
                     character-budget
                     (length minimum))
             :character-budget character-budget
             :minimum-required (length minimum)))
    (if (null skills)
        (values minimum 0 0)
        (let ((selected nil)
              (lines nil))
          (dolist (metadata skills)
            (let* ((candidate-lines
                     (append lines
                             (list (skill--catalog-line metadata))))
                   (omitted
                     (- (length skills) (length candidate-lines)))
                   (rendered
                     (skill--catalog-compose candidate-lines omitted)))
              (when (<= (length rendered) character-budget)
                (setf selected
                      (append selected (list metadata))
                      lines candidate-lines))))
          (let ((omitted (- (length skills) (length selected))))
            (loop for metadata in selected
                  for position from 0
                  for description = (skill-metadata-description metadata)
                  for current = (skill--catalog-compose lines omitted)
                  for available = (- character-budget (length current))
                  for full-line =
                    (skill--catalog-line
                     metadata
                     :description description)
                  for base-line = (nth position lines)
                  for full-cost = (- (length full-line)
                                     (length base-line))
                  do
                     (cond
                       ((<= full-cost available)
                        (setf (nth position lines) full-line))
                       ((>= available 6)
                        (let* ((prefix-length
                                 (min (length description)
                                      (- available 5)))
                               (prefix
                                 (string-right-trim
                                  '(#\Space
                                    #\Tab
                                    #\Newline
                                    #\Return
                                    #\Page)
                                  (subseq description
                                          0
                                          prefix-length))))
                          (when (plusp (length prefix))
                            (setf
                             (nth position lines)
                             (skill--catalog-line
                              metadata
                              :description
                              (concatenate 'string prefix "..."))))))))
            (values (skill--catalog-compose lines omitted)
                    (length selected)
                    omitted))))))


;;;; -- Autolith Skill Roots --

(-> skill-roots (configuration) list)
(defun skill-roots (configuration)
  "Return the effective project, user, and optional bundled skill roots."
  (remove-duplicates
   (list
    (merge-pathnames
     ".autolith/skills/"
     (workspace-project-root
      (configuration-working-directory configuration)))
    (merge-pathnames "skills/"
                     (configuration-config-root configuration))
    (merge-pathnames "skills/"
                     (configuration-source-root configuration)))
   :test #'equal
   :from-end nil))

(-> skill-catalog-for-configuration (configuration) skill-catalog)
(defun skill-catalog-for-configuration (configuration)
  "Discover the current request's skill catalog for CONFIGURATION."
  (skill-catalog-discover (skill-roots configuration)))


;;;; -- Request-Local Skill Instructions --

(-> skill--explicit-instruction (skill-metadata string) string)
(defun skill--explicit-instruction (metadata instructions)
  "Return a request-local contribution containing selected INSTRUCTIONS."
  (format nil
          "Skill ~A is selected for this request. Its complete current :instructions string from ~A follows. Apply it for this request only; do not carry it into later turns unless selected again.~2%~A"
          (skill-metadata-name metadata)
          (namestring (skill-metadata-pathname metadata))
          instructions))

(-> skill--read-failure-instruction (skill-metadata skill-read-error) string)
(defun skill--read-failure-instruction (metadata condition)
  "Return an ephemeral warning for a selected unreadable skill."
  (format nil
          "Skill ~A was selected for this request but could not be read from ~A: ~A Continue with the best fallback and do not claim that its instructions were applied."
          (skill-metadata-name metadata)
          (namestring (skill-metadata-pathname metadata))
          condition))

(-> skill--diagnostic-summary (skill-catalog) (option string))
(defun skill--diagnostic-summary (catalog)
  "Return a bounded summary of non-routine CATALOG diagnostics."
  (let* ((diagnostics
           (remove ':missing-root
                   (skill-catalog-diagnostics catalog)
                   :key #'skill-diagnostic-kind))
         (counts nil))
    (dolist (diagnostic diagnostics)
      (let* ((kind (skill-diagnostic-kind diagnostic))
             (entry (assoc kind counts)))
        (if entry
            (incf (rest entry))
            (push (cons kind 1) counts))))
    (when counts
      (format nil
              "~D skill entr~:@P had diagnostics and were not silently applied (~{~(~A~): ~D~^, ~}). Run /skills for bounded details."
              (length diagnostics)
              (loop for (kind . count) in (nreverse counts)
                    append (list kind count))))))

(-> skill--catalog-instruction (skill-catalog) string)
(defun skill--catalog-instruction (catalog)
  "Render CATALOG with a bounded summary of discovery diagnostics."
  (multiple-value-bind (rendered included omitted)
      (skill-catalog-render catalog)
    (declare (ignore included omitted))
    (let ((summary (skill--diagnostic-summary catalog)))
      (if (and summary
               (<= (+ (length rendered) 2 (length summary))
                   *skill-catalog-character-budget*))
          (format nil "~A~2%~A" rendered summary)
          rendered))))

(-> skill--context-identifier (string string) string)
(defun skill--context-identifier (prefix name)
  "Return a stable context contribution identifier from PREFIX and skill NAME."
  (format nil "~A-~A" prefix name))

(-> skill--truncate-string (string (integer 1)) string)
(defun skill--truncate-string (text character-limit)
  "Return TEXT truncated to exactly CHARACTER-LIMIT characters at most."
  (if (<= (length text) character-limit)
      text
      (if (<= character-limit 3)
          (subseq text 0 character-limit)
          (concatenate
           'string
           (subseq text 0 (- character-limit 3))
           "..."))))

(-> skill--bounded-warning (string) string)
(defun skill--bounded-warning (warning)
  "Return WARNING truncated to the exact request-local warning limit."
  (skill--truncate-string warning *skill-warning-character-limit*))

(-> skill--warning-contribution (skill-metadata string) context-contribution)
(defun skill--warning-contribution (metadata warning)
  "Return one mandatory request-local WARNING for selected skill METADATA."
  (make-context-contribution
   :identifier
   (skill--context-identifier "skill-warning" (skill-metadata-name metadata))
   :instruction (skill--bounded-warning warning)
   :priority 910
   :class ':mandatory
   :deduplication-key
   (skill--context-identifier "skill-warning" (skill-metadata-name metadata))))

(-> skill--missing-selection-contribution (string) context-contribution)
(defun skill--missing-selection-contribution (name)
  "Return a request-local warning when selected skill NAME disappeared."
  (let ((identifier (skill--context-identifier "skill-warning" name)))
    (make-context-contribution
     :identifier identifier
     :instruction
     (skill--bounded-warning
      (format nil
              "Skill ~A was selected for this request but its valid SKILL.sexp is no longer discoverable. Continue with the best fallback and do not claim that its instructions were applied."
              name))
     :priority 910
     :class ':mandatory
     :deduplication-key identifier)))

(-> skill--changed-selection-contribution
    (skill-metadata)
    context-contribution)
(defun skill--changed-selection-contribution (metadata)
  "Return a warning when selected METADATA is no longer the catalog winner."
  (skill--warning-contribution
   metadata
   (format nil
           "Skill ~A was selected for this request, but its winning SKILL.sexp path or filesystem identity changed before use. The replacement was not applied. Continue with the best fallback and do not claim that the selected instructions were applied."
           (skill-metadata-name metadata))))

(-> skill--warning-contribution-p (context-contribution) boolean)
(defun skill--warning-contribution-p (contribution)
  "Return true when CONTRIBUTION is one selected-skill warning."
  (let ((identifier (context-contribution-identifier contribution)))
    (and (>= (length identifier) (length "skill-warning-"))
         (string= identifier
                  "skill-warning-"
                  :end1 (length "skill-warning-")
                  :end2 (length "skill-warning-")))))

(-> skill--warning-overflow-contribution ((integer 1) (integer 1))
    context-contribution)
(defun skill--warning-overflow-contribution (omitted-count character-limit)
  "Return one bounded mandatory warning for OMITTED-COUNT collapsed warnings."
  (let ((identifier "skill-warning-overflow"))
    (make-context-contribution
     :identifier identifier
     :instruction
     (skill--truncate-string
      (format nil
              "~D additional selected-skill warning~:P were omitted because their aggregate output reached the ~D-character limit. Treat those skills as unavailable and do not claim their instructions were applied."
              omitted-count
              *skill-warning-aggregate-character-limit*)
      character-limit)
     :priority 910
     :class ':mandatory
     :deduplication-key identifier)))

(-> skill--bound-warning-contributions (list) list)
(defun skill--bound-warning-contributions (contributions)
  "Bound warning payloads in CONTRIBUTIONS and collapse every excess warning."
  (let* ((warnings
           (remove-if-not #'skill--warning-contribution-p contributions))
         (ordinary
           (remove-if #'skill--warning-contribution-p contributions))
         (total
           (loop for warning in warnings
                 sum (length
                      (context-contribution-instruction warning)))))
    (if (<= total *skill-warning-aggregate-character-limit*)
        contributions
        (let ((kept nil)
              (used 0)
              (remaining (length warnings)))
          (dolist (warning warnings)
            (let* ((warning-length
                     (length
                      (context-contribution-instruction warning)))
                   (next-remaining (1- remaining))
                   (overflow
                     (skill--warning-overflow-contribution
                      (max 1 next-remaining)
                      *skill-warning-aggregate-character-limit*))
                   (overflow-length
                     (length
                      (context-contribution-instruction overflow))))
              (if (and (plusp next-remaining)
                       (<= (+ used warning-length overflow-length)
                           *skill-warning-aggregate-character-limit*))
                  (progn
                    (push warning kept)
                    (incf used warning-length)
                    (setf remaining next-remaining))
                  (return))))
          (let* ((omitted (- (length warnings) (length kept)))
                 (available
                   (max 1
                        (- *skill-warning-aggregate-character-limit*
                           used)))
                 (overflow
                   (skill--warning-overflow-contribution omitted available)))
            (append ordinary
                    (nreverse kept)
                    (list overflow)))))))

(-> skill--request-contributions-for-catalog
    (skill-catalog conversation)
    list)
(defun skill--request-contributions-for-catalog (catalog conversation)
  "Return request-local contributions selected from metadata CATALOG."
  (declare (ignore conversation))
  (let* ((skills (skill-catalog-skills catalog))
         (selection-names
           (if *skill-logical-turn-active-p*
               (copy-list *skill-logical-turn-selection-names*)
               nil))
         (selection-metadata
           (if *skill-logical-turn-active-p*
               (copy-list *skill-logical-turn-selection-metadata*)
               nil)))
    (when (or skills selection-names)
      (let ((contributions
              (list (make-context-contribution
                     :identifier "skill-catalog"
                     :instruction (skill--catalog-instruction catalog)
                     :priority 900
                     :class ':mandatory
                     :deduplication-key "skill-catalog")))
            (selected
              (skill-catalog-select-names catalog selection-names))
            (selected-characters 0))
        (dolist (name selection-names)
          (unless (skill-catalog-find catalog name)
            (setf contributions
                  (append
                   contributions
                   (list
                    (skill--missing-selection-contribution name))))))
        (dolist (current-metadata selected)
          (let ((metadata
                  (find
                   (skill-metadata-name current-metadata)
                   selection-metadata
                   :key #'skill-metadata-name
                   :test #'string=)))
            (cond
              ((null metadata)
               nil)
              ((not
                (skill-metadata-same-definition-p
                 metadata
                 current-metadata))
               (setf contributions
                     (append
                      contributions
                      (list
                       (skill--changed-selection-contribution metadata)))))
              (t
               (handler-case
                   (let* ((instructions (skill-metadata-read metadata))
                          (instruction
                            (skill--explicit-instruction
                             metadata
                             instructions))
                          (next-total
                            (+ selected-characters
                               (length instruction))))
                     (if (> next-total
                            *skill-selection-character-limit*)
                         (setf contributions
                               (append
                                contributions
                                (list
                                 (skill--warning-contribution
                                  metadata
                                  (format nil
                                          "Skill ~A was selected but omitted because selected skill instructions exceed the ~D-character aggregate limit. Continue with the best fallback and report the omission."
                                          (skill-metadata-name metadata)
                                          *skill-selection-character-limit*)))))
                         (progn
                           (setf selected-characters next-total)
                           (setf contributions
                                 (append
                                  contributions
                                  (list
                                   (make-context-contribution
                                    :identifier
                                    (skill--context-identifier
                                     "skill-selected"
                                     (skill-metadata-name metadata))
                                    :instruction instruction
                                    :priority 920
                                    :class ':mandatory
                                    :deduplication-key
                                    (skill--context-identifier
                                     "skill-selected"
                                     (skill-metadata-name metadata)))))))))
                 (skill-read-error (condition)
                   (setf contributions
                         (append
                          contributions
                          (list
                           (skill--warning-contribution
                            metadata
                            (skill--read-failure-instruction
                             metadata
                             condition)))))))))))
        (skill--bound-warning-contributions contributions)))))

(-> skill-request-contributions
    (configuration conversation)
    list)
(defun skill-request-contributions (configuration conversation)
  "Return catalog and selected skill contributions for one provider request."
  (skill--request-contributions-for-catalog
   (skill-catalog-for-configuration configuration)
   conversation))

(-> skill-request-instructions
    (configuration conversation)
    list)
(defun skill-request-instructions (configuration conversation)
  "Return ephemeral skill developer instructions for one provider request.

The returned catalog and selected instructions are assembled fresh and are
never written to conversation history, memories, summaries, or saved images."
  (mapcar #'context-contribution-instruction
          (skill-request-contributions configuration conversation)))

(-> skill--load-tool-visible-p (request-context) boolean)
(defun skill--load-tool-visible-p (request)
  "Return true when REQUEST exposes the exact skill.load tool."
  (not
   (null
    (loop for namespace
            across (request-context-tool-namespaces request)
          for namespace-name =
            (and
             (json-object-p namespace)
             (json-get namespace "name"))
          for tools =
            (and
             (json-object-p namespace)
             (json-get namespace "tools"))
          thereis
          (and
           (stringp namespace-name)
           (string= namespace-name "skill")
           (vectorp tools)
           (loop for tool across tools
                 for tool-name =
                   (and
                    (json-object-p tool)
                    (json-get tool "name"))
                 thereis
                 (and
                  (stringp tool-name)
                  (string= tool-name "load"))))))))

(-> skill-context-contributor (request-context) list)
(defun skill-context-contributor (request)
  "Return request-local skill contributions for normal provider REQUEST."
  (unless (or (request-context-compaction-p request)
              (not (skill--load-tool-visible-p request)))
    (skill-request-contributions
     (request-context-configuration request)
     (request-context-conversation request))))

(register-context-contributor
 "skills" 'skill-context-contributor :source ':built-in)


;;;; -- Skill Status --

(-> skill--diagnostic-line (skill-diagnostic) string)
(defun skill--diagnostic-line (diagnostic)
  "Return one concise human-readable skill DIAGNOSTIC line."
  (format nil
          "~(~A~)  ~A  ~A"
          (skill-diagnostic-kind diagnostic)
          (namestring (skill-diagnostic-pathname diagnostic))
          (skill-diagnostic-message diagnostic)))

(-> skill-status (configuration) string)
(defun skill-status (configuration)
  "Return discovered skills and diagnostics for CONFIGURATION."
  (let* ((catalog (skill-catalog-for-configuration configuration))
         (skills (skill-catalog-skills catalog))
         (diagnostics
           (remove ':missing-root
                   (skill-catalog-diagnostics catalog)
                   :key #'skill-diagnostic-kind))
         (shown-skills
           (subseq skills
                   0
                   (min (length skills)
                        *skill-status-entry-limit*)))
         (shown-diagnostics
           (subseq diagnostics
                   0
                   (min (length diagnostics)
                        *skill-status-entry-limit*)))
         (omitted-skills (- (length skills) (length shown-skills)))
         (omitted-diagnostics
           (- (length diagnostics) (length shown-diagnostics))))
    (with-output-to-string (stream)
      (format stream "Skills:~%")
      (if shown-skills
          (loop for metadata in shown-skills
                for first-p = t then nil
                do
                   (unless first-p
                     (terpri stream))
                   (format stream
                           "~A~:[~;  workflow~:[ no-self~;~]~]  ~A~%  ~A"
                           (skill-metadata-name metadata)
                           (skill-metadata-workflow metadata)
                           (skill-metadata-workflow-self-tools-p metadata)
                           (namestring (skill-metadata-pathname metadata))
                           (skill-metadata-description metadata)))
          (format stream
                  "none discovered~%~%Create a directory named for the skill with one SKILL.sexp beneath one of:~%~{  ~A~^~%~}"
                  (mapcar #'namestring (skill-roots configuration))))
      (when (plusp omitted-skills)
        (format stream
                "~%... ~D more skill~:P omitted."
                omitted-skills))
      (when shown-diagnostics
        (format stream
                "~2%Diagnostics:~%~{~A~^~%~}"
                (mapcar #'skill--diagnostic-line shown-diagnostics)))
      (when (plusp omitted-diagnostics)
        (format stream
                "~%... ~D more diagnostic~:P omitted."
                omitted-diagnostics)))))
