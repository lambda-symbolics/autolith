(in-package #:autolith)

(load (merge-pathnames "../script/roots.lisp" *load-truename*))

;;;; -- Recovery State --

(defparameter *recovery-image-protocol-version* 2
  "The launcher handshake version implemented by this pristine recovery image.")

(defparameter *recovery-rollback-status* 75
  "The active-process status requesting an explicitly selected rollback.")

(defclass recovery-context ()
  ((source-root
    :initarg :source-root
    :reader recovery-context-source-root
    :type pathname
    :documentation "The stable Autolith source checkout containing Git history.")
   (generation-root
    :initarg :generation-root
    :reader recovery-context-generation-root
    :type pathname
    :documentation "The retained generation directory.")
   (worktree-root
    :initarg :worktree-root
    :reader recovery-context-worktree-root
    :type pathname
    :documentation "The directory for exact-commit recovery worktrees.")
   (state-root
    :initarg :state-root
    :reader recovery-context-state-root
    :type pathname
    :documentation "The Autolith state directory containing selection and journals.")
   (current-pathname
    :initarg :current-pathname
    :reader recovery-context-current-pathname
    :type pathname
    :documentation "The atomically selected generation record."))
  (:documentation "Stable paths available to the pristine recovery image."))

(deftype recovery-generation ()
  "One retained generation as this image sees it."
  'sbcl-generations:generation)

(serapeum:-> recovery-generation-identifier (recovery-generation) string)
(defun recovery-generation-identifier (generation)
  "Return GENERATION's validated retained identifier."
  (sbcl-generations:generation-identifier generation))

(serapeum:-> recovery-generation-core-pathname (recovery-generation) pathname)
(defun recovery-generation-core-pathname (generation)
  "Return GENERATION's contained saved core pathname."
  (sbcl-generations:generation-core-pathname generation))

(serapeum:-> recovery-generation-manifest-pathname (recovery-generation) pathname)
(defun recovery-generation-manifest-pathname (generation)
  "Return GENERATION's validated manifest pathname."
  (sbcl-generations:generation-manifest-pathname generation))

(serapeum:-> recovery-generation-created-at (recovery-generation) integer)
(defun recovery-generation-created-at (generation)
  "Return the universal time at which GENERATION was created."
  (sbcl-generations:generation-created-at generation))

(serapeum:-> recovery-generation-property (recovery-generation symbol) t)
(defun recovery-generation-property (generation key)
  "Return GENERATION's manifest property KEY.

The Autolith fields live in the manifest properties rather than in slots, because
the library that reads a generation owns only the fields every host shares."
  (getf (sbcl-generations:generation-metadata generation) key))

(serapeum:-> recovery-generation-reconstruction-pathname
    (recovery-generation)
    (or null pathname))
(defun recovery-generation-reconstruction-pathname (generation)
  "Return GENERATION's contained reconstruction script, when it has one."
  (let ((value (recovery-generation-property generation :reconstruction)))
    (and (stringp value) (pathname value))))

(serapeum:-> recovery-generation-image-commit-identifier
    (recovery-generation)
    (or null string))
(defun recovery-generation-image-commit-identifier (generation)
  "Return the private image commit GENERATION captured, when it captured one."
  (recovery-generation-property generation :image-commit))

(serapeum:-> recovery-generation-mutation-history-commit
    (recovery-generation)
    (or null string))
(defun recovery-generation-mutation-history-commit (generation)
  "Return the private Git commit retaining GENERATION's captured image state."
  (recovery-generation-property generation :mutation-history-commit))

(serapeum:-> recovery-generation-git-commit (recovery-generation) string)
(defun recovery-generation-git-commit (generation)
  "Return the exact source revision paired with GENERATION's core."
  (recovery-generation-property generation :git-commit))

(serapeum:-> recovery-generation-sbcl-version (recovery-generation) string)
(defun recovery-generation-sbcl-version (generation)
  "Return the SBCL version that wrote GENERATION's core."
  (recovery-generation-property generation :sbcl-version))

(serapeum:-> recovery-generation-operating-system (recovery-generation) string)
(defun recovery-generation-operating-system (generation)
  "Return the operating system type that wrote GENERATION's core."
  (recovery-generation-property generation :operating-system))

(serapeum:-> recovery-generation-operating-system-version
    (recovery-generation)
    string)
(defun recovery-generation-operating-system-version (generation)
  "Return the operating system build that wrote GENERATION's core."
  (recovery-generation-property generation :operating-system-version))

(serapeum:-> recovery-generation-architecture (recovery-generation) string)
(defun recovery-generation-architecture (generation)
  "Return the machine architecture that wrote GENERATION's core."
  (recovery-generation-property generation :architecture))

(defclass recovery-terminal-state ()
  ((settings
    :initarg :settings
    :reader recovery-terminal-state-settings
    :type (or null string)
    :documentation "The trusted STTY settings captured before a retained core starts."))
  (:documentation "Terminal state restored between retained-generation attempts."))

(serapeum:-> recovery-environment-directory (string pathname) pathname)
(defun recovery-environment-directory (variable fallback)
  "Return absolute directory VARIABLE, or FALLBACK when it is unset or invalid."
  (let* ((value (uiop:getenv variable))
         (pathname (and value
                        (plusp (length value))
                        (pathname value))))
    (uiop:ensure-directory-pathname
     (if (and pathname (uiop:absolute-pathname-p pathname))
         pathname
         fallback))))

(serapeum:-> recovery-context-create (pathname) recovery-context)
(defun recovery-context-create (source-root)
  "Return recovery context rooted at SOURCE-ROOT and XDG user directories."
  (let* ((data-root (autolith-application-root :data))
         (state-root (autolith-application-root :state)))
    (make-instance
     'recovery-context
     :source-root (uiop:ensure-directory-pathname source-root)
     :generation-root (merge-pathnames "generations/" data-root)
     :worktree-root (merge-pathnames "recovery-worktrees/" data-root)
     :state-root state-root
     :current-pathname (merge-pathnames "current-generation.sexp" state-root))))

(serapeum:-> recovery-terminal-state-capture () recovery-terminal-state)
(defun recovery-terminal-state-capture ()
  "Capture the current terminal settings without failing on non-terminal input."
  (make-instance
   'recovery-terminal-state
   :settings
   (handler-case
       (let ((settings
               (string-trim
                '(#\Space #\Tab #\Newline #\Return)
                (uiop:run-program '("stty" "-g")
                                  :input ':interactive
                                  :output ':string
                                  :error-output ':output))))
         (and (plusp (length settings)) settings))
     (error ()
       nil))))

(serapeum:-> recovery-terminal-state-restore (recovery-terminal-state) null)
(defun recovery-terminal-state-restore (state)
  "Restore trusted terminal STATE and disable presentation modes left by a failed core."
  (let ((settings (recovery-terminal-state-settings state)))
    (when settings
      (ignore-errors
        (uiop:run-program (list "stty" settings)
                          :input ':interactive
                          :output nil
                          :error-output nil)))
    (ignore-errors
      (with-open-file (stream #P"/dev/tty"
                              :direction ':output
                              :if-does-not-exist nil
                              :external-format ':utf-8)
        (when stream
          (format stream "~C[?2004l~C[?25h~C[0m"
                  #\Escape #\Escape #\Escape)
          (finish-output stream)))))
  nil)


;;;; -- Safe Data and Presentation --

(serapeum:-> recovery-sanitize-text (t) string)
(defun recovery-sanitize-text (value)
  "Return VALUE as one terminal-safe line without C0, C1, or escape controls."
  (let ((text (if (stringp value) value (princ-to-string value))))
    (map 'string
         (lambda (character)
           (let ((code (char-code character)))
             (if (or (< code 32)
                     (= code 127)
                     (<= 128 code 159))
                 #\Space
                 character)))
         text)))

(serapeum:-> recovery-print-introduction () null)
(defun recovery-print-introduction ()
  "Explain the pristine recovery image before it inspects or boots state."
  (let ((styled-p
          (and (interactive-stream-p *error-output*)
               (null (uiop:getenv "NO_COLOR")))))
    (when styled-p
      (format *error-output* "~C[1;31m" #\Escape))
    (write-line "RECOVERY IMAGE" *error-output*)
    (when styled-p
      (format *error-output* "~C[0m" #\Escape))
    (format *error-output*
            "This pristine image starts after active Autolith fails or when ~
             recovery is requested.~%It inspects the failure and boots a ~
             compatible retained generation or clean source fallback without ~
             loading the damaged active core.~2%")
    (finish-output *error-output*))
  nil)

(serapeum:-> recovery-read-form (pathname) t)
(defun recovery-read-form (pathname)
  "Read exactly one portable form from PATHNAME with reader evaluation disabled."
  (with-open-file (stream pathname :direction ':input :external-format ':utf-8)
    (let ((*read-eval* nil)
          (end-marker (cons nil nil)))
      (let ((form (read stream t nil)))
        (unless (eq (read stream nil end-marker) end-marker)
          (error "Recovery record ~A contains trailing forms." pathname))
        form))))

(serapeum:-> recovery-identifier-p (t) boolean)
(defun recovery-identifier-p (value)
  "Return true for a bounded path-component-safe generation identifier."
  (and (stringp value)
       (plusp (length value))
       (<= (length value) 128)
       (every (lambda (character)
                (or (alphanumericp character) (char= character #\-)))
              value)
       t))

(serapeum:-> recovery-git-commit-p (t) boolean)
(defun recovery-git-commit-p (value)
  "Return true when VALUE is one full hexadecimal Git object identifier."
  (and (stringp value)
       (= (length value) 40)
       (every (lambda (character) (digit-char-p character 16)) value)
       t))

(serapeum:-> recovery-image-commit-identifier-p (t) boolean)
(defun recovery-image-commit-identifier-p (value)
  "Return true when VALUE is a safe private image-commit identifier."
  (and (stringp value)
       (plusp (length value))
       (<= (length value) 128)
       (every (lambda (character)
                (or (alphanumericp character)
                    (find character "-_")))
              value)
       t))

(serapeum:-> recovery-history-commit-p (t) boolean)
(defun recovery-history-commit-p (value)
  "Return true when VALUE is one full private Git object identifier."
  (and (stringp value)
       (member (length value) '(40 64))
       (every (lambda (character) (digit-char-p character 16)) value)
       t))

(serapeum:-> recovery-read-journal-records (pathname) list)
(defun recovery-read-journal-records (pathname)
  "Read complete journal forms from PATHNAME, ignoring an incomplete final form."
  (if (probe-file pathname)
      (with-open-file (stream pathname :direction ':input :external-format ':utf-8)
        (let ((*read-eval* nil)
              (end-marker (cons nil nil))
              (records nil))
          (handler-case
              (loop for record = (read stream nil end-marker)
                    until (eq record end-marker)
                    do (push record records))
            (end-of-file ()
              nil))
          (nreverse records)))
      nil))

(serapeum:-> recovery-report-mutations (recovery-context) null)
(defun recovery-report-mutations (context)
  "Print pending durable mutation identities from CONTEXT's journal."
  (let ((latest (make-hash-table :test #'equal))
        (pathname (merge-pathnames "mutations.sexp"
                                   (recovery-context-state-root context))))
    (handler-case
        (progn
          (dolist (record (recovery-read-journal-records pathname))
            (when (and (listp record)
                       (eq (first record) :mutation)
                       (eq (getf (rest record) :kind) :durable-definition)
                       (recovery-identifier-p (getf (rest record) :id)))
              (setf (gethash (getf (rest record) :id) latest) record)))
          (let ((pending
                  (loop for record being the hash-values of latest
                        unless (member (getf (rest record) :result)
                                       '(:durable :failed :superseded)
                                       :test #'eq)
                          collect record)))
            (when pending
              (format *error-output* "Pending durable mutations: ~D~%"
                      (length pending))
              (dolist (record pending)
                (format *error-output* "  ~A  ~A  ~A~%"
                        (recovery-sanitize-text (getf (rest record) :id))
                        (recovery-sanitize-text (getf (rest record) :result))
                        (recovery-sanitize-text (getf (rest record) :pathname)))))))
      (error (condition)
        (format *error-output* "Could not inspect the mutation journal: ~A~%"
                (recovery-sanitize-text condition)))))
  nil)


;;;; -- Generation Validation --

(serapeum:-> recovery-manifest-pathname
    (recovery-context string)
    pathname)
(defun recovery-manifest-pathname (context identifier)
  "Return IDENTIFIER's contained manifest pathname in CONTEXT."
  (unless (recovery-identifier-p identifier)
    (error "Invalid generation identifier ~A."
           (recovery-sanitize-text identifier)))
  (merge-pathnames
   "manifest.sexp"
   (merge-pathnames (format nil "~A/" identifier)
                    (recovery-context-generation-root context))))

(serapeum:-> recovery-validate-manifest-properties
    (list pathname &key (:expected-identifier t))
    null)
(defun recovery-validate-manifest-properties
    (properties pathname &key expected-identifier)
  "Re-check every Autolith field PROPERTIES must carry at PATHNAME.

This image cannot trust its own state directory, because the active image that
wrote it may be the thing that broke. Nothing is assumed from the fact that a
manifest parsed: the identifier must be a safe path component naming its own
directory, every recorded identity must have the right shape, and every runtime
field must be present."
  (let* ((identifier (getf properties :id))
         (version (getf properties :version))
         (directory (uiop:pathname-directory-pathname pathname))
         (reconstruction-value (getf properties :reconstruction))
         (reconstruction-pathname
           (and (stringp reconstruction-value) (pathname reconstruction-value)))
         (image-commit-identifier (getf properties :image-commit))
         (mutation-history-commit (getf properties :mutation-history-commit)))
    (unless (and (recovery-identifier-p identifier)
                 (or (null expected-identifier)
                     (string= identifier expected-identifier))
                 (string= identifier
                          (first (last (pathname-directory directory))))
                 (or (= version 1)
                     (and reconstruction-pathname
                          (uiop:subpathp reconstruction-pathname directory)
                          (probe-file reconstruction-pathname)))
                 (or (/= version 3)
                     (if image-commit-identifier
                         (and (recovery-image-commit-identifier-p
                               image-commit-identifier)
                              (recovery-history-commit-p
                               mutation-history-commit))
                         (null mutation-history-commit)))
                 (recovery-git-commit-p (getf properties :git-commit))
                 (stringp (getf properties :sbcl-version))
                 (stringp (getf properties :operating-system))
                 (stringp (getf properties :operating-system-version))
                 (stringp (getf properties :architecture))
                 (integerp (getf properties :created-at)))
      (error "Invalid retained generation manifest at ~A."
             (recovery-sanitize-text pathname))))
  nil)

(serapeum:-> recovery-generation-store
    (recovery-context &key (:expected-identifier t))
    sbcl-generations:generation-store)
(defun recovery-generation-store (context &key expected-identifier)
  "Return the generation store this image reads CONTEXT's generations through.

The store keeps the library's plain-Lisp record reader rather than the active
image's, so recovery needs no foreign-function or filesystem library to read a
generation. Its validator carries this image's own stricter checks."
  (sbcl-generations:make-generation-store
   :root (recovery-context-generation-root context)
   :current-pathname (recovery-context-current-pathname context)
   :core-name "autolith.core"
   :temporary-core-name ".autolith.core.tmp"
   :manifest-version 3
   :accepted-manifest-versions '(1 2 3)
   :manifest-validator
   (lambda (properties manifest-pathname)
     (recovery-validate-manifest-properties
      properties manifest-pathname
      :expected-identifier expected-identifier))))

(serapeum:-> recovery-load-generation
    (recovery-context pathname &key (:expected-identifier t))
    recovery-generation)
(defun recovery-load-generation (context pathname &key expected-identifier)
  "Load and validate one generation at PATHNAME inside CONTEXT."
  (unless (and (uiop:subpathp pathname
                              (recovery-context-generation-root context))
               (probe-file pathname))
    (error "Generation manifest is absent or outside the retained root."))
  (handler-case
      (sbcl-generations:generation-load-manifest
       pathname
       (recovery-generation-store context
                                  :expected-identifier expected-identifier))
    (sbcl-generations:checkpoint-error (condition)
      (error "~A"
             (recovery-sanitize-text
              (sbcl-generations:checkpoint-error-message condition))))))

(serapeum:-> recovery-generation-compatible-p (recovery-generation) boolean)
(defun recovery-generation-compatible-p (generation)
  "Return true when GENERATION has a plausible core for this exact SBCL host."
  (sbcl-generations:generation-compatible-p generation))

(serapeum:-> recovery-generation-bootable-p
    (recovery-generation &key (:source-commit (or null string)))
    boolean)
(defun recovery-generation-bootable-p (generation &key source-commit)
  "Return true when GENERATION is compatible and matches SOURCE-COMMIT if given."
  (and (recovery-generation-compatible-p generation)
       (or (null source-commit)
           (string= (recovery-generation-git-commit generation)
                    source-commit))
       t))

(serapeum:-> recovery-generation-list (recovery-context) list)
(defun recovery-generation-list (context)
  "Return valid retained generations in CONTEXT, newest first."
  (let ((generations nil)
        (root (recovery-context-generation-root context)))
    (when (probe-file root)
      (dolist (directory (uiop:subdirectories root))
        (let ((pathname (merge-pathnames "manifest.sexp" directory)))
          (when (probe-file pathname)
            (handler-case
                (push (recovery-load-generation context pathname) generations)
              (error (condition)
                (format *error-output* "Skipping invalid manifest ~A: ~A~%"
                        (recovery-sanitize-text pathname)
                        (recovery-sanitize-text condition))))))))
    (sort generations #'> :key #'recovery-generation-created-at)))

(serapeum:-> recovery-selected-generation (recovery-context) recovery-generation)
(defun recovery-selected-generation (context)
  "Return the generation named by CONTEXT's atomic selection record."
  (let ((pathname (recovery-context-current-pathname context)))
    (unless (probe-file pathname)
      (error "No retained generation is selected."))
    (let* ((record (recovery-read-form pathname))
           (identifier
             (and (listp record)
                  (eq (first record) :current-generation)
                  (getf (rest record) :id)))
           (manifest
             (and (listp record)
                  (eq (first record) :current-generation)
                  (getf (rest record) :manifest))))
      (unless (and (recovery-identifier-p identifier)
                   (stringp manifest)
                   (uiop:subpathp (pathname manifest)
                                  (recovery-context-generation-root context)))
        (error "The selected-generation record is invalid."))
      (recovery-load-generation context
                                (pathname manifest)
                                :expected-identifier identifier))))

(serapeum:-> recovery-newest-compatible-generation
    (recovery-context &key (:source-commit (or null string)))
    (or null recovery-generation))
(defun recovery-newest-compatible-generation (context &key source-commit)
  "Return CONTEXT's newest bootable generation for SOURCE-COMMIT, if any."
  (find-if
   (lambda (generation)
     (recovery-generation-bootable-p
      generation
      :source-commit source-commit))
   (recovery-generation-list context)))

(serapeum:-> recovery-selected-generation-or-fallback
    (recovery-context &key (:source-commit (or null string)))
    (or null recovery-generation))
(defun recovery-selected-generation-or-fallback (context &key source-commit)
  "Return the selected or newest bootable generation for SOURCE-COMMIT, if any."
  (let ((selected
          (handler-case
              (recovery-selected-generation context)
            (error (condition)
              (format *error-output*
                      "Could not load the selected generation: ~A~%"
                      (recovery-sanitize-text condition))
              nil))))
    (if (and selected
             (recovery-generation-bootable-p
              selected
              :source-commit source-commit))
        selected
        (let ((fallback
                (recovery-newest-compatible-generation
                 context
                 :source-commit source-commit)))
          (cond
            ((and selected
                  source-commit
                  (recovery-generation-compatible-p selected)
                  (not
                   (string=
                    (recovery-generation-git-commit selected)
                    source-commit)))
             (format *error-output*
                     "Selected generation ~A belongs to source ~A, not current source ~A.~%"
                     (recovery-sanitize-text
                      (recovery-generation-identifier selected))
                     (recovery-sanitize-text
                      (recovery-generation-git-commit selected))
                     (recovery-sanitize-text source-commit)))
            (selected
             (format *error-output*
                     "Selected generation ~A is incompatible or corrupt.~%"
                     (recovery-sanitize-text
                      (recovery-generation-identifier selected))))
            (t
             (format *error-output*
                     "The selected-generation record is unusable.~%")))
          (if fallback
              (format *error-output*
                      "Using the newest compatible retained generation~:[.~; for current source.~]~%"
                      (not (null source-commit)))
              (format *error-output*
                      "No compatible retained generation~:[~; for current source~] is available.~%"
                      (not (null source-commit))))
          fallback))))

(serapeum:-> recovery-print-generations (recovery-context) null)
(defun recovery-print-generations (context)
  "Print retained generation identifiers, revisions, and replay scripts."
  (let ((generations (recovery-generation-list context)))
    (if generations
        (dolist (generation generations)
          (format t
                  "~A  ~A  source ~A~@[~%  image ~A~]~@[~%  history ~A~]~@[~%  replay ~A~]~%"
                  (recovery-sanitize-text
                   (recovery-generation-identifier generation))
                  (if (recovery-generation-compatible-p generation)
                      "compatible"
                      "incompatible")
                  (recovery-sanitize-text
                   (recovery-generation-git-commit generation))
                  (and (recovery-generation-image-commit-identifier generation)
                       (recovery-sanitize-text
                        (recovery-generation-image-commit-identifier
                         generation)))
                  (and (recovery-generation-mutation-history-commit generation)
                       (recovery-sanitize-text
                        (recovery-generation-mutation-history-commit
                         generation)))
                  (and (recovery-generation-reconstruction-pathname generation)
                       (recovery-sanitize-text
                        (namestring
                         (recovery-generation-reconstruction-pathname
                          generation))))))
        (format t "No retained generations exist.~%")))
  nil)


;;;; -- Crash Context --

(serapeum:-> recovery-clear-reconnection-environment () null)
(defun recovery-clear-reconnection-environment ()
  "Remove retained crash reconnection metadata from the recovery environment."
  (sb-posix:unsetenv "AUTOLITH_RECOVERY_CONVERSATION_ID")
  (sb-posix:unsetenv "AUTOLITH_RECOVERY_RENDERED_SEQUENCE")
  (sb-posix:unsetenv "AUTOLITH_RECOVERY_HISTORY_FLOOR_SEQUENCE")
  nil)

(serapeum:-> recovery-conversation-identifier-display (string) string)
(defun recovery-conversation-identifier-display (identifier)
  "Return a short conversation identifier with its visual hyphen."
  (if (and (= (length identifier) 7)
           (every
            (lambda (character)
              (find character
                    "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
                    :test #'char=))
            identifier))
      (format nil "~A-~A" (subseq identifier 0 1) (subseq identifier 1))
      identifier))

(serapeum:-> recovery-conversation-identifier-stored-p (t) boolean)
(defun recovery-conversation-identifier-stored-p (value)
  "Return true when VALUE is one stored short conversation identifier."
  (and (stringp value)
       (= (length value) 7)
       (every
        (lambda (character)
          (find character
                "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
                :test #'char=))
        value)
       t))

(serapeum:-> recovery-conversation-identifier-normalize-display (string) string)
(defun recovery-conversation-identifier-normalize-display (identifier)
  "Return stored syntax for a displayed short IDENTIFIER, otherwise IDENTIFIER."
  (if (and (= (length identifier) 8)
           (char= (char identifier 1) #\-)
           (recovery-conversation-identifier-stored-p
            (concatenate 'string
                         (subseq identifier 0 1)
                         (subseq identifier 2))))
      (concatenate 'string
                   (subseq identifier 0 1)
                   (subseq identifier 2))
      identifier))

(serapeum:-> recovery-legacy-conversation-identifier-p (t) boolean)
(defun recovery-legacy-conversation-identifier-p (value)
  "Return true when VALUE has the historical UUID conversation ID shape."
  (and (stringp value)
       (= (length value) 36)
       (loop for index below (length value)
             for character = (char value index)
             always (if (member index '(8 13 18 23))
                        (char= character #\-)
                        (not (null (digit-char-p character 16)))))))

(serapeum:-> recovery-proper-list-p (t) boolean)
(defun recovery-proper-list-p (value)
  "Return true when VALUE is a finite proper list."
  (handler-case
      (and (listp value) (or (list-length value) (null value)) t)
    (type-error ()
      nil)))

(serapeum:-> recovery-conversation-migration-entry-p (t) boolean)
(defun recovery-conversation-migration-entry-p (value)
  "Return true when VALUE is one safe legacy conversation mapping entry."
  (and (recovery-proper-list-p value)
       (= (length value) 6)
       (recovery-legacy-conversation-identifier-p (getf value :old))
       (recovery-conversation-identifier-stored-p (getf value :new))
       (integerp (getf value :created-at))
       (not (minusp (getf value :created-at)))
       (loop for key in value by #'cddr
             always (member key '(:old :new :created-at) :test #'eq))))

(serapeum:-> recovery-conversation-migration-entries
    (recovery-context)
    list)
(defun recovery-conversation-migration-entries (context)
  "Return validated durable legacy conversation mappings for CONTEXT."
  (let ((pathname
          (merge-pathnames "conversation-identifier-migration.sexp"
                           (recovery-context-state-root context))))
    (if (probe-file pathname)
        (let* ((record (recovery-read-form pathname))
               (properties (and (recovery-proper-list-p record)
                                (rest record)))
               (entries (and properties (getf properties :entries))))
          (unless (and (recovery-proper-list-p record)
                       (= (length record) 9)
                       (eq (first record) :conversation-identifier-migration)
                       (= (or (getf properties :version) 0) 1)
                       (member (getf properties :status)
                               '(:prepared :conversations :references
                                 :artifacts :complete)
                               :test #'eq)
                       (integerp (getf properties :updated-at))
                       (not (minusp (getf properties :updated-at)))
                       (loop for key in properties by #'cddr
                             always
                             (member key
                                     '(:version :status :updated-at :entries)
                                     :test #'eq))
                       (recovery-proper-list-p entries)
                       (every #'recovery-conversation-migration-entry-p
                              entries)
                       (= (length entries)
                          (length
                           (remove-duplicates
                            entries
                            :test #'string=
                            :key (lambda (entry) (getf entry :old)))))
                       (= (length entries)
                          (length
                           (remove-duplicates
                            entries
                            :test #'string=
                            :key (lambda (entry) (getf entry :new))))))
            (error "The conversation identifier migration record is invalid."))
          entries)
        nil)))

(serapeum:-> recovery-conversation-identifier-resolve
    (recovery-context string)
    string)
(defun recovery-conversation-identifier-resolve (context identifier)
  "Resolve displayed syntax or a durable legacy alias for old retained code."
  (let ((normalized
          (recovery-conversation-identifier-normalize-display identifier)))
    (if (recovery-legacy-conversation-identifier-p normalized)
        (handler-case
            (let ((entry
                    (find normalized
                          (recovery-conversation-migration-entries context)
                          :test #'string=
                          :key (lambda (candidate)
                                 (getf candidate :old)))))
              (if entry (getf entry :new) normalized))
          (error (condition)
            (format *error-output*
                    "Could not resolve legacy conversation ~A: ~A~%"
                    (recovery-sanitize-text normalized)
                    (recovery-sanitize-text condition))
            normalized))
        normalized)))

(serapeum:-> recovery-normalize-forwarded-arguments
    (recovery-context list)
    list)
(defun recovery-normalize-forwarded-arguments (context arguments)
  "Return restart-safe application ARGUMENTS with an explicit resume ID resolved."
  (let ((normalized nil)
        (remaining arguments))
    (loop while remaining
          for argument = (pop remaining)
          do (if (and (stringp argument)
                      (string= argument "--localgroup-handoff"))
                 ;; This one-shot startup record was claimed before the active
                 ;; process failed and cannot be reused by recovery.
                 (when remaining
                   (pop remaining))
                 (push argument normalized)))
    (setf normalized (nreverse normalized))
    (loop for remaining on normalized
          when (and (stringp (first remaining))
                    (string= (first remaining) "resume")
                    (rest remaining)
                    (stringp (second remaining))
                    (plusp (length (second remaining)))
                    (not (uiop:string-prefix-p "-" (second remaining))))
            do (setf (second remaining)
                     (recovery-conversation-identifier-resolve
                      context
                      (second remaining)))
               (return))
    normalized))

(serapeum:-> recovery-publish-reconnection-environment
    (string t &key (:history-floor-sequence t))
    null)
(defun recovery-publish-reconnection-environment
    (conversation-id rendered-sequence &key history-floor-sequence)
  "Publish validated canonical recovery metadata for a child process."
  (when (recovery-identifier-p conversation-id)
    (sb-posix:setenv "AUTOLITH_RECOVERY_CONVERSATION_ID" conversation-id 1))
  (when (and (integerp rendered-sequence)
             (not (minusp rendered-sequence)))
    (sb-posix:setenv "AUTOLITH_RECOVERY_RENDERED_SEQUENCE"
                     (write-to-string rendered-sequence)
                     1))
  (when (and (integerp history-floor-sequence)
             (plusp history-floor-sequence))
    (sb-posix:setenv "AUTOLITH_RECOVERY_HISTORY_FLOOR_SEQUENCE"
                     (write-to-string history-floor-sequence)
                     1))
  nil)

(serapeum:-> recovery-report-crash-capsule
    (recovery-context t)
    (or null string))
(defun recovery-report-crash-capsule (context capsule)
  "Report and publish one valid CAPSULE, returning its normalized pathname."
  (when (and (stringp capsule) (plusp (length capsule)))
    (handler-case
        (let* ((capsule-pathname (pathname capsule))
               (crash-root (merge-pathnames "crashes/"
                                            (recovery-context-state-root context))))
          (unless (and (uiop:subpathp capsule-pathname crash-root)
                       (probe-file capsule-pathname))
            (error "The crash capsule is absent or outside private Autolith state."))
          (let* ((record (recovery-read-form capsule-pathname))
                 (properties (and (listp record) (rest record)))
                 (conversation-id (and properties
                                       (getf properties :conversation-id)))
                 (rendered-sequence (and properties
                                         (getf properties :rendered-sequence)))
                 (history-floor-sequence
                   (and properties
                        (getf properties :history-floor-sequence)))
                 (resolved-conversation-id
                   (and (stringp conversation-id)
                        (recovery-conversation-identifier-resolve
                         context
                         conversation-id))))
            (unless (eq (first record) :crash)
              (error "The crash capsule has an invalid header."))
            (recovery-clear-reconnection-environment)
            (format *error-output*
                    "Crash capsule: ~A~%Condition: ~A~%Conversation: ~A~%"
                    (recovery-sanitize-text capsule-pathname)
                    (recovery-sanitize-text
                     (or (getf properties :condition) "unknown"))
                    (recovery-sanitize-text
                     (if resolved-conversation-id
                         (recovery-conversation-identifier-display
                          resolved-conversation-id)
                         "unknown")))
            (when resolved-conversation-id
              (recovery-publish-reconnection-environment
               resolved-conversation-id
               rendered-sequence
               :history-floor-sequence history-floor-sequence))
            (namestring capsule-pathname)))
      (error (condition)
        (format *error-output* "Could not read crash capsule ~A: ~A~%"
                (recovery-sanitize-text capsule)
                (recovery-sanitize-text condition))
        nil))))

(serapeum:-> recovery-read-crash-pointer
    (recovery-context)
    (or null string))
(defun recovery-read-crash-pointer (context)
  "Return the contained capsule named by this launcher's current pointer."
  (let ((pointer-value (uiop:getenv "AUTOLITH_CRASH_POINTER")))
    (when (and (stringp pointer-value) (plusp (length pointer-value)))
      (let* ((pointer-pathname (pathname pointer-value))
             (pointer-root (merge-pathnames "crash-pointers/"
                                            (recovery-context-state-root context)))
             (crash-root (merge-pathnames "crashes/"
                                          (recovery-context-state-root context))))
        (unless (uiop:subpathp pointer-pathname pointer-root)
          (error "The crash pointer is outside private Autolith state."))
        (when (probe-file pointer-pathname)
          (with-open-file (stream pointer-pathname
                                  :direction ':input
                                  :external-format ':utf-8)
            (let ((capsule (read-line stream nil nil))
                  (trailing-line (read-line stream nil nil)))
              (unless (and (stringp capsule)
                           (plusp (length capsule))
                           (<= (length capsule) 4096)
                           (null trailing-line))
                (error "The crash pointer is not one bounded pathname line."))
              (let ((capsule-pathname (pathname capsule)))
                (unless (and (uiop:subpathp capsule-pathname crash-root)
                             (probe-file capsule-pathname))
                  (error "The crash pointer names an invalid capsule."))
                (namestring capsule-pathname)))))))))

(serapeum:-> recovery-read-session-pointer
    (recovery-context)
    (or null list))
(defun recovery-read-session-pointer (context)
  "Return this launcher's validated recovery-session record, when published."
  (let ((pointer-value
          (uiop:getenv "AUTOLITH_RECOVERY_SESSION_POINTER")))
    (when (and (stringp pointer-value) (plusp (length pointer-value)))
      (let* ((pointer-pathname (pathname pointer-value))
             (pointer-root
               (merge-pathnames "recovery-session-pointers/"
                                (recovery-context-state-root context))))
        (unless (uiop:subpathp pointer-pathname pointer-root)
          (error "The recovery-session pointer is outside private Autolith state."))
        (when (probe-file pointer-pathname)
          (let* ((record (recovery-read-form pointer-pathname))
                 (properties (and (recovery-proper-list-p record)
                                  (rest record)))
                 (conversation-id
                   (and properties (getf properties :conversation-id)))
                 (rendered-sequence
                   (and properties (getf properties :rendered-sequence)))
                 (history-floor-sequence
                   (and properties
                        (getf properties :history-floor-sequence)))
                 (history-floor-present-p
                   (and properties
                        (loop for key in properties by #'cddr
                              thereis (eq key :history-floor-sequence)))))
            (unless (and (recovery-proper-list-p record)
                         (member (length record) '(7 9) :test #'=)
                         (eq (first record) :recovery-session)
                         (= (or (getf properties :version) 0) 1)
                         (stringp conversation-id)
                         (recovery-identifier-p conversation-id)
                         (integerp rendered-sequence)
                         (not (minusp rendered-sequence))
                         (or (not history-floor-present-p)
                             (null history-floor-sequence)
                             (and (integerp history-floor-sequence)
                                  (plusp history-floor-sequence)))
                         (loop for key in properties by #'cddr
                               always
                               (member key
                                       '(:version :conversation-id
                                         :rendered-sequence
                                         :history-floor-sequence)
                                       :test #'eq)))
              (error "The recovery-session pointer record is invalid."))
            record))))))

(serapeum:-> recovery-report-session-pointer (recovery-context) boolean)
(defun recovery-report-session-pointer (context)
  "Publish a valid per-launch session pointer and return whether one existed."
  (handler-case
      (let ((record (recovery-read-session-pointer context)))
        (when record
          (let* ((properties (rest record))
                 (conversation-id (getf properties :conversation-id))
                 (rendered-sequence (getf properties :rendered-sequence))
                 (history-floor-sequence
                   (getf properties :history-floor-sequence))
                 (resolved-conversation-id
                   (recovery-conversation-identifier-resolve
                    context
                    conversation-id)))
            (recovery-clear-reconnection-environment)
            (recovery-publish-reconnection-environment
             resolved-conversation-id
             rendered-sequence
             :history-floor-sequence history-floor-sequence)
            (format *error-output*
                    "Recovery session: ~A~%"
                    (recovery-sanitize-text
                     (recovery-conversation-identifier-display
                      resolved-conversation-id))))
          t))
    (error (condition)
      (format *error-output* "Could not read the recovery-session pointer: ~A~%"
              (recovery-sanitize-text condition))
      nil)))

(serapeum:-> recovery-refresh-crash-context
    (recovery-context (or null string) &key (:session-p boolean))
    (or null string))
(defun recovery-refresh-crash-context
    (context current-capsule &key (session-p t))
  "Publish the newest capsule or per-launch session available after a failed boot."
  (handler-case
      (let ((pointer-capsule (recovery-read-crash-pointer context)))
        (cond
          ((null pointer-capsule)
           (when session-p
             (recovery-report-session-pointer context))
           current-capsule)
          ((and current-capsule (string= pointer-capsule current-capsule))
           (when session-p
             (recovery-report-session-pointer context))
           current-capsule)
          (t
           (format *error-output*
                   "Refreshing recovery context from the latest crash capsule.~%")
           (or (recovery-report-crash-capsule context pointer-capsule)
               current-capsule))))
    (error (condition)
      (format *error-output* "Could not refresh the crash pointer: ~A~%"
              (recovery-sanitize-text condition))
      current-capsule)))

(serapeum:-> recovery-report-crash
    (recovery-context
     &key (:status t) (:capsule t) (:original-arguments list))
    (or null string))
(defun recovery-report-crash
    (context &key status capsule (original-arguments nil))
  "Report bounded crash context and publish safe reconnection metadata."
  (recovery-clear-reconnection-environment)
  (when status
    (format *error-output* "Active Autolith exited with status ~A.~%"
            (recovery-sanitize-text status)))
  (let ((reported-capsule (recovery-report-crash-capsule context capsule)))
    (unless reported-capsule
      (recovery-report-session-pointer context))
    (when original-arguments
      (format *error-output* "Original arguments: ~{~A~^ ~}~%"
              (mapcar #'recovery-sanitize-text original-arguments)))
    (recovery-report-mutations context)
    reported-capsule))


;;;; -- Exact Source and Core Boot --

(serapeum:-> recovery-git-output (pathname list) string)
(defun recovery-git-output (repository arguments)
  "Return trimmed output from one Git command in REPOSITORY."
  (string-trim
   '(#\Space #\Tab #\Newline #\Return)
   (uiop:run-program
    (append (list "git" "-C" (namestring repository)) arguments)
    :output ':string
    :error-output ':output)))

(serapeum:-> recovery-source-commit (recovery-context) string)
(defun recovery-source-commit (context)
  "Return the validated commit currently checked out at CONTEXT's source root."
  (let ((commit
          (recovery-git-output
           (recovery-context-source-root context)
           '("rev-parse" "--verify" "HEAD^{commit}"))))
    (unless (recovery-git-commit-p commit)
      (error "The current source revision is not one full Git commit."))
    commit))

(serapeum:-> recovery-automatic-source-commit
    (recovery-context (or null string) (or null string))
    (or null string))
(defun recovery-automatic-source-commit (context generation status)
  "Return the source revision constraining automatic failure recovery."
  (when (and (null generation)
             status
             (/= (parse-integer status :junk-allowed nil)
                 *recovery-rollback-status*))
    (recovery-source-commit context)))

(serapeum:-> recovery-source-checkout-valid-p (pathname string) boolean)
(defun recovery-source-checkout-valid-p (checkout commit)
  "Return true when CHECKOUT is a clean detached copy of COMMIT."
  (handler-case
      (and (string= (recovery-git-output checkout '("rev-parse" "HEAD"))
                    commit)
           (zerop
            (length
             (recovery-git-output checkout '("status" "--porcelain"))))
           t)
    (error ()
      nil)))

(serapeum:-> recovery-source-checkout
    (recovery-context string string)
    pathname)
(defun recovery-source-checkout (context commit identifier)
  "Return a private clean checkout of COMMIT named by safe IDENTIFIER."
  (unless (and (recovery-git-commit-p commit)
               (recovery-identifier-p identifier))
    (error "Invalid recovery source identity."))
  (let* ((root (recovery-context-worktree-root context))
         (checkout (merge-pathnames (format nil "~A/" identifier) root)))
    (when (and (probe-file checkout)
               (not (recovery-source-checkout-valid-p checkout commit)))
      (uiop:delete-directory-tree checkout
                                  :validate t
                                  :if-does-not-exist ':ignore))
    (unless (probe-file checkout)
      (ensure-directories-exist root)
      (let ((temporary
              (merge-pathnames
               (format nil ".~A.~D.tmp/" identifier (sb-posix:getpid))
               root)))
        (when (probe-file temporary)
          (uiop:delete-directory-tree temporary
                                      :validate t
                                      :if-does-not-exist ':ignore))
        (unwind-protect
             (progn
               (uiop:run-program
                (list "git" "clone" "--quiet" "--no-checkout"
                      "--no-hardlinks"
                      (namestring (recovery-context-source-root context))
                      (namestring temporary))
                :output ':string
                :error-output ':output)
               (recovery-git-output temporary
                                    (list "checkout" "--quiet" "--detach"
                                          commit))
               (unless (recovery-source-checkout-valid-p temporary commit)
                 (error "The private recovery checkout failed validation."))
               (rename-file temporary checkout))
          (when (probe-file temporary)
            (uiop:delete-directory-tree temporary
                                        :validate t
                                        :if-does-not-exist ':ignore)))))
    checkout))

(serapeum:-> recovery-source-worktree
    (recovery-context recovery-generation)
    pathname)
(defun recovery-source-worktree (context generation)
  "Return a private clean checkout of GENERATION's exact source revision."
  (recovery-source-checkout
   context
   (recovery-generation-git-commit generation)
   (recovery-generation-identifier generation)))

(serapeum:-> recovery-project-setup (recovery-context) (or null pathname))
(defun recovery-project-setup (context)
  "Return the usable locked dependency setup for recovery children, if any."
  (let ((override (uiop:getenv "AUTOLITH_PROJECT_SETUP"))
        (source-setup
          (merge-pathnames ".qlot/setup.lisp"
                           (recovery-context-source-root context))))
    (cond
      ((and override (plusp (length override)) (probe-file override))
       (pathname override))
      ((probe-file source-setup)
       source-setup)
      (t
       nil))))

(serapeum:-> recovery-prepare-source-environment
    (recovery-context pathname)
    null)
(defun recovery-prepare-source-environment (context source-root)
  "Point recovered children at SOURCE-ROOT and the original dependency store."
  (sb-posix:setenv "AUTOLITH_SOURCE_ROOT" (namestring source-root) 1)
  (sb-posix:setenv "AUTOLITH_RECOVERED" "1" 1)
  (let ((project-setup (recovery-project-setup context)))
    (if project-setup
        (sb-posix:setenv "AUTOLITH_PROJECT_SETUP"
                        (namestring project-setup)
                        1)
        (sb-posix:unsetenv "AUTOLITH_PROJECT_SETUP")))
  nil)

(serapeum:-> recovery-boot-generation
    (recovery-context recovery-generation list)
    integer)
(defun recovery-boot-generation (context generation forwarded-arguments)
  "Boot GENERATION with FORWARDED-ARGUMENTS and return its process status."
  (unless (recovery-generation-compatible-p generation)
    (error "Generation ~A is incompatible with this SBCL runtime."
           (recovery-sanitize-text
            (recovery-generation-identifier generation))))
  (let* ((worktree (recovery-source-worktree context generation))
         (sbcl-command (or (uiop:getenv "AUTOLITH_SBCL") "sbcl")))
    (recovery-prepare-source-environment context worktree)
    (let ((process
            (uiop:launch-program
             (append
              (list sbcl-command
                    "--noinform"
                    "--core"
                    (namestring (recovery-generation-core-pathname generation))
                    "--end-runtime-options")
              forwarded-arguments)
             :directory worktree
             :input ':interactive
             :output ':interactive
             :error-output ':interactive
             :wait nil)))
      (uiop:wait-process process))))

(serapeum:-> recovery-boot-source
    (recovery-context list)
    integer)
(defun recovery-boot-source (context forwarded-arguments)
  "Boot clean committed source with FORWARDED-ARGUMENTS and return its status."
  (let* ((commit (recovery-source-commit context))
         (identifier (format nil "source-~A" commit))
         (checkout (recovery-source-checkout context commit identifier))
         (launcher (merge-pathnames "bin/autolith-active" checkout))
         (sbcl-command (or (uiop:getenv "AUTOLITH_SBCL") "sbcl"))
         (terminal-state (recovery-terminal-state-capture)))
    (unless (probe-file launcher)
      (error "The clean source checkout lacks bin/autolith-active."))
    (format *error-output*
            "Starting clean committed source ~A.~%"
            (recovery-sanitize-text commit))
    (recovery-prepare-source-environment context checkout)
    (unwind-protect
         (let ((process
                 (uiop:launch-program
                  (append (list sbcl-command
                                "--noinform"
                                "--script"
                                (namestring launcher))
                          forwarded-arguments)
                  :directory checkout
                  :input ':interactive
                  :output ':interactive
                  :error-output ':interactive
                  :wait nil)))
           (uiop:wait-process process))
      (recovery-terminal-state-restore terminal-state))))

(serapeum:-> recovery-boot-source-with-report
    (recovery-context list &key (:capsule (or null string)))
    integer)
(defun recovery-boot-source-with-report (context forwarded-arguments &key capsule)
  "Boot clean committed source and report any secondary recovery failure."
  (let ((status (recovery-boot-source context forwarded-arguments)))
    (unless (recovery-generation-terminal-status-p status)
      (format *error-output*
              "Clean committed source exited with status ~D.~%"
              status)
      (recovery-refresh-crash-context context capsule))
    status))

(serapeum:-> recovery-generation-terminal-status-p (integer) boolean)
(defun recovery-generation-terminal-status-p (status)
  "Return true when STATUS should end retained-generation fallback."
  (not (null (member status '(0 130 143) :test #'=))))

(serapeum:-> recovery-boot-with-fallback
    (recovery-context recovery-generation list
     &key (:capsule (or null string))
          (:source-commit (or null string)))
    integer)
(defun recovery-boot-with-fallback
    (context selected forwarded-arguments &key capsule source-commit)
  "Boot SELECTED and fall back only to generations matching SOURCE-COMMIT."
  (let ((candidates
          (cons selected
                (remove (recovery-generation-identifier selected)
                        (recovery-generation-list context)
                        :key #'recovery-generation-identifier
                        :test #'string=)))
        (terminal-state (recovery-terminal-state-capture))
        (current-capsule
          (recovery-refresh-crash-context
           context capsule :session-p nil)))
    (loop for remaining on candidates
          for generation = (first remaining)
          do (if (recovery-generation-bootable-p
                  generation
                  :source-commit source-commit)
                 (let ((status nil)
                       (completed-p nil))
                   (unwind-protect
                        (handler-case
                            (setf status
                                  (recovery-boot-generation
                                   context
                                   generation
                                   forwarded-arguments)
                                  completed-p t)
                          (error (condition)
                            (format *error-output*
                                    "Could not boot generation ~A: ~A~%"
                                    (recovery-sanitize-text
                                     (recovery-generation-identifier generation))
                                    (recovery-sanitize-text condition))))
                     (recovery-terminal-state-restore terminal-state))
                   (when (and completed-p
                              (recovery-generation-terminal-status-p status))
                     (return-from recovery-boot-with-fallback status))
                   (when completed-p
                     (format *error-output*
                             "Generation ~A exited with status ~D; trying fallback.~%"
                             (recovery-sanitize-text
                              (recovery-generation-identifier generation))
                             status))
                   (when (rest remaining)
                     (setf current-capsule
                           (recovery-refresh-crash-context context
                                                           current-capsule))))
                 (format *error-output*
                         "Skipping ~:[incompatible~;cross-revision~] generation ~A.~%"
                         (and
                          source-commit
                          (recovery-generation-compatible-p generation)
                          (not
                           (string=
                            (recovery-generation-git-commit generation)
                            source-commit)))
                         (recovery-sanitize-text
                          (recovery-generation-identifier generation)))))
    (error "No retained generation could be booted.")))

(serapeum:-> recovery-boot-with-source-fallback
    (recovery-context (or null recovery-generation) list
     &key (:capsule (or null string))
          (:source-commit (or null string)))
    integer)
(defun recovery-boot-with-source-fallback
    (context selected forwarded-arguments &key capsule source-commit)
  "Boot retained state when possible, otherwise boot clean committed source."
  (if selected
      (handler-case
          (recovery-boot-with-fallback context
                                       selected
                                       forwarded-arguments
                                       :capsule capsule
                                       :source-commit source-commit)
      (error (condition)
        (format *error-output*
                "Retained-generation recovery failed: ~A~%"
                (recovery-sanitize-text condition))
        (recovery-boot-source-with-report
         context forwarded-arguments :capsule capsule)))
    (recovery-boot-source-with-report
     context forwarded-arguments :capsule capsule)))


;;;; -- Argument Parsing and Entry --

(serapeum:-> recovery-parse-arguments (list) *)
(defun recovery-parse-arguments (arguments)
  "Return recovery options and forwarded application arguments from ARGUMENTS."
  (let ((generation nil)
        (list-p nil)
        (status nil)
        (capsule nil)
        (forwarded nil)
        (original-arguments nil)
        (remaining arguments))
    (loop while remaining
          for argument = (pop remaining)
          do (cond
               ((string= argument "--")
                (setf forwarded remaining
                      remaining nil))
               ((string= argument "--list")
                (setf list-p t))
               ((string= argument "--generation")
                (setf generation
                      (or (pop remaining)
                          (error "--generation requires an identifier."))))
               ((string= argument "--status")
                (setf status
                      (or (pop remaining)
                          (error "--status requires an exit code."))))
               ((string= argument "--capsule")
                (setf capsule
                      (or (pop remaining)
                          (error "--capsule requires a pathname."))))
               ((string= argument "--original-argument")
                (push (or (pop remaining)
                          (error "--original-argument requires a value."))
                      original-arguments))
               (t
                (setf forwarded (cons argument remaining)
                      remaining nil))))
    (values generation
            list-p
            status
            capsule
            forwarded
            (nreverse original-arguments))))

(serapeum:-> recovery-run (list) integer)
(defun recovery-run (arguments)
  "Run pristine recovery using complete command-line ARGUMENTS."
  (let ((source-root
          (uiop:ensure-directory-pathname
           (or (first arguments)
               (error "The recovery image needs the source root.")))))
    (if (equal (rest arguments) '("--probe"))
        (progn
          (let ((*print-readably* t))
            (prin1 (list :recovery-probe
                         :version *recovery-image-protocol-version*
                         :sbcl-version (lisp-implementation-version)
                         :operating-system (software-type)
                         :operating-system-version (software-version)
                         :architecture (machine-type)))
            (terpri)
            (finish-output))
          0)
        (let ((context (recovery-context-create source-root)))
          (multiple-value-bind
              (generation list-p status capsule forwarded original-arguments)
              (recovery-parse-arguments (rest arguments))
            (setf forwarded
                  (recovery-normalize-forwarded-arguments context forwarded))
            (if list-p
                (progn
                  (recovery-print-generations context)
                  0)
                (progn
                  (recovery-print-introduction)
                  (let ((reported-capsule
                          (recovery-report-crash
                           context
                           :status status
                           :capsule capsule
                           :original-arguments original-arguments)))
                    (let* ((source-commit
                             (recovery-automatic-source-commit
                              context generation status))
                           (selected
                            (if generation
                                (recovery-load-generation
                                 context
                                 (recovery-manifest-pathname context generation)
                                 :expected-identifier generation)
                                (recovery-selected-generation-or-fallback
                                 context
                                 :source-commit source-commit))))
                      (recovery-boot-with-source-fallback
                       context
                       selected
                       forwarded
                       :capsule reported-capsule
                       :source-commit source-commit))))))))))

(serapeum:-> recovery-main () null)
(defun recovery-main ()
  "Run the pristine recovery core and terminate with its explicit status."
  (sb-ext:disable-debugger)
  (restart-case
      (handler-case
          (uiop:quit (recovery-run (uiop:command-line-arguments)))
        (error (condition)
          (format *error-output* "Recovery could not continue: ~A~%"
                  (recovery-sanitize-text condition))
          (uiop:quit 1)))
    (abort ()
      :report "Exit the pristine Autolith recovery image."
      (uiop:quit 1)))
  nil)

(serapeum:-> recovery-image-save (pathname) null)
(defun recovery-image-save (pathname)
  "Save the current minimal image to PATHNAME with RECOVERY-MAIN as its entry."
  (ensure-directories-exist pathname)
  (sb-ext:save-lisp-and-die (namestring pathname)
                            :toplevel #'recovery-main
                            :executable nil
                            :purify t
                            :compression nil))
