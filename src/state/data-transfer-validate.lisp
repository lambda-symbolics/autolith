(in-package #:autolith)

;;;; -- Archive Validation --

(-> data-transfer--directory-p (t) boolean)
(defun data-transfer--directory-p (value)
  "Return true for a canonical absolute, non-wild workspace directory key."
  (and (non-empty-string-p value)
       (uiop:absolute-pathname-p (pathname value))
       (not (wild-pathname-p (pathname value)))
       (equal value (namestring (uiop:ensure-directory-pathname value)))
       (notany (lambda (part) (member part '(:up :back)))
               (pathname-directory (pathname value)))
       t))

(-> data-transfer--unique (list function t) null)
(defun data-transfer--unique (entries key pathname)
  "Reject duplicate stable keys in ENTRIES, even when their content agrees."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (let ((identity (funcall key entry)))
        (when (gethash identity seen)
          (data-transfer--fail pathname ':invalid "Duplicate identity in archive."))
        (setf (gethash identity seen) t))))
  nil)

(-> data-transfer--validate-session (list t) null)
(defun data-transfer--validate-session (session pathname)
  "Validate session identity, chunk headers, and strictly increasing sequences."
  (unless (and (data-transfer--properties-p session '(:kind :id :directory :segments))
               (member (getf session :kind) '(:conversation :inference))
               (data-transfer--safe-component-p (getf session :id))
               (data-transfer--directory-p (getf session :directory))
               (consp (getf session :segments)))
    (data-transfer--fail pathname ':invalid "Malformed session entry."))
  (let ((previous 0) (first-p t)
        (identifier (getf session :id)))
    (data-transfer--unique (getf session :segments)
                           (lambda (segment) (getf segment :name)) pathname)
    (dolist (segment (getf session :segments))
      (unless (data-transfer--properties-p segment '(:name :records))
        (data-transfer--fail pathname ':invalid "Malformed session segment."))
      (let* ((name (getf segment :name))
             (records (getf segment :records))
             (identity (make-pathname :directory '(:absolute) :name identifier :type "sexp"))
             (header (first records)))
        (unless (and (or (and first-p (null name))
                         (and (data-transfer--safe-component-p name)
                              (equal (pathname-type name) "sexp")
                              (conversation-chunk-start-sequence (pathname name))))
                     (consp records)
                     (equal (getf (rest header) :id) identifier)
                     (equal (getf (rest header) :directory)
                            (getf session :directory)))
          (data-transfer--fail pathname ':invalid "Invalid chunk ownership or filename."))
        (conversation--from-header
         identity
         (if name
             (merge-pathnames name (conversation-storage-directory-pathname identity))
             identity)
         header)
        (when (and name
                   (/= (conversation-chunk-start-sequence (pathname name))
                       (getf (rest header) :chunk-start-sequence 1)))
          (data-transfer--fail pathname ':invalid "Chunk sequence disagrees with its header."))
        (unless (= (getf (rest header) :chunk-start-sequence 1) (1+ previous))
          (data-transfer--fail pathname ':invalid "Chunk starts do not follow the complete record history."))
        (dolist (record (rest records))
          (unless (and (conversation--record-form-p record)
                       (typep (getf (rest record) :seq) '(integer 1))
                       (= (getf (rest record) :seq) (1+ previous))
                       (typep (getf (rest record) :time) 'timestamp))
            (data-transfer--fail pathname ':invalid "Invalid session record sequence."))
          (setf previous (getf (rest record) :seq)))
        (setf first-p nil))))
  nil)

(-> data-transfer--validate-history (list keyword t) null)
(defun data-transfer--validate-history (records kind pathname)
  "Validate memory and papercut lifecycle records, retaining their full history."
  (let ((seen (make-hash-table :test #'equal))
        (closed (make-hash-table :test #'equal)))
    (dolist (record records)
      (unless (and (conversation--record-form-p record)
                   (non-empty-string-p (getf (rest record) :id)))
        (data-transfer--fail pathname ':invalid "Invalid durable history record."))
      (let ((identifier (getf (rest record) :id)))
        (ecase kind
          (:memory
           (case (first record)
             (:memory
              (memory--record->memory #P"memories.sexp" record)
              (setf (gethash identifier seen) t))
             (:memory-forgotten
              (unless (and (gethash identifier seen)
                           (eql (getf (rest record) :version) 1)
                           (typep (getf (rest record) :time) 'timestamp)
                           (typep (getf (rest record) :source-conversation) '(or null string)))
                (data-transfer--fail pathname ':invalid "Invalid memory tombstone.")))
             (otherwise
              (data-transfer--fail pathname ':invalid "Unknown memory record."))))
          (:papercut
           (case (first record)
             (:papercut
              (papercut--record->papercut #P"papercuts.sexp" record)
              (when (gethash identifier seen)
                (data-transfer--fail pathname ':invalid "Repeated papercut report."))
              (setf (gethash identifier seen) t))
             (:papercut-assessed
              (papercut--validate-assessed-record #P"papercuts.sexp" record)
              (unless (and (gethash identifier seen) (not (gethash identifier closed)))
                (data-transfer--fail pathname ':invalid "Assessment has no active papercut.")))
             (:papercut-closed
              (papercut--validate-closed-record #P"papercuts.sexp" record)
              (unless (gethash identifier seen)
                (data-transfer--fail pathname ':invalid "Closure has no papercut."))
              (setf (gethash identifier closed) t))
             (otherwise
              (data-transfer--fail pathname ':invalid "Unknown papercut record."))))))))
  nil)

(-> data-transfer--file-key (list) list)
(defun data-transfer--file-key (entry)
  "Return the logical identity of one asset file."
  (list (getf entry :area) (getf entry :owner) (getf entry :path)))

(-> data-transfer--session-key (list) list)
(defun data-transfer--session-key (entry)
  "Return the logical identity of a conversation or inference session."
  (list (getf entry :kind) (getf entry :id)))

(-> data-transfer--digest (vector) string)
(defun data-transfer--digest (bytes)
  "Return the lowercase SHA-256 content identifier for BYTES."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence ':sha256
                             (map '(vector (unsigned-byte 8)) #'identity bytes))))

(-> data-transfer--validate-files (list list t) null)
(defun data-transfer--validate-files (files sessions pathname)
  "Validate asset ownership, safe paths, and content-addressed object digests."
  (data-transfer--unique files #'data-transfer--file-key pathname)
  (dolist (file files)
    (unless (and (data-transfer--properties-p file '(:area :owner :path :bytes))
                 (member (getf file :area) '(:images :input :scratchpad :tasks :context))
                 (data-transfer--safe-component-p (getf file :owner))
                 (data-transfer--safe-path-p (getf file :path))
                 (vectorp (getf file :bytes))
                 (not (stringp (getf file :bytes)))
                 (every (lambda (byte) (typep byte '(unsigned-byte 8)))
                        (getf file :bytes)))
      (data-transfer--fail pathname ':invalid "Malformed asset file."))
    (when (eq (getf file :area) ':input)
      (unless (and (equal (getf file :path)
                          (list (format nil "~A.image" (data-transfer--digest (getf file :bytes)))))
                   (yolokuva:image-format (getf file :bytes)))
        (data-transfer--fail pathname ':invalid "Queued image content or digest is invalid.")))
    (if (eq (getf file :area) ':context)
        (unless (and (equal (getf file :owner) (data-transfer--digest (getf file :bytes)))
                     (equal (getf file :path)
                            (list (format nil "~A.txt" (getf file :owner)))))
          (data-transfer--fail pathname ':invalid "Context object digest does not match its content."))
        (unless (find (list ':conversation (getf file :owner)) sessions
                      :key #'data-transfer--session-key :test #'equal)
          (data-transfer--fail pathname ':invalid "Asset has no owning conversation."))))
  nil)

(-> data-transfer--image-descriptors (t) list)
(defun data-transfer--image-descriptors (object)
  "Find image descriptors in structured records, ignoring all transcript strings."
  (let ((pending (list object)) (descriptors nil))
    (loop while pending for value = (pop pending) do
      (when (consp value)
        (when (and (keywordp (first value))
                   (handler-case (and (getf value :id) (getf value :artifact) (getf value :mime))
                     (error () nil)))
          (push value descriptors))
        (push (first value) pending)
        (push (rest value) pending)))
    (remove-duplicates descriptors :test #'equal)))

(-> data-transfer--input-basename (string &key (:owner string) (:files list) (:pathname t)) string)
(defun data-transfer--input-basename (name &key owner files pathname)
  "Resolve a queued-image archive token to a validated included basename."
  (let ((prefix "/__autolith_transfer_input__/"))
    (unless (and (stringp name) (uiop:string-prefix-p prefix name))
      (data-transfer--fail pathname ':invalid "Pending input refers outside the archive."))
    (let ((basename (subseq name (length prefix))))
      (unless (find (list ':input owner (list basename)) files
                    :key #'data-transfer--file-key :test #'equal)
        (data-transfer--fail pathname ':invalid "Pending input image is missing from the archive."))
      basename)))

(-> data-transfer--validate-references (list t) null)
(defun data-transfer--validate-references (archive pathname)
  "Require image descriptors to resolve to safe included artifacts."
  (dolist (session (getf archive :sessions))
    (dolist (descriptor (data-transfer--image-descriptors (getf session :segments)))
      (let ((identifier (getf descriptor :id))
            (artifact (getf descriptor :artifact)))
        (unless (and (non-empty-string-p identifier) (stringp artifact)
                     (image-input--artifact-name-p artifact identifier)
                     (find (list ':images (getf session :id) (list artifact))
                           (getf archive :files)
                           :key #'data-transfer--file-key :test #'equal))
          (data-transfer--fail pathname ':invalid "Image descriptor has no included artifact.")))))
  (dolist (state (getf archive :states))
    (data-transfer--map-input-images
     (getf state :form)
     (lambda (name)
       (data-transfer--input-basename name :owner (getf state :owner)
                                          :files (getf archive :files) :pathname pathname)
       name)))
  nil)

(-> data-transfer--validate-children (list t) null)
(defun data-transfer--validate-children (archive pathname)
  "Validate child ownership, transcript identities, results, and image references."
  (data-transfer--unique
   (append (remove ':inference (getf archive :sessions) :key (lambda (session) (getf session :kind)))
           (mapcar (lambda (child) (getf child :session)) (getf archive :children)))
   (lambda (session) (getf session :id)) pathname)
  (dolist (child (getf archive :children))
    (unless (and (data-transfer--properties-p child '(:owner :session :result))
                 (eq (getf (getf child :session) :kind) ':conversation)
                 (find (list ':conversation (getf child :owner)) (getf archive :sessions)
                       :key #'data-transfer--session-key :test #'equal)
                 (listp (getf child :result)))
      (data-transfer--fail pathname ':invalid "Invalid child session ownership or result."))
    (let* ((session (getf child :session)) (identifier (getf session :id))
           (result (getf child :result)))
      (data-transfer--validate-session session pathname)
      (when (and (getf result :conversation-file)
                 (not (equal (getf result :conversation-file)
                             (format nil "task:~A/~A" (getf child :owner) identifier))))
        (data-transfer--fail pathname ':invalid "Child result references another transcript."))
      (dolist (descriptor (data-transfer--image-descriptors (getf session :segments)))
        (unless (and (image-input--artifact-name-p (getf descriptor :artifact) (getf descriptor :id))
                     (find (list ':tasks (getf child :owner)
                                 (list "conversation-images" identifier (getf descriptor :artifact)))
                           (getf archive :files) :key #'data-transfer--file-key :test #'equal))
          (data-transfer--fail pathname ':invalid "A child image artifact is missing.")))))
  nil)

(-> data-transfer--validate (t t) list)
(defun data-transfer--validate (archive pathname)
  "Validate the entire archive and every destination-bearing field before writes."
  (handler-case
      (progn
        (unless (and (data-transfer--portable-p archive)
                     (record-check
                      archive :properties-p t :versions (list *data-transfer-version*)
                      :allow-other-keys nil
                      :fields
                      (append
                       (list (list :indicator ':format :required t
                                   :validate (lambda (value) (eq value ':autolith-data)))
                             (list :indicator ':knowledge :validate #'listp))
                       (mapcar (lambda (key) (list :indicator key :required t))
                               '(:workspace :workspaces :sessions :children :memories
                                 :papercuts :agendas :plans :files :states))))
                     (or (null (getf archive :workspace))
                         (data-transfer--directory-p (getf archive :workspace)))
                     (every (lambda (key) (listp (getf archive key)))
                            '(:workspaces :sessions :children :memories :papercuts :agendas :plans :files :states))
                     (every #'data-transfer--directory-p (getf archive :workspaces)))
          (data-transfer--fail pathname ':invalid "Malformed or unsupported data archive."))
        (data-transfer--unique (getf archive :workspaces) #'identity pathname)
        (data-transfer--unique (getf archive :sessions) #'data-transfer--session-key pathname)
        (dolist (session (getf archive :sessions))
          (data-transfer--validate-session session pathname))
        (data-transfer--validate-children archive pathname)
        (data-transfer--validate-history (getf archive :memories) ':memory pathname)
        (data-transfer--validate-history (getf archive :papercuts) ':papercut pathname)
        (data-transfer--validate-knowledge archive pathname)
        (dolist (record (getf archive :agendas))
          (unless (and (agenda--record-form-p record *agenda-version*)
                       (data-transfer--directory-p (getf (rest record) :directory)))
            (data-transfer--fail pathname ':invalid "Malformed workspace agenda.")))
        (dolist (record (getf archive :plans))
          (unless (and (plan--form-p record)
                       (data-transfer--directory-p (getf (rest record) :directory)))
            (data-transfer--fail pathname ':invalid "Malformed workspace plan.")))
        (dolist (key '(:agendas :plans))
          (data-transfer--unique (getf archive key)
                                 (lambda (record) (getf (rest record) :directory)) pathname))
        (data-transfer--validate-files
         (getf archive :files)
         (append (getf archive :sessions)
                 (mapcar (lambda (child) (getf child :session)) (getf archive :children)))
         pathname)
        (data-transfer--unique (getf archive :states)
                               (lambda (state) (list (getf state :kind) (getf state :owner))) pathname)
        (data-transfer--validate-ownership archive pathname)
        (dolist (state (getf archive :states))
          (unless (and (data-transfer--properties-p state '(:kind :owner :form))
                       (member (getf state :kind) '(:pending :vault))
                       (find (list ':conversation (getf state :owner)) (getf archive :sessions)
                             :key #'data-transfer--session-key :test #'equal)
                       (equal (getf (rest (getf state :form)) :conversation-id) (getf state :owner))
                       (data-transfer--input-state-p state pathname))
            (data-transfer--fail pathname ':invalid "Malformed conversation input state.")))
        (data-transfer--validate-references archive pathname)
        archive)
    (data-transfer-error (condition)
      (error condition))
    (error (condition)
      (data-transfer--fail pathname ':invalid (princ-to-string condition)))))

(-> data-transfer--input-state-p (list t) boolean)
(defun data-transfer--input-state-p (state pathname)
  "Validate complete pending or vault schemas with the persistence readers."
  (let* ((form (getf state :form))
         (properties (rest form))
         (owner (getf state :owner)))
    (if (eq (getf state :kind) ':pending)
        (and (application-recovery-input-vault--pending-form-shape-p form)
             (not (null (application-input-controller--pending-state form owner))))
        (and (application-recovery-input-vault--form-shape-p form)
             (progn
               (data-transfer--unique (getf properties :captures)
                                      (lambda (capture) (getf capture :id)) pathname)
               (every (lambda (capture)
                        (application-recovery-input-vault--capture-from-form
                         capture owner (or pathname #P"archive.sexp")))
                      (getf properties :captures)))
             t))))

(-> data-transfer--validate-ownership (list t) null)
(defun data-transfer--validate-ownership (archive pathname)
  "Require the declared scope to match every entity's structured ownership."
  (let* ((workspace (getf archive :workspace))
         (actual
           (remove-duplicates
            (remove nil
                    (append (when workspace (list workspace))
                            (mapcar (lambda (session) (getf session :directory)) (getf archive :sessions))
                            (mapcar (lambda (child) (getf (getf child :session) :directory)) (getf archive :children))
                            (loop for key in '(:memories :papercuts) append
                              (mapcar (lambda (record) (getf (rest record) :workspace)) (getf archive key)))
                            (loop for key in '(:agendas :plans) append
                              (mapcar (lambda (record) (getf (rest record) :directory)) (getf archive key)))
                            (data-transfer--knowledge-workspaces archive)))
            :test #'equal)))
    (unless (and (every #'data-transfer--directory-p actual)
                 (null (set-exclusive-or actual (getf archive :workspaces) :test #'equal))
                 (or (null workspace) (every (lambda (key) (equal key workspace)) actual)))
      (data-transfer--fail pathname ':invalid "Declared archive scope disagrees with entity ownership."))
    (when workspace
      (let ((linked (data-transfer--memory-links
                     (getf archive :memories)
                     :workspace workspace
                     :linked (loop for agenda in (getf archive :agendas) append
                               (loop for item in (getf (rest agenda) :items)
                                     append (getf (rest item) :memory-ids))))))
        (dolist (record (getf archive :memories))
          (when (and (eq (first record) ':memory) (eq (getf (rest record) :scope) ':global)
                     (not (member (getf (rest record) :id) linked :test #'equal)))
            (data-transfer--fail pathname ':invalid "An unlinked global memory is outside this workspace archive."))))))
  nil)

(-> data-transfer--remap (list string) list)
(defun data-transfer--remap (archive directory)
  "Relocate explicit ownership metadata, preserving identifiers and transcript text."
  (let* ((copy (copy-tree archive))
         (old (getf copy :workspace)))
    (setf (getf copy :workspace) directory
          (getf copy :workspaces) (remove-duplicates
                                  (substitute directory old (getf copy :workspaces) :test #'equal)
                                  :test #'equal))
    (dolist (session (append (getf copy :sessions)
                             (mapcar (lambda (child) (getf child :session)) (getf copy :children))))
      (when (equal (getf session :directory) old)
        (setf (getf session :directory) directory)
        (dolist (segment (getf session :segments))
          (setf (getf (rest (first (getf segment :records))) :directory) directory))))
    (dolist (key '(:memories :papercuts))
      (dolist (record (getf copy key))
        (when (equal (getf (rest record) :workspace) old)
          (setf (getf (rest record) :workspace) directory))))
    (setf copy (data-transfer--remap-knowledge copy old directory))
    (dolist (key '(:agendas :plans))
      (dolist (record (getf copy key))
        (when (equal (getf (rest record) :directory) old)
          (setf (getf (rest record) :directory) directory))))
    copy))
