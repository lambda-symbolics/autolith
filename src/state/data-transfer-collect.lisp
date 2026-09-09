(in-package #:autolith)

;;;; -- Transfer Collection and Ownership --

(-> data-transfer--call-with-locks (configuration function) t)
(defun data-transfer--call-with-locks (configuration function)
  "Run FUNCTION under the existing durable collection locks in a fixed order."
  (with-recursive-lock-held (*data-transfer-lock*)
    (with-recursive-lock-held (*agenda-lock*)
      (with-recursive-lock-held (*memory-lock*)
        (with-lock-held (*papercut-lock*)
          (labels ((acquire (paths)
                     (if paths
                         (call-with-file-lock (first paths)
                                              (lambda () (acquire (rest paths))))
                         (plan--call-with-lock configuration function))))
            (acquire
             (list (merge-pathnames "data-transfer.lock"
                                    (configuration-state-root configuration))
                   (readable-state-lock-pathname (configuration-agenda-path configuration)
                                                 "agendas.lock")
                   (readable-state-lock-pathname (configuration-memory-path configuration)
                                                 "memories.lock")
                   (readable-state-lock-pathname (configuration-papercut-path configuration)
                                                 "papercuts.lock")))))))))

(-> data-transfer--histories (list) list)
(defun data-transfer--histories (records)
  "Group ordered log RECORDS by stable identity without discarding tombstones."
  (let ((table (make-hash-table :test #'equal)) (order nil))
    (dolist (record records)
      (let ((identifier (getf (rest record) :id)))
        (unless (gethash identifier table) (push identifier order))
        (push record (gethash identifier table))))
    (loop for identifier in (nreverse order)
          collect (cons identifier (nreverse (gethash identifier table))))))

(-> data-transfer--log-records (pathname keyword) list)
(defun data-transfer--log-records (pathname tag)
  "Return complete log records after validating the versioned header."
  (let ((records (log-read pathname)))
    (when records
      (unless (and (eq (first (first records)) tag)
                   (eql (getf (rest (first records)) :version) 1))
        (data-transfer--fail pathname ':invalid "Unsupported durable log header.")))
    (rest records)))

(-> data-transfer--history-workspace (list keyword) (option string))
(defun data-transfer--history-workspace (history tag)
  "Return the most recent ownership key in one identity HISTORY."
  (getf (rest (find tag (reverse (rest history)) :key #'first)) :workspace))

(-> data-transfer--filter-histories (list keyword &key (:workspace t) (:linked list)) list)
(defun data-transfer--filter-histories (records tag &key workspace linked)
  "Select workspace histories and explicitly linked global identities."
  (loop for history in (data-transfer--histories records)
        for owner = (data-transfer--history-workspace history tag)
        when (or (null workspace) (equal workspace owner)
                 (and (null owner) (member (first history) linked :test #'equal)))
          append (if workspace
                     (remove-if (lambda (record)
                                  (and (eq (first record) tag)
                                       (not (equal owner (getf (rest record) :workspace)))))
                                (rest history))
                     (rest history))))

(-> data-transfer--session-root (configuration keyword) pathname)
(defun data-transfer--session-root (configuration kind)
  "Return the durable log root for a conversation or inference KIND."
  (ecase kind
    (:conversation (configuration-conversation-root configuration))
    (:inference (configuration-inference-root configuration))))

(-> data-transfer--session-identities (pathname) list)
(defun data-transfer--session-identities (root)
  "Enumerate all durable session identities, including header-only sessions."
  (let ((identities nil))
    (when (uiop:directory-exists-p root)
      (dolist (pathname (uiop:directory-files root "*.sexp"))
        (pushnew (pathname-name pathname) identities :test #'equal))
      (dolist (directory (uiop:subdirectories root))
        (let ((identifier (first (last (pathname-directory directory)))))
          (when (and (data-transfer--safe-component-p identifier)
                     (not (equal identifier "objects"))
                     (conversation-storage-active-pathname
                      (conversation-identifier--pathname root identifier)))
            (pushnew identifier identities :test #'equal)))))
    (sort identities #'string<)))

(-> data-transfer--session (configuration keyword string &key (:root (option pathname))) list)
(defun data-transfer--session (configuration kind identifier &key root)
  "Read one session's complete ordered segments, omitting regenerable sidecars."
  (let* ((root (or root (data-transfer--session-root configuration kind)))
         (identity (conversation-identifier--pathname root identifier))
         (segments
           (loop for pathname in (conversation-storage-pathnames identity)
                 do (data-transfer--check-path root pathname)
                 collect (list :name (unless (equal pathname identity)
                                        (file-namestring pathname))
                               :records (log-read pathname))))
         (header (first (getf (first segments) :records))))
    (unless (and header (eq (first header) ':conversation)
                 (equal (getf (rest header) :id) identifier))
      (data-transfer--fail identity ':invalid "Session has no valid identity header."))
    (list :kind kind :id identifier
          :directory (getf (rest header) :directory)
          :segments segments)))

(-> data-transfer--child-root (configuration string string) pathname)
(defun data-transfer--child-root (configuration owner identifier)
  "Return the canonical transcript root of an owned child session."
  (merge-pathnames (format nil "tasks/~A/~A/" owner identifier)
                   (configuration-data-root configuration)))

(-> data-transfer--children (configuration list) (values list list))
(defun data-transfer--children (configuration sessions)
  "Collect child transcripts and terminal results, returning excluded raw-file paths."
  (let ((children nil) (excluded nil))
    (dolist (session sessions)
      (when (eq (getf session :kind) ':conversation)
        (let* ((owner (getf session :id))
               (root (merge-pathnames (format nil "tasks/~A/" owner)
                                      (configuration-data-root configuration))))
          (when (uiop:directory-exists-p root)
            (dolist (directory (uiop:subdirectories root))
              (let* ((identifier (first (last (pathname-directory directory))))
                     (identity (conversation-identifier--pathname directory identifier))
                     (result-path (merge-pathnames "result.sexp" directory)))
                (when (conversation-storage-active-pathname identity)
                  (let ((result (when (probe-file result-path) (snapshot-read result-path))))
                    (when (getf result :conversation-file)
                      (setf (getf result :conversation-file) (format nil "task:~A/~A" owner identifier)))
                    (push (list :owner owner
                                :session (data-transfer--session configuration ':conversation identifier :root directory)
                                :result result) children))
                  (setf excluded (append (conversation-storage-pathnames identity)
                                         (conversation-picker-sidecar-pathnames identity)
                                         (list result-path) excluded)))))))))
    (values (nreverse children) excluded)))

(-> data-transfer--walk-files (pathname) list)
(defun data-transfer--walk-files (root)
  "Collect confined regular files recursively without following symlinks."
  (let ((files nil))
    (labels ((walk (directory)
               (data-transfer--check-path root directory)
               (dolist (file (uiop:directory-files directory))
                 (data-transfer--check-path root file)
                 (push file files))
               (dolist (child (uiop:subdirectories directory))
                 (data-transfer--check-path root child)
                 (walk child))))
      (when (uiop:directory-exists-p root) (walk root)))
    (sort files #'string< :key #'namestring)))

(-> data-transfer--asset-root (configuration keyword string) pathname)
(defun data-transfer--asset-root (configuration area owner)
  "Map one validated asset AREA and OWNER to its private root."
  (merge-pathnames
   (ecase area
     (:images (format nil "conversation-images/~A/" owner))
     (:input (format nil "conversation-inputs/~A/" owner))
     (:scratchpad (format nil "scratchpads/~A/" owner))
     (:tasks (format nil "tasks/~A/" owner))
     (:context "inferences/objects/"))
   (configuration-data-root configuration)))

(-> data-transfer--assets (configuration keyword string) list)
(defun data-transfer--assets (configuration area owner)
  "Collect only files owned by one session asset subtree."
  (let ((root (data-transfer--asset-root configuration area owner)))
    (data-transfer--check-path (configuration-data-root configuration) root)
    (loop for pathname in (data-transfer--walk-files root)
          collect (list :area area :owner owner
                        :path (data-transfer--relative-components pathname root)
                        :bytes (data-transfer--bytes pathname)))))

(-> data-transfer--map-input-images (t function) t)
(defun data-transfer--map-input-images (object function)
  "Copy OBJECT, transforming only structured pending-input image pathnames."
  (cond
    ((and (consp object) (eq (first object) ':user-message-input))
     (let ((copy (copy-tree object)))
       (setf (getf (rest copy) :image-pathnames)
             (mapcar function (getf (rest copy) :image-pathnames)))
       copy))
    ((consp object)
     (cons (data-transfer--map-input-images (first object) function)
           (data-transfer--map-input-images (rest object) function)))
    (t
     object)))

(-> data-transfer--collect-inputs (list) (values list list))
(defun data-transfer--collect-inputs (states)
  "Snapshot queued images and replace their structured paths with archive tokens."
  (let ((files nil))
    (values
     (loop for state in states
           for owner = (getf state :owner)
           collect
           (list :kind (getf state :kind) :owner owner
                 :form
                 (data-transfer--map-input-images
                  (getf state :form)
                  (lambda (name)
                    (let* ((path (image-input-validate-pathname name))
                           (bytes (data-transfer--bytes path))
                           (basename (format nil "~A.image" (data-transfer--digest bytes))))
                      (pushnew (list :area ':input :owner owner
                                     :path (list basename) :bytes bytes)
                               files :test #'equalp)
                      (format nil "/__autolith_transfer_input__/~A" basename))))))
     files)))

(-> data-transfer--strings (t) list)
(defun data-transfer--strings (object)
  "Return strings in portable data for reference discovery without rewriting text."
  (let ((pending (list object)) (strings nil))
    (loop while pending for value = (pop pending) do
      (cond
        ((stringp value) (push value strings))
        ((consp value) (push (first value) pending) (push (rest value) pending))))
    strings))

(-> data-transfer--referenced-p (string list) boolean)
(defun data-transfer--referenced-p (identifier strings)
  "Return true for an explicit context URI, not an arbitrary identifier substring."
  (let ((uri (format nil "context:~A" identifier)))
    (not (null (some (lambda (string) (search uri string :test #'char=)) strings)))))

(-> data-transfer--states (configuration list) list)
(defun data-transfer--states (configuration sessions)
  "Collect pending inputs and recovery vaults associated with selected sessions."
  (loop for session in sessions
        when (eq (getf session :kind) ':conversation)
          append
          (loop for (kind directory) in '((:pending "pending-inputs/")
                                          (:vault "recovery-input-vault/"))
                for identifier = (getf session :id)
                for path = (merge-pathnames
                            (format nil "~A~A.sexp" directory identifier)
                            (configuration-state-root configuration))
                when (probe-file path)
                  collect (progn
                            (data-transfer--check-path
                             (configuration-state-root configuration) path)
                            (multiple-value-bind (form complete-p) (snapshot-read path)
                              (unless complete-p
                                (data-transfer--fail path ':invalid "Incomplete session state."))
                              (list :kind kind :owner identifier :form form))))))

(-> data-transfer--plans (configuration t) list)
(defun data-transfer--plans (configuration workspace)
  "Collect workspace plan snapshots, including a surviving legacy plan."
  (let* ((root (merge-pathnames "plans/" (configuration-state-root configuration)))
         (paths (append (when (uiop:directory-exists-p root)
                          (uiop:directory-files root "*.sexp"))
                        (when (probe-file (configuration-legacy-plan-path configuration))
                          (list (configuration-legacy-plan-path configuration)))))
         (plans nil))
    (dolist (path paths)
      (data-transfer--check-path (configuration-state-root configuration) path)
      (multiple-value-bind (form complete-p) (snapshot-read path)
        (unless (and complete-p (plan--form-p form))
          (data-transfer--fail path ':invalid "Invalid workspace plan."))
        (when (or (null workspace) (equal workspace (getf (rest form) :directory)))
          (unless (find (getf (rest form) :directory) plans
                        :key (lambda (plan) (getf (rest plan) :directory)) :test #'equal)
            (push form plans)))))
    (nreverse plans)))

(-> data-transfer--collect (configuration t) list)
(defun data-transfer--collect (configuration workspace)
  "Collect owned durable records and their session and context assets."
  (let* ((agendas
           (remove-if-not
            (lambda (record)
              (or (null workspace)
                  (equal workspace (workspace-agenda-directory record))))
            (agenda-state-records (agenda--read configuration :lock-held-p t))))
         (agenda-forms (mapcar #'agenda--record->form agendas))
         (linked (loop for agenda in agendas append
                   (loop for item in (workspace-agenda-items agenda)
                         append (agenda-item-memory-identifiers item))))
         (memories (data-transfer--filter-histories
                    (data-transfer--log-records (configuration-memory-path configuration)
                                                ':memories)
                    ':memory :workspace workspace :linked linked))
         (papercuts (data-transfer--filter-histories
                     (data-transfer--log-records (configuration-papercut-path configuration)
                                                 ':papercuts)
                     ':papercut :workspace workspace))
         (candidates
           (loop for kind in '(:conversation :inference) append
             (loop for identifier in (data-transfer--session-identities
                                       (data-transfer--session-root configuration kind))
                   collect (data-transfer--session configuration kind identifier))))
         (sessions (remove-if-not
                    (lambda (session)
                      (or (null workspace) (equal workspace (getf session :directory))))
                    candidates))
         (child-data (multiple-value-list (data-transfer--children configuration sessions)))
         (children (first child-data))
         (excluded (second child-data))
         (files nil)
         (plans (data-transfer--plans configuration workspace))
         (states (data-transfer--states configuration sessions)))
    (dolist (session sessions)
      (when (eq (getf session :kind) ':conversation)
        (dolist (area '(:images :scratchpad :tasks))
          (setf files (append files (data-transfer--assets
                                    configuration area (getf session :id)))))))
    (setf files
          (remove-if (lambda (file)
                       (and (eq (getf file :area) ':tasks)
                            (member (data-transfer--path
                                     (data-transfer--asset-root configuration ':tasks (getf file :owner))
                                     (getf file :path)) excluded :test #'equal))) files))
    (dolist (child children)
      (setf files (append files (data-transfer--assets
                                configuration ':scratchpad (getf (getf child :session) :id)))))
    (let ((strings (append
                    (data-transfer--strings
                     (list sessions children memories papercuts agenda-forms plans states))
                    (loop for file in files
                          for text = (ignore-errors
                                       (sb-ext:octets-to-string (getf file :bytes)
                                                                :external-format ':utf-8))
                          when text collect text)))
          (root (merge-pathnames "inferences/objects/" (configuration-data-root configuration))))
      (when (uiop:directory-exists-p root)
        (dolist (path (uiop:directory-files root "*.txt"))
          (when (or (null workspace)
                    (data-transfer--referenced-p (pathname-name path) strings))
            (data-transfer--check-path (configuration-data-root configuration) path)
            (push (list :area ':context :owner (pathname-name path)
                        :path (list (file-namestring path))
                        :bytes (data-transfer--bytes path)) files)))))
    (let* ((workspaces
             (remove-duplicates
              (remove nil
                      (append (when workspace (list workspace))
                              (mapcar (lambda (session) (getf session :directory)) sessions)
                              (mapcar (lambda (child) (getf (getf child :session) :directory)) children)
                              (mapcar (lambda (record) (getf (rest record) :workspace)) memories)
                              (mapcar (lambda (record) (getf (rest record) :workspace)) papercuts)
                              (mapcar (lambda (record) (getf (rest record) :directory))
                                      (append plans agenda-forms)))) :test #'equal)))
      (multiple-value-bind (states input-files)
          (data-transfer--collect-inputs states)
        (list :format ':autolith-data :version *data-transfer-version*
              :workspace workspace :workspaces (sort workspaces #'string<)
              :sessions sessions :children children :memories memories :papercuts papercuts
              :agendas agenda-forms :plans plans :files (append files input-files)
              :states states)))))
