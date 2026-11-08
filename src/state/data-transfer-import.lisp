(in-package #:autolith)

;;;; -- Merge Planning and Transactional Publication --

(-> data-transfer--state-path (configuration list) pathname)
(defun data-transfer--state-path (configuration state)
  "Return the canonical destination for one validated conversation state."
  (merge-pathnames
   (format nil "~A/~A.sexp"
           (ecase (getf state :kind)
             (:pending
              "pending-inputs")
             (:vault
              "recovery-input-vault"))
           (getf state :owner))
   (config :state-root configuration)))

(-> data-transfer--merge-histories (list list t) list)
(defun data-transfer--merge-histories (existing imported pathname)
  "Return records for new identities, rejecting divergent existing histories."
  (let ((old (data-transfer--histories existing)) (added nil))
    (dolist (history (data-transfer--histories imported))
      (let ((match (assoc (first history) old :test #'equal)))
        (cond
          ((null match)
           (setf added (append added (rest history))))
          ((not (equal match history))
           (data-transfer--fail pathname ':conflict
                                (format nil "Durable identity ~A already has different history."
                                        (first history)))))))
    added))

(-> data-transfer--merge-agendas (list list t) list)
(defun data-transfer--merge-agendas (existing imported pathname)
  "Merge agenda items by stable ID, rejecting changed existing items."
  (let ((result (copy-tree existing)))
    (dolist (agenda imported)
      (let* ((directory (getf (rest agenda) :directory))
             (match     (find directory result :test #'equal
                              :key                   (lambda (record) (getf (rest record) :directory)))))
        (if (null match)
            (setf result (append result (list agenda)))
            (dolist (item (getf (rest agenda) :items))
              (let ((old (find (getf (rest item) :id) (getf (rest match) :items)
                               :test #'equal :key (lambda (entry) (getf (rest entry) :id)))))
                (cond
                  ((null old)
                   (setf (getf (rest match) :items)
                         (append (getf (rest match) :items) (list item))))
                  ((not (equal old item))
                   (data-transfer--fail pathname ':conflict "An agenda item has different content."))))))))
    result))

(-> data-transfer--plan-write (pathname vector pathname &key (:replace-p boolean)) list)
(defun data-transfer--plan-write (pathname bytes root &key replace-p)
  "Validate an exact destination and return a write only when its bytes differ."
  (data-transfer--check-path root pathname)
  (let ((old (when (probe-file pathname) (data-transfer--bytes pathname))))
    (cond
      ((and old (equalp old bytes))
       nil)
      ((and old (not replace-p))
       (data-transfer--fail pathname ':conflict "The destination already contains different data."))
      (t
       (list :path pathname :bytes bytes :old old :root root)))))

(-> data-transfer--check-session-identities (configuration list) null)
(defun data-transfer--check-session-identities (configuration archive)
  "Reject conversation IDs already owned by a different top-level or child session."
  (let* ((expected          (make-hash-table :test #'equal))
         (data-root         (config :data-root configuration))
         (conversation-root (configuration-conversation-root configuration))
         (task-root         (merge-pathnames "tasks/" data-root)))
    (dolist (session (getf archive :sessions))
      (when (eq (getf session :kind) ':conversation)
        (setf (gethash (getf session :id) expected)
              (conversation-identifier--pathname conversation-root (getf session :id)))))
    (dolist (child (getf archive :children))
      (let ((identifier (getf (getf child :session) :id)))
        (setf (gethash identifier expected)
              (conversation-identifier--pathname
               (data-transfer--child-root configuration (getf child :owner) identifier)
               identifier))))
    (flet ((check (identifier identity)
             (let ((destination (gethash identifier expected)))
               (when (and destination (not (equal destination identity))
                          (conversation-storage-occupied-p identity))
                 (data-transfer--fail
                  identity ':conflict
                  (format nil "Conversation identity ~A already belongs to another session."
                          identifier))))))
      (when (plusp (hash-table-count expected))
        (dolist (identifier (data-transfer--session-identities conversation-root))
          (check identifier (conversation-identifier--pathname conversation-root identifier)))
        (data-transfer--check-path data-root task-root)
        (when (uiop:directory-exists-p task-root)
          (dolist (owner-root (uiop:subdirectories task-root))
            (data-transfer--check-path data-root owner-root)
            (dolist (child-root (uiop:subdirectories owner-root))
              (let ((identifier (first (last (pathname-directory child-root)))))
                (when (gethash identifier expected)
                  (data-transfer--check-path data-root child-root)
                  (check identifier
                         (conversation-identifier--pathname child-root identifier))))))))))
  nil)

(-> data-transfer--plan-segments (list pathname pathname) list)
(defun data-transfer--plan-segments (session identity data-root)
  "Plan transcript segments at IDENTITY, reusing equal records regardless of printing."
  (loop for segment in (getf session :segments)
        for path = (if (getf segment :name)
                       (merge-pathnames (getf segment :name)
                                        (conversation-storage-directory-pathname identity))
                       identity)
        unless (and (probe-file path) (equal (log-read path) (getf segment :records)))
        collect (data-transfer--plan-write path
                                           (data-transfer--forms-bytes (getf segment :records))
                                           data-root)))

(defmethod data-transfer-family-plan ((family (eql :sessions)) configuration archive)
  "Plan root session segments after checking existing records and owned assets."
  (declare (ignore family))
  (let ((data-root (config :data-root configuration))
        (child-exclusions
         (nth-value 1 (data-transfer--children configuration (getf archive :sessions)))))
    (loop for session in (getf archive :sessions)
          append (let* ((kind       (getf session :kind))
                        (identifier (getf session :id))
                        (root       (data-transfer--session-root configuration kind))
                        (identity   (conversation-identifier--pathname root identifier)))
                   (data-transfer--check-path data-root identity)
                   (when (conversation-storage-occupied-p identity)
                     (unless (equal session (data-transfer--session configuration kind identifier))
                       (data-transfer--fail identity ':conflict
                                            "A session identity already has different records."))
                     (when (eq kind ':conversation)
                       (dolist (area '(:images :scratchpad :tasks))
                         (dolist (existing (data-transfer--assets configuration area identifier))
                           (unless
                               (or (find existing (getf archive :files) :test #'equalp)
                                   (and (eq area ':tasks)
                                        (member
                                         (data-transfer--path
                                          (data-transfer--asset-root configuration area identifier)
                                          (getf existing :path))
                                         child-exclusions :test #'equal)))
                             (data-transfer--fail identity ':conflict
                                                  "The existing session has different owned assets."))))))
                   (data-transfer--plan-segments session identity data-root)))))

(defmethod data-transfer-family-plan ((family (eql :children)) configuration archive)
  "Plan child transcript segments and relocated result records."
  (declare (ignore family))
  (let ((data-root (config :data-root configuration)))
    (loop for child in (getf archive :children)
          append (let* ((session    (getf child :session))
                        (identifier (getf session :id))
                        (root
                         (data-transfer--child-root configuration (getf child :owner) identifier))
                        (identity   (conversation-identifier--pathname root identifier)))
                   (when
                       (and (conversation-storage-occupied-p identity)
                            (not
                             (equal session
                                    (data-transfer--session configuration ':conversation identifier
                                                            :root root))))
                     (data-transfer--fail identity ':conflict
                                          "An existing child transcript has different records."))
                   (append (data-transfer--plan-segments session identity data-root)
                           (when (getf child :result)
                             (let ((result (copy-tree (getf child :result)))
                                   (path   (merge-pathnames "result.sexp" root)))
                               (when (getf result :conversation-file)
                                 (setf (getf result :conversation-file) (namestring identity)))
                               (unless (and (probe-file path) (equal (snapshot-read path) result))
                                 (list
                                  (data-transfer--plan-write path
                                                             (data-transfer--forms-bytes
                                                              (list result))
                                                             data-root))))))))))

(-> data-transfer--plan-histories (configuration list &key (:kind keyword) (:path pathname)) list)
(defun data-transfer--plan-histories (configuration archive &key kind path)
  "Plan additions to one identity-history log, preserving its existing byte prefix."
  (let* ((data-root (config :data-root configuration))
         (existing  (data-transfer--log-records path kind))
         (added     (data-transfer--merge-histories existing (getf archive kind) path)))
    (when added
      (multiple-value-bind (forms incomplete-p) (log-read path)
        (declare (ignore forms))
        (when incomplete-p
          (data-transfer--fail path ':conflict
                               "An incomplete durable log tail needs repair before import.")))
      (list
       (data-transfer--plan-write
        path
        (if (probe-file path)
            (concatenate '(vector (unsigned-byte 8))
                         (data-transfer--bytes path) (vector 10)
                         (data-transfer--forms-bytes
                          (append (unless (log-read path) (list (list kind :version 1))) added)))
            (data-transfer--forms-bytes (cons (list kind :version 1) added)))
        data-root :replace-p t)))))

(defmethod data-transfer-family-plan ((family (eql :memories)) configuration archive)
  "Plan additions to complete memory histories."
  (data-transfer--plan-histories configuration archive
                                :kind family :path (configuration-memory-path configuration)))

(defmethod data-transfer-family-plan ((family (eql :papercuts)) configuration archive)
  "Plan additions to complete papercut histories."
  (data-transfer--plan-histories configuration archive
                                :kind family :path (configuration-papercut-path configuration)))

(defmethod data-transfer-family-plan ((family (eql :knowledge)) configuration archive)
  "Plan knowledge-log additions after conflict and tail checks."
  (declare (ignore family))
  (let* ((data-root (config :data-root configuration))
         (path      (configuration-knowledge-path configuration))
         (existing  (data-transfer--knowledge-records configuration))
         (added     (data-transfer--merge-knowledge existing (getf archive :knowledge) path)))
    (when added
      (when (and (probe-file path) (nth-value 1 (log-read path)))
        (data-transfer--fail path ':conflict
                             "An incomplete knowledge log tail needs repair before import."))
      (list
       (data-transfer--plan-write
        path
        (if (probe-file path)
            (concatenate '(vector (unsigned-byte 8))
                         (data-transfer--bytes path) (vector 10)
                         (data-transfer--forms-bytes added))
            (data-transfer--forms-bytes
             (cons (list :knowledge :version *knowledge-format-version*) added)))
        data-root :replace-p t)))))

(defmethod data-transfer-family-plan ((family (eql :agendas)) configuration archive)
  "Plan merged workspace agendas when imported records add content."
  (declare (ignore family))
  (let* ((data-root (config :data-root configuration))
         (path      (configuration-agenda-path configuration))
         (old       (mapcar #'agenda--record->form
                            (agenda-state-records (agenda--read configuration :lock-held-p t))))
         (merged    (data-transfer--merge-agendas old (getf archive :agendas) path)))
    (unless (equal old merged)
      (list (data-transfer--plan-write
             path
             (data-transfer--forms-bytes
              (list (list :agendas :version *agenda-version* :records merged)))
             data-root :replace-p t)))))

(defmethod data-transfer-family-plan ((family (eql :plans)) configuration archive)
  "Plan workspace snapshots after checking legacy ownership conflicts."
  (declare (ignore family))
  (let ((state-root (config :state-root configuration)))
    (loop for plan in (getf archive :plans)
          append (let ((path
                        (configuration-plan-path configuration
                                                 (data-transfer--workspace-identifier
                                                  (getf (rest plan) :directory)))))
                   (let* ((legacy-path (configuration-legacy-plan-path configuration))
                          (legacy      (when (probe-file legacy-path) (snapshot-read legacy-path))))
                     (when
                         (and legacy
                              (equal (getf (rest legacy) :directory) (getf (rest plan) :directory))
                              (not (equal legacy plan)))
                       (data-transfer--fail legacy-path ':conflict
                                            "A legacy plan already owns this workspace with different content.")))
                   (unless (and (probe-file path) (equal (snapshot-read path) plan))
                     (list
                      (data-transfer--plan-write path (data-transfer--forms-bytes (list plan))
                                                 state-root)))))))

(-> data-transfer--plan-import (configuration list) list)
(defun data-transfer--plan-import (configuration archive)
  "Resolve every family and destination conflict before publishing any data."
  (data-transfer--check-session-identities configuration archive)
  (let ((writes     (loop for family in *data-transfer-families*
                          append (data-transfer-family-plan family configuration archive)))
        (data-root  (config :data-root configuration))
        (state-root (config :state-root configuration)))
    (flet ((add (write)
             (when write (push write writes))))
      (dolist (file (getf archive :files))
        (add (data-transfer--plan-write
              (data-transfer--path
               (data-transfer--asset-root configuration (getf file :area) (getf file :owner))
               (getf file :path))
              (getf file :bytes) data-root)))
      (dolist (state (getf archive :states))
        (let* ((owner (getf state :owner))
               (path  (data-transfer--state-path configuration state))
               (root  (data-transfer--asset-root configuration ':input owner))
               (form  (data-transfer--map-input-images
                       (getf state :form)
                       (lambda (name)
                         (namestring
                          (data-transfer--path
                           root
                           (list (data-transfer--input-basename
                                  name :owner owner :files (getf archive :files) :pathname path))))))))
          (unless (and (probe-file path) (equal (snapshot-read path) form))
            (add (data-transfer--plan-write
                  path (data-transfer--forms-bytes (list form)) state-root))))))
    (data-transfer--unique writes (lambda (write) (namestring (getf write :path))) nil)
    writes))

(-> data-transfer--publish-writes (list) integer)
(defun data-transfer--publish-writes (writes)
  "Publish planned WRITES together, checking every path against its trusted root."
  (data-transfer--publish-files
   writes
   :roots (mapcar (lambda (write)
                    (cons (namestring (getf write :path)) (getf write :root)))
                  writes)))

(-> data-transfer--install (configuration list) integer)
(defun data-transfer--install (configuration archive)
  "Reserve imported conversation identities, then validate and publish the merge."
  (let ((leases nil))
    (unwind-protect
         (handler-case
             (progn
               (dolist (session (sort (copy-list (getf archive :sessions)) #'string<
                                      :key (lambda (session) (getf session :id))))
                 (when (eq (getf session :kind) ':conversation)
                   (push (conversation-lease-acquire configuration (getf session :id)) leases)))
               (data-transfer--publish-writes (data-transfer--plan-import configuration archive)))
           (data-transfer-error (condition)
             (error condition))
           (conversation-error (condition)
             (data-transfer--fail (conversation-error-pathname condition) ':conflict
                                  (princ-to-string condition)))
           (error (condition)
             (data-transfer--fail nil ':publication (princ-to-string condition))))
      (dolist (lease leases) (conversation-lease-release lease)))))
