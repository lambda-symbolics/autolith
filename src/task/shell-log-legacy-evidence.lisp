(in-package #:autolith)

;;;; -- Temporary Legacy Tool Evidence Reader --

;;; Remove this compatibility reader in 0.68.0 after old tool evidence has
;;; canonical :SHELL-LOGS provenance. Never rewrite evidence identities or audit.

(defparameter *shell-log-legacy-evidence-cache-limit* 8
  "Maximum immutable legacy-resolution snapshots retained in this process.")

(defvar *shell-log-legacy-evidence-cache* nil
  "Newest-first immutable resolution snapshots; readers need no cache lock.")

(defvar *shell-log-legacy-evidence-cache-lock*
  (make-lock "Autolith legacy shell evidence cache")
  "Serialize publication only; never held during storage or knowledge access.")

(defclass shell-log-legacy-evidence-snapshot ()
  ((base
    :initarg :base :reader shell-log-legacy-evidence-snapshot-base
    :documentation "The exact immutable folded knowledge base that was resolved.")
   (revision
    :initarg :revision :reader shell-log-legacy-evidence-snapshot-revision
    :documentation "The folded base revision when resolution started.")
   (root
    :initarg :root :reader shell-log-legacy-evidence-snapshot-root
    :documentation "The data root in which owner transcripts were resolved.")
   (time
    :initarg :time :reader shell-log-legacy-evidence-snapshot-time
    :documentation "Resolution time; a backwards clock requires rebuilding.")
   (knowledge
    :initarg :knowledge :reader shell-log-legacy-evidence-snapshot-knowledge
    :documentation "Authoritative knowledge pathname, object identity, size and times.")
   (sources
    :initarg :sources :reader shell-log-legacy-evidence-snapshot-sources
    :documentation "Owner identity pathnames and every transcript segment stamp.")
   (evidence
    :initarg :evidence :reader shell-log-legacy-evidence-snapshot-evidence
    :documentation "Exact source and digest of each resolved evidence identifier.")
   (references
    :initarg :references :reader shell-log-legacy-evidence-snapshot-references
    :documentation "Evidence identifiers to canonical stream identity lists, including NIL."))
  (:documentation "One bounded-cache entry, published only after a stable read."))

(-> shell-log-legacy-evidence--file-stamp (pathname) list)
(defun shell-log-legacy-evidence--file-stamp (pathname)
  "Describe PATHNAME without opening its contents or acquiring transcript locks."
  (let ((status (platform-path-status *platform* pathname :follow-links-p t)))
    (list (namestring pathname)
          (when status
            (list (platform-file-status-kind status)
                  (platform-file-status-identity status)
                  (platform-file-status-size status)
                  (platform-file-status-modification-time status)
                  (platform-file-status-change-time status))))))

(-> shell-log-legacy-evidence--source-stamp (pathname) list)
(defun shell-log-legacy-evidence--source-stamp (identity)
  "Stamp every segment of IDENTITY, including rotation, replacement and append."
  (mapcar #'shell-log-legacy-evidence--file-stamp
          (conversation-storage-pathnames identity)))

(-> shell-log-legacy-evidence--snapshot
    (configuration knowledge-base) (option shell-log-legacy-evidence-snapshot))
(defun shell-log-legacy-evidence--snapshot (configuration base)
  "Find BASE's published snapshot without I/O or acquiring cache locks."
  (let ((root (namestring (config :data-root configuration))))
    (find-if (lambda (entry)
               (and (eq base (shell-log-legacy-evidence-snapshot-base entry))
                    (= (knowledge-base-revision base)
                       (shell-log-legacy-evidence-snapshot-revision entry))
                    (equal root (shell-log-legacy-evidence-snapshot-root entry))))
             *shell-log-legacy-evidence-cache*)))

(-> shell-log-legacy-evidence-cache-valid-p
    (configuration knowledge-base hash-table) boolean)
(defun shell-log-legacy-evidence-cache-valid-p (configuration base references)
  "Validate the exact warmed map using storage stamps, never transcript reads.

Call under the knowledge lock before taking the shell lock. The caller also
checks that BASE is still the authoritative folded snapshot."
  (let ((entry (shell-log-legacy-evidence--snapshot configuration base)))
    (and entry
         (equal (knowledge-base-pathname base) (configuration-knowledge-path configuration))
         (eq references (shell-log-legacy-evidence-snapshot-references entry))
         (>= (get-universal-time) (shell-log-legacy-evidence-snapshot-time entry))
         (equal (shell-log-legacy-evidence-snapshot-knowledge entry)
                (shell-log-legacy-evidence--file-stamp
                 (configuration-knowledge-path configuration)))
         (every (lambda (source)
                  (equal (third source)
                         (shell-log-legacy-evidence--source-stamp (second source))))
                (shell-log-legacy-evidence-snapshot-sources entry))
         t)))

(-> shell-log-legacy-evidence-cached-references
    (configuration knowledge-base list) (values list hash-table))
(defun shell-log-legacy-evidence-cached-references (configuration base evidence)
  "Return old EVIDENCE's exact references and warmed map, without I/O or locks.

The transaction must validate the returned map before taking the shell lock,
then require every returned reference to be present before writing support."
  (let* ((entry (shell-log-legacy-evidence--snapshot configuration base))
         (id    (getf evidence :id)))
    (when entry
      (multiple-value-bind (references present-p)
          (gethash id (shell-log-legacy-evidence-snapshot-references entry))
        (when (and present-p
                   (equal (list (getf evidence :source) (getf evidence :digest))
                          (gethash id (shell-log-legacy-evidence-snapshot-evidence entry))))
          (return-from shell-log-legacy-evidence-cached-references
            (values references (shell-log-legacy-evidence-snapshot-references entry))))))
    (knowledge--fail ':unresolved-legacy-shell-evidence id
                     "Old tool evidence has no current exact provenance snapshot; re-observe its source outside the transaction.")))

(-> shell-log-legacy-evidence--old-tool-p (list) boolean)
(defun shell-log-legacy-evidence--old-tool-p (evidence)
  "Recognize tool evidence without the new explicit raw-dependency field."
  (let ((source (getf evidence :source)))
    (and (eq (first source) ':tool)
         (not (nth-value 2 (get-properties (rest source) '(:shell-logs))))
         t)))

(-> shell-log-legacy-evidence--covers-p (shell-log-legacy-evidence-snapshot list) boolean)
(defun shell-log-legacy-evidence--covers-p (entry evidence)
  "Check that ENTRY resolves every explicitly requested old evidence row."
  (every (lambda (item)
           (or (not (shell-log-legacy-evidence--old-tool-p item))
               (and (nth-value 1 (gethash (getf item :id)
                                        (shell-log-legacy-evidence-snapshot-references entry)))
                    (equal (list (getf item :source) (getf item :digest))
                           (gethash (getf item :id)
                                    (shell-log-legacy-evidence-snapshot-evidence entry))))))
         evidence))

(-> shell-log-legacy-evidence--required
    (knowledge-base integer &key (:include-evidence list)) list)
(defun shell-log-legacy-evidence--required (base now &key include-evidence)
  "Select retained legacy dependencies plus explicitly requested old evidence.

Future-valid supports retain raw evidence too. Dormant or orphan evidence is
resolved only when a transaction explicitly requests its reuse."
  (let ((required (make-hash-table :test #'equal)))
    (maphash
     (lambda (id support)
       (declare (ignore id))
       (let* ((until    (getf support :valid-until))
              (evidence (gethash (getf support :evidence) (knowledge-base-evidence base)))
              (expires  (getf evidence :valid-until)))
         (when (and evidence
                    (not (getf support :retracted-at))
                    (not (and until (> now until)))
                    (not (and expires (> now expires)))
                    (shell-log-legacy-evidence--old-tool-p evidence))
           (setf (gethash (getf evidence :id) required) evidence))))
     (knowledge-base-supports base))
    (dolist (evidence include-evidence)
      (when (shell-log-legacy-evidence--old-tool-p evidence)
        (setf (gethash (getf evidence :id) required) evidence)))
    (loop for evidence being the hash-values of required collect evidence)))

(-> shell-log-legacy-evidence--record-references (configuration list) list)
(defun shell-log-legacy-evidence--record-references (configuration record)
  "Use actual stored metadata, never rendered text or the synthetic source call."
  (let* ((properties (rest record))
         (details    (getf properties :details)))
    (unless (and (nth-value 2 (get-properties properties '(:details)))
                 (proper-list-p details) (evenp (length details))
                 (not (getf details :omitted-p))
                 (eql 0 (getf details :shell-logs-omitted 0)))
      (knowledge--fail ':unresolved-legacy-shell-evidence (getf properties :seq)
                       "The matching stored tool result has absent, omitted or unsupported metadata."))
    (sort (shell-log-result-evidence-references
           configuration
           (make-instance 'shell-command-tool-result
                          :content (getf properties :output) :details details :success-p t)
           :require-present-p nil)
          #'string<)))

(-> shell-log-legacy-evidence--resolve-owner
    (configuration pathname list) hash-table)
(defun shell-log-legacy-evidence--resolve-owner (configuration identity evidence)
  "Resolve all requested digests in one validated, read-only owner scan."
  (let ((requests (make-hash-table :test #'equal))
        (matches  (make-hash-table :test #'equal))
        (resolved (make-hash-table :test #'equal)))
    (dolist (item evidence)
      (let* ((source (getf item :source))
             (digest (getf item :digest))
             (key    (list (getf (rest source) :name) digest)))
        (unless (and digest (memory-source-digest-p digest))
          (knowledge--fail ':unresolved-legacy-shell-evidence (getf item :id)
                           "Old tool evidence has no exact content digest."))
        (push item (gethash key requests))))
    (handler-case
        (conversation--map-storage-records
         identity
         (lambda (record)
           (when (eq (first record) ':tool-result)
             (let* ((properties (rest record))
                    (output     (getf properties :output))
                    (key        (and (stringp output)
                                     (list (getf properties :tool)
                                           (memory-source-text-digest output)))))
               (when (gethash key requests)
                 (pushnew (shell-log-legacy-evidence--record-references configuration record)
                          (gethash key matches) :test #'equal))))))
      (conversation-error (condition)
        (knowledge--fail ':unresolved-legacy-shell-evidence (namestring identity)
                         "Cannot resolve old tool evidence from its stored conversation: ~A" condition)))
    (maphash
     (lambda (key items)
       (let ((candidates (gethash key matches)))
         (unless (= (length candidates) 1)
           (knowledge--fail
            ':unresolved-legacy-shell-evidence (mapcar (lambda (item) (getf item :id)) items)
            "Old tool evidence has ~D distinct matching capture sets; exact provenance is unresolved."
            (length candidates)))
         (dolist (item items)
           (setf (gethash (getf item :id) resolved) (first candidates)))))
     requests)
    resolved))

(-> shell-log-legacy-evidence--publish (shell-log-legacy-evidence-snapshot) null)
(defun shell-log-legacy-evidence--publish (entry)
  "Publish one immutable bounded-cache entry without retaining cache locks."
  (with-lock-held (*shell-log-legacy-evidence-cache-lock*)
    (let ((entries (cons entry
                         (remove (shell-log-legacy-evidence-snapshot-base entry)
                                 *shell-log-legacy-evidence-cache*
                                 :key #'shell-log-legacy-evidence-snapshot-base :test #'eq))))
      (setf *shell-log-legacy-evidence-cache*
            (subseq entries 0 (min (length entries) *shell-log-legacy-evidence-cache-limit*)))))
  nil)

(-> shell-log-legacy-evidence-references
    (configuration knowledge-base &key (:include-evidence list)) hash-table)
(defun shell-log-legacy-evidence-references (configuration base &key include-evidence)
  "Warm exact old tool dependencies outside knowledge and shell locks.

INCLUDE-EVIDENCE requests exact stored rows being reused by a transaction,
including dormant or orphan evidence. SOURCE :CALL is a synthetic contract
identity, never a provider call ID. Match only owner, tool name and full content
digest, then validate actual metadata. Missing or divergent proof refuses."
  (unless (equal (knowledge-base-pathname base) (configuration-knowledge-path configuration))
    (knowledge--fail ':unresolved-legacy-shell-evidence (knowledge-base-pathname base)
                     "Legacy resolution requires the authoritative knowledge pathname."))
  (let ((cached (shell-log-legacy-evidence--snapshot configuration base)))
    (when (and cached
               (shell-log-legacy-evidence--covers-p cached include-evidence)
               (shell-log-legacy-evidence-cache-valid-p
                configuration base (shell-log-legacy-evidence-snapshot-references cached)))
      (return-from shell-log-legacy-evidence-references
        (shell-log-legacy-evidence-snapshot-references cached))))
  (loop repeat 3
        do (let* ((now          (get-universal-time))
                  (required     (shell-log-legacy-evidence--required
                                 base now :include-evidence include-evidence))
                  (groups       (make-hash-table :test #'equal))
                  (references   (make-hash-table :test #'equal))
                  (fingerprints (make-hash-table :test #'equal))
                  (knowledge    (shell-log-legacy-evidence--file-stamp
                                 (configuration-knowledge-path configuration)))
                  (sources      nil))
             (dolist (evidence required)
               (let ((owner (getf (rest (getf evidence :source)) :conversation)))
                 (unless (non-empty-string-p owner)
                   (knowledge--fail ':unresolved-legacy-shell-evidence (getf evidence :id)
                                    "Old tool evidence has no owner conversation."))
                 (push evidence (gethash owner groups))
                 (setf (gethash (getf evidence :id) fingerprints)
                       (list (getf evidence :source) (getf evidence :digest)))))
             (let ((identities (conversation-pathnames-for-ids
                                configuration (loop for owner being the hash-keys of groups collect owner))))
               (maphash
                (lambda (owner evidence)
                  (let* ((identity (gethash owner identities))
                         (before   (shell-log-legacy-evidence--source-stamp identity))
                         (resolved (shell-log-legacy-evidence--resolve-owner configuration identity evidence)))
                    (push (list owner identity before) sources)
                    (maphash (lambda (id refs) (setf (gethash id references) refs)) resolved)))
                groups))
             (let ((entry (make-instance 'shell-log-legacy-evidence-snapshot
                                        :base base :revision (knowledge-base-revision base)
                                        :root (namestring (config :data-root configuration))
                                        :time now :knowledge knowledge :sources sources
                                        :evidence fingerprints :references references)))
               (when (and (equal knowledge
                                 (shell-log-legacy-evidence--file-stamp
                                  (configuration-knowledge-path configuration)))
                          (every (lambda (source)
                                   (equal (third source)
                                          (shell-log-legacy-evidence--source-stamp (second source))))
                                 sources))
                 (shell-log-legacy-evidence--publish entry)
                 (return-from shell-log-legacy-evidence-references references)))))
  (knowledge--fail ':unresolved-legacy-shell-evidence (knowledge-base-revision base)
                   "Legacy evidence storage kept changing during resolution; retry outside retention locks."))
