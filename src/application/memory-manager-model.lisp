(in-package #:autolith)

;;;; -- Memory Manager Model --

(defclass memory-manager-item ()
  ((identifier :initarg :identifier :reader memory-manager-item-identifier
               :documentation "The unabridged memory or claim identifier.")
   (key :initarg :key :reader memory-manager-item-key
        :documentation "A selection key prefixed with note: or fact:.")
   (kind :initarg :kind :reader memory-manager-item-kind
         :documentation "NOTE or FACT.")
   (title :initarg :title :reader memory-manager-item-title
          :documentation "A terminal-safe, single-line title.")
   (scope :initarg :scope :reader memory-manager-item-scope
          :documentation "WORKSPACE or GLOBAL.")
   (status :initarg :status :reader memory-manager-item-status
           :documentation "The assessed note status or evaluated fact status.")
   (search-text :initarg :search-text :reader memory-manager-item-search-text
                :documentation "Title, content, identifier, tags and evidence for search.")
   (data :initarg :data :reader memory-manager-item-data
         :documentation "The captured memory or knowledge node.")
   (version :initarg :version :reader memory-manager-item--version
            :documentation "An immutable comparison snapshot, not a timestamp alone.")
   (updated-at :initarg :updated-at :reader memory-manager-item--updated-at
               :documentation "The newest durable change time used to sort the item.")
   (deletable-p :initarg :deletable-p :reader memory-manager-item--deletable-p
                :documentation "Whether the snapshot has an independently retractable assertion."))
  (:documentation "One captured, versioned row in the fullscreen memory manager."))

(define-condition memory-manager-conflict (autolith-error)
  ((key :initarg :key :reader memory-manager-conflict-key
        :documentation "The item whose captured version no longer matches."))
  (:documentation "A memory manager mutation used a changed or disappeared item.")
  (:default-initargs :message "This memory changed or disappeared. Refresh before editing.")
  (:report (lambda (condition stream)
             (format stream "Memory ~A changed or disappeared. Refresh before editing."
                     (memory-manager-conflict-key condition)))))

(-> memory-manager-item--fact-version (knowledge-view knowledge-node) list)
(defun memory-manager-item--fact-version (view node)
  "Capture NODE's assertion records and evaluated support without mutable aliases."
  (let ((base (knowledge-view-base view)))
    (copy-tree
     (list (knowledge-base-claim base (knowledge-node-identifier node))
           (loop for support in (knowledge-node-supports node)
                 collect (knowledge-base-support base (getf support :id)))
           (knowledge-node-supports node)
           (knowledge-node-justifications node)
           (knowledge-node-conflicts node)
           (knowledge-node-status view node)
           (knowledge-node-text node)))))

(-> application-memory-manager--note (memory memory-assessor) memory-manager-item)
(defun application-memory-manager--note (memory assessor)
  "Capture MEMORY and its current assessment for one manager row."
  (let ((identifier (memory-identifier memory)))
    (make-instance
     'memory-manager-item
     :identifier identifier
     :key (concatenate 'string "note:" identifier)
     :kind ':note
     :title (application-memory--title memory)
     :scope (memory-scope memory)
     :status (memory-assessment-status (memory-assess assessor memory))
     :search-text (format nil "~A ~A ~A ~{~A ~} ~S ~S"
                          (memory-title memory) (memory-content memory) identifier
                          (memory-tags memory) (memory-sources memory) (memory-premises memory))
     :data memory
     :version (copy-tree (memory--record memory))
     :updated-at (memory-updated-at memory)
     :deletable-p t)))

(-> application-memory-manager--fact (knowledge-view knowledge-node) memory-manager-item)
(defun application-memory-manager--fact (view node)
  "Capture NODE, including direct support provenance and retraction eligibility."
  (let* ((base (knowledge-view-base view))
         (identifier (knowledge-node-identifier node))
         (supports (loop for support in (knowledge-node-supports node)
                         collect (knowledge-base-support base (getf support :id))))
         (evidence (loop for support in supports
                         when (getf support :evidence)
                           collect (knowledge-base-evidence-record base (getf support :evidence)))))
    (make-instance
     'memory-manager-item
     :identifier identifier
     :key (concatenate 'string "fact:" identifier)
     :kind ':fact
     :title (application-knowledge--text node)
     :scope (knowledge-node-scope node)
     :status (if (eq (knowledge-node-status view node) ':conflicted)
                 ':conflicted
                 (knowledge-node-applicability node))
     :search-text (format nil "~A ~A ~S ~S ~S" (knowledge-node-text node) identifier
                          (knowledge-node-proposition node) evidence
                          (knowledge-node-justifications node))
     :data node
     :version (memory-manager-item--fact-version view node)
     :updated-at (loop for support in supports maximize (or (getf support :time) 0) into time
                       finally (return (or time 0)))
     :deletable-p (and (some (lambda (support)
                              (and support (not (getf support :retracted-at))))
                            supports)
                      t))))

(-> application-memory-manager--rank (memory-manager-item) integer)
(defun application-memory-manager--rank (item)
  "Sort conflicted/stale items before unverified items and current ones."
  (case (memory-manager-item-status item)
    ((:conflicted :stale :expired) 0)
    ((:unverified :pending) 1)
    (otherwise 2)))

(-> application-memory-manager-items (application) list)
(defun application-memory-manager-items (application)
  "Return every visible note and listed fact, attention first and newest next."
  (let* ((configuration (application-configuration application))
         (assessor (memory-assessor-create configuration))
         (view (application-knowledge--view application))
         (items (append
                 (mapcar (lambda (memory) (application-memory-manager--note memory assessor))
                         (memory-list configuration))
                 (loop for node being the hash-values of (knowledge-view-nodes view)
                       when (application-knowledge--listed-p node)
                         collect (application-memory-manager--fact view node)))))
    (stable-sort
     items
     (lambda (left right)
       (let ((left-rank (application-memory-manager--rank left))
             (right-rank (application-memory-manager--rank right)))
         (cond
           ((/= left-rank right-rank)
            (< left-rank right-rank))
           ((/= (memory-manager-item--updated-at left) (memory-manager-item--updated-at right))
            (> (memory-manager-item--updated-at left) (memory-manager-item--updated-at right)))
           ((not (string-equal (memory-manager-item-title left) (memory-manager-item-title right)))
            (string-lessp (memory-manager-item-title left) (memory-manager-item-title right)))
           (t
            (string< (memory-manager-item-key left) (memory-manager-item-key right)))))))))

(-> application-memory-manager-detail (application memory-manager-item) list)
(defun application-memory-manager-detail (application item)
  "Explain ITEM with the existing evidence and dependent-impact presentation."
  (let ((result (ecase (memory-manager-item-kind item)
                  (:note
                   (application-memory-entry application (memory-manager-item-identifier item)))
                  (:fact
                   (let* ((view (application-knowledge--view application))
                          (node (knowledge-view-node view (memory-manager-item-identifier item))))
                     (if node
                         (application-knowledge-fact-entry application view node)
                         (application-memory--missing (memory-manager-item-identifier item))))))))
    (if (stringp result) (list (terminal-span ':notice result)) result)))

(-> application-memory-manager-editable-p (memory-manager-item) boolean)
(defun application-memory-manager-editable-p (item)
  "Return true for note memories, whose prose and tags may be edited."
  (eq (memory-manager-item-kind item) ':note))

(-> application-memory-manager-deletable-p (memory-manager-item) boolean)
(defun application-memory-manager-deletable-p (item)
  "Return true for notes or facts with direct, unretracted assertion support."
  (memory-manager-item--deletable-p item))

(-> application-memory-manager--check (application memory-manager-item) t)
(defun application-memory-manager--check (application item)
  "Reject a changed, hidden or disappeared ITEM before applying a mutation."
  (let ((current
          (ecase (memory-manager-item-kind item)
            (:note
             (let ((memory (find (memory-manager-item-identifier item)
                                 (memory-list (application-configuration application))
                                 :test #'string= :key #'memory-identifier)))
               (and memory (memory--record memory))))
            (:fact
             (let* ((view (application-knowledge--view application))
                    (node (knowledge-view-node view (memory-manager-item-identifier item))))
               (and node (application-knowledge--listed-p node)
                    (memory-manager-item--fact-version view node)))))))
    (unless (and current (equal current (memory-manager-item--version item)))
      (error 'memory-manager-conflict :key (memory-manager-item-key item)))))

(-> application-memory-manager-create
    (application &key (:title string) (:content string) (:tags list) (:scope memory-scope)) memory)
(defun application-memory-manager-create (application &key title content tags (scope ':workspace))
  "Create an attributed note using the durable memory validation and mutation API."
  (memory-remember (application-configuration application)
                   :title title :content content :tags tags :scope scope
                   :source-conversation
                   (conversation-identifier (application-conversation application))))

(-> application-memory-manager-save
    (application memory-manager-item &key (:title string) (:content string) (:tags list)
                                         (:scope (option memory-scope))) memory)
(defun application-memory-manager-save (application item &key title content tags scope)
  "Replace a captured note, preserving its recorded evidence and premise versions."
  (unless (application-memory-manager-editable-p item)
    (error 'memory-error :message "Only note memories can be edited."
           :pathname (configuration-memory-path (application-configuration application))
           :identifier (memory-manager-item-identifier item)))
  (with-recursive-lock-held (*memory-lock*)
    (application-memory-manager--check application item)
    (memory-remember (application-configuration application)
                     :identifier (memory-manager-item-identifier item)
                     :title title :content content :tags tags :scope scope
                     :source-conversation
                     (conversation-identifier (application-conversation application)))))

(-> application-memory-manager-forget (application memory-manager-item) list)
(defun application-memory-manager-forget (application item)
  "Forget a captured note or retract an assertion, including dependent impact."
  (with-recursive-lock-held (*memory-lock*)
    (with-recursive-lock-held (*knowledge-lock*)
      (application-memory-manager--check application item)
      (unless (application-memory-manager-deletable-p item)
        (error 'memory-error :message "Derived facts must be changed through their premises."
               :pathname (configuration-memory-path (application-configuration application))
               :identifier (memory-manager-item-identifier item)))
      (let ((result
              (ecase (memory-manager-item-kind item)
                (:note
                 (application-memory-forget-entry application (memory-manager-item-identifier item)))
                (:fact
                 (application-knowledge-forget-entry application (memory-manager-item-identifier item))))))
        (if (stringp result) (list (terminal-span ':notice result)) result)))))
