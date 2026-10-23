(in-package #:autolith)

;;;; -- Knowledge Explanations and Changes --

;;; An explanation is the bounded tree of what supports one claim: its stored
;;; supports with their evidence, and its derivations with their rule versions,
;;; bindings, and premises, explained in turn. Trees are paged: past the node
;;; limit, unexpanded premises are returned as a cursor to continue from.
;;;
;;; A snapshot records which claims a view supports. Comparing a snapshot with
;;; a later view yields the changes between them: new and withdrawn claims,
;;; keyed values that moved, and new conflicts, each with the cause when it is
;;; known. Snapshots are portable, so the last state a person or model saw can
;;; be kept and changes reported once.

(defparameter *knowledge-explanation-node-limit* 32
  "The most claims one page of an explanation expands.")

(defparameter *knowledge-change-limit* 64
  "The most changes one comparison reports.")


;;;; -- Text --

(-> knowledge-node-text (knowledge-node) string)
(defun knowledge-node-text (node)
  "Return NODE's claim as a sentence."
  (or (knowledge-node--text node)
      (setf (knowledge-node--text node)
            (knowledge-proposition-display (knowledge-node-proposition node)
                                           (knowledge-node-schema node)))))

(-> knowledge-evidence-text (list) string)
(defun knowledge-evidence-text (evidence)
  "Return a short description of where EVIDENCE, an evidence plist, came from."
  (let* ((source (getf evidence :source))
         (properties (rest source))
         (quote (getf properties :quote)))
    (format nil "~A~@[ \"~A\"~]"
            (ecase (first source)
              (:file (getf properties :path))
              (:url (getf properties :url))
              (:user "you said")
              (:tool (format nil "tool ~A" (getf properties :name)))
              (:memory (format nil "memory ~A" (getf properties :id)))
              (:mission (format nil "mission ~A" (getf properties :id)))
              (:skill (format nil "skill ~A" (getf properties :id))))
            quote)))

(-> knowledge-applicability-text (keyword (option keyword)) string)
(defun knowledge-applicability-text (state reason)
  "Return why evidence in STATE, refined by REASON, does or does not apply."
  (ecase state
    (:current "holds")
    (:changed "changed")
    (:missing (if (eq reason ':forgotten) "was forgotten" "is missing"))
    (:expired "expired")
    (:retracted "was retracted")
    (:pending "is not valid yet")
    (:unverified "could not be checked")))


(-> knowledge-node-json (knowledge-view knowledge-node) json-object)
(defun knowledge-node-json (view node)
  "Return NODE as a complete JSON fact for a model: identity, sentence, values, basis.

Status appears only when the fact is conflicted or otherwise not plainly
supported; a derived fact names its rule version."
  (let ((object (json-object
                 "id" (knowledge-node-identifier node)
                 "fact" (knowledge-node-text node)
                 "predicate" (knowledge-proposition-predicate (knowledge-node-proposition node))
                 "arguments" (map 'vector #'knowledge-term->json
                                  (knowledge-proposition-arguments
                                   (knowledge-node-proposition node)))
                 "basis" (map 'vector
                              (lambda (set)
                                (format nil "~{~(~A~)~^+~}" set))
                              (knowledge-node-bases node)))))
    (when (knowledge-node-negated-p node)
      (setf (gethash "negated" object) t))
    (when (knowledge-node-derived-p node)
      (let ((justification (first (knowledge-node-justifications node))))
        (setf (gethash "derived" object)
              (format nil "~A v~D" (getf justification :rule) (getf justification :version)))))
    (unless (eq (knowledge-node-status view node) ':supported)
      (setf (gethash "status" object)
            (string-downcase (symbol-name (knowledge-node-status view node)))))
    object))

(-> knowledge-node-relevance (knowledge-node list) (integer 0))
(defun knowledge-node-relevance (node terms)
  "Return how many lowercase TERMS appear in NODE's sentence or values."
  (let ((haystack (or (knowledge-node--haystack node)
                      (setf (knowledge-node--haystack node)
                            (string-downcase
                             (format nil "~A ~{~A~^ ~}"
                                     (knowledge-node-text node)
                                     (cons (knowledge-proposition-predicate
                                            (knowledge-node-proposition node))
                                           (mapcar #'knowledge-term-display
                                                   (knowledge-proposition-arguments
                                                    (knowledge-node-proposition node))))))))))
    (count-if (lambda (term) (search term haystack)) terms)))


;;;; -- Explanations --

(-> knowledge--explain-support (knowledge-view list) list)
(defun knowledge--explain-support (view support)
  "Return the explanation of one evaluated SUPPORT plist."
  (let ((evidence (and (getf support :evidence)
                       (knowledge-base-evidence-record (knowledge-view-base view)
                                                       (getf support :evidence)))))
    (list :support (getf support :id)
          :basis (getf support :basis)
          :state (getf support :state)
          :reason (getf support :reason)
          :evidence (and evidence (getf evidence :id))
          :text (if evidence (knowledge-evidence-text evidence) "assumed")
          :observed-at (and evidence (getf evidence :observed-at)))))

(-> knowledge-explain
    (knowledge-view (or string list) &key (:limit integer))
    (values list list))
(defun knowledge-explain (view identifiers &key (limit *knowledge-explanation-node-limit*))
  "Return explanation trees for claim IDENTIFIERS and the cursor to continue.

IDENTIFIERS is one claim identifier or a cursor returned earlier. Claims are
expanded breadth first, at most LIMIT per page; a premise left unexpanded is
marked :MORE and its identifier joins the returned cursor."
  (let ((queue (if (stringp identifiers) (list identifiers) (copy-list identifiers)))
        (trees (make-hash-table :test #'equal))
        (roots (if (stringp identifiers) (list identifiers) (copy-list identifiers)))
        (expanded 0)
        (cursor nil))
    (loop while queue
          do (let* ((identifier (pop queue))
                    (node (knowledge-view-node view identifier)))
               (cond
                 ((gethash identifier trees))
                 ((null node)
                  (setf (gethash identifier trees)
                        (list :claim identifier :status ':unknown :applicability ':unknown)))
                 ((>= expanded limit)
                  (push identifier cursor)
                  (setf (gethash identifier trees)
                        (list :claim identifier :text (knowledge-node-text node) :more t)))
                 (t
                  (incf expanded)
                  (setf (gethash identifier trees)
                        (list :claim identifier
                              :text (knowledge-node-text node)
                              :status (knowledge-node-status view node)
                              :applicability (knowledge-node-applicability node)
                              :bases (knowledge-node-bases node)
                              :conflicts (knowledge-node-conflicts node)
                              :supports (mapcar (lambda (support)
                                                  (knowledge--explain-support view support))
                                                (knowledge-node-supports node))
                              :justifications
                              (mapcar (lambda (justification)
                                        (list :rule (getf justification :rule)
                                              :version (getf justification :version)
                                              :bindings (getf justification :bindings)
                                              :premises (getf justification :premises)))
                                      (knowledge-node-justifications node))))
                  (dolist (justification (knowledge-node-justifications node))
                    (setf queue (append queue (getf justification :premises))))))))
    (labels ((tree (identifier seen)
               (let ((entry (copy-list (gethash identifier trees))))
                 (when (getf entry :justifications)
                   (setf (getf entry :justifications)
                         (mapcar (lambda (justification)
                                   (let ((copy (copy-list justification)))
                                     (setf (getf copy :premises)
                                           (mapcar (lambda (premise)
                                                     (if (member premise seen :test #'string=)
                                                         (list :claim premise :repeated t)
                                                         (tree premise (cons premise seen))))
                                                   (getf justification :premises)))
                                     copy))
                                 (getf entry :justifications))))
                 entry)))
      (values (mapcar (lambda (root) (tree root (list root))) roots)
              (nreverse cursor)))))


;;;; -- Snapshots and Changes --

(-> knowledge-view-snapshot (knowledge-view) list)
(defun knowledge-view-snapshot (view)
  "Return the portable record of which claims VIEW supports.

Each entry is (:CLAIM :TEXT :KEY :PREMISES :CONFLICTED); PREMISES are those of
a derived claim's first derivation, so a later withdrawal can name its cause."
  (sort
   (loop for node being the hash-values of (knowledge-view-nodes view)
         when (knowledge-node-supported-p node)
           collect (list :claim (knowledge-node-identifier node)
                         :text (knowledge-node-text node)
                         :key (and (knowledge-node-schema node)
                                   (knowledge-proposition-key (knowledge-node-proposition node)
                                                              (knowledge-node-schema node)))
                         :premises (getf (first (knowledge-node-justifications node)) :premises)
                         :conflicted (and (knowledge-node-conflicts node) t)))
   #'string< :key (lambda (entry) (getf entry :claim))))

(-> knowledge--withdrawal-causes (knowledge-view list) list)
(defun knowledge--withdrawal-causes (view entry)
  "Return sentences explaining why snapshot ENTRY's claim is no longer supported."
  (let ((node (knowledge-view-node view (getf entry :claim))))
    (cond
      ((and node (knowledge-node-supports node) (null (getf entry :premises)))
       (loop for support in (knowledge-node-supports node)
             unless (eq (getf support :state) ':current)
               collect (let ((explained (knowledge--explain-support view support)))
                         (format nil "~A ~A" (getf explained :text)
                                 (knowledge-applicability-text (getf support :state)
                                                               (getf support :reason))))))
      ((getf entry :premises)
       (loop for premise in (getf entry :premises)
             for premise-node = (knowledge-view-node view premise)
             unless (and premise-node (knowledge-node-supported-p premise-node))
               append (if premise-node
                          (let ((reasons (knowledge--withdrawal-causes
                                          view (list :claim premise))))
                            (list (format nil "~A ~(~A~)~@[: ~{~A~^; ~}~]"
                                          (knowledge-node-text premise-node)
                                          (knowledge-node-applicability premise-node)
                                          reasons)))
                          (list (format nil "premise ~A is unknown" premise)))))
      (t
       nil))))

(-> knowledge-view-changes (list knowledge-view) list)
(defun knowledge-view-changes (snapshot view)
  "Return what changed from SNAPSHOT to VIEW, at most the change limit.

Changes are plists of :KIND :CHANGED (a keyed value moved :FROM one text :TO
another), :NEW, :WITHDRAWN (with :CAUSES), or :CONFLICT."
  (let* ((current (knowledge-view-snapshot view))
         (before (make-hash-table :test #'equal))
         (after (make-hash-table :test #'equal))
         (changes nil)
         (moved (make-hash-table :test #'equal)))
    (dolist (entry snapshot)
      (setf (gethash (getf entry :claim) before) entry))
    (dolist (entry current)
      (setf (gethash (getf entry :claim) after) entry))
    (dolist (entry current)
      (unless (gethash (getf entry :claim) before)
        (let ((replaced (and (getf entry :key)
                             (find-if (lambda (old)
                                        (and (equal (getf old :key) (getf entry :key))
                                             (null (gethash (getf old :claim) after))))
                                      snapshot))))
          (cond
            (replaced
             (setf (gethash (getf replaced :claim) moved) t)
             (push (list :kind ':changed
                         :claim (getf entry :claim)
                         :from (getf replaced :text)
                         :to (getf entry :text)
                         :causes (knowledge--withdrawal-causes view replaced))
                   changes))
            (t
             (push (list :kind ':new :claim (getf entry :claim) :text (getf entry :text))
                   changes))))))
    (dolist (entry snapshot)
      (unless (or (gethash (getf entry :claim) after)
                  (gethash (getf entry :claim) moved))
        (push (list :kind ':withdrawn
                    :claim (getf entry :claim)
                    :text (getf entry :text)
                    :causes (knowledge--withdrawal-causes view entry))
              changes)))
    (dolist (entry current)
      (let ((old (gethash (getf entry :claim) before)))
        (when (and (getf entry :conflicted) (or (null old) (not (getf old :conflicted))))
          (push (list :kind ':conflict :claim (getf entry :claim) :text (getf entry :text))
                changes))))
    (let ((ordered (nreverse changes)))
      (subseq ordered 0 (min (length ordered) *knowledge-change-limit*)))))

(-> knowledge-change-lines (list) list)
(defun knowledge-change-lines (changes)
  "Return one line describing each knowledge change."
  (mapcar (lambda (change)
            (ecase (getf change :kind)
              (:new (format nil "now holds: ~A (~A)" (getf change :text) (getf change :claim)))
              (:withdrawn (format nil "no longer holds: ~A~@[ because ~{~A~^; ~}~]"
                                  (getf change :text) (getf change :causes)))
              (:changed (format nil "changed: ~A, was ~A" (getf change :to) (getf change :from)))
              (:conflict (format nil "conflicts: ~A (~A)" (getf change :text)
                                 (getf change :claim)))))
          changes))
