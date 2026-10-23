(in-package #:autolith)

;;;; -- Memory Manager Model Tests --

(-> memory-manager-model-tests--application (configuration) application)
(defun memory-manager-model-tests--application (configuration)
  "Return an isolated application with a durable conversation for mutations."
  (make-instance 'application :configuration configuration
                             :conversation (conversation-create configuration)))

(-> memory-manager-model-tests--item (application string) memory-manager-item)
(defun memory-manager-model-tests--item (application identifier)
  "Find the current manager snapshot for IDENTIFIER."
  (or (find identifier (application-memory-manager-items application)
            :test #'string= :key #'memory-manager-item-identifier)
      (error "Missing test item ~A." identifier)))

(-> memory-manager-model-tests--schema (configuration) list)
(defun memory-manager-model-tests--schema (configuration)
  "Define a global test vocabulary and return its input schema."
  (let ((schema (knowledge-make-schema
                 :predicate "manager.value"
                 :arguments '((:name "subject" :type :entity) (:name "value" :type :number))
                 :key '("subject") :template "{subject} has value {value}")))
    (knowledge-define-schema configuration schema :scope ':global)
    schema))

(-> memory-manager-model-tests--assert
    (configuration list list &key (:scope memory-scope)) string)
(defun memory-manager-model-tests--assert (configuration schema values &key (scope ':workspace))
  "Assert VALUES with searchable user evidence and return the claim ID."
  (knowledge-assert configuration (knowledge-make-proposition schema values)
                    :scope scope :basis ':user-assertion
                    :evidence (knowledge-observe :source '(:user :quote "manager evidence")
                                                 :adapter "user/1")))

(-> memory-manager-model-tests--conflict-p (function) boolean)
(defun memory-manager-model-tests--conflict-p (function)
  "Return true only for the adapter's typed, user-presentable conflict."
  (handler-case
      (progn (funcall function) nil)
    (memory-manager-conflict (condition)
      (and (typep condition 'autolith-error)
           (non-empty-string-p (autolith-error-message condition))))))

(-> test-memory-manager-model-listing () null)
(defun test-memory-manager-model-listing ()
  "Cover complete combined listings, visibility, status, search evidence and order."
  (with-test-configuration (base root)
    (let* ((configuration (configuration-copy base :working-directory root))
           (application (memory-manager-model-tests--application configuration))
           (schema (memory-manager-model-tests--schema configuration))
           (source (merge-pathnames "evidence.txt" root))
           (other-root (merge-pathnames "other/" root))
           (other (configuration-copy configuration :working-directory other-root)))
      (ensure-directories-exist other-root)
      (with-open-file (stream source :direction ':output :if-exists ':supersede)
        (write-string "original evidence" stream))
      (dotimes (index 55)
        (application-memory-manager-create application :title (format nil "Note ~2,'0D" index)
                                                      :content "body text" :tags '("tag-search")))
      (let* ((global (application-memory-manager-create application :title "Global note"
                                                                  :content "Across workspaces"
                                                                  :scope ':global))
             (stale (memory-remember configuration :title "Stale note" :content "old text"
                                     :sources (list (memory-source-observe-file
                                                     source :quote "original evidence" :time 1))))
             (hidden (memory-remember other :title "Other workspace" :content "Not visible")))
        (dotimes (index 25)
          (memory-manager-model-tests--assert configuration schema (list (format nil "fact~D" index) index)))
        (let ((global-fact (memory-manager-model-tests--assert configuration schema '("global" 7)
                                                             :scope ':global)))
          (memory-manager-model-tests--assert configuration schema '("fact0" 100))
          (memory-manager-model-tests--assert other schema '("hidden" 1))
          (with-open-file (stream source :direction ':output :if-exists ':supersede)
            (write-string "changed evidence" stream))
          (let* ((items (application-memory-manager-items application))
                 (notes (remove-if-not (lambda (item) (eq (memory-manager-item-kind item) ':note)) items))
                 (facts (remove-if-not (lambda (item) (eq (memory-manager-item-kind item) ':fact)) items))
                 (stale-item (find (memory-identifier stale) items :test #'string=
                                  :key #'memory-manager-item-identifier)))
            (test-assert (= 57 (length notes)) "all notes, including more than fifty, are listed")
            (test-assert (= 27 (length facts)) "all facts, including more than twenty, are listed")
            (test-assert (= (length items)
                            (length (remove-duplicates items :test #'string= :key #'memory-manager-item-key)))
                         "note/fact selection keys are unique")
            (test-assert (not (find (memory-identifier hidden) items :test #'string=
                                   :key #'memory-manager-item-identifier))
                         "another workspace's notes are hidden")
            (test-assert (not (find "hidden has value 1" facts :test #'string=
                                   :key #'memory-manager-item-title))
                         "another workspace's structured facts are hidden")
            (test-assert (and (eq ':global (memory-manager-item-scope
                                           (memory-manager-model-tests--item application (memory-identifier global))))
                              (eq ':global (memory-manager-item-scope
                                           (memory-manager-model-tests--item application global-fact))))
                         "both note and fact scope are exposed")
            (test-assert (and (eq ':stale (memory-manager-item-status stale-item))
                              (eq ':current (memory-manager-item-status
                                             (memory-manager-model-tests--item application (memory-identifier global))))
                              (= 2 (count ':conflicted facts :key #'memory-manager-item-status)))
                         "stale/current note and conflicted fact statuses are assessed")
            (test-assert (every (lambda (item) (member (memory-manager-item-status item) '(:stale :conflicted)))
                                (subseq items 0 3))
                         "items needing attention precede current items")
            (test-assert (and (search "original evidence" (memory-manager-item-search-text stale-item))
                              (search (memory-identifier stale) (memory-manager-item-search-text stale-item))
                              (some (lambda (item) (search "tag-search" (memory-manager-item-search-text item))) notes)
                              (every (lambda (item) (search "manager evidence" (memory-manager-item-search-text item))) facts))
                         "search text includes note content, tags, raw IDs and fact evidence")
            (test-assert (search "changed" (terminal--spans-text
                                             (application-memory-manager-detail application stale-item)))
                         "note detail uses the evidence explanation"))))))
  nil)

(-> test-memory-manager-model-note-mutations () null)
(defun test-memory-manager-model-note-mutations ()
  "Exercise attributed note CRUD, scope changes, evidence retention and replay."
  (with-test-configuration (configuration)
    (let* ((application (memory-manager-model-tests--application configuration))
           (conversation-id (conversation-identifier (application-conversation application)))
           (premise (application-memory-manager-create application :title "Premise" :content "First"))
           (note (memory-remember configuration :title "Dependent" :content "Conclusion"
                                  :sources '((:user :quote "The original statement" :observed-at 1))
                                  :premises (list (memory-identifier premise))))
           (snapshot (memory-manager-model-tests--item application (memory-identifier note))))
      (test-assert (and (eq ':workspace (memory-scope premise))
                        (string= conversation-id (memory-source-conversation premise))
                        (application-memory-manager-editable-p snapshot)
                        (application-memory-manager-deletable-p snapshot))
                   "new notes default to workspace scope and are attributed/editable/deletable")
      (memory-remember configuration :identifier (memory-identifier premise)
                                    :title "Premise" :content "Changed")
      (let ((saved (application-memory-manager-save application snapshot :title "Retitled"
                                                    :content "Edited prose" :tags '("edited"))))
        (test-assert (and (equal (memory-sources note) (memory-sources saved))
                          (equal (memory-premises note) (memory-premises saved))
                          (string= conversation-id (memory-source-conversation saved))
                          (equal '("edited") (memory-tags saved)))
                     "ordinary edits preserve exact source/premise versions and attribute the change")
        (test-assert (eq ':stale (memory-manager-item-status
                                 (memory-manager-model-tests--item application (memory-identifier note))))
                     "editing prose does not refresh changed evidence")
        (test-assert (string= "Edited prose" (memory-content
                                              (find (memory-identifier saved) (memory-list configuration)
                                                    :test #'string= :key #'memory-identifier)))
                     "edited note is durable on reread"))
      (let* ((global (application-memory-manager-create application :title "Global" :content "Shared"
                                                                  :scope ':global))
             (saved (application-memory-manager-save
                     application (memory-manager-model-tests--item application (memory-identifier global))
                     :title "Local" :content "Here" :tags '("local") :scope ':workspace)))
        (test-assert (and (eq ':workspace (memory-scope saved))
                          (string= (memory-workspace saved) (namestring (config :working-directory configuration))))
                     "scope edits use the existing placement semantics"))
      (let* ((premise-item (memory-manager-model-tests--item application (memory-identifier premise)))
             (result (application-memory-manager-forget application premise-item)))
        (test-assert (and (listp result) (search "Retitled" (terminal--spans-text result)))
                     "forget returns spans naming dependent notes")
        (test-assert (not (find (memory-identifier premise) (memory-list configuration)
                               :test #'string= :key #'memory-identifier))
                     "forget persists a tombstone"))))
  nil)

(-> test-memory-manager-model-conflicts () null)
(defun test-memory-manager-model-conflicts ()
  "Reject stale edits and deletions, including same-second changes and disappearance."
  (with-test-configuration (configuration)
    (let* ((application (memory-manager-model-tests--application configuration))
           (note (application-memory-manager-create application :title "Original" :content "Original"))
           (snapshot (memory-manager-model-tests--item application (memory-identifier note))))
      (memory-remember configuration :identifier (memory-identifier note) :title "Concurrent"
                                    :content "Concurrent edit")
      (test-assert (memory-manager-model-tests--conflict-p
                    (lambda () (application-memory-manager-save application snapshot :title "Overwrite" :content "Lost")))
                   "stale saves signal a typed conflict")
      (test-assert (memory-manager-model-tests--conflict-p
                    (lambda () (application-memory-manager-forget application snapshot)))
                   "stale deletions signal a typed conflict")
      (test-assert (string= "Concurrent edit"
                            (memory-content (memory-manager-item-data
                                             (memory-manager-model-tests--item application (memory-identifier note)))))
                   "conflicted mutations do not overwrite the newer value")
      (let ((current (memory-manager-model-tests--item application (memory-identifier note))))
        (memory-forget configuration (memory-identifier note))
        (test-assert (memory-manager-model-tests--conflict-p
                      (lambda () (application-memory-manager-forget application current)))
                     "a disappeared note signals a conflict")
        (test-assert (memory-manager-model-tests--conflict-p
                      (lambda () (application-memory-manager-save application current :title "Recreate" :content "No")))
                     "saving a disappeared note does not recreate it"))))
  nil)

(-> test-memory-manager-model-fact-mutations () null)
(defun test-memory-manager-model-fact-mutations ()
  "Cover fact derivation, guarded retraction, dependent impact and persistence."
  (with-test-configuration (configuration)
    (let* ((application (memory-manager-model-tests--application configuration))
           (schema (memory-manager-model-tests--schema configuration))
           (claim (memory-manager-model-tests--assert configuration schema '("direct" 1))))
      (knowledge-define-schema
       configuration (knowledge-make-schema :predicate "manager.copy"
                                            :arguments '((:name "subject" :type :entity) (:name "value" :type :number))
                                            :template "{subject} copies {value}"))
      (knowledge-propose-rule configuration :id "manager-copy"
                              :premises '(("manager.value" "?subject" "?value"))
                              :conclusion '("manager.copy" "?subject" "?value"))
      (knowledge-admit-rule configuration "manager-copy")
      (let* ((direct (memory-manager-model-tests--item application claim))
             (derived (find "direct copies 1" (application-memory-manager-items application)
                            :test #'string= :key #'memory-manager-item-title)))
        (test-assert (and derived (not (application-memory-manager-editable-p derived))
                          (not (application-memory-manager-deletable-p derived))
                          (not (application-memory-manager-editable-p direct))
                          (application-memory-manager-deletable-p direct))
                     "only direct assertion facts are deletable, and facts are not prose-editable")
        (test-assert (handler-case (progn (application-memory-manager-forget application derived) nil)
                       (memory-error () t))
                     "derived retraction is refused at the mutation boundary")
        (test-assert (search "manager-copy" (terminal--spans-text
                                              (application-memory-manager-detail application derived)))
                     "derived detail includes its rule explanation")
        (knowledge-assert configuration (knowledge-make-proposition schema '("direct" 1))
                          :basis ':assumption)
        (test-assert (memory-manager-model-tests--conflict-p
                      (lambda () (application-memory-manager-forget application direct)))
                     "new concurrent support invalidates a captured fact deletion")
        (let* ((current (memory-manager-model-tests--item application claim))
               (result (application-memory-manager-forget application current))
               (base (progn (clrhash *knowledge-base-cache*) (knowledge-base-read configuration))))
          (test-assert (and (listp result) (search "direct copies 1" (terminal--spans-text result)))
                       "fact retraction returns spans naming withdrawn derived facts")
          (test-assert (and (every (lambda (identifier)
                                    (getf (knowledge-base-support base identifier) :retracted-at))
                                  (getf (knowledge-base-claim base claim) :supports))
                            (search (conversation-identifier (application-conversation application))
                                    (uiop:read-file-string (configuration-knowledge-path configuration))))
                       "retraction and active-conversation attribution survive log replay")
          (test-assert (memory-manager-model-tests--conflict-p
                        (lambda () (application-memory-manager-forget application current)))
                       "retracting an already-disappeared fact signals a conflict")))))
  nil)
