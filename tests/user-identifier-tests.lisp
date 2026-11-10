(in-package #:autolith)

;;;; -- Persistent User Identifiers --

(-> user-identifier-tests--deterministic (function) t)
(defun user-identifier-tests--deterministic (function)
  "Run allocation checks with a fixed timestamp and initial seed."
  (let ((*random-index-function* (lambda (limit) (declare (ignore limit)) 10)))
    (unwind-protect
         (test-call-with-function-replacements
          (list (list 'make-user-identifier
                      (lambda (&key (namespace "autolith") occupied-p)
                        (identifier-generate :timestamp 3994000000
                                             :namespace namespace :occupied-p occupied-p))))
          (lambda ()
            (identifier-clear-reservations)
            (funcall function)))
      (identifier-clear-reservations))))

(-> test-agenda-identifier-references () null)
(defun test-agenda-identifier-references ()
  "Exercise fresh-state short writes, exact priority, and full identity preservation."
  (with-test-configuration (configuration)
    (let* ((state       (agenda-load configuration))
           (legacy      "aaaaaaaa-1111-4111-8111-111111111111")
           (other       "aaaaaaaa-2222-4222-8222-222222222222")
           (identifiers (list "K8vQ2mp" legacy other "abc" "abc-more")))
      (agenda--call-with-transaction
       configuration state
       (lambda (current)
         (setf (agenda-state-records current)
               (list (make-instance
                      'workspace-agenda
                      :directory (agenda-directory-name
                                  configuration (config :working-directory configuration)
                                  :require-existing-p t)
                      :items     (loop for identifier in identifiers
                                       collect (make-instance 'agenda-item
                                                              :identifier identifier :text identifier
                                                              :status ':todo :created-at 1 :updated-at 1)))))
         t))
      (let ((updated (agenda-update configuration state "aaaaaaaa-1" :text "updated legacy")))
        (test-assert (string= legacy (agenda-item-identifier updated))
                     "short updates preserve the selected full legacy UUID"))
      (test-assert (string= "abc" (agenda-item-identifier
                                   (agenda-update configuration state "abc" :status ':done)))
                   "an exact identity takes priority over longer identities")
      (test-assert (string= "K8vQ2mp" (agenda-item-identifier
                                       (agenda-update configuration state "K8v" :status ':doing)))
                   "native short IDs also accept unique prefixes")
      (dolist (operation (list (lambda () (agenda-update configuration state "aaaa" :text "bad"))
                               (lambda () (agenda-remove configuration state "aaaa"))))
        (test-assert
         (handler-case (progn (funcall operation) nil)
           (identifier-ambiguity (condition)
             (equal (sort (copy-list (identifier-ambiguity-identifiers condition)) #'string<)
                    (sort (list legacy other) #'string<))))
         "ambiguous writes report the full matching identities"))
      (let* ((loaded (agenda-load configuration))
             (items  (workspace-agenda-items (agenda-current configuration loaded))))
        (test-assert (= 5 (length items)) "updates do not append or lose agenda entries")
        (test-assert (string= "updated legacy"
                              (agenda-item-text (find legacy items :key #'agenda-item-identifier :test #'string=)))
                     "reload retains the full legacy identity and replacement text")
        (test-assert (agenda-remove configuration loaded "aaaaaaaa-1")
                     "a unique legacy reference removes its selected entry")
        (test-assert (find other (workspace-agenda-items
                                  (agenda-current configuration (agenda-load configuration)))
                           :key #'agenda-item-identifier :test #'string=)
                     "removal preserves the other legacy identity"))))
  nil)

(-> test-papercut-identifier-references () null)
(defun test-papercut-identifier-references ()
  "Exercise legacy papercut reads, transactional writes, and distinguishing display."
  (with-test-configuration (configuration)
    (let ((identifiers '("aaaaaaaa-1111-4111-8111-111111111111"
                         "aaaaaaaa-2222-4222-8222-222222222222"
                         "K8vQ2mp" "abc" "abc-more")))
      (papercut--transact
       configuration
       (lambda (active)
         (declare (ignore active))
         (values (loop for identifier in identifiers
                       collect (papercut--record
                                (make-instance 'papercut :identifier identifier
                                               :workspace            (papercut--workspace configuration)
                                               :title identifier :content identifier
                                               :reported-at 1 :source-conversation nil)))
                 t t)))
      (test-assert (string= "abc" (papercut-identifier (papercut-find configuration "abc")))
                   "exact papercut IDs win over longer matching identities")
      (test-assert (string= "K8vQ2mp" (papercut-identifier (papercut-find configuration "K8v")))
                   "native papercuts accept unique references")
      (test-assert (eq ':found (nth-value 1 (papercut-resolve configuration "abc")))
                   "resolver shares exact-first priority")
      (test-assert (eq ':ambiguous (nth-value 1 (papercut-resolve configuration "aaaa")))
                   "the public resolver reports ambiguity")
      (test-assert
       (handler-case
           (progn (papercut-assess configuration "aaaa" :verdict ':improved :note "checked") nil)
         (identifier-ambiguity (condition)
           (= 2 (length (identifier-ambiguity-identifiers condition)))))
       "assessment rejects ambiguity with candidate identities")
      (test-assert
       (handler-case
           (progn (papercut-mark-closed configuration "aaaa" :resolution "checked") nil)
         (identifier-ambiguity () t))
       "closure rejects ambiguous references")
      (dolist (report (papercut-list configuration))
        (test-assert (string= (papercut-identifier report)
                              (papercut-identifier
                               (papercut-find configuration (papercut-short-identifier report))))
                     "every displayed prefix resolves to its full stored identity"))
      (papercut-assess configuration "aaaaaaaa-1" :verdict ':improved :note "checked")
      (test-assert
       (eq ':improved (papercut-assessment-verdict (papercut-find configuration (first identifiers))))
       "short assessments persist under the full legacy UUID")
      (let ((closed (papercut-mark-closed configuration "aaaaaaaa-1" :resolution "verified")))
        (test-assert (string= (first identifiers) (papercut-identifier closed))
                     "closure returns the full selected UUID")
        (test-assert (null (papercut-find configuration (first identifiers)))
                     "short closure persists and replays successfully"))))
  nil)

(-> test-persisted-user-identifier-collisions () null)
(defun test-persisted-user-identifier-collisions ()
  "Simulate reservation loss while agenda, closed reports, and journal IDs persist."
  (with-test-configuration (configuration)
    (user-identifier-tests--deterministic
     (lambda ()
       (let* ((state (agenda-load configuration))
              (first (agenda-add :configuration configuration :state state :text "first")))
         (identifier-clear-reservations)
         (let ((second (agenda-add :configuration configuration :state (agenda-load configuration) :text "second")))
           (test-assert (not (string= (agenda-item-identifier first) (agenda-item-identifier second)))
                        "agenda allocation excludes persisted IDs after reservation loss")
           (identifier-clear-reservations)
           (let* ((source (make-instance 'agenda-item
                                         :identifier (agenda-item-identifier first)
                                         :text "transported" :status ':todo
                                         :created-at 1 :updated-at 1))
                  (merged (agenda--merge-items configuration (list first second) (list source))))
             (test-assert (= 3 (length (remove-duplicates (mapcar #'agenda-item-identifier merged) :test #'string=)))
                          "transport probes past every occupied target identity"))))
       (let* ((target (first (workspace-agenda-items
                              (agenda-current configuration (agenda-load configuration)))))
              (source (make-instance 'agenda-item
                                     :identifier (agenda-item-identifier target)
                                     :text "colliding source" :status ':todo
                                     :created-at 1 :updated-at 1))
              (later  (make-instance 'agenda-item
                                     :identifier "K8vQ2mp"
                                     :text "later source" :status ':todo
                                     :created-at 1 :updated-at 1)))
         (test-call-with-function-replacements
          (list (list 'make-user-identifier
                      (lambda (&key namespace occupied-p)
                        (declare (ignore namespace))
                        (if (funcall occupied-p "K8vQ2mp") "K8vQ2mq" "K8vQ2mp"))))
          (lambda ()
            (let ((merged (agenda--merge-items configuration (list target) (list source later))))
              (test-assert (string= "K8vQ2mp" (agenda-item-identifier (third merged)))
                           "transport preserves a later source identity during collision allocation")
              (test-assert (string= "K8vQ2mq" (agenda-item-identifier (second merged)))
                           "replacement allocation excludes every source identity")))))
       (identifier-clear-reservations)
       (let ((closed (papercut-report configuration :title "red fox" :content "red fox")))
         (papercut-mark-closed configuration (papercut-identifier closed) :resolution "verified")
         (identifier-clear-reservations)
         (let ((fresh (papercut-report configuration :title "blue whale" :content "blue whale")))
           (test-assert (not (string= (papercut-identifier closed) (papercut-identifier fresh)))
                        "new reports exclude historical closed IDs")
           (test-assert (= 1 (length (papercut-list configuration)))
                        "the lifecycle reducer accepts the new historical-unique identity")))
       (identifier-clear-reservations)
       (let ((first (mutation-identifier-allocate configuration)))
         (identifier-clear-reservations)
         (test-assert (not (string= first (mutation-identifier-allocate configuration)))
                      "mutation allocation consults durable journal reservations"))
       (identifier-clear-reservations)
       (test-call-with-function-replacements
        (list (list 'checkpoint--source-snapshot (lambda (configuration)
                                                  (declare (ignore configuration)) nil))
              (list 'checkpoint--revalidate-source (lambda (configuration commit)
                                                    (declare (ignore configuration commit)) nil))
              (list 'checkpoint-single-threaded-p (lambda () t))
              (list 'generation--metadata
                    (lambda (configuration identifier &key git-commit mutation-checker)
                      (declare (ignore configuration git-commit mutation-checker))
                      ;; Observe allocation at the real checkpoint boundary before any fork or save.
                      (throw 'allocated-generation identifier))))
        (lambda ()
          (flet ((allocate ()
                   (catch 'allocated-generation
                     (checkpoint-create (checkpoint-backend-create configuration nil)))))
            (let ((first (allocate)))
              (test-assert (string= first (identifier-normalize first))
                           "the production checkpoint backend allocates native identities")
              (identifier-clear-reservations)
              (test-assert (not (string= first (allocate)))
                           "checkpoint allocation excludes pending directories after reservation loss"))))))))
  nil)

(-> test-generation-identifier-references () null)
(defun test-generation-identifier-references ()
  "Resolve persisted native and UUID generation manifests, including rollback selection."
  (with-test-configuration (configuration)
    (let ((identifiers '("K8vQ2mp" "K8vQ2mp-long"
                         "aaaaaaaa-1111-4111-8111-111111111111"
                         "aaaaaaaa-2222-4222-8222-222222222222")))
      (dolist (identifier identifiers)
        (let* ((generation (generation-tests--generation configuration identifier "0123456789abcdef"))
               (runner     (make-instance 'test-generation-core-probe-runner
                                          :output (generation-core-probe-output
                                                   (generation-core-probe-record generation)))))
          (generation-tests--write-fake-core generation)
          (generation-tests--write-reconstruction generation)
          (generation-publish configuration generation :probe-runner runner)
          (generation-tests--make-core-plausible generation)))
      (test-assert (string= "K8vQ2mp" (generation-identifier (generation-find configuration "K8vQ2mp")))
                   "exact native generation identities win over longer matches")
      (test-assert (string= (third identifiers)
                            (generation-identifier (generation-find configuration "aaaaaaaa-1")))
                   "legacy generation references resolve from persisted manifests")
      (test-assert
       (handler-case (progn (generation-find configuration "aaaa") nil)
         (identifier-ambiguity (condition)
           (= 2 (length (identifier-ambiguity-identifiers condition)))))
       "generation ambiguity includes matching full identities")
      (test-assert
       (handler-case (progn (generation-request-rollback configuration "aaaa") nil)
         (identifier-ambiguity () t))
       "ambiguous rollback references do not select a generation")
      (test-assert
       (handler-case (progn (generation-request-rollback configuration "aaaaaaaa-1") nil)
         (rollback-requested () t))
       "a unique legacy prefix reaches rollback selection")
      (test-assert (string= (third identifiers)
                            (generation-identifier (generation-selected configuration)))
                   "rollback persists the full selected legacy identity")))
  nil)

(-> test-mutation-identifier-references () null)
(defun test-mutation-identifier-references ()
  "Resolve mutation references from the persisted effective journal selection."
  (with-test-configuration (configuration)
    (let ((*image-state-initialized-p*       t)
          (*active-image-lineage-identifier* "identifier-reference-test")
          (*active-image-commit-identifier*  nil)
          (*active-image-history-commit*     nil)
          (identifiers                       '("K8vQ2mp" "K8vQ2mp-long"
                                               "aaaaaaaa-1111-4111-8111-111111111111"
                                               "aaaaaaaa-2222-4222-8222-222222222222")))
      (dolist (identifier identifiers)
        (mutation-journal-append
         configuration
         (list :mutation :kind ':set :result ':installed :id identifier
               :lineage *active-image-lineage-identifier*
               :target (format nil "*test-~A*" identifier) :proposed "1")))
      (test-assert (string= (first identifiers)
                            (getf (rest (self-discard--record configuration "K8vQ2mp")) :id))
                   "exact mutation identities win over longer matches")
      (test-assert (string= (third identifiers)
                            (getf (rest (self-discard--record configuration "aaaaaaaa-1")) :id))
                   "mutation references retain the selected full UUID")
      (test-assert
       (handler-case (progn (self-discard--record configuration "aaaa") nil)
         (identifier-ambiguity (condition)
           (= 2 (length (identifier-ambiguity-identifiers condition)))))
       "ambiguous mutation references expose matching persistent identities")))
  nil)
