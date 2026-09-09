(in-package #:autolith)

;;;; -- Persistent Memory Tests --

(-> memory-tests--configuration-in-workspace
    (configuration pathname)
    configuration)
(defun memory-tests--configuration-in-workspace (configuration workspace)
  "Return CONFIGURATION's roots with WORKSPACE selected."
  (make-instance 'configuration
                 :source-root (configuration-source-root configuration)
                 :working-directory workspace
                 :config-root (configuration-config-root configuration)
                 :data-root (configuration-data-root configuration)
                 :state-root (configuration-state-root configuration)
                 :cache-root (configuration-cache-root configuration)
                 :config-root (configuration-config-root configuration)
                 :codex-auth-path (configuration-codex-auth-path configuration)
                 :model (configuration-model configuration)
                 :reasoning-effort
                 (configuration-reasoning-effort configuration)
                 :web-search-mode
                 (configuration-web-search-mode configuration)
                 :context-window
                 (configuration-context-window configuration)
                 :compaction-threshold-percent
                 (configuration-compaction-threshold-percent configuration)
                 :provider-endpoint
                 (configuration-provider-endpoint configuration)))


(-> test-memory-persistence () null)
(defun test-memory-persistence ()
  "Test memory replay, scope selection, search, replacement, and tombstones."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (other-workspace (merge-pathnames "other-workspace/" root))
         (other (memory-tests--configuration-in-workspace
                 configuration
                 other-workspace)))
    (unwind-protect
         (progn
           (ensure-directories-exist other-workspace)
           (let* ((workspace-memory
                    (memory-remember
                     configuration
                     :title "Build command"
                     :content "Run ./script/check before every commit."
                     :tags '("tests" "workflow")
                     :source-conversation "first"))
                  (global-memory
                    (memory-remember
                     configuration
                     :title "Response preference"
                     :content "Keep final answers concise."
                     :scope :global
                     :tags '("style")
                     :source-conversation "first"))
                  (other-memory
                    (memory-remember
                     other
                     :title "Other project"
                     :content "This belongs elsewhere."
                     :tags nil
                     :source-conversation "second")))
             (test-assert (= (length (memory-list configuration)) 2)
                          "relevant memory selection includes global and current workspace")
             (test-assert (= (length (memory-list other)) 2)
                          "another workspace sees its own and global memories")
             (test-assert (= (length (memory-list configuration
                                                   :visibility ':all))
                             3)
                          "all-scope memory listing crosses workspaces")
             (test-assert
              (not (find (memory-identifier other-memory)
                         (memory-list configuration)
                         :test #'string=
                         :key #'memory-identifier))
              "unrelated workspace memory is absent from relevant selection")
             (test-assert
              (equal (mapcar #'memory-identifier
                             (memory-search configuration "script WORKFLOW"))
                     (list (memory-identifier workspace-memory)))
              "memory search matches all terms across content and tags")
             (test-assert
              (handler-case
                  (progn
                    (memory-remember
                     configuration
                     :identifier "missing-memory"
                     :title "Missing"
                     :content "This replacement must fail."
                     :tags nil)
                    nil)
                (memory-error ()
                  t))
              "replacement rejects an unknown memory identifier")
             (test-assert
              (handler-case
                  (progn
                    (memory-search configuration "   ")
                    nil)
                (memory-error ()
                  t))
              "memory search rejects an empty query")
             (test-assert
              (handler-case
                  (progn
                    (memory-remember
                     configuration
                     :title "Oversized"
                     :content (make-string (1+ *memory-content-limit*)
                                           :initial-element #\x)
                     :tags nil)
                    nil)
                (memory-error ()
                  t))
              "memory bodies have a hard size bound")
             (let ((replacement
                     (memory-remember
                      configuration
                      :identifier (memory-identifier workspace-memory)
                      :title "Repository check"
                      :content "Run the complete ./script/check command."
                      :tags '("tests")
                      :source-conversation "third")))
               (test-assert (string= (memory-source-conversation replacement)
                                     "third")
                            "replacement records retain their newest source"))
             (memory-forget configuration (memory-identifier global-memory))
             (test-assert (null (memory-find configuration
                                            (memory-identifier global-memory)))
                          "memory tombstones remove active recall")
             (let* ((matches (memory-rank configuration "Repository script"))
                    (best (first matches))
                    (conversation
                      (conversation-create configuration
                                           :identifier "memory-context")))
               (test-assert
                (and best
                     (string= (memory-title (memory-match-memory best))
                              "Repository check")
                     (plusp (memory-match-score best)))
                "memory ranking favors weighted title and content matches")
               (conversation-append-user-message conversation
                                                 "Check the repository script")
               (let* ((request
                        (make-instance 'request-context
                                       :configuration configuration
                                       :conversation conversation
                                       :tool-namespaces #()))
                      (contribution (memory-related-context request))
                      (evidence
                        (and contribution
                             (context-contribution-evidence contribution))))
                 (test-assert
                  (and evidence
                       (search "Repository check" evidence)
                       (not (search "Other project" evidence)))
                  "request-local recall offers ranked relevant memory metadata")))
             (with-open-file (stream (configuration-memory-path configuration)
                                     :direction ':output
                                     :if-exists ':append
                                     :external-format ':utf-8)
               (write-string "#.(error \"reader evaluation ran\")" stream))
             (let* ((conversation
                      (conversation-create configuration
                                           :identifier "memory-corruption"))
                    (*context-contributors* nil)
                    (*context-last-delivery* nil))
               (conversation-append-user-message conversation "repository script")
               (register-context-contributor "related-memories"
                                             'memory-related-context
                                             :source ':built-in)
               (let ((delivery
                       (context-resolve-request configuration conversation #())))
                 (test-assert
                  (string= (first (first
                                    (context-delivery-failures delivery)))
                           "related-memories")
                  "malformed memory data degrades to context diagnostics without reader evaluation")))))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)
