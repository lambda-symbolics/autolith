(in-package #:autolith)

;;;; -- Persistent Memory Tests --

(-> memory-tests--configuration-in-workspace
    (configuration pathname)
    configuration)
(defun memory-tests--configuration-in-workspace (configuration workspace)
  "Return CONFIGURATION's roots with WORKSPACE selected."
  (configuration-copy configuration :working-directory workspace))


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


(-> test-memory-context-notices () null)
(defun test-memory-context-notices ()
  "Exercise delivered memory counts and dim, per-input terminal notices."
  (with-test-configuration (configuration)
    (let* ((*context-contributors* nil)
           (*context-resolver* (cl-llm-provider-api:make-context-resolver))
           (*context-last-deliveries* (make-hash-table :test #'equal))
           (*context-last-delivery-order* nil)
           (*memory-context-result-limit* 2)
           (conversation (conversation-create configuration))
           (terminal (make-instance 'recording-terminal :columns 120))
           (ui (terminal-ui-create :terminal terminal))
           (application (make-instance 'application
                                       :configuration configuration
                                       :conversation conversation
                                       :ui ui))
           (observer (application-agent-observer application
                                                 :user-message-input "granite"))
           (callback (agent--provider-event-callback observer))
           (present (symbol-function 'application-present))
           (notices nil)
           (counts nil))
      (loop for index from 1 to 3
            do (memory-remember configuration
                                :title (format nil "Reference ~D" index)
                                :content "Saved context."
                                :tags '("granite")
                                :scope (if (= index 1) ':global ':workspace)))
      (register-context-contributor "related-memories" 'memory-related-context
                                    :source ':built-in)
      (labels ((notify (&key compaction-p)
                 "Resolve a request and forward its selected presentation events."
                 (let ((delivery (context-resolve-request
                                  configuration conversation #()
                                  :compaction-p compaction-p)))
                   (context-delivery-notify
                    delivery
                    (lambda (event)
                      (push (memory-context-event-count event) counts)
                      (funcall callback event)))
                   delivery)))
        (test-call-with-function-replacements
         (list (list 'application-present
                     (lambda (application entry)
                       (push entry notices)
                       (funcall present application entry))))
         (lambda ()
           (unwind-protect
                (progn
                  (terminal-ui-start ui)
                  (multiple-value-bind (item record)
                      (conversation-append-user-message conversation "granite")
                    (declare (ignore item))
                    (agent-observer-status
                     observer :user-message-persisted
                     (list :sequence (getf (rest record) :seq)
                           :time (getf (rest record) :time))))
                  (agent-observer-status observer :provider-request-started nil)
                  (notify)
                  (let* ((entry (first notices))
                         (output (recording-terminal-output terminal))
                         (input-position (search "granite" output))
                         (notice-position (search (terminal--spans-text entry) output)))
                    (test-assert (and input-position notice-position
                                      (< input-position notice-position))
                                 "the memory notice is visible below the triggering input")
                    (test-assert (eq (terminal-span-style (first entry)) ':dim)
                                 "the memory notice uses dim presentation"))
                  (agent-observer-status observer :provider-request-started nil)
                  (notify)
                  (funcall callback (make-instance 'provider-retry-event
                                                   :attempt 1 :maximum-attempts 2
                                                   :delay 0))
                  (notify)
                  (test-assert (and (= (length notices) 1)
                                    (equal counts '(2 2 2)))
                               "requests and retries expose the capped count without repeated notices")
                  (let ((*context-advice-token-budget* 0))
                    (notify))
                  (notify :compaction-p t)
                  (test-assert (equal counts '(2 2 2))
                               "omitted advice and compaction do not announce memories")
                  (conversation-append-user-message conversation "granite")
                  (agent-observer-status observer :steering-applied nil)
                  (let ((*memory-context-result-limit* 1))
                    (notify))
                  (test-assert (and (= (length notices) 2)
                                    (= (first counts) 1))
                               "a new input gets its own notice with the current offered count")
                  (conversation-append-user-message conversation "basalt")
                  (agent-observer-status observer :steering-applied nil)
                  (notify)
                  (test-assert (and (= (length notices) 2)
                                    (= (length counts) 4))
                               "input without matching memories produces no notice"))
             (terminal-ui-stop ui)))))))
  nil)
