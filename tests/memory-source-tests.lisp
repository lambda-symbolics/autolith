(in-package #:autolith)

;;;; -- Sourced Memory Tests --

(-> memory-source-tests--write (pathname string) pathname)
(defun memory-source-tests--write (pathname content)
  "Replace PATHNAME with exact UTF-8 CONTENT and return PATHNAME."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction ':output
                          :if-exists ':supersede
                          :if-does-not-exist ':create
                          :external-format ':utf-8)
    (write-string content stream))
  pathname)

(-> memory-source-tests--workspace (configuration pathname)
    (values configuration pathname))
(defun memory-source-tests--workspace (configuration root)
  "Return CONFIGURATION working in a fresh workspace below ROOT, and that workspace."
  (let ((workspace (merge-pathnames "workspace/" root)))
    (ensure-directories-exist workspace)
    (values (memory-tests--configuration-in-workspace configuration workspace)
            workspace)))

(-> memory-source-tests--error-p (function) boolean)
(defun memory-source-tests--error-p (function)
  "Return true when FUNCTION signals MEMORY-ERROR."
  (handler-case
      (progn
        (funcall function)
        nil)
    (memory-error ()
      t)))


(-> test-memory-source-records () null)
(defun test-memory-source-records ()
  "Test durable sources: observation, placement, replay, replacement, and validation."
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (let* ((notes (memory-source-tests--write
                     (merge-pathnames "docs/notes.txt" workspace)
                     (format nil "Deploy with~%    make release-prod~%after tagging.")))
             (observed (memory-source-observe-file
                        notes
                        :quote "with make   release-prod"
                        :time (get-universal-time)))
             (sourced (memory-remember configuration
                                       :title "Deploy command"
                                       :content "Deploy with make release-prod."
                                       :sources (list observed)))
             (plain (memory-remember configuration
                                     :title "Plain"
                                     :content "No evidence attached.")))
        (test-assert (equal (getf (rest observed) :path)
                            (uiop:native-namestring notes))
                     "an observed file source starts absolute")
        (let ((source (first (memory-sources sourced))))
          (test-assert (and (equal (getf (rest source) :path) "docs/notes.txt")
                            (eq (getf (rest source) :relative) t))
                       "a workspace memory records files inside it relatively")
          (test-assert (equal (memory-sources
                               (memory-find configuration (memory-identifier sourced)))
                              (memory-sources sourced))
                       "sources survive replay from the memory log")
          (test-assert (equal (memory-source-file-pathname
                               source (memory-workspace sourced))
                              notes)
                       "a relative source resolves against its memory's workspace"))
        (test-assert (null (nth-value 2 (get-properties (rest (memory--record plain))
                                                        '(:sources :premises))))
                     "memories without evidence keep their earlier record form")
        (let ((kept (memory-remember configuration
                                     :identifier (memory-identifier sourced)
                                     :title "Deploy command"
                                     :content "Deploy with make release-prod after tagging.")))
          (test-assert (equal (memory-sources kept) (memory-sources sourced))
                       "a replacement that omits sources keeps them"))
        (let ((global (memory-remember configuration
                                       :identifier (memory-identifier sourced)
                                       :scope ':global
                                       :title "Deploy command"
                                       :content "Deploy with make release-prod.")))
          (test-assert (let ((source (first (memory-sources global))))
                         (and (equal (getf (rest source) :path)
                                      (uiop:native-namestring notes))
                              (null (getf (rest source) :relative))))
                       "moving a memory to global scope records its files absolutely"))
        (let ((dropped (memory-remember configuration
                                        :identifier (memory-identifier sourced)
                                        :title "Deploy command"
                                        :content "Deploy with make release-prod."
                                        :sources nil)))
          (test-assert (null (memory-sources dropped))
                       "an explicit empty source list removes the sources"))
        (test-assert (memory-source-tests--error-p
                      (lambda ()
                        (memory-source-observe-file notes
                                                    :quote "make release-staging"
                                                    :time (get-universal-time))))
                     "a quote absent from the file is rejected")
        (test-assert (memory-source-tests--error-p
                      (lambda ()
                        (memory-source-observe-file notes
                                                    :quote "make"
                                                    :time (get-universal-time))))
                     "a quote too short to identify a passage is rejected")
        (test-assert (memory-source-tests--error-p
                      (lambda ()
                        (memory-source-observe-file
                         (merge-pathnames "missing.txt" workspace)
                         :time (get-universal-time))))
                     "a missing file cannot be cited")
        (dolist (case (list (list "a relative file source in a global memory"
                                  ':global
                                  (list (list :file :path "x.txt" :relative t
                                              :digest (memory-source-text-digest "x")
                                              :observed-at 1)))
                            (list "an unknown source kind"
                                  ':workspace
                                  (list (list :rumor :quote "somebody said so"
                                              :observed-at 1)))
                            (list "a user source without a quote"
                                  ':workspace
                                  (list (list :user :observed-at 1)))))
          (destructuring-bind (label scope sources) case
            (test-assert
             (memory-source-tests--error-p
              (lambda ()
                (memory--record->memory
                 #P"memories.sexp"
                 (list :memory :version *memory-format-version*
                       :id "invalid" :created-at 1 :updated-at 1
                       :scope scope
                       :workspace (and (eq scope ':workspace)
                                       (namestring workspace))
                       :title "Invalid" :content "Invalid sources."
                       :tags nil :source-conversation nil
                       :sources sources))))
             (format nil "~A is rejected when the memory log is read" label)))))))
  nil)

(-> test-memory-premise-records () null)
(defun test-memory-premise-records ()
  "Test derived memories: premise digests, rebinding, scope, and cycles."
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (declare (ignore workspace))
      (let* ((other (memory-tests--configuration-in-workspace
                     base (merge-pathnames "other/" root)))
             (meeting (memory-remember configuration
                                       :title "Meeting time"
                                       :content "The review meets at 13:00."))
             (travel (memory-remember configuration
                                      :scope ':global
                                      :title "Travel time"
                                      :content "The office is 30 minutes away."))
             (departure (memory-remember configuration
                                         :title "Departure"
                                         :content "Leave at 12:20."
                                         :premises (list (memory-identifier meeting)
                                                         (memory-identifier travel))))
             (foreign (memory-remember other
                                       :title "Elsewhere"
                                       :content "Another workspace fact.")))
        (test-assert (equal (memory-premises departure)
                            (list (list :id (memory-identifier meeting)
                                        :digest (memory-content-digest
                                                 "Meeting time"
                                                 "The review meets at 13:00."))
                                  (list :id (memory-identifier travel)
                                        :digest (memory-content-digest
                                                 "Travel time"
                                                 "The office is 30 minutes away."))))
                     "premises record the exact text they were derived from")
        (memory-remember configuration
                         :identifier (memory-identifier meeting)
                         :title "Meeting time"
                         :content "The review meets at 14:00.")
        (test-assert (equal (getf (first (memory-premises
                                          (memory-find configuration
                                                       (memory-identifier departure))))
                                  :digest)
                            (memory-content-digest "Meeting time"
                                                   "The review meets at 13:00."))
                     "replacing a premise leaves the derived memory's record unchanged")
        (let ((restated (memory-remember configuration
                                         :identifier (memory-identifier departure)
                                         :title "Departure"
                                         :content "Leave at 13:20.")))
          (test-assert (equal (getf (first (memory-premises restated)) :digest)
                              (memory-content-digest "Meeting time"
                                                     "The review meets at 13:00."))
                       "a replacement that omits premises keeps the recorded versions"))
        (let ((rebound (memory-remember configuration
                                        :identifier (memory-identifier departure)
                                        :title "Departure"
                                        :content "Leave at 13:20."
                                        :premises (list (memory-identifier meeting)
                                                        (memory-identifier travel)))))
          (test-assert (equal (getf (first (memory-premises rebound)) :digest)
                              (memory-content-digest "Meeting time"
                                                     "The review meets at 14:00."))
                       "supplying premises again rebinds them to their current text"))
        (dolist (case
                 (list
                  (list "a missing premise"
                        (lambda ()
                          (memory-remember configuration
                                           :title "Orphan" :content "Derived from nothing."
                                           :premises (list "no-such-memory"))))
                  (list "a memory as its own premise"
                        (lambda ()
                          (memory-remember configuration
                                           :identifier (memory-identifier meeting)
                                           :title "Meeting time"
                                           :content "Circular."
                                           :premises (list (memory-identifier meeting)))))
                  (list "a premise cycle"
                        (lambda ()
                          (memory-remember configuration
                                           :identifier (memory-identifier meeting)
                                           :title "Meeting time"
                                           :content "Circular."
                                           :premises (list (memory-identifier departure)))))
                  (list "a global memory relying on a workspace memory"
                        (lambda ()
                          (memory-remember configuration
                                           :scope ':global
                                           :title "Global" :content "Too broad."
                                           :premises (list (memory-identifier meeting)))))
                  (list "a premise from another workspace"
                        (lambda ()
                          (memory-remember configuration
                                           :title "Mixed" :content "Wrong workspace."
                                           :premises (list (memory-identifier foreign)))))))
          (destructuring-bind (label function) case
            (test-assert (memory-source-tests--error-p function)
                         (format nil "~A is rejected" label)))))))
  nil)

(-> memory-source-tests--status (configuration memory) (values keyword list))
(defun memory-source-tests--status (configuration memory)
  "Return MEMORY's status and reason kinds from a fresh assessment pass."
  (let ((assessment (memory-assess (memory-assessor-create configuration)
                                   (memory-find configuration
                                                (memory-identifier memory)))))
    (values (memory-assessment-status assessment)
            (mapcar (lambda (reason)
                      (getf reason :kind))
                    (memory-assessment-reasons assessment)))))

(-> memory-source-tests--cite (configuration pathname &key (:quote (option string)) (:title string))
    memory)
(defun memory-source-tests--cite (configuration pathname &key quote title)
  "Remember a memory titled TITLE that cites PATHNAME, optionally by QUOTE."
  (memory-remember configuration
                   :title title
                   :content (format nil "~A is documented." title)
                   :sources (list (memory-source-observe-file
                                   pathname
                                   :quote quote
                                   :time (get-universal-time)))))


(-> test-memory-source-status () null)
(defun test-memory-source-status ()
  "Test that cited files keep, lose, or cannot confirm their memories' support."
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (let* ((config-file (memory-source-tests--write
                           (merge-pathnames "config.toml" workspace)
                           (format nil "[server]~%port = 8080~%host = \"local\"~%")))
             (readme (memory-source-tests--write
                      (merge-pathnames "README" workspace)
                      (format nil "The server listens on port 8080.~%")))
             (quoted (memory-source-tests--cite configuration config-file
                                                :title "Port" :quote "port = 8080"))
             (whole (memory-source-tests--cite configuration config-file
                                               :title "Whole file"))
             (alternatives
               (memory-remember
                configuration
                :title "Port with two sources"
                :content "The port is 8080."
                :sources (list (memory-source-observe-file
                                config-file :quote "port = 8080" :time 1)
                               (memory-source-observe-file
                                readme :quote "listens on port 8080" :time 1))))
             (statement
               (memory-remember
                configuration
                :title "Preference"
                :content "Prefers tabs."
                :sources (list (list :user :quote "I always want tabs, please"
                                     :conversation "c" :observed-at 1)
                               (list :url :url "https://example.com/style"
                                     :quote nil :observed-at 1))))
             (mixed
               (memory-remember
                configuration
                :title "Port the user confirmed"
                :content "The port is 8080, as the user said."
                :sources (list (memory-source-observe-file
                                config-file :quote "port = 8080" :time 1)
                               (list :user :quote "the port is 8080"
                                     :conversation "c" :observed-at 1))))
             (plain (memory-remember configuration
                                     :title "Plain" :content "Nothing cited.")))
        (dolist (memory (list quoted whole alternatives statement mixed plain))
          (test-assert (eq (memory-source-tests--status configuration memory) ':current)
                       (format nil "~A starts current" (memory-title memory))))
        (memory-source-tests--write
         config-file (format nil "# edited~%[server]~%port   =   8080~%host = \"remote\"~%"))
        (test-assert (eq (memory-source-tests--status configuration quoted) ':current)
                     "an unrelated edit keeps a quoted passage's memory current")
        (test-assert (equal (multiple-value-list
                             (memory-source-tests--status configuration whole))
                            '(:stale (:file-changed)))
                     "any edit makes a memory citing the whole file stale")
        (memory-remember configuration
                         :identifier (memory-identifier whole)
                         :title "Whole file"
                         :content "Whole file is documented."
                         :tags '("config"))
        (test-assert (equal (multiple-value-list
                             (memory-source-tests--status configuration whole))
                            '(:stale (:file-changed)))
                     "retagging a memory does not reobserve its changed file")
        (memory-source-tests--write config-file (format nil "[server]~%port = 9090~%"))
        (test-assert (equal (multiple-value-list
                             (memory-source-tests--status configuration quoted))
                            '(:stale (:file-changed)))
                     "removing the quoted passage makes the memory stale")
        (test-assert (equal (multiple-value-list
                             (memory-source-tests--status configuration mixed))
                            '(:stale (:file-changed)))
                     "a statement does not keep a memory current once its cited file changes")
        (test-assert (eq (memory-source-tests--status configuration alternatives) ':current)
                     "one holding source keeps a memory with alternatives current")
        (delete-file readme)
        (test-assert (equal (multiple-value-list
                             (memory-source-tests--status configuration alternatives))
                            '(:stale (:file-changed :file-missing)))
                     "a memory whose every source fails is stale for each reason")
        (test-assert (equal (memory-assessment-summary
                             (memory-assess (memory-assessor-create configuration)
                                            (memory-find configuration
                                                         (memory-identifier alternatives))))
                            "config.toml changed; README is missing")
                     "the summary names each failed source by its workspace path")
        (with-open-file (stream config-file
                                :direction ':output
                                :if-exists ':supersede
                                :element-type '(unsigned-byte 8))
          (write-sequence (make-array 3 :element-type '(unsigned-byte 8)
                                        :initial-contents '(#xff #xfe #x00))
                          stream))
        (test-assert (equal (multiple-value-list
                             (memory-source-tests--status configuration quoted))
                            '(:unverified (:file-unchecked)))
                     "a cited file that cannot be read leaves its memory unverified")
        (test-assert (eq (memory-source-tests--status configuration statement) ':current)
                     "statements and web pages continue to hold")
        (let ((assessor (memory-assessor-create configuration)))
          (test-assert (equal (memory-assessment-summary
                               (memory-assess assessor
                                              (memory-find configuration
                                                           (memory-identifier alternatives))))
                              "config.toml could not be checked (not text); README is missing")
                       "an unreadable alternative leaves the memory unverified, not stale")))))
  nil)

(-> test-memory-premise-status () null)
(defun test-memory-premise-status ()
  "Test that derived memories follow their premises' changes and staleness."
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (let* ((schedule (memory-source-tests--write
                        (merge-pathnames "schedule.txt" workspace)
                        (format nil "Review at 13:00 in room 4.~%")))
             (meeting (memory-source-tests--cite configuration schedule
                                                 :title "Meeting" :quote "Review at 13:00"))
             (travel (memory-remember configuration
                                      :title "Travel" :content "30 minutes to the office."))
             (departure (memory-remember configuration
                                         :title "Departure" :content "Leave at 12:20."
                                         :premises (list (memory-identifier meeting)
                                                         (memory-identifier travel))))
             (reminder (memory-remember configuration
                                        :title "Reminder" :content "Set an alarm for 12:10."
                                        :premises (list (memory-identifier departure)))))
        (test-assert (eq (memory-source-tests--status configuration reminder) ':current)
                     "a chain of derived memories starts current")
        (memory-source-tests--write schedule (format nil "Review at 14:00 in room 4.~%"))
        (test-assert (equal (multiple-value-list
                             (memory-source-tests--status configuration departure))
                            '(:stale (:premise-stale)))
                     "a stale premise makes the derived memory stale")
        (test-assert (equal (multiple-value-list
                             (memory-source-tests--status configuration reminder))
                            '(:stale (:premise-stale)))
                     "staleness propagates through the whole chain")
        (memory-source-tests--write schedule (format nil "Review at 13:00 in room 5.~%"))
        (test-assert (eq (memory-source-tests--status configuration reminder) ':current)
                     "restoring the cited passage restores the whole chain")
        (memory-remember configuration
                         :identifier (memory-identifier travel)
                         :title "Travel" :content "45 minutes to the office.")
        (test-assert (equal (multiple-value-list
                             (memory-source-tests--status configuration departure))
                            '(:stale (:premise-changed)))
                     "a replaced premise makes the derived memory stale")
        (memory-remember configuration
                         :identifier (memory-identifier departure)
                         :title "Departure" :content "Leave at 12:20."
                         :tags '("travel"))
        (test-assert (equal (multiple-value-list
                             (memory-source-tests--status configuration departure))
                            '(:stale (:premise-changed)))
                     "retagging a stale derived memory does not make it current")
        (memory-remember configuration
                         :identifier (memory-identifier departure)
                         :title "Departure" :content "Leave at 12:05."
                         :premises (list (memory-identifier meeting)
                                         (memory-identifier travel)))
        (test-assert (eq (memory-source-tests--status configuration departure) ':current)
                     "rebinding the premises after recomputing makes it current")
        (memory-forget configuration (memory-identifier meeting))
        (test-assert (equal (multiple-value-list
                             (memory-source-tests--status configuration departure))
                            '(:stale (:premise-forgotten)))
                     "a forgotten premise makes the derived memory stale")
        (let ((assessor (memory-assessor-create configuration)))
          (test-assert (equal (mapcar #'memory-identifier
                                      (memory-assessor-dependents
                                       assessor (memory-identifier departure)))
                              (list (memory-identifier reminder)))
                       "dependents are the memories derived directly from a memory")))))
  nil)

(-> memory-source-tests--recall (configuration conversation)
    (values (option context-contribution) list))
(defun memory-source-tests--recall (configuration conversation)
  "Resolve one request for CONVERSATION and return its memory advice and events."
  (let* ((*context-contributors* nil)
         (*context-resolver* (cl-llm-provider-api:make-context-resolver))
         (*context-last-deliveries* (make-hash-table :test #'equal))
         (*context-last-delivery-order* nil)
         (events nil))
    (register-context-contributor "related-memories" 'memory-related-context
                                  :source ':built-in)
    (let ((delivery (context-resolve-request configuration conversation #())))
      (context-delivery-notify delivery (lambda (event)
                                          (push event events)))
      (values (find "related-memories" (context-delivery-contributions delivery)
                    :key #'context-contribution-identifier
                    :test #'string=)
              events))))


(-> test-memory-recall-status () null)
(defun test-memory-recall-status ()
  "Test that recall flags stale memories and leaves evidence-free recall unchanged."
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (let* ((conversation (conversation-create configuration))
             (notes (memory-source-tests--write
                     (merge-pathnames "NOTES.md" workspace)
                     (format nil "Deploy with make release-prod after tagging.~%"))))
        (memory-remember configuration
                         :title "Deploy approvals"
                         :content "Deploys need one approval.")
        (conversation-append-user-message conversation "how do I deploy?")
        (multiple-value-bind (plain events)
            (memory-source-tests--recall configuration conversation)
          (test-assert (and plain
                            (not (search "\"status\"" (context-contribution-evidence plain)))
                            (not (search "Entries with a status"
                                         (context-contribution-instruction plain)))
                            (zerop (memory-context-event-stale-count (first events))))
                       "memories without evidence are recalled exactly as before"))
        (memory-remember configuration
                         :title "Deploy command"
                         :content "Deploy with make release-prod."
                         :sources (list (memory-source-observe-file
                                         notes :quote "make release-prod" :time 1)))
        (memory-source-tests--write notes (format nil "Deploy with make ship.~%"))
        (multiple-value-bind (advice events)
            (memory-source-tests--recall configuration conversation)
          (test-assert (search "\"status\":\"stale\",\"because\":\"NOTES.md changed\""
                               (context-contribution-evidence advice))
                       "a stale recalled memory carries its status and reason")
          (test-assert (search "Entries with a status"
                               (context-contribution-instruction advice))
                       "recall explains stale entries only when it offers one")
          (test-assert (and (= (memory-context-event-count (first events)) 2)
                            (= (memory-context-event-stale-count (first events)) 1)
                            (zerop (memory-context-event-unverified-count (first events))))
                       "the presentation event counts the stale memory"))
        (test-assert (equal (list (application--related-memories-notice 1)
                                  (application--related-memories-notice 3 :stale 1)
                                  (application--related-memories-notice
                                   2 :stale 1 :unverified 1))
                            '("@ 1 relevant memory exists"
                              "@ 3 relevant memories exist · 1 stale"
                              "@ 2 relevant memories exist · 1 stale · 1 unverified"))
                     "the notice names stale and unverified memories"))))
  nil)

(-> memory-source-tests--text ((or string list)) string)
(defun memory-source-tests--text (entry)
  "Return the plain text of a transcript ENTRY."
  (if (stringp entry)
      entry
      (terminal--spans-text entry)))


(-> test-memory-commands () null)
(defun test-memory-commands ()
  "Test (memories), (memory), and (forget) as a person uses them."
  (with-test-configuration (base root)
    (multiple-value-bind (configuration workspace)
        (memory-source-tests--workspace base root)
      (let* ((conversation (conversation-create configuration))
             (application (make-instance 'application
                                         :configuration configuration
                                         :conversation conversation)))
        (test-assert (search "No memories are saved"
                             (memory-source-tests--text
                              (application-memories-entry application)))
                     "an empty listing says how memories come to exist")
        (let* ((notes (memory-source-tests--write
                       (merge-pathnames "NOTES.md" workspace)
                       (format nil "Deploy with make release-prod after tagging.~%")))
               (identifiers (list "abcd1111-0000-4000-8000-000000000000"
                                  "abcd2222-0000-4000-8000-000000000000"
                                  "ef003333-0000-4000-8000-000000000000"))
               (deploy nil)
               (style nil)
               (release nil))
          (test-call-with-function-replacements
           (list (list 'make-identifier (lambda () (pop identifiers))))
           (lambda ()
             (setf deploy (memory-remember
                           configuration
                           :title "Deploy command"
                           :content "Deploy with make release-prod."
                           :sources (list (memory-source-observe-file
                                           notes :quote "make release-prod" :time 1)))
                   style (memory-remember configuration
                                          :title "Indentation"
                                          :content "Use tabs.")
                   release (memory-remember configuration
                                            :title "Release day"
                                            :content "Tag, then make release-prod."
                                            :premises (list (memory-identifier deploy))))))
          (memory-source-tests--write notes (format nil "Deploy with make ship.~%"))
          (let ((listing (memory-source-tests--text
                          (application-memories-entry application))))
            (test-assert (search "memories  3 · 2 stale" listing)
                         "the listing header counts stale memories")
            (test-assert (< (search "Deploy command" listing)
                            (search "Indentation" listing))
                         "stale memories are listed before current ones")
            (test-assert (and (search "stale: NOTES.md changed" listing)
                              (search "(memory \"abcd1111\")" listing)
                              (search "from 1 memory" listing))
                         "rows give the reason, the explaining call, and provenance"))
          (test-assert (let ((filtered (memory-source-tests--text
                                        (application-memories-entry
                                         application :query "tabs?"))))
                         (and (search "Indentation" filtered)
                              (not (search "Deploy command" filtered))))
                       "a query narrows the listing")
          (let ((explained (memory-source-tests--text
                            (application-memory-entry application "ABCD1"))))
            (test-assert (and (search "memory  stale · NOTES.md changed" explained)
                              (search "✗ file NOTES.md \"make release-prod\" changed" explained)
                              (search "relied on by" explained)
                              (search "\"Release day\"" explained))
                         "(memory) explains the evidence and names dependents"))
          (test-assert (search "\"Deploy command\" is stale"
                               (memory-source-tests--text
                                (application-memory-entry application "ef00")))
                       "a derived memory explains which premise went stale")
          (test-assert (search "ambiguous"
                               (memory-source-tests--text
                                (application-memory-entry application "abcd")))
                       "an ambiguous prefix asks for more characters")
          (test-assert (search "none recorded"
                               (memory-source-tests--text
                                (application-memory-entry
                                 application (memory-identifier style))))
                       "a memory without evidence says so")
          (test-assert (search "No memory matches"
                               (memory-source-tests--text
                                (application-memory-entry application "9999")))
                       "an unknown ID points to (memories)")
          (let ((forgotten (memory-source-tests--text
                            (application-memory-forget-entry application "abcd1"))))
            (test-assert (and (search "forgot  Deploy command" forgotten)
                              (search "1 memory relied on it and is now stale" forgotten)
                              (search "\"Release day\"" forgotten)
                              (null (memory-find configuration (memory-identifier deploy))))
                         "(forget) forgets and names the memories that relied on it"))
          (test-assert (null (memory-find configuration (memory-identifier deploy)))
                       "the forgotten memory is gone")
          (test-assert (and (equal (application--memories-slash-arguments " deploy prod ")
                                   '("deploy prod"))
                            (null (application--memories-slash-arguments "  ")))
                       "/memories takes the rest of the line as one query")
          (test-assert (string= (memory-identifier release)
                                (memory-identifier
                                 (memory-resolve configuration "ef003333-0000-4000-8000-000000000000")))
                       "a full identifier resolves exactly")))))
  nil)

(-> test-memory-premise-export () null)
(defun test-memory-premise-export ()
  "Test that a workspace export carries the global memories its memories rely on."
  (with-test-configuration (base root)
    (with-test-configuration (target target-root)
      (let* ((source (test-data-transfer--workspace
                      base (merge-pathnames "project/" root)))
             (notes (memory-source-tests--write
                     (merge-pathnames "project/NOTES.md" root)
                     (format nil "Deploy with make release-prod after tagging.~%")))
             (policy (memory-remember source :scope ':global
                                      :title "Release policy"
                                      :content "Releases happen on Tuesdays."))
             (calendar (memory-remember source :scope ':global
                                        :title "Release calendar"
                                        :content "Next release is on Tuesday."
                                        :premises (list (memory-identifier policy))))
             (plan (memory-remember source
                                    :title "Release plan"
                                    :content "Tag on Monday with make release-prod."
                                    :sources (list (memory-source-observe-file
                                                    notes :quote "make release-prod" :time 1))
                                    :premises (list (memory-identifier calendar))))
             (path (merge-pathnames "workspace.sexp" root))
             (destination (merge-pathnames "moved/" target-root)))
        (memory-remember source :scope ':global
                         :title "Unrelated" :content "Not a dependency.")
        (test-assert (= (getf (data-export path
                                           :workspace (config :working-directory source)
                                           :configuration source)
                              :memories)
                        3)
                     "the export includes the premise chain and nothing unrelated")
        (memory-source-tests--write (merge-pathnames "NOTES.md" destination)
                                    (format nil "Deploy with make release-prod after tagging.~%"))
        (data-import path :workspace (namestring destination) :configuration target)
        (let ((relocated (test-data-transfer--workspace target destination)))
          (test-assert (eq (memory-source-tests--status relocated plan) ':current)
                       "an imported workspace memory finds its relative source and premises")))))
  nil)
