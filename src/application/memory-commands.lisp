(in-package #:autolith)

;;;; -- Memory Commands --

(defparameter *memory-command-premise-depth* 3
  "How many levels of premises (memory \"ID\") expands below a memory.")


;;; Shared presentation

(-> application-memory--call (memory) string)
(defun application-memory--call (memory)
  "Return the Lisp call that explains MEMORY."
  (format nil "(memory ~S)" (memory-short-identifier memory)))

(-> application-memory--short (string) string)
(defun application-memory--short (identifier)
  "Return the prefix of IDENTIFIER people type to select it."
  (subseq identifier 0 (min *memory-short-identifier-length* (length identifier))))

(-> application-memory--date (timestamp) string)
(defun application-memory--date (timestamp)
  "Return TIMESTAMP's UTC calendar date."
  (subseq (memory--timestamp-string timestamp) 0 10))

(-> application-memory--title (memory) string)
(defun application-memory--title (memory)
  "Return MEMORY's title as one safe terminal line."
  (sanitize-text (memory-title memory) :single-line-p t))

(-> application-memory--source-label (list) string)
(defun application-memory--source-label (source)
  "Return the short label naming where SOURCE came from."
  (ecase (first source)
    (:file
     (sanitize-text (memory-source-file-display source) :single-line-p t))
    (:url
     (sanitize-text (or (ignore-errors (quri:uri-host (quri:uri (getf (rest source) :url))))
                        (getf (rest source) :url))
                    :single-line-p t))
    (:user
     "you said")))

(-> application-memory--provenance (memory) (option string))
(defun application-memory--provenance (memory)
  "Return a compact summary of MEMORY's sources and premises, or NIL."
  (let ((labels (remove-duplicates
                 (mapcar #'application-memory--source-label (memory-sources memory))
                 :test #'string=
                 :from-end t))
        (premise-count (length (memory-premises memory))))
    (when (plusp premise-count)
      (setf labels (append labels
                           (list (format nil "from ~D memor~:@P" premise-count)))))
    (when labels
      (format nil "~{~A~^, ~}" labels))))

(-> application-memory--status-style (memory-status) keyword)
(defun application-memory--status-style (status)
  "Return the terminal style presenting STATUS."
  (ecase status
    (:current ':success)
    (:stale ':failure)
    (:unverified ':notice)))

(-> application-memory--status-rank (memory-assessment) integer)
(defun application-memory--status-rank (assessment)
  "Order stale memories first, then unverified ones, then current ones."
  (ecase (memory-assessment-status assessment)
    (:stale 0)
    (:unverified 1)
    (:current 2)))

(-> application-memory--reference (memory &key (:indent integer)) list)
(defun application-memory--reference (memory &key (indent 4))
  "Return one line naming MEMORY by title and its explaining call."
  (list (terminal-span ':plain
                       (format nil "~vT\"~A\" " indent (application-memory--title memory)))
        (terminal-span ':dim
                       (format nil "~A~%" (application-memory--call memory)))))

(-> application-memory--ambiguous (string list) list)
(defun application-memory--ambiguous (identifier matches)
  "Return the notice for an IDENTIFIER prefix shared by MATCHES."
  (append
   (list (terminal-span
          ':failure
          (format nil "Memory ID ~A is ambiguous. Enter more characters.~%"
                  (sanitize-text identifier :single-line-p t))))
   (loop for match in matches
         append (application-memory--reference match :indent 2))))

(-> application-memory--missing (string) string)
(defun application-memory--missing (identifier)
  "Return the notice for an IDENTIFIER matching no relevant memory."
  (format nil "No memory matches ~S. Run (memories) to list them."
          (sanitize-text identifier :single-line-p t)))


;;; (memories)

(-> application-memory--row (memory-assessment) list)
(defun application-memory--row (assessment)
  "Return one compact listing row for ASSESSMENT's memory."
  (let* ((memory (memory-assessment-memory assessment))
         (status (memory-assessment-status assessment))
         (style (application-memory--status-style status)))
    (append
     (list
      (if (eq status ':current)
          (terminal-span ':dim "  · ")
          (terminal-span style (if (eq status ':stale) "  ! " "  ? ")))
      (terminal-span ':strong (format nil "~A~%" (application-memory--title memory))))
     (unless (eq status ':current)
       (list (terminal-span style
                            (format nil "    ~(~A~): ~A~%"
                                    status
                                    (sanitize-text (memory-assessment-summary assessment)
                                                   :single-line-p t)))))
     (list
      (terminal-span ':dim
                     (format nil "    ~A · ~(~A~)~@[ · ~A~] · ~A~%"
                             (application-memory--call memory)
                             (memory-scope memory)
                             (application-memory--provenance memory)
                             (application-memory--date (memory-updated-at memory))))))))

(-> application-memories-entry (application &key (:query (option string))) (or string list))
(defun application-memories-entry (application &key query)
  "Return the memories relevant to this workspace, stale ones first.

QUERY narrows the listing with the same lexical ranking recall uses."
  (let* ((configuration (application-configuration application))
         (memories (if (non-empty-string-p query)
                       (memory-search configuration query)
                       (memory-list configuration)))
         (assessor (memory-assessor-create configuration))
         (facts (application-knowledge-facts-section (memory-assessor-view assessor)
                                                     :query query
                                                     :memories-p (and memories t))))
    (cond
      ((and (null memories) (null facts))
       (list (terminal-span
              ':hint
              (if (non-empty-string-p query)
                  (format nil "No memories match ~S." (sanitize-text query :single-line-p t))
                  "No memories are saved for this workspace yet. Ask Autolith to remember something."))))
      ((null memories)
       facts)
      (t
       (let ((assessments (stable-sort
                           (mapcar (lambda (memory)
                                     (memory-assess assessor memory))
                                   memories)
                           #'<
                           :key #'application-memory--status-rank)))
         (append
          (list (terminal-span ':brand "memories")
                (terminal-span
                 ':dim
                 (format nil "  ~D~@[ matching ~S~]~[~:; · ~:*~D stale~]~[~:; · ~:*~D unverified~]~%"
                         (length assessments)
                         (and (non-empty-string-p query)
                              (sanitize-text query :single-line-p t))
                         (count ':stale assessments :key #'memory-assessment-status)
                         (count ':unverified assessments :key #'memory-assessment-status)))
                (terminal-span
                 ':dim
                 (format nil "  stale first, then newest · (memory \"ID\") shows why~%")))
          (loop for assessment in assessments
                append (application-memory--row assessment))
          facts))))))


;;; (memory "ID")

(-> application-memory--source-row (list &key (:indent integer)) list)
(defun application-memory--source-row (outcome &key (indent 4))
  "Return the evidence line for one source OUTCOME, indented by INDENT."
  (let* ((source (getf outcome :source))
         (quote (getf (rest source) :quote))
         (reason (getf (getf outcome :reason) :kind)))
    (list
     ;; Only files are checked; statements and pages are provenance.
     (if (eq (first source) :file)
         (ecase (getf outcome :outcome)
           (:holds (terminal-span ':success (format nil "~vT✓ " indent)))
           (:failed (terminal-span ':failure (format nil "~vT✗ " indent)))
           (:unverified (terminal-span ':notice (format nil "~vT? " indent))))
         (terminal-span ':dim (format nil "~vT· " indent)))
     (terminal-span ':plain
                    (ecase (first source)
                      (:file
                       (format nil "file ~A" (application-memory--source-label source)))
                      (:url
                       (format nil "web ~A" (sanitize-text (getf (rest source) :url)
                                                           :single-line-p t)))
                      (:user
                       "you said")))
     (terminal-span ':dim
                    (if quote
                        (format nil " \"~A\"" (sanitize-text quote :single-line-p t))
                        ""))
     (terminal-span ':failure
                    (format nil "~A~%"
                            (case reason
                              (:file-changed " changed")
                              (:file-missing " is missing")
                              (:file-unchecked " could not be checked")
                              (otherwise "")))))))

(-> application-memory--premise-rows (list &key (:indent integer) (:depth integer)) list)
(defun application-memory--premise-rows (outcome &key (indent 4) (depth 1))
  "Return the lines for one premise OUTCOME, expanding its own evidence below it."
  (let* ((memory (getf outcome :memory))
         (claim (getf outcome :claim))
         (assessment (getf outcome :assessment))
         (kind (getf (getf outcome :reason) :kind)))
    (append
     (list
      (ecase (getf outcome :outcome)
        (:holds (terminal-span ':success (format nil "~vT✓ " indent)))
        (:failed (terminal-span ':failure (format nil "~vT✗ " indent)))
        (:unverified (terminal-span ':notice (format nil "~vT? " indent))))
      (cond
        (memory
         (terminal-span ':plain
                        (format nil "from \"~A\" " (application-memory--title memory))))
        (claim
         (terminal-span ':plain
                        (format nil "from fact \"~A\" "
                                (sanitize-text (knowledge-node-text claim) :single-line-p t))))
        ((eq kind ':premise-claim)
         (terminal-span ':plain
                        (format nil "from fact \"~A\" "
                                (sanitize-text (getf (getf outcome :reason) :title)
                                               :single-line-p t))))
        (t
         (terminal-span ':plain
                        (format nil "from forgotten memory ~A"
                                (sanitize-text (getf outcome :id) :single-line-p t)))))
      (terminal-span ':dim (cond
                             (memory (application-memory--call memory))
                             ((or claim (eq kind ':premise-claim))
                              (format nil "(memory ~S)"
                                      (application-memory--short (getf outcome :id))))
                             (t "")))
      (terminal-span ':failure
                     (format nil "~A~%"
                             (case kind
                               (:premise-changed " changed since")
                               (:premise-stale " is stale")
                               (:premise-unverified " is unverified")
                               (:premise-cycle " relies on this memory")
                               (:premise-too-deep " is too deep to check")
                               (:premise-claim
                                (let ((applicability (getf (getf outcome :reason)
                                                           :applicability)))
                                  (if (eq applicability ':unknown)
                                      " no longer holds"
                                      (format nil " is ~(~A~)" applicability))))
                               (otherwise "")))))
     (when (and assessment (< depth *memory-command-premise-depth*))
       (append
        (loop for source in (memory-assessment-source-outcomes assessment)
              append (application-memory--source-row source :indent (+ indent 2)))
        (loop for premise in (memory-assessment-premise-outcomes assessment)
              append (application-memory--premise-rows premise
                                                       :indent (+ indent 2)
                                                       :depth (1+ depth))))))))

(-> application-memory-entry (application string) (or string list))
(defun application-memory-entry (application identifier)
  "Return one memory with its evidence, premises, and dependents."
  (let ((configuration (application-configuration application)))
    (multiple-value-bind (memory status matches)
        (memory-resolve configuration identifier)
      (case status
        (:found
         (let* ((assessor (memory-assessor-create configuration))
                (assessment (memory-assess assessor memory))
                (state (memory-assessment-status assessment))
                (dependents (memory-assessor-dependents
                             assessor (memory-identifier memory))))
           (append
            (list
             (terminal-span ':brand "memory  ")
             (terminal-span (application-memory--status-style state)
                            (format nil "~(~A~)~@[ · ~A~]~%"
                                    state
                                    (and (memory-assessment-summary assessment)
                                         (sanitize-text
                                          (memory-assessment-summary assessment)
                                          :single-line-p t))))
             (terminal-span
              ':dim
              (format nil "  id ~A~%  ~A · saved ~A~:[ · updated ~A~;~*~]~@[ · conversation ~A~]~2%"
                      (memory-identifier memory)
                      (if (eq (memory-scope memory) ':global)
                          "global"
                          (format nil "workspace ~A"
                                  (sanitize-text (memory-workspace memory)
                                                 :single-line-p t)))
                      (application-memory--date (memory-created-at memory))
                      (= (memory-created-at memory) (memory-updated-at memory))
                      (application-memory--date (memory-updated-at memory))
                      (and (memory-source-conversation memory)
                           (conversation-identifier-display
                            (memory-source-conversation memory)))))
             (terminal-span ':strong
                            (format nil "  ~A~2%" (application-memory--title memory)))
             (terminal-span ':plain
                            (format nil "~{  ~A~%~}~%"
                                    (uiop:split-string (sanitize-text (memory-content memory))
                                                       :separator '(#\Newline))))
             (terminal-span ':dim (format nil "  evidence~%")))
            (if (or (memory-sources memory) (memory-premises memory))
                (append
                 (loop for source in (memory-assessment-source-outcomes assessment)
                       append (application-memory--source-row source))
                 (loop for premise in (memory-assessment-premise-outcomes assessment)
                       append (application-memory--premise-rows premise)))
                (list (terminal-span
                       ':hint
                       (format nil "    none recorded; written from the conversation~%"))))
            (when dependents
              (cons (terminal-span ':dim (format nil "  relied on by~%"))
                    (loop for dependent in dependents
                          append (application-memory--reference dependent)))))))
        (:ambiguous
         (application-memory--ambiguous identifier matches))
        (otherwise
         (let ((view (application-knowledge--view application)))
           (multiple-value-bind (node fact-status fact-matches)
               (application-knowledge-resolve view identifier)
             (case fact-status
               (:found
                (application-knowledge-fact-entry application view node))
               (:ambiguous
                (append
                 (list (terminal-span
                        ':failure
                        (format nil "Fact ID ~A is ambiguous. Enter more characters.~%"
                                (sanitize-text identifier :single-line-p t))))
                 (loop for match in fact-matches
                       append (list (terminal-span
                                     ':plain
                                     (format nil "  \"~A\" " (application-knowledge--text match)))
                                    (terminal-span
                                     ':dim
                                     (format nil "~A~%" (application-knowledge--call match)))))))
               (otherwise
                (application-memory--missing identifier))))))))))


;;; (forget "ID")

(-> application-memory-forget-entry (application string) (or string list))
(defun application-memory-forget-entry (application identifier)
  "Forget the memory selected by IDENTIFIER and name the memories that relied on it."
  (let ((configuration (application-configuration application)))
    (multiple-value-bind (memory status matches)
        (memory-resolve configuration identifier)
      (case status
        (:found
         (memory-forget configuration
                        (memory-identifier memory)
                        :source-conversation
                        (conversation-identifier (application-conversation application)))
         (let ((dependents (memory-assessor-dependents
                            (memory-assessor-create configuration)
                            (memory-identifier memory))))
           (append
            (list (terminal-span ':success "forgot  ")
                  (terminal-span ':strong
                                 (format nil "~A~%" (application-memory--title memory))))
            (when dependents
              (cons (terminal-span
                     ':notice
                     (format nil "  ~D memor~:@P relied on it and ~:[are~;is~] now stale:~%"
                             (length dependents)
                             (= (length dependents) 1)))
                    (loop for dependent in dependents
                          append (application-memory--reference dependent)))))))
        (:ambiguous
         (application-memory--ambiguous identifier matches))
        (otherwise
         (application-knowledge-forget-entry application identifier))))))

(-> application-knowledge-forget-entry (application string) (or string list))
(defun application-knowledge-forget-entry (application identifier)
  "Retract the fact selected by IDENTIFIER and name what stopped holding with it."
  (let ((configuration (application-configuration application))
        (view (application-knowledge--view application)))
    (multiple-value-bind (node status) (application-knowledge-resolve view identifier)
      (if (and (eq status ':found)
               (knowledge-base-claim (knowledge-view-base view)
                                     (knowledge-node-identifier node)))
          (let ((before (knowledge-view-snapshot view)))
            (knowledge-retract configuration (knowledge-node-identifier node)
                               :reason "Forgotten with (forget)."
                               :source-conversation
                               (conversation-identifier (application-conversation application)))
            (let ((lost (remove (knowledge-node-identifier node)
                                (remove-if-not
                                 (lambda (change)
                                   (member (getf change :kind) '(:withdrawn :changed)))
                                 (knowledge-view-changes before
                                                         (application-knowledge--view application)))
                                :key (lambda (change) (getf change :claim))
                                :test #'string=)))
              (append
               (list (terminal-span ':success "forgot  ")
                     (terminal-span ':strong
                                    (format nil "~A~%" (application-knowledge--text node))))
               (when lost
                 (cons (terminal-span ':notice
                                      (format nil "  ~D derived fact~:P changed or stopped holding:~%"
                                              (length lost)))
                       (loop for change in lost
                             collect (terminal-span
                                      ':plain
                                      (format nil "    ~A~%"
                                              (sanitize-text (or (getf change :from)
                                                                 (getf change :text))
                                                             :single-line-p t)))))))))
          (application-memory--missing identifier)))))


;;;; -- Command Definitions --

(-> application--memories-slash-arguments (string) list)
(defun application--memories-slash-arguments (text)
  "Treat everything after /memories as one search query."
  (let ((query (string-trim '(#\Space #\Tab) text)))
    (and (plusp (length query)) (list query))))

(define-application-command application--builtin-memories-command
    (:name "/memories"
     :argument "[QUERY]"
     :description "browse and manage saved memories and facts"
     :tip "Browse notes, facts, and their evidence in fullscreen; filter with a query."
     :busy-behavior :hold
     :terminal-behavior :exclusive
     :callable t
     :slash-parser application--memories-slash-arguments)
    (application &optional query)
  (let ((ui (application-ui application)))
    (if (and ui (terminal-ui-fullscreen-p ui)
             (terminal-interactive-p (terminal-ui-terminal ui)))
        (application-memory-manager application :query query)
        (application-present application (application-memories-entry application :query query))))
  ':continue)

(define-application-command application--builtin-memory-command
    (:name "/memory"
     :aliases ("/why")
     :argument "[ID]"
     :description "inspect one memory or fact and its evidence"
     :tip "Inspect a memory or fact, its sources and premises, and what relies on it."
     :busy-behavior :hold
     :terminal-behavior :exclusive
     :callable t)
    (application &optional identifier)
  (let ((ui (application-ui application)))
    (if (and ui (terminal-ui-fullscreen-p ui)
             (terminal-interactive-p (terminal-ui-terminal ui)))
        (application-memory-manager application :identifier identifier)
        (application-present
         application
         (if identifier
             (application-memory-entry application identifier)
             "Usage: (memory \"ID\"). Run (memories) to list memories."))))
  ':continue)

(define-application-command application--builtin-forget-command
    (:name "/forget"
     :argument "[ID]"
     :description "forget one memory or fact"
     :tip "removes a memory or fact and names what relied on it."
     :busy-behavior :apply
     :terminal-behavior :shared
     :callable t)
    (application &optional identifier)
  (application-present
   application
   (if identifier
       (application-memory-forget-entry application identifier)
       "Usage: (forget \"ID\"). Run (memories) to list memories."))
  ':continue)
