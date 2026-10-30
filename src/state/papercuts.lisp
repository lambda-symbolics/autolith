(in-package #:autolith)

;;;; -- Legacy Papercut Context --


(defparameter *papercut-clock* #'get-universal-time
  "The function returning the universal time stamped on new reports.")

(-> papercut--context (configuration) autolith-supervisor::papercut-context)
(defun papercut--context (configuration)
  "Select the shared store with trusted legacy path, workspace, and clock."
  (make-instance 'autolith-supervisor::papercut-context
                 :pathname          (configuration-papercut-path configuration)
                 :workspace         (namestring (config :working-directory configuration))
                 :clock             *papercut-clock*
                 :identifier-source #'make-identifier))

(-> papercut--transact (configuration function) (values t list))
(defun papercut--transact (configuration update)
  "Run UPDATE in the shared locked lifecycle and receipt transaction."
  (autolith-supervisor::papercut--transact (papercut--context configuration) update))

(-> papercut--note-report (papercut boolean) null)
(defun papercut--note-report (papercut duplicate-p)
  "Export a newly committed report through the consent-gated projection."
  (telemetry--call-safely
   (lambda ()
     (telemetry-note-report :report-id       (papercut-telemetry-id papercut)
                           :tool             (papercut-tool papercut)
                           :issue-kind       (papercut-issue-kind papercut)
                           :duplicate-p      duplicate-p
                           :summary-function (lambda () (or (papercut-diagnostic papercut)
                                                            (papercut-title papercut))))))
  nil)

(-> papercut--note-repair (papercut string (option string)) null)
(defun papercut--note-repair (papercut outcome summary)
  "Export an assessment verdict or closure without claiming passed checks."
  (telemetry--call-safely
   (lambda ()
     (telemetry-note-repair :report-id       (papercut-telemetry-id papercut)
                           :incident-id      (papercut-telemetry-id papercut)
                           :event-kind       (if (equal outcome "closed") "closure" "assessment")
                           :target           (papercut-tool papercut)
                           :issue-kind       (papercut-issue-kind papercut)
                           :repair-kind "other" :outcome outcome :verified nil
                           :operation        *telemetry-tool-name*
                           :summary-function (lambda () summary))))
  nil)

(-> papercut--telemetry-record (papercut) list)
(defun papercut--telemetry-record (papercut)
  "Return a durable identity annotation when a previously unexported issue gains consent."
  (unless (papercut-telemetry-id papercut)
    (let ((identifier (telemetry-new-report-id)))
      (when identifier
        (setf (papercut-telemetry-id papercut) identifier)
        (list (list :papercut-telemetry :version *papercut-format-version*
                    :id (papercut-identifier papercut) :telemetry-id identifier))))))


;;;; -- Selection and Mutation --

(-> papercut--workspace (configuration) non-empty-string)
(defun papercut--workspace (configuration)
  "Return CONFIGURATION's current workspace identity used by papercut records."
  (namestring (config :working-directory configuration)))

(-> papercut--workspace-reports (configuration list) list)
(defun papercut--workspace-reports (configuration active)
  "Select this workspace's reports from shared transaction state."
  (autolith-supervisor::papercut--workspace-reports (papercut--context configuration) active))

(-> papercut-list (configuration) list)
(defun papercut-list (configuration)
  "Read active reports through the shared lifecycle and receipt reducer."
  (autolith-supervisor::papercut-list (papercut--context configuration)))

(-> papercut-find (configuration string) (option papercut))
(defun papercut-find (configuration identifier)
  "Find an exact active report in the trusted workspace."
  (autolith-supervisor::papercut-find (papercut--context configuration) identifier))

(-> papercut-resolve (configuration string) (values (option papercut) keyword list))
(defun papercut-resolve (configuration identifier)
  "Resolve an exact or abbreviated identifier through the shared domain."
  (autolith-supervisor::papercut-resolve (papercut--context configuration) identifier))

(-> papercut-repair-context (configuration string) list)
(defun papercut-repair-context (configuration identifier)
  "Validate an exact active incident and persist its consented telemetry identity."
  (with-lock-held (*papercut-lock*)
    (papercut--transact
     configuration
     (lambda (active)
       (let ((papercut (find identifier (papercut--workspace-reports configuration active)
                            :key #'papercut-identifier :test #'equal)))
         (unless papercut
           (error 'papercut-error :message   "No active incident has the supplied exact identifier."
                                 :pathname   (configuration-papercut-path configuration)
                                 :identifier identifier))
         (values (papercut--telemetry-record papercut)
                 (list :incident-id (papercut-telemetry-id papercut)
                       :tool        (papercut-tool papercut)
                       :issue-kind  (papercut-issue-kind papercut))))))))

(-> papercut--duplicate
    (configuration list &key (:title non-empty-string) (:content non-empty-string)
     (:source-conversation (option string)) (:now integer))
    (option papercut))
(defun papercut--duplicate (configuration active &key title content source-conversation now)
  "Find a duplicate through the shared domain with trusted workspace context."
  (autolith-supervisor::papercut--duplicate
   (papercut--context configuration) active :title title :content content
   :source-conversation source-conversation :now now))

(-> papercut--report-unlocked
    (configuration &key (:title non-empty-string) (:content non-empty-string)
     (:diagnostic (option string)) (:source-conversation (option string))
     (:issue-kind string) (:tool (option string)) (:active list))
    (values list list boolean))
(defun papercut--report-unlocked
    (configuration &key title content diagnostic source-conversation (issue-kind "other") tool active)
  "Build shared lifecycle records and add only consented telemetry identity."
  (multiple-value-bind (records result changed-p)
      (autolith-supervisor::papercut--report-unlocked
       (papercut--context configuration) :title title :content content :diagnostic diagnostic
       :source-conversation source-conversation :issue-kind issue-kind :tool (telemetry--tool tool)
       :active active)
    (let ((annotations (papercut--telemetry-record (first result))))
      (values (append records annotations) result (or changed-p (not (null annotations)))))))

(-> papercut-report
    (configuration &key (:title string) (:content string)
                   (:diagnostic (option string))
                   (:issue-kind string) (:tool (option string))
                   (:source-conversation (option string)))
    (values papercut boolean))
(defun papercut-report (configuration &key title content diagnostic source-conversation (issue-kind "other") tool)
  "Record one new user-visible report about a problem in the current workspace.

Return the recorded papercut and NIL, or, when the report repeats an active
papercut of the workspace, that existing papercut and T. A duplicate may append
its first consented telemetry identity without recording another report."
  (let ((validated-title
          (papercut--validate-text title "title" *papercut-title-limit*))
        (validated-content
          (papercut--validate-text content "content" *papercut-content-limit*)))
    (setf diagnostic (telemetry-diagnostic-text diagnostic))
    (unless (or (null source-conversation)
                (non-empty-string-p source-conversation))
      (error 'papercut-error
             :message    "Papercut source conversation must be a non-empty string."
             :pathname   (configuration-papercut-path configuration)
             :identifier nil))
    (destructuring-bind (papercut duplicate-p)
        (with-lock-held (*papercut-lock*)
          (papercut--transact
           configuration
           (lambda (active)
             (papercut--report-unlocked
              configuration :title validated-title :content validated-content
                            :diagnostic          diagnostic
                            :issue-kind issue-kind :tool tool
                            :source-conversation source-conversation
                            :active              active))))
      (papercut--note-report papercut duplicate-p)
      (values papercut duplicate-p))))

(-> papercut--assess-unlocked
    (configuration non-empty-string &key (:active list)
     (:verdict (member :improved :worse :unchanged :too-early))
     (:note non-empty-string) (:diagnostic (option string)))
    (values list papercut boolean))
(defun papercut--assess-unlocked (configuration identifier &key active verdict note diagnostic)
  "Build a shared assessment and persist any consented telemetry identity."
  (multiple-value-bind (records report changed-p)
      (autolith-supervisor::papercut--assess-unlocked
       (papercut--context configuration) identifier :active active :verdict verdict
       :note note :diagnostic diagnostic)
    (values (append (papercut--telemetry-record report) records) report changed-p)))

(-> papercut-assess
    (configuration string &key (:verdict keyword) (:note string) (:diagnostic (option string)))
    papercut)
(defun papercut-assess (configuration identifier &key verdict note diagnostic)
  "Append an effectiveness assessment for active papercut IDENTIFIER."
  (setf diagnostic (telemetry-diagnostic-text diagnostic))
  (unless (non-empty-string-p identifier)
    (error 'papercut-error
           :message    "Papercut identifier must be a non-empty string."
           :pathname   (configuration-papercut-path configuration)
           :identifier nil))
  (unless (member verdict '(:improved :worse :unchanged :too-early))
    (error 'papercut-error
           :message    "Papercut assessment verdict must be improved, worse, unchanged, or too-early."
           :pathname   (configuration-papercut-path configuration)
           :identifier identifier))
  (let ((validated-note
          (papercut--validate-text
           note "assessment note" *papercut-assessment-note-limit*)))
    (let ((papercut
            (with-lock-held (*papercut-lock*)
              (papercut--transact
               configuration
               (lambda (active)
                 (papercut--assess-unlocked
                  configuration identifier :active active :verdict verdict
                                           :note validated-note :diagnostic diagnostic))))))
      (papercut--note-repair papercut (string-downcase (symbol-name verdict)) diagnostic)
      papercut)))

(-> papercut--mark-closed-unlocked
    (configuration non-empty-string non-empty-string &key (:active list) (:diagnostic (option string)))
    (values list papercut boolean))
(defun papercut--mark-closed-unlocked (configuration identifier resolution &key active diagnostic)
  "Build a shared closure and persist any consented telemetry identity."
  (multiple-value-bind (records report changed-p)
      (autolith-supervisor::papercut--mark-closed-unlocked
       (papercut--context configuration) identifier resolution :active active :diagnostic diagnostic)
    (values (append (papercut--telemetry-record report) records) report changed-p)))

(-> papercut-mark-closed (configuration string &key (:resolution string) (:diagnostic (option string))) papercut)
(defun papercut-mark-closed (configuration identifier &key resolution diagnostic)
  "Close active papercut IDENTIFIER with a durable RESOLUTION and return it."
  (setf diagnostic (telemetry-diagnostic-text diagnostic))
  (unless (non-empty-string-p identifier)
    (error 'papercut-error
           :message    "Papercut identifier must be a non-empty string."
           :pathname   (configuration-papercut-path configuration)
           :identifier nil))
  (let ((validated-resolution
          (papercut--validate-text
           resolution "closure resolution" *papercut-resolution-limit*)))
    (let ((papercut
            (with-lock-held (*papercut-lock*)
              (papercut--transact
               configuration
               (lambda (active)
                 (papercut--mark-closed-unlocked
                  configuration identifier validated-resolution :active active :diagnostic diagnostic))))))
      (papercut--note-repair papercut "closed" diagnostic)
      papercut)))
