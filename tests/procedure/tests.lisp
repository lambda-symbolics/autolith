(in-package #:autolith)

;;;; -- Procedures --

(-> test-procedure-definition () null)
(defun test-procedure-definition ()
  "Test that a defined procedure calls tools as functions. Test that a retired one does not run."
  (with-test-configuration (base root)
    (let* ((configuration (configuration-copy base :working-directory root))
           (registry      (make-default-tool-registry :configuration configuration))
           (context       (make-instance 'tool-context
                                         :configuration configuration :worker nil :registry registry
                                         :conversation (conversation-create configuration))))
      (test-write-text (merge-pathnames "notes.txt" root) "quarterly notes")
      (flet ((call (name &rest arguments)
               (test-resource-call registry context
                                   :namespace "procedure" :name name :arguments arguments)))
        (test-assert (not (tool-result-success-p
                           (call "define" "id" "read-note" "source" "(lambda (name) \"Read NAME.\" name)")))
                     "a procedure with positional parameters is rejected")
        (test-assert (tool-result-success-p
                      (call "define" "id" "read-note"
                            "source" "(lambda (&key name)
  \"Read the workspace file NAME.\"
  (resource.read :uri (format nil \"workspace:~A\" name)))"))
                     "a procedure is defined")
        (test-assert (search "quarterly notes"
                             (tool-result-content
                              (call "run" "id" "read-note" "arguments" (json-object "name" "notes.txt"))))
                     "the procedure calls a tool as a function")
        (procedure-retire configuration "read-note")
        (test-assert (not (tool-result-success-p
                           (call "run" "id" "read-note" "arguments" (json-object "name" "notes.txt"))))
                     "a retired procedure does not run"))))
  nil)

(-> test-procedure-syntax () null)
(defun test-procedure-syntax ()
  "Test that malformed procedure forms signal knowledge errors."
  (dolist (source '("(lambda (&key . name) \"Read.\" name)"
                    "(lambda (&key (42 nil)) \"Read.\" nil)"
                    "(lambda (&key &optional name) \"Read.\" name)"
                    "(lambda (&key name name) \"Read.\" name)"
                    "(lambda (&key (&key nil)) \"Read.\" nil)"
                    "(lambda (&key (name nil 42)) \"Read.\" name)"
                    "(lambda (nil) \"Read.\" nil)"
                    "(lambda (&key nil) \"Read.\" nil)"
                    "(lambda (&key :name) \"Read.\" nil)"
                    "(lambda (&key (name nil name)) \"Read.\" name)"
                    "(lambda nil \"Read.\" . name)"
                    "(lambda nil \"Read.\")"
                    "(lambda #1=(&key . #1#) \"Read.\" nil)"
                    "#1=(lambda nil \"Read.\" . #1#)"
                    "#.(error \"Read-time evaluation.\")"
                    "(lambda nil \"Read.\" nil) (error \"Extra form.\")"))
    (test-assert (handler-case (progn (procedure-form source) nil)
                   (knowledge-error () t))
                 (format nil "malformed procedure source is rejected: ~A" source)))
  (dolist (source '("(lambda nil \"Read.\" nil)"
                    "(lambda (&key name) \"Read.\" name)"
                    "(lambda (&key ((:path name) \"notes.txt\" supplied-p)) \"Read.\" (list name supplied-p))"
                    "(lambda (&key name &allow-other-keys &aux (value name)) \"Read.\" value)"))
    (test-assert (consp (procedure-form source))
                 (format nil "valid keyword procedure source is accepted: ~A" source)))
  nil)

(-> test-procedure-persistence () null)
(defun test-procedure-persistence ()
  "Test that procedure catalogs reflect persisted definitions and retirement."
  (with-test-configuration (base root)
    (let* ((configuration (configuration-copy base :working-directory root))
           (registry      (make-default-tool-registry :configuration configuration))
           (conversation  (conversation-create configuration))
           (context       (make-instance 'tool-context :configuration configuration
                                                      :worker nil :registry registry
                                                      :conversation   conversation))
           (request       (make-instance 'request-context :configuration  configuration
                                                         :conversation    conversation
                                                         :tool-namespaces #()))
           (workspace     (knowledge--workspace configuration))
           (source        "(lambda (&key (amount 1)) \"Return AMOUNT.\" amount)"))
      (procedure-define configuration "amount" source)
      (let* ((snapshot (knowledge-base-read configuration))
             (entry    (knowledge-base-procedure snapshot workspace "amount"))
             (catalog  (procedure-catalog-context request)))
        (test-assert (and (search "amount" (procedure-signature entry))
                          (search "amount" (context-contribution-instruction catalog))
                          (search (procedure-documentation entry)
                                  (context-contribution-instruction catalog)))
                     "the procedure catalog contains the signature and documentation")
        (setf source "(lambda (&key (amount 7)) \"Return the updated AMOUNT.\" amount)")
        (procedure-define configuration "amount" source)
        (test-assert (equal source
                            (getf (knowledge-base-procedure
                                   (knowledge-tests--reread configuration) workspace "amount")
                                  :source))
                     "redefinition is reconstructed from the knowledge log")
        (test-assert (and (search "7" (procedure-run context "amount" nil))
                          (search "19" (procedure-run context "amount" '(:amount 19))))
                     "the reloaded procedure accepts defaults and supplied arguments")
        (test-assert (and (procedure-retire configuration "amount")
                          (not (procedure-retire configuration "amount"))
                          (getf (knowledge-base-procedure
                                 (knowledge-tests--reread configuration) workspace "amount")
                                :retired-p)
                          (null (procedure-catalog-context request))
                          (not (getf entry :retired-p)))
                     "retirement is persistent and idempotent without changing prior snapshots")
        (let ((archive     (merge-pathnames "procedures.sexp" root))
              (destination (merge-pathnames "moved/" root)))
          (ensure-directories-exist destination)
          (data-export archive :workspace root :configuration configuration)
          (with-test-configuration (target)
            (let ((target (configuration-copy target :working-directory destination)))
              (data-import archive :workspace destination :configuration target)
              (let ((entry (knowledge-base-procedure
                            (knowledge-tests--reread target) (knowledge--workspace target) "amount")))
                (test-assert (and entry (equal source (getf entry :source))
                                  (getf entry :retired-p))
                             "workspace archives retain procedure definitions and retirement after remapping")))))
        (procedure-define configuration "amount" source)
        (let ((kinds nil))
          (sexp-store:log-map
           (lambda (record)
             (case (first record)
               (:transaction
                (setf kinds (append (mapcar #'first (getf (rest record) :records)) kinds)))
              (:knowledge
               nil)
              (otherwise
               (push (first record) kinds))))
           (configuration-knowledge-path configuration))
          (test-assert
           (and kinds (notany (lambda (kind)
                                (member kind '(:procedure-defined :procedure-admitted
                                               :procedure-retired :procedure-run)))
                              kinds))
           "Lisp procedure records have distinct kinds for rollback to step-procedure readers"))
        (knowledge-transact
         configuration
         (lambda (base)
           (declare (ignore base))
           (values
            (loop for record in
                  '((:procedure-defined
                     :procedure (:procedure :id "amount" :parameters nil
                                 :steps ((:tool "resource.read" :arguments (:object)))))
                    (:procedure-admitted :id "amount" :procedure-version 1)
                    (:procedure-run :id "amount" :procedure-version 1
                     :status :ok :caller :user :steps 1)
                    (:procedure-retired :id "amount"))
                  collect (append record (list :scope ':workspace :workspace workspace :time 1)))
            nil
            t)))
        (test-assert (and (not (getf (knowledge-base-procedure
                                      (knowledge-tests--reread configuration) workspace "amount")
                                    :retired-p))
                          (procedure-catalog-context request))
                     "legacy step-procedure history does not replace or retire a Lisp procedure"))))
  nil)
