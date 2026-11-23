(in-package #:autolith)

;;;; -- Persistent Saved Context Rule Boundaries --

(-> saved-context-rule-tests--rule (string string) list)
(defun saved-context-rule-tests--rule (id instruction)
  "Return a turn-lifetime saved rule plist with ID and INSTRUCTION."
  (list :id id :when '(:languages ("common-lisp")) :instruction instruction
        :priority 10 :lifetime ':turn))

(-> test-saved-context-rule-persistence-and-replacement () null)
(defun test-saved-context-rule-persistence-and-replacement ()
  "Exercise detached persistence, reload, conflict, and replacement."
  (with-test-configuration (configuration root)
    (let* ((configuration (configuration-copy configuration :working-directory root))
           (rule          (saved-context-rule-tests--rule "saved" "Saved advice."))
           (replacement   (saved-context-rule-tests--rule "saved" "Replaced advice."))
           (path          (context-rule-saved-path configuration :workspace)))
      (context-rule-save configuration rule)
      (test-assert (probe-file path) "saving publishes the snapshot")
      (test-assert (= 1 (length (context-rule-saved configuration :scope :workspace)))
                   "saved workspace rules reload")
      (test-assert (= 1 (length (context-rule-saved
                                 (configuration-copy configuration)
                                 :scope :workspace)))
                   "a fresh configuration reloads saved rules")
      (test-assert (handler-case
                       (progn (context-rule-save configuration replacement) nil)
                     (context-rule-save-conflict () t))
                   "duplicate IDs require explicit replacement")
      (context-rule-save configuration replacement :replace-p t)
      (test-assert (string= "Replaced advice."
                            (getf (first (context-rule-saved configuration
                                                              :scope :workspace))
                                  :instruction))
                   "replacement updates the existing record")))
  nil)

(-> test-saved-context-rule-scope-and-precedence () null)
(defun test-saved-context-rule-scope-and-precedence ()
  "Exercise global/workspace isolation and workspace ID precedence."
  (with-test-configuration (configuration root)
    (let* ((workspace  (configuration-copy configuration :working-directory root))
           (other-root (test-make-temporary-root root))
           (other      (configuration-copy configuration :working-directory other-root)))
      (context-rule-save workspace
                         (saved-context-rule-tests--rule "same" "Workspace")
                         :scope ':workspace)
      (context-rule-save workspace
                         (saved-context-rule-tests--rule "same" "Global")
                         :scope ':global)
      (test-assert (equal '("same") (mapcar (lambda (r) (getf r ':id))
                                            (context-rule-saved workspace)))
                   "workspace rules override global IDs")
      (test-assert (string= "Global"
                            (getf (first (context-rule-saved other)) ':instruction))
                   "workspace files are isolated by canonical directory")))
  nil)

(-> test-saved-context-rule-corruption-is-reported () null)
(defun test-saved-context-rule-corruption-is-reported ()
  "Reject malformed existing data without treating it as an empty collection."
  (with-test-configuration (configuration)
    (let ((path (context-rule-saved-path configuration ':global)))
      (ensure-directories-exist path)
      (with-open-file (stream path :direction ':output :if-exists ':supersede
                             :if-does-not-exist ':create :external-format ':utf-8)
        (write-string "(progn (format t \"unsafe\"))" stream))
      (test-assert (handler-case
                       (progn (context-rule-saved configuration) nil)
                     (context-rule-corruption-error () t))
                   "corrupt saved data signals a typed error")))
  nil)

(defparameter *saved-context-rule-tests-read-evaluated-p* nil
  "Whether an unsafe persisted reader form ran during a storage test.")

(-> saved-context-rule-tests--write-form (pathname t &key (:raw-p boolean)) null)
(defun saved-context-rule-tests--write-form (pathname form &key raw-p)
  "Write a readable FORM, or raw reader input when RAW-P is true."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname :direction ':output :if-exists ':supersede
                          :if-does-not-exist ':create :external-format ':utf-8)
    (if raw-p
        (write-string form stream)
        (let ((*print-readably* t))
          (write form :stream stream))))
  nil)

(-> saved-context-rule-tests--instructions (configuration conversation) list)
(defun saved-context-rule-tests--instructions (configuration conversation)
  "Return advice actually selected by the ordinary request resolver."
  (mapcar #'context-contribution-instruction
          (context-delivery-contributions
           (context-resolve-request configuration conversation #()))))

(-> test-saved-context-rule-resolution-boundaries () null)
(defun test-saved-context-rule-resolution-boundaries ()
  "Deliver live saved output rules through the shared resolver budget and lifecycle."
  (with-test-configuration (configuration root)
    (let* ((configuration          (configuration-copy configuration :working-directory root))
           (conversation           (conversation-create configuration))
           (*context-contributors* nil)
           (*context-resolver*     (cl-llm-provider-api:make-context-resolver))
           (rule                   (list :id "output" :when '(:output-contains ("distinctive output"))
                                         :instruction "Saved output advice.")))
      (register-context-contributor "saved-test" #'context-rule-saved-contributions)
      (context-rule-save configuration rule)
      (test-assert (null (saved-context-rule-tests--instructions configuration conversation))
                   "saved output predicates require observed assistant evidence")
      (conversation-append-provider-item
       conversation (json-object "type" "message" "role" "assistant"
                                 "content" (vector (json-object "type" "output_text"
                                                                "text" "Distinctive output"))))
      (test-assert (equal '("Saved output advice.")
                           (saved-context-rule-tests--instructions configuration conversation))
                   "a saved literal trigger delivers advice on the next request")
      (setf (getf rule :instruction) "Replacement advice.")
      (context-rule-save (configuration-copy configuration) rule :replace-p t)
      (test-assert (equal '("Replacement advice.")
                           (saved-context-rule-tests--instructions configuration conversation))
                   "another configuration's save is visible to the live resolver")
      (let ((*context-advice-token-budget* 0))
        (test-assert (null (saved-context-rule-tests--instructions configuration conversation))
                     "saved advice uses the ordinary shared token budget"))
      (test-assert (null (context-rule-saved-contributions
                          (make-instance 'request-context :configuration configuration
                                         :conversation conversation :tool-namespaces #()
                                         :compaction-p                   t)))
                   "compaction excludes saved advisory rules")))
  nil)

(-> test-saved-context-rule-corrupt-input-boundaries () null)
(defun test-saved-context-rule-corrupt-input-boundaries ()
  "Reject unsafe reader input, invalid record shapes and duplicate rule IDs."
  (with-test-configuration (configuration)
    (let ((path                                        (context-rule-saved-path configuration ':global))
          (*saved-context-rule-tests-read-evaluated-p* nil))
      (saved-context-rule-tests--write-form
       path "#.(progn (setf autolith::*saved-context-rule-tests-read-evaluated-p* t) nil)"
       :raw-p t)
      (test-assert (handler-case (progn (context-rule-saved configuration) nil)
                     (context-rule-corruption-error () t))
                   "reader evaluation syntax is rejected")
      (test-assert (not *saved-context-rule-tests-read-evaluated-p*)
                   "persisted input cannot execute reader code")
      (dolist (form (list '(:context-rules :version 1 :bogus nil)
                         '(:context-rules :version 1 :rules 7)
                         '(:context-rules :version 1 :rules nil :rules nil)
                         '(:context-rules :version 99 :rules nil)
                         (list :context-rules :version 1
                               :rules         (list (saved-context-rule-tests--rule "x" "one")
                                                    (saved-context-rule-tests--rule "x" "two")))))
        (saved-context-rule-tests--write-form path form)
        (test-assert (handler-case (progn (context-rule-saved configuration) nil)
                       (context-rule-corruption-error () t))
                     "malformed collections are rejected as corruption"))))
  nil)

(-> test-saved-context-rule-save-preserves-corruption () null)
(defun test-saved-context-rule-save-preserves-corruption ()
  "Reject corrupt source data before publishing a save."
  (with-test-configuration (configuration)
    (let* ((path    (context-rule-saved-path configuration ':global))
           (corrupt "(:context-rules :version 999 :rules nil)"))
      (saved-context-rule-tests--write-form path corrupt :raw-p t)
      (test-assert (handler-case
                      (progn (context-rule-save configuration
                                                (saved-context-rule-tests--rule "new" "new")
                                                :scope ':global)
                             nil)
                     (context-rule-corruption-error () t))
                   "save rejects corrupt source data")
      (test-assert (equal corrupt (uiop:read-file-string path))
                   "rejected save preserves the existing corrupt bytes")))
  nil)

(-> test-saved-context-rule-publication-failures () null)
(defun test-saved-context-rule-publication-failures ()
  "Translate directory and atomic replacement failures without losing saved rules."
  (with-test-configuration (configuration)
    (let* ((rule   (saved-context-rule-tests--rule "original" "Original advice."))
           (path   (context-rule-save configuration rule :scope ':global))
           (before (uiop:read-file-string path))
           (rename (symbol-function 'uiop:rename-file-overwriting-target)))
      (test-call-with-function-replacements
       (list (list 'uiop:rename-file-overwriting-target
                   (lambda (source target)
                     (if (equal target path)
                         (error 'file-error :pathname target)
                         (funcall rename source target)))))
       (lambda ()
         (test-assert (handler-case
                         (progn (context-rule-save configuration
                                                   (saved-context-rule-tests--rule "new" "New advice.")
                                                   :scope ':global)
                                nil)
                        (context-rule-io-error () t))
                      "failed atomic replacement signals a typed storage error")))
      (test-assert (equal before (uiop:read-file-string path))
                   "failed publication preserves the previously committed bytes")
      (test-assert (equal '("original") (mapcar (lambda (item) (getf item :id))
                                               (context-rule-saved configuration :scope ':global)))
                   "the previous snapshot is readable after publication failure")
      (let* ((blocker (merge-pathnames "directory-blocker" (config :data-root configuration)))
             (blocked (configuration-copy configuration
                                          :data-root (uiop:ensure-directory-pathname blocker))))
        (with-open-file (stream blocker :direction ':output :if-exists ':error)
          (write-string "ordinary file" stream))
        (test-assert (handler-case (progn (context-rule-save blocked rule :scope ':global) nil)
                       (context-rule-io-error () t))
                     "directory failures use the same typed storage boundary"))))
  nil)

(-> test-saved-context-rule-concurrent-writers () null)
(defun test-saved-context-rule-concurrent-writers ()
  "Serialize independent store instances without losing concurrent rule IDs."
  (with-test-configuration (configuration root)
    (let ((configuration   (configuration-copy configuration :working-directory root))
          (lock            (make-lock "saved-rule-test"))
          (ready-condition (make-condition-variable))
          (start-condition (make-condition-variable))
          (ready           0)
          (start-p         nil)
          (errors          nil)
          (threads         nil))
      (labels ((write-rules (prefix)
                 (with-lock-held (lock)
                   (incf ready)
                   (condition-notify ready-condition)
                   (loop until start-p do (condition-wait start-condition lock)))
                 (handler-case
                     (dotimes (index 6)
                       (context-rule-save
                        (configuration-copy configuration)
                        (saved-context-rule-tests--rule (format nil "~A-~D" prefix index) "Concurrent advice.")))
                   (error (condition)
                     (with-lock-held (lock) (push condition errors))))))
        (unwind-protect
             (progn
               (setf threads (loop for prefix in '("first" "second")
                                   collect (let ((prefix prefix))
                                             (make-thread (lambda () (write-rules prefix))))))
               (with-lock-held (lock)
                 (loop until (= ready 2) do (condition-wait ready-condition lock))
                 (setf start-p t)
                 (condition-notify start-condition)
                 (condition-notify start-condition))
               (mapc #'join-thread threads)
               (test-assert (null errors) "independent concurrent saves complete successfully")
               (test-assert (= 12 (length (context-rule-saved configuration :scope ':workspace)))
                            "both writers' independent IDs survive locked publication"))
          (with-lock-held (lock)
            (setf start-p t)
            (condition-notify start-condition)
            (condition-notify start-condition))
          (dolist (thread threads)
            (when (thread-alive-p thread) (join-thread thread)))))))
  nil)
