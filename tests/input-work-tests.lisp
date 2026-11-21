(in-package #:autolith)

;;;; -- Durable Input Payloads --

(-> input-work-tests--command () application-command)
(defun input-work-tests--command ()
  "Return a command fixture independent of cwd admission policy."
  (application-command-create
   :definition-name 'input-work-tests--command :name "input-work-command"
   :aliases '("input-work-alias") :argument "VALUE" :description "codec fixture"
   :tip "exercise durable command input." :busy-behavior ':hold :terminal-behavior ':shared
   :handler (lambda (application invocation)
              (declare (ignore application invocation))
              ':continue)))

(-> input-work-tests--committed-owners (conversation command-message-input) null)
(defun input-work-tests--committed-owners (conversation input)
  "Retain committed command ownership after its receipt is durable, through recovery."
  (let* ((committed       (user-message-input-copy input))
         (identifier      (command-message-input-identifier committed))
         (conversation-id (conversation-identifier conversation)))
    (setf (command-message-input-phase committed) ':committed)
    (conversation-ensure-automatic-user-message
     conversation (command-message-input-notification committed) :pending-input-identifier identifier)
    (test-assert (conversation-pending-input-identifier-p conversation identifier)
                 "the recovery fixture has a durable receipt for this owner")
    (labels ((restore-owner (candidate position)
               "Return the recovered owner and its pending-input identity."
               (let* ((active-work
                        (case position
                          (:active
                           (input-work-from-input candidate))
                          (:in-flight
                           (list ':message "unpersisted primary"))))
                      (state
                        (list :snapshot-identifier             "committed-owner-snapshot"
                              :active-work                     active-work
                              :active-work-identifier          (when active-work
                                                                 (if (eq position ':active)
                                                                     identifier "unpersisted-primary"))
                              :steering-in-flight-items
                              (unless (eq position ':active)
                                (list (agent-steering-input-create :identifier identifier :content candidate)))
                              :steering-promotion-prefix-count 0))
                      (form     (input-state->form state conversation-id))
                      (readable (let ((*read-eval* nil))
                                  (read-from-string (write-to-string form :readably t))))
                      (restored (input-state-from-form readable conversation-id))
                      (filtered (input-state-filter-persisted restored conversation)))
                 (ecase position
                   (:active
                    (values (second (getf filtered :active-work))
                            (getf filtered :active-work-identifier)))
                   (:in-flight
                    (let ((entry (first (getf filtered :steering-in-flight-items))))
                      (values (when entry (agent-steering-input-content entry))
                              (when entry (agent-steering-input-identifier entry)))))
                   (:promoted
                    (let ((owner (second (first (getf filtered :work-items)))))
                      (values owner (when (typep owner 'command-message-input)
                                      (command-message-input-identifier owner)))))))))
      (dolist (position '(:active :in-flight :promoted))
        (multiple-value-bind (owner owner-id) (restore-owner committed position)
          (test-assert
           (and (typep owner 'command-message-input)
                (eq (command-message-input-phase owner) ':committed)
                (equal (command-message-input-identifier owner) identifier)
                (equal owner-id identifier)
                (equal (application-command-invocation-arguments (command-message-input-invocation owner))
                       (application-command-invocation-arguments (command-message-input-invocation committed))))
           (format nil "a committed typed ~A owner survives its durable receipt" position)))
        (test-assert (null (restore-owner "ordinary prose" position))
                     (format nil "a receipt retires an ordinary ~A message owner" position)))))
  nil)

(-> input-work-tests--command-codec () null)
(defun input-work-tests--command-codec ()
  "Exercise command copy, native persistence, malformed records and steering recovery."
  (let ((registry (application-command--registry-snapshot)))
    (unwind-protect
         (let* ((command (input-work-tests--command))
                (path    (merge-pathnames "workspace/" (uiop:temporary-directory)))
                (source  (user-message-input-create
                          :text "(cwd)" :submission '(:id "submission" :client "editor" :source :steering))))
           (register-application-command command :source ':runtime)
           (dolist (argument (list "~/workspace" path))
             (let* ((invocation
                      (make-instance 'application-command-invocation
                                     :input "(cwd)" :syntax ':lisp :name "input-work-alias"
                                     :remainder "" :argument (when (stringp argument) argument)
                                     :arguments (list argument) :supplied-argument-count 1 :command command))
                    (input (command-message-input-create source invocation)))
               (test-assert (and (eq (command-message-input-phase input) ':pending)
                                 (null (command-message-input-prepared-arguments input))
                                 (null (command-message-input-notification input))
                                 (null (command-message-input-identifier input)))
                            "new commands begin pending without prepared state")
               (setf (command-message-input-identifier input) "pending-command")
               (dolist (phase '(:pending :prepared :committed :failed))
                 (setf (command-message-input-phase input) phase
                       (command-message-input-prepared-arguments input)
                       (list path :physical-p t :note "prepared")
                       (command-message-input-notification input) "Workspace changed.")
                 (let* ((form     (input-value->form input))
                        (readable (let ((*read-eval* nil))
                                    (read-from-string (write-to-string form :readably t))))
                        (restored (input-value-from-form readable))
                        (copy     (user-message-input-copy input)))
                   (test-assert (and (typep restored 'command-message-input)
                                     (equal form (input-value->form restored))
                                     (equal form (input-value->form copy)))
                                "all phases and native values survive readable persistence and copy")
                   (test-assert (and (not (eq invocation (command-message-input-invocation input)))
                                     (not (eq (command-message-input-invocation input)
                                              (command-message-input-invocation copy)))
                                     (equal (application-command-invocation-arguments
                                             (command-message-input-invocation restored))
                                            (list argument))
                                     (eq command (application-command-invocation-command
                                                  (command-message-input-invocation restored))))
                                "the argument-free display label never replaces actual native arguments")
                   (test-assert (and (eq (first (input-work-from-input restored)) ':command)
                                     (null (input-message restored)))
                                "typed invocations are command work rather than Lisp or prose")
                   (test-assert
                    (every (lambda (kind) (null (input-work-from-form (list kind readable))))
                           '(:message :lisp :native-lisp))
                    "command payloads cannot be restored as prose or Lisp")))
               (let* ((request        (list :name "cwd" :arguments (list path)))
                      (entry          (agent-steering-input-create :identifier "pending-command" :content input
                                                                  :operation-request request))
                      (state          (list :active-work (input-work-from-input input) :active-work-identifier "active"
                                            :steering-in-flight-items (list entry) :steering-items (list input)
                                            :work-items                      (list (input-work-from-input input))
                                            :steering-promotion-prefix-count 0))
                      (restored       (input-state-from-payload (input-state->payload state)))
                      (restored-entry (first (getf restored :steering-in-flight-items))))
                 (test-assert (and restored
                                   (equal request (agent-steering-input-operation-request restored-entry))
                                   (not (eq request (agent-steering-input-operation-request restored-entry)))
                                   (string= "pending-command" (command-message-input-identifier
                                                               (agent-steering-input-content restored-entry))))
                              "steering retains native operation provenance and pending identity")
                 (with-test-configuration (configuration root)
                   (declare (ignore root))
                   (let* ((application (recovery-input-vault-tests--application configuration "command-codec"))
                          (promoted    (getf (input-state-filter-persisted
                                              (list :steering-in-flight-items (list restored-entry)
                                                    :steering-items (list input) :steering-promotion-prefix-count 0)
                                              (application-conversation application)) :work-items)))
                     (test-assert (and (= (length promoted) 2)
                                       (every (lambda (work)
                                                (and (eq (first work) ':command)
                                                     (string= "pending-command"
                                                              (command-message-input-identifier (second work)))))
                                              promoted))
                                  "recovery promotion preserves command kind and pending identity")
                     (input-work-tests--committed-owners
                      (application-conversation application) input))))
               (when (stringp argument)
                 (let ((copy (user-message-input-copy input)))
                   (setf (char (first (application-command-invocation-arguments
                                       (command-message-input-invocation copy))) 0) #\X
                         (char (user-message-input-text copy) 0) #\X
                         (char (getf (user-message-input-submission copy) :id) 0) #\X
                         (char (getf (rest (command-message-input-prepared-arguments copy)) :note) 0) #\X
                         (char (command-message-input-notification copy) 0) #\X
                         (char (command-message-input-identifier copy) 0) #\X)
                   (test-assert (and (string= argument (first (application-command-invocation-arguments
                                                               (command-message-input-invocation input))))
                                     (string= "(cwd)" (user-message-input-text input))
                                     (string= "submission" (getf (user-message-input-submission input) :id))
                                     (string= "prepared"
                                              (getf (rest (command-message-input-prepared-arguments input)) :note))
                                     (string= "Workspace changed." (command-message-input-notification input))
                                     (string= "pending-command" (command-message-input-identifier input)))
                                "mutable command values are detached by copy")))))
           (let* ((input (command-message-input-create source
                          (make-instance 'application-command-invocation
                                         :input "(cwd)" :syntax ':lisp :name "input-work-alias"
                                         :remainder "" :argument nil :arguments (list path)
                                         :supplied-argument-count 1 :command command)))
                  (form  (input-value->form input)))
             (dolist (case '((:version 2) (:phase :unknown) (:identifier "")
                             (:prepared-arguments "not a list") (:notification 42) (:text nil)
                             (:image-pathnames ("relative.png")) (:submission (:id "incomplete"))
                             (:invocation nil)))
               (let ((bad (copy-tree form)))
                 (setf (getf (rest bad) (first case)) (second case))
                 (test-assert (null (input-value-from-form bad)) "malformed command state is rejected")))
             (dolist (case '((:version 2) (:syntax :json) (:name nil) (:remainder nil)
                             (:argument #P"/not-a-text-argument/") (:arguments ("dotted" . "tail"))
                             (:supplied-argument-count -1) (:supplied-argument-count 0)
                              (:command "missing-command") (:command "input-work-alias")))
               (let ((bad (copy-tree form)))
                 (setf (getf (rest (getf (rest bad) :invocation)) (first case)) (second case))
                 (test-assert (null (input-value-from-form bad)) "malformed or noncanonical invocation is rejected")))
             (let ((bad (copy-tree form)))
               (setf (getf (rest bad) :prepared-arguments) (list #'identity))
               (test-assert (null (input-value-from-form bad)) "unreadable native objects are rejected"))
             (test-assert (null (input-value-from-form (append form (list :phase ':pending))))
                          "duplicate command fields are rejected")
             (let ((replacement (input-work-tests--command)))
               (register-application-command replacement :source ':runtime)
               (test-assert (eq replacement (application-command-invocation-command
                                             (command-message-input-invocation (input-value-from-form form))))
                            "restored invocations resolve the current registered definition"))
             (unregister-application-command 'input-work-tests--command :source ':runtime)
             (test-assert (null (input-value-from-form form)) "unregistered commands cannot be restored")))
      (application-command--registry-restore registry)))
  nil)

(-> test-input-work-payload-codec () null)
(defun test-input-work-payload-codec ()
  "Round-trip input attachments, queue provenance, steering and native work."
  (let* ((image      (merge-pathnames "input.png" (uiop:temporary-directory)))
         (submission '(:id "submission" :client "editor" :source :queue))
         (input      (user-message-input-create :text "image" :image-pathnames (list image)
                                                :submission submission))
         (state      (list :active-work
                           (list ':native-lisp "(error \"unevaluated\")" :submission submission)
                           :active-work-identifier          "active"
                           :steering-in-flight-items
                           (list (agent-steering-input-create :identifier "steering" :content input))
                           :steering-items                  (list input)
                           :work-items
                           (list (input-work-from-input input)
                                 (list ':native-lisp "(values)" :submission submission)
                                (input-work-from-input
                                 (user-message-input-create :text "(+ 1 2)" :submission submission)))
                           :steering-promotion-prefix-count 2))
         (payload    (input-state->payload state))
         (restored   (input-state-from-payload payload)))
    (test-assert restored "a payload is decodable without snapshot or conversation identity")
    (test-assert (equal payload (input-state->payload restored))
                 "direct payloads preserve images, identified work and queue order")
    (test-assert (= 6 (input-state-count restored))
                 "the input model counts active, in-flight, steering and queued input")
    (dolist (work (cons (getf restored :active-work) (getf restored :work-items)))
      (test-assert (equal submission (getf (nthcdr 2 work) :submission))
                   "native, prose and Lisp work retain submission provenance"))
    (let ((form (input-state->form (list* :snapshot-identifier "snapshot" state) "conversation")))
      (test-assert (equal payload
                          (input-state->payload (input-state-from-form form "conversation")))
                   "the pending envelope uses the same payload codec")
      (test-assert (null (input-state-from-form form "other"))
                   "pending envelopes enforce their conversation identity"))
    (let ((bad (copy-tree payload)))
      (setf (getf (getf bad :native-work) :steering-promotion-prefix-count) 100)
      (test-assert (null (input-state-from-payload bad))
                   "invalid native queue metadata is rejected")))
  (dolist (form '((:lisp "(values)" :submission (:id "id"))
                  (:message "hello" :unexpected (:id "id" :client "client" :source :prompt))))
    (test-assert (null (input-work-from-form form)) "invalid work provenance is rejected"))
  (test-assert (equal (user-message-input-submission
                       (user-message-input-create
                        :text "hello" :submission '(:id "id" :client "client" :source "steering")))
                      '(:id "id" :client "client" :source :steering))
               "a source an older client or record spelled as a string is read as its keyword")
  (input-work-tests--command-codec)
  nil)
