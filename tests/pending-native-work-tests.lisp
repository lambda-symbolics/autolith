(in-package #:autolith)

;;;; -- Cross-Version Pending Work --

(-> test-pending-native--state (&key (:work list) (:active (option list)) (:prefix (integer 0))) list)
(defun test-pending-native--state (&key work active (prefix 0))
  "Return identified pending state with steering and optional native WORK."
  (list :snapshot-identifier       "native-snapshot"
        :active-work active :active-work-identifier (and active "active-id")
        :steering-in-flight-items
        (list (agent-steering-input-create :identifier "flight-id" :content "flight"))
        :steering-items '("steer") :work-items work
        :vault-capture-identifiers '("capture-id")
        :steering-promotion-prefix-count prefix :legacy-p nil))

(-> test-pending-native--call-old (function) t)
(defun test-pending-native--call-old (function)
  "Call FUNCTION with the complete 0.67.8 decoder, work reader and writer."
  (test-call-with-function-replacements
   (list (list 'input-state-from-form
               #'test-pending-native--old-state)
         (list 'input-work-from-form
               #'test-pending-native--old-work)
         (list 'input-state->form
               #'test-pending-native--old-form))
   function))

(-> test-pending-native--projection (list) list)
(defun test-pending-native--projection (state)
  "Return the observable payload and identities of normalized STATE."
  (list (getf state :active-work) (getf state :active-work-identifier)
        (mapcar (lambda (entry)
                  (list (agent-steering-input-identifier entry)
                        (agent-steering-input-content entry)))
                (getf state :steering-in-flight-items))
        (getf state :steering-items) (getf state :work-items)
        (getf state :vault-capture-identifiers)
        (getf state :steering-promotion-prefix-count)))

(-> test-pending-snapshot-readable-by-pre-native-readers () null)
(defun test-pending-snapshot-readable-by-pre-native-readers ()
  "Test old-reader acceptance and exact current round trips at every FIFO prefix."
  (dolist (work '(() ((:message "ordinary"))
                  ((:native-lisp "(error \"never evaluate\")"))
                  ((:native-lisp "head") (:message "middle") (:native-lisp "tail"))
                  ((:message "first") (:native-lisp "middle") (:command "/help")
                   (:lisp "(+ 1 2)"))))
    (dolist (active '(nil (:message "active") (:native-lisp "active-native")))
      (loop for prefix from 0 to (length work)
            do (let* ((state    (test-pending-native--state
                                 :work work :active active :prefix prefix))
                      (form     (input-state->form state "c"))
                      (current  (input-state-from-form form "c"))
                      (ordinary (remove ':native-lisp work :key #'first))
                      (old      (test-pending-native--call-old
                                 (lambda ()
                                   (input-state-from-form form "c")))))
                 (test-assert current "current writer output is valid before downgrade")
                 (test-assert
                  (equal (test-pending-native--projection current)
                         (test-pending-native--projection state))
                  "current reader round-trips all work, steering, identities and full prefix")
                 (test-assert old "exact old decoder accepts current snapshots")
                 (when old
                   (let ((expected (copy-list state)))
                     (when (eq (first active) ':native-lisp)
                       (setf (getf expected :active-work) nil
                             (getf expected :active-work-identifier) nil))
                     (setf (getf expected :work-items) ordinary
                           (getf expected :steering-promotion-prefix-count)
                           (count-if (lambda (item) (not (eq (first item) ':native-lisp)))
                                     (subseq work 0 prefix)))
                     (test-assert
                      (equal (test-pending-native--projection old)
                             (test-pending-native--projection expected))
                      "old reader keeps ordinary inputs and exactly projects the prefix")))))))
  nil)

(-> test-pending-native--extended-form () list)
(defun test-pending-native--extended-form ()
  "Return a proposed-format fixture independent of the production writer."
  '(:pending-inputs :version 2 :snapshot-identifier "snapshot" :conversation-id "c"
    :active-work nil :steering-in-flight nil :steering nil :work ((:message "keep"))
    :vault-capture-identifiers nil :steering-promotion-prefix-count 0
    :native-work (:active-work nil :items ((:position 1 :work (:native-lisp "never")))
                  :steering-promotion-prefix-count 0)))

(-> test-pending-native-work-form-rejections () null)
(defun test-pending-native-work-form-rejections ()
  "Test finite strict extension records, conflicts, positions and prefix projection."
  (test-assert (input-state-from-form
                (test-pending-native--extended-form) "c")
               "valid proposed format is accepted before malformed controls")
  (dolist (extension
           '(nil () (:items nil) (:active-work nil :items nil :steering-promotion-prefix-count 0)
             (:active-work nil :items ((:position -1 :work (:native-lisp "x")))
              :steering-promotion-prefix-count 0)
             (:active-work nil :items ((:position 0.5 :work (:native-lisp "x")))
              :steering-promotion-prefix-count 0)
             (:active-work nil :items ((:position 2 :work (:native-lisp "x")))
              :steering-promotion-prefix-count 0)
             (:active-work nil :items ((:position 0 :work (:message "x")))
              :steering-promotion-prefix-count 0)
             (:active-work nil :items ((:position 0 :position 1 :work (:native-lisp "x")))
              :steering-promotion-prefix-count 0)
             (:active-work nil :items ((:position 0 :work (:native-lisp "x" "extra")))
              :steering-promotion-prefix-count 0)
             (:active-work (:identifier "" :work (:native-lisp "x")) :items nil
              :steering-promotion-prefix-count 0)
             (:active-work (:identifier "id" :work (:native-lisp 42)) :items nil
              :steering-promotion-prefix-count 0)
             (:active-work nil :items ((:position 1 :work (:native-lisp "x"))
                                       (:position 0 :work (:native-lisp "y")))
              :steering-promotion-prefix-count 0)
             (:active-work nil :items ((:position 0 :work (:native-lisp "x"))
                                       (:position 0 :work (:native-lisp "y")))
              :steering-promotion-prefix-count 0)
             (:active-work nil :items ((:position 1 :work (:native-lisp "x")))
              :steering-promotion-prefix-count 3)
             (:active-work nil :items ((:position 1 :work (:native-lisp "x")))
              :steering-promotion-prefix-count 1)
             (:active-work nil :items ((:position 1 :work (:native-lisp "x")))
              :steering-promotion-prefix-count 0 :unknown t)
             (:active-work nil :items ((:position 1 :work (:native-lisp "x")))
              :steering-promotion-prefix-count 0 :items nil)))
    (let ((form (copy-tree (test-pending-native--extended-form))))
      (setf (getf (rest form) :native-work) extension)
      (test-assert (null (input-state-from-form form "c"))
                   "malformed native metadata rejects the entire snapshot")))
  nil)

(-> TEST-PENDING-NATIVE--OLD-WORK (T) (OPTION LIST))
(DEFUN TEST-PENDING-NATIVE--OLD-WORK (FORM)
  "Return one validated in-memory work item restored from durable FORM."
  (WHEN
      (AND (LISTP FORM)
           (MEMBER (FIRST FORM) '(:MESSAGE :COMMAND :LISP) :TEST #'EQ))
    (LET ((INPUT (input-value-from-form (SECOND FORM))))
      (WHEN INPUT (LIST (FIRST FORM) INPUT)))))

(-> TEST-PENDING-NATIVE--OLD-STATE (LIST STRING) (OPTION LIST))
(DEFUN TEST-PENDING-NATIVE--OLD-STATE (FORM CONVERSATION-IDENTIFIER)
  "Return normalized validated pending state from durable FORM."
  (WHEN
      (AND (LISTP FORM) (EQ (FIRST FORM) ':PENDING-INPUTS)
           (STRINGP (GETF (REST FORM) :CONVERSATION-ID))
           (STRING= (GETF (REST FORM) :CONVERSATION-ID)
                    CONVERSATION-IDENTIFIER))
    (LET ((VERSION (OR (GETF (REST FORM) :VERSION) 0)))
      (CASE VERSION
        (1
         (LET* ((STEERING-FORMS (GETF (REST FORM) :STEERING))
                (WORK-FORMS     (GETF (REST FORM) :WORK))
                (STEERING
                 (AND (LISTP STEERING-FORMS)
                      (MAPCAR #'input-value-from-form
                              STEERING-FORMS)))
                (WORK
                 (AND (LISTP WORK-FORMS)
                      (MAPCAR #'input-work-from-form
                              WORK-FORMS))))
           (WHEN
               (AND (LISTP STEERING-FORMS) (LISTP WORK-FORMS)
                    (EVERY #'IDENTITY STEERING) (EVERY #'IDENTITY WORK))
             (LIST :SNAPSHOT-IDENTIFIER (MAKE-IDENTIFIER) :ACTIVE-WORK NIL
                   :ACTIVE-WORK-IDENTIFIER NIL :STEERING-IN-FLIGHT-ITEMS NIL
                   :STEERING-ITEMS STEERING :WORK-ITEMS WORK
                   :VAULT-CAPTURE-IDENTIFIERS NIL
                   :STEERING-PROMOTION-PREFIX-COUNT 0 :LEGACY-P T))))
        (2
         (LET* ((SNAPSHOT-IDENTIFIER (GETF (REST FORM) :SNAPSHOT-IDENTIFIER))
                (ACTIVE-FORM         (GETF (REST FORM) :ACTIVE-WORK))
                (ACTIVE-WORK
                 (AND ACTIVE-FORM
                      (input-work-from-form
                       (GETF ACTIVE-FORM :WORK))))
                (ACTIVE-WORK-IDENTIFIER
                 (AND ACTIVE-FORM (GETF ACTIVE-FORM :IDENTIFIER)))
                (IN-FLIGHT-FORMS     (GETF (REST FORM) :STEERING-IN-FLIGHT))
                (IN-FLIGHT
                 (AND (LISTP IN-FLIGHT-FORMS)
                      (MAPCAR
                       #'input-steering-from-form
                       IN-FLIGHT-FORMS)))
                (STEERING-FORMS      (GETF (REST FORM) :STEERING))
                (STEERING
                 (AND (LISTP STEERING-FORMS)
                      (MAPCAR #'input-value-from-form
                              STEERING-FORMS)))
                (WORK-FORMS          (GETF (REST FORM) :WORK))
                (WORK
                 (AND (LISTP WORK-FORMS)
                      (MAPCAR #'input-work-from-form
                              WORK-FORMS)))
                (VAULT-CAPTURE-IDENTIFIERS
                 (OR (GETF (REST FORM) :VAULT-CAPTURE-IDENTIFIERS) NIL))
                (STEERING-PROMOTION-PREFIX-COUNT
                 (OR (GETF (REST FORM) :STEERING-PROMOTION-PREFIX-COUNT) 0)))
           (WHEN
               (AND (NON-EMPTY-STRING-P SNAPSHOT-IDENTIFIER)
                    (OR (NULL ACTIVE-FORM)
                        (AND (NON-EMPTY-STRING-P ACTIVE-WORK-IDENTIFIER)
                             ACTIVE-WORK))
                    (LISTP IN-FLIGHT-FORMS) (EVERY #'IDENTITY IN-FLIGHT)
                    (LISTP STEERING-FORMS) (EVERY #'IDENTITY STEERING)
                    (LISTP WORK-FORMS) (EVERY #'IDENTITY WORK)
                    (LISTP VAULT-CAPTURE-IDENTIFIERS)
                    (EVERY #'NON-EMPTY-STRING-P VAULT-CAPTURE-IDENTIFIERS)
                    (= (LENGTH VAULT-CAPTURE-IDENTIFIERS)
                       (LENGTH
                        (REMOVE-DUPLICATES VAULT-CAPTURE-IDENTIFIERS :TEST
                                           #'STRING=)))
                    (TYPEP STEERING-PROMOTION-PREFIX-COUNT '(INTEGER 0))
                    (<= STEERING-PROMOTION-PREFIX-COUNT (LENGTH WORK)))
             (LIST :SNAPSHOT-IDENTIFIER (COPY-SEQ SNAPSHOT-IDENTIFIER)
                   :ACTIVE-WORK ACTIVE-WORK :ACTIVE-WORK-IDENTIFIER
                   (AND ACTIVE-WORK-IDENTIFIER
                        (COPY-SEQ ACTIVE-WORK-IDENTIFIER))
                   :STEERING-IN-FLIGHT-ITEMS IN-FLIGHT :STEERING-ITEMS STEERING
                   :WORK-ITEMS WORK :VAULT-CAPTURE-IDENTIFIERS
                   (MAPCAR #'COPY-SEQ VAULT-CAPTURE-IDENTIFIERS)
                   :STEERING-PROMOTION-PREFIX-COUNT
                   STEERING-PROMOTION-PREFIX-COUNT :LEGACY-P NIL))))
        (OTHERWISE
         NIL)))))

(-> test-pending-native--lifecycle (&key (:old-p boolean) (:legacy-p boolean) (:native-active-p boolean) (:empty-p boolean)) null)
(defun test-pending-native--lifecycle (&key old-p legacy-p native-active-p empty-p)
  "Execute actual restore and publication against an independent snapshot."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (let* ((conversation (conversation-create configuration :identifier "c"))
           (ui           (terminal-ui-create :terminal (make-instance 'waiting-recording-terminal :columns 60)))
           (application  (make-instance 'application :configuration configuration
                                       :conversation conversation :ui ui))
           (path         (configuration-pending-inputs-path configuration (conversation-pathname conversation)))
           (legacy       (configuration-legacy-pending-inputs-path configuration))
           (state        (if empty-p
                             (list :snapshot-identifier "empty" :active-work nil :work-items '((:native-lisp "never"))
                                   :steering-promotion-prefix-count 1)
                             (test-pending-native--state
                              :active (if native-active-p '(:native-lisp "active-native") '(:message "active"))
                              :work '((:native-lisp "head") (:message "keep") (:native-lisp "tail")) :prefix 1)))
           (form         (input-state->form state "c"))
           (controller   nil))
      (conversation-append-user-message conversation "seed")
      (snapshot-write (if legacy-p legacy path) form)
      (test-assert (second (multiple-value-list (snapshot-read (if legacy-p legacy path))))
                   "checksummed fixture publication succeeds before controller creation")
      (unwind-protect
           (flet ((run ()
                    "Create the real controller using the selected complete decoder."
                    (setf controller (application-input-controller-create application))))
             (if old-p (test-pending-native--call-old #'run) (run))
             (cond
               (empty-p
                (test-assert
                 (equal (application-input-controller--state controller :work-items)
                        (unless old-p '((:native-lisp "never"))))
                 "native-only restore drops only native work in an old image")
                (test-assert (if old-p (not (probe-file path)) (probe-file path))
                             "empty downgrade publication may remove the native-only file"))
               (t
                ;; Without an active item, the existing filter promotes steering into work.
                (test-assert
                 (equal (application-input-controller--state controller :work-items)
                        (cond
                          ((and old-p native-active-p)
                           '((:message "flight") (:message "steer") (:message "keep")))
                          (old-p
                           '((:message "active") (:message "keep")))
                          (native-active-p
                           '((:native-lisp "active-native") (:native-lisp "head")
                             (:message "keep") (:native-lisp "tail")))
                          (t
                           '((:message "active") (:native-lisp "head")
                             (:message "keep") (:native-lisp "tail")))))
                 "controller restore follows active movement and steering-filter ordering")
                (test-assert
                 (equal (application-input-controller--state controller :steering-items)
                        (unless (and old-p native-active-p) '("flight" "steer")))
                 "in-flight steering moves to steering unless promoted by the existing filter")
                (multiple-value-bind (saved complete-p) (snapshot-read path)
                  (test-assert complete-p "initial controller publication keeps ordinary inputs")
                  (test-assert (input-state-from-form saved "c")
                               "published output is accepted by the current reader"))))
             (when legacy-p
               (test-assert (if (and old-p empty-p) (probe-file legacy) (not (probe-file legacy)))
                            "legacy cleanup follows successful canonical publication")))
        (when controller (application-input-controller-stop controller)))))
  nil)

(-> test-pending-native-work-lifecycle () null)
(defun test-pending-native-work-lifecycle ()
  "Test old/current terminal restore, republish, and legacy global migration."
  (dolist (old-p '(nil t))
    (dolist (legacy-p '(nil t))
      (dolist (native-active-p '(nil t))
        (test-pending-native--lifecycle :old-p old-p :legacy-p legacy-p
                                       :native-active-p native-active-p))
      (test-pending-native--lifecycle :old-p old-p :legacy-p legacy-p :empty-p t)))
  nil)

(-> test-pending-native-work-vault () null)
(defun test-pending-native-work-vault ()
  "Test proposed-format import, candidate capture persistence and native payload equality."
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (let* ((application  (recovery-input-vault-tests--application configuration "c"))
           (conversation (application-conversation application))
           (path         (configuration-pending-inputs-path configuration (conversation-pathname conversation)))
           (form         (test-pending-native--extended-form)))
      (snapshot-write path form)
      (test-assert (second (multiple-value-list (snapshot-read path))) "vault fixture writes correctly")
      (test-assert (input-state-form-shape-p form)
                   "strict vault shape accepts the proposed additive section")
      (application-recovery-input-vault-import application)
      (let ((captures (application-recovery-input-vault-captures application)))
        (test-assert (= (length captures) 1) "recovery import captures the proposed-format input")
        (when captures
          (test-assert (equal (getf (first captures) :work-items)
                              '((:message "keep") (:native-lisp "never")))
                       "vault disk round trip retains the native item and order")))
      (dolist (active '(nil (:native-lisp "active-native")))
        (let* ((state    (test-pending-native--state
                          :active active :work '((:message "keep") (:native-lisp "never")) :prefix 1))
               (capture  (append (list :id "capture" :captured-at 1) state))
               (saved    (application-recovery-input-vault--capture-form capture))
               (restored (application-recovery-input-vault--capture-from-form saved path))
               (other    (copy-tree state)))
          (setf (getf state :vault-capture-identifiers) nil)
          (test-assert restored "candidate capture writer output decodes after restart")
          (test-assert (equal (test-pending-native--projection restored)
                              (test-pending-native--projection state))
                       "capture round trip retains native active identity and full payload")
          (setf (getf other :work-items) '((:message "keep") (:native-lisp "different")))
          (test-assert (not (application-recovery-input-vault--pending-payload-equal-p state other))
                       "payload comparison distinguishes native source changes")))))
  nil)

(-> test-pending-native-work-finite-and-legacy () null)
(defun test-pending-native-work-finite-and-legacy ()
  "Test finite parsing, duplicate/conflicting records, and historical input formats."
  (dolist (location '(:outer :native :items :item :source))
    (let* ((form   (copy-tree (test-pending-native--extended-form)))
           (native (getf (rest form) :native-work))
           (cycle  (list :cycle)))
      (setf (rest cycle) cycle)
      (ecase location
        (:outer
         (setf (rest (last form)) form))
        (:native
         (setf (getf (rest form) :native-work) cycle))
        (:items
         (setf (getf native :items) cycle))
        (:item
         (setf (getf native :items) (list cycle)))
        (:source
         (setf (getf (first (getf native :items)) :work) cycle)))
      (test-assert (null (input-state-from-form form "c"))
                   "circular extension structures reject without traversal hangs")))
  (dolist (kind '(:duplicate :conflict :inline :improper :wrong-conversation))
    (let ((form (copy-tree (test-pending-native--extended-form))))
      (ecase kind
        (:duplicate
         (setf form (append form '(:work nil))))
        (:conflict
         (setf (getf (rest form) :active-work) '(:identifier "a" :work (:message "ordinary"))
               (getf (getf (rest form) :native-work) :active-work)
               '(:identifier "b" :work (:native-lisp "native"))))
        (:inline
         (setf (getf (rest form) :work) '((:native-lisp "ambiguous"))))
        (:improper
         (setf (getf (rest form) :native-work) '(:items . :bad)))
        (:wrong-conversation
         (setf (getf (rest form) :conversation-id) "other")))
      (test-assert (null (input-state-from-form form "c"))
                   "conflicting and wrong-identity extensions reject")))
  (test-assert
   (equal (getf (input-state-from-form
                 '(:pending-inputs :version 1 :conversation-id "c" :steering ("s")
                   :work ((:message "m"))) "c") :work-items)
          '((:message "m")))
   "v1 restore keeps its historical behavior")
  (test-assert
   (equal (getf (input-state-from-form
                 '(:pending-inputs :version 2 :snapshot-identifier "old" :conversation-id "c"
                   :active-work (:identifier "id" :work (:native-lisp "old-active"))
                   :steering-in-flight nil :steering nil :work ((:native-lisp "old-inline"))) "c")
               :work-items) '((:native-lisp "old-inline")))
   "historical inline-native v2 input stays readable without an extension")
  nil)

(-> test-acp-input-validate-storage-native-work () null)
(defun test-acp-input-validate-storage-native-work ()
  "Test actual ACP initialization and typed native-head/malformed rejection."
  (acp-input-test--session
   (lambda (session client)
     (declare (ignore client))
     (let* ((application   (acp-session-application session))
            (configuration (application-configuration application))
            (conversation  (application-conversation application))
            (identifier    (conversation-identifier conversation))
            (path          (configuration-pending-inputs-path configuration (conversation-pathname conversation)))
            (state         (list :snapshot-identifier             "acp-native"
                                 :work-items                      '((:native-lisp "never") (:message "keep"))
                                 :steering-promotion-prefix-count 0))
            (form          (input-state->form state identifier)))
       (snapshot-write path form)
       (test-assert (null (acp-input--validate-storage application)) "ACP accepts candidate storage")
       (let ((controller (acp-input-initialize session)))
         (test-assert (equal (application-input-controller--state controller :work-items)
                             '((:native-lisp "never") (:message "keep")))
                      "ACP initialization retains native work and ordinary queued input")
         (test-assert (handler-case (progn (acp-input--prepare-queued-turn session) nil)
                        (agentcomms:acp-state-error () t))
                      "ACP refuses a native head with its existing typed terminal-required error"))
       (let ((bad (copy-tree (test-pending-native--extended-form))))
         (setf (getf (rest bad) :conversation-id) identifier
               (getf (rest bad) :native-work) nil)
         (snapshot-write path bad)
         (let ((before (recovery-input-vault-tests--octets path)))
           (test-assert (handler-case (progn (acp-input--validate-storage application) nil)
                          (agentcomms:acp-state-error () t))
                        "ACP rejects malformed native metadata loudly")
           (test-assert (equalp before (recovery-input-vault-tests--octets path))
                        "failed ACP validation leaves the input bytes intact"))))))
  nil)

(-> test-pending-native--old-form (list string) list)
(defun test-pending-native--old-form (state conversation-identifier)
  "Return the version-two durable form for normalized pending STATE."
  (let ((active-work            (getf state :active-work))
        (active-work-identifier (getf state :active-work-identifier)))
    (list :pending-inputs :version 2 :snapshot-identifier
          (getf state :snapshot-identifier) :conversation-id
          conversation-identifier :active-work
          (and active-work active-work-identifier
               (list :identifier active-work-identifier :work
                     (input-work->form
                      active-work)))
          :steering-in-flight
          (mapcar #'input-steering->form
                  (getf state :steering-in-flight-items))
          :steering
          (mapcar #'input-value->form
                  (getf state :steering-items))
          :work
          (input-work-list->forms
           (getf state :work-items))
          :vault-capture-identifiers
          (mapcar #'copy-seq (getf state :vault-capture-identifiers))
          :steering-promotion-prefix-count
          (or (getf state :steering-promotion-prefix-count) 0))))

(-> test-pending-native-work-identities () null)
(defun test-pending-native-work-identities ()
  "Test serialized FIFO indices, persisted native identity, and foreign legacy input."
  (let* ((state   (test-pending-native--state
                   :work '((:ignored "omit") (:native-lisp "native") (:message "keep")) :prefix 2))
         (form    (input-state->form state "c"))
         (decoded (input-state-from-form form "c")))
    (test-assert (equal (getf decoded :work-items) '((:native-lisp "native") (:message "keep")))
                 "native positions index only the writer's restorable FIFO")
    (test-assert (= (getf decoded :steering-promotion-prefix-count) 1)
                 "full prefix is projected through omitted non-restorable work"))
  (with-test-configuration (configuration root)
    (declare (ignore root))
    (let* ((application  (recovery-input-vault-tests--application configuration "c"))
           (conversation (application-conversation application))
           (state        (test-pending-native--state :active '(:native-lisp "active")
                                                      :work '((:message "keep")) :prefix 1))
           (form         (input-state->form state "c"))
           (legacy       (configuration-legacy-pending-inputs-path configuration)))
      (conversation-append-user-message conversation "already committed"
                                        :pending-input-identifier "active-id")
      (let ((filtered (input-state-filter-persisted
                       (input-state-from-form form "c") conversation)))
        (test-assert (null (getf filtered :active-work))
                     "restored native active identity is filtered after durable acknowledgement"))
      (setf (getf (rest form) :conversation-id) "foreign")
      (snapshot-write legacy form)
      (let ((before     (recovery-input-vault-tests--octets legacy))
            (controller (make-instance 'application-input-controller :application application :main-thread nil)))
        (application-input-controller--load-pending controller)
        (test-assert (null (application-input-controller--state controller :work-items))
                     "legacy input belonging to another conversation is never adopted")
        (test-assert (equalp before (recovery-input-vault-tests--octets legacy))
                     "failed legacy identity validation preserves the foreign bytes"))
      (let* ((capture  '(:id "native-only" :captured-at 1
                         :active-work (:native-lisp "only-active") :active-work-identifier "native-id"
                         :work-items nil :steering-promotion-prefix-count 0))
             (saved    (application-recovery-input-vault--capture-form capture))
             (restored (application-recovery-input-vault--capture-from-form saved legacy)))
        (test-assert (and (equal (getf restored :active-work) '(:native-lisp "only-active"))
                          (equal (getf restored :active-work-identifier) "native-id")
                          (null (getf restored :work-items)))
                     "native-active-only captures retain their identity without an ordinary payload"))
      (let* ((path (configuration-pending-inputs-path configuration (conversation-pathname conversation)))
             (bad  (copy-tree (test-pending-native--extended-form))))
        (setf (getf (rest bad) :native-work) nil)
        (snapshot-write path bad)
        (let ((before (recovery-input-vault-tests--octets path)))
          (test-assert (not (application-recovery-input-vault-import application))
                       "malformed native metadata fails actual recovery import")
          (test-assert (typep (application-recovery-input-vault-failure application)
                              'recovery-input-vault-error)
                       "failed import records a typed recovery failure")
          (test-assert (equalp before (recovery-input-vault-tests--octets path))
                       "failed recovery import preserves the original pending bytes")))
      (let ((capture '(:id "capture" :captured-at 1 :active-work nil
                       :steering-in-flight nil :steering nil :work ((:message "keep"))
                       :native-work nil)))
        (test-assert (null (application-recovery-input-vault--capture-from-form capture legacy))
                     "capture reconstruction preserves present NIL metadata for rejection"))))
  nil)
