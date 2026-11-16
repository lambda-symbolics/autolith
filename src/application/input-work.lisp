(in-package #:autolith)

;;;; -- Durable Input and Work --

;; Work is a (KIND INPUT) list. Queue snapshots are normalized property lists;
;; their portable payload has no snapshot or conversation identity. Controllers
;; own locks and publication, while this model owns classification and codecs.

(defclass command-message-input (user-message-input)
  ((identifier
    :initarg       :identifier
    :initform      nil
    :accessor      command-message-input-identifier
    :type          (option string)
    :documentation "The pending-input identity retained through promotion and retry.")
   (invocation
    :initarg       :invocation
    :reader        command-message-input-invocation
    :type          application-command-invocation
    :documentation "The resolved invocation with its original native arguments.")
   (prepared-arguments
    :initarg       :prepared-arguments
    :initform      nil
    :accessor      command-message-input-prepared-arguments
    :type          list
    :documentation "The arguments prepared for execution at the steering boundary.")
   (phase
    :initarg       :phase
    :initform      ':pending
    :accessor      command-message-input-phase
    :type          (member :pending :prepared :committed :failed)
    :documentation "The durable preparation and commit phase of this command.")
   (notification
    :initarg       :notification
    :initform      nil
    :accessor      command-message-input-notification
    :type          (option string)
    :documentation "The notification prepared for publication after commit."))
  (:documentation "A command-bearing user submission in the durable steering lane."))

(-> input-work--copy-native-value (t) t)
(defun input-work--copy-native-value (value)
  "Copy bounded native literal trees, retaining pathname and string types."
  (portable-value-map
   value
   (lambda (item child)
     (declare (ignore child))
     (typecase item
       (string
        (copy-seq item))
       ((or pathname number character)
        item)
       (symbol
        (if (symbol-package item)
            item
            (error 'configuration-error :message "A command value requires an interned symbol.")))
       (t
        (error 'configuration-error :message "A command value must be readable native data."))))))

(-> command-message-input--copy-invocation
    (application-command-invocation) application-command-invocation)
(defun command-message-input--copy-invocation (invocation)
  "Detach INVOCATION's values while retaining its immutable command definition."
  (make-instance
   'application-command-invocation
   :input                   (copy-seq (application-command-invocation-input invocation))
   :syntax                  (application-command-invocation-syntax invocation)
   :name                    (copy-seq (application-command-invocation-name invocation))
   :remainder               (copy-seq (application-command-invocation-remainder invocation))
   :argument                (let ((argument (application-command-invocation-argument invocation)))
                              (when argument (copy-seq argument)))
   :arguments               (input-work--copy-native-value
                             (application-command-invocation-arguments invocation))
   :supplied-argument-count (application-command-invocation-supplied-argument-count invocation)
   :command                 (application-command-invocation-command invocation)))

(-> command-message-input-create
    ((or string user-message-input) application-command-invocation) command-message-input)
(defun command-message-input-create (input invocation)
  "Capture INPUT and the actual INVOCATION without reparsing its display text."
  (make-instance 'command-message-input
                 :text            (copy-seq (user-message-input-text input))
                 :image-pathnames (copy-list (user-message-input-image-pathnames input))
                 :submission      (input-work--copy-native-value (user-message-input-submission input))
                 :invocation      (command-message-input--copy-invocation invocation)))

(defmethod user-message-input-copy ((input command-message-input))
  "Detach INPUT's invocation, prepared arguments and mutable publication state."
  (let ((copy (command-message-input-create input (command-message-input-invocation input))))
    (setf (command-message-input-identifier copy)
          (when (command-message-input-identifier input)
            (copy-seq (command-message-input-identifier input))))
    (setf (command-message-input-prepared-arguments copy)
          (input-work--copy-native-value (command-message-input-prepared-arguments input))
          (command-message-input-phase copy) (command-message-input-phase input)
          (command-message-input-notification copy)
          (when (command-message-input-notification input)
            (copy-seq (command-message-input-notification input))))
    copy))

(-> input-work--invocation->form (application-command-invocation) list)
(defun input-work--invocation->form (invocation)
  "Encode every invocation field and its canonical registered command name."
  (let ((command (application-command-invocation-command invocation)))
    (unless command
      (error 'configuration-error :message "A durable command requires a resolved command definition."))
    (list :application-command-invocation :version 1
          :input                          (copy-seq (application-command-invocation-input invocation))
          :syntax                         (application-command-invocation-syntax invocation)
          :name                           (copy-seq (application-command-invocation-name invocation))
          :remainder                      (copy-seq (application-command-invocation-remainder invocation))
          :argument                       (let ((argument (application-command-invocation-argument invocation)))
                                            (when argument (copy-seq argument)))
          :arguments                      (input-work--copy-native-value
                                           (application-command-invocation-arguments invocation))
          :supplied-argument-count        (application-command-invocation-supplied-argument-count invocation)
          :command                        (copy-seq (application-command-name command)))))

(-> input-work--invocation-from-form (t) (option application-command-invocation))
(defun input-work--invocation-from-form (form)
  "Restore native invocation fields against the current canonical command registry."
  (when (and (record-check
              form :tag ':application-command-invocation :allow-other-keys nil
              :fields '((:indicator :version :required t) (:indicator :input :required t)
                        (:indicator :syntax :required t) (:indicator :name :required t)
                        (:indicator :remainder :required t) (:indicator :argument :required t)
                        (:indicator :arguments :required t)
                        (:indicator :supplied-argument-count :required t)
                        (:indicator :command :required t)))
             (eql (record-version form) 1))
    (let* ((properties (rest form))
           (name       (getf properties :command))
           (arguments  (getf properties :arguments))
           (count      (getf properties :supplied-argument-count))
           (command    (and (non-empty-string-p name) (application-command-find name))))
      (when (and command (string= name (application-command-name command))
                 (stringp (getf properties :input))
                 (member (getf properties :syntax) '(:slash :lisp))
                 (non-empty-string-p (getf properties :name))
                 (stringp (getf properties :remainder))
                 (typep (getf properties :argument) '(or null string))
                 (proper-list-p arguments) (typep count '(integer 0))
                 (= count (length arguments)))
        (make-instance
         'application-command-invocation
         :input                   (copy-seq (getf properties :input))
         :syntax                  (getf properties :syntax)
         :name                    (copy-seq (getf properties :name))
         :remainder               (copy-seq (getf properties :remainder))
         :argument                (let ((argument (getf properties :argument)))
                                    (when argument (copy-seq argument)))
         :arguments               (input-work--copy-native-value arguments)
         :supplied-argument-count count
         :command                 command)))))

(-> input-work--command->form (command-message-input) list)
(defun input-work--command->form (input)
  "Encode a command-bearing submission and its durable execution phase."
  (list :command-message-input :version 1
        :identifier            (when (command-message-input-identifier input)
                                 (copy-seq (command-message-input-identifier input)))
        :text                  (copy-seq (user-message-input-text input))
        :image-pathnames       (mapcar #'namestring (user-message-input-image-pathnames input))
        :submission            (input-work--copy-native-value (user-message-input-submission input))
        :invocation            (input-work--invocation->form (command-message-input-invocation input))
        :prepared-arguments    (input-work--copy-native-value
                                (command-message-input-prepared-arguments input))
        :phase                 (command-message-input-phase input)
        :notification          (when (command-message-input-notification input)
                                 (copy-seq (command-message-input-notification input)))))

(-> input-work--command-from-form (t) (option command-message-input))
(defun input-work--command-from-form (form)
  "Restore a validated command submission, rejecting malformed native values."
  (handler-case
      (when (and (record-check
                  form :tag ':command-message-input :allow-other-keys nil
                  :fields '((:indicator :version :required t) (:indicator :text :required t)
                            (:indicator :identifier :required t)
                            (:indicator :image-pathnames :required t)
                            (:indicator :submission :required t)
                            (:indicator :invocation :required t)
                            (:indicator :prepared-arguments :required t)
                            (:indicator :phase :required t) (:indicator :notification :required t)))
                 (eql (record-version form) 1))
        (let* ((properties (rest form))
               (invocation (input-work--invocation-from-form (getf properties :invocation)))
               (identifier (getf properties :identifier))
               (text       (getf properties :text))
               (images     (getf properties :image-pathnames))
               (prepared   (getf properties :prepared-arguments))
               (phase      (getf properties :phase))
               (notice     (getf properties :notification)))
          (when (and invocation (stringp text) (proper-list-p images) (every #'stringp images)
                     (or (null identifier) (non-empty-string-p identifier))
                     (proper-list-p prepared) (member phase '(:pending :prepared :committed :failed))
                     (typep notice '(or null string)))
            (let ((input (command-message-input-create
                          (user-message-input-create
                           :text text :image-pathnames (mapcar #'pathname images)
                           :submission (getf properties :submission))
                          invocation)))
              (setf (command-message-input-identifier input)
                    (when identifier (copy-seq identifier)))
              (setf (command-message-input-prepared-arguments input)
                    (input-work--copy-native-value prepared)
                    (command-message-input-phase input) phase
                    (command-message-input-notification input) (when notice (copy-seq notice)))
              input))))
    (error ()
      nil)))

(-> input-lisp-p (string) boolean)
(defun input-lisp-p (text)
  "Return true when TEXT begins with explicit Common Lisp input."
  (and (plusp (length text))
       (char= #\( (char text 0))))

(-> input-async-lisp-prefix-length (string) (option (integer 2)))
(defun input-async-lisp-prefix-length (text)
  "Return the length of TEXT's async Lisp prefix, or NIL when absent."
  (when (and (> (length text) 1)
             (char= #\? (char text 0))
             (find (char text 1) '(#\Space #\Tab #\Newline #\Return #\Page)))
    (or (position-if-not
         (lambda (character)
           (find character '(#\Space #\Tab #\Newline #\Return #\Page)))
         text :start 1)
        (length text))))

(-> input-async-lisp-p (string) boolean)
(defun input-async-lisp-p (text)
  "Return true when TEXT begins with a question-mark Lisp draft prefix."
  (not (null (input-async-lisp-prefix-length text))))

(-> input-message
    ((or string user-message-input))
    (option (or string user-message-input)))
(defun input-message (input)
  "Return INPUT's model message, or NIL for commands, empty, Lisp, or slash input."
  (let ((text (user-message-input-text input)))
    (cond
      ((typep input 'command-message-input)
       nil)
      ((and (not (non-empty-string-p text))
            (null (user-message-input-image-pathnames input)))
       nil)
      ((uiop:string-prefix-p "//" text)
       (etypecase input
         (string
          (subseq text 1))
         (user-message-input
          (user-message-input-create
           :text            (subseq text 1)
           :image-pathnames (user-message-input-image-pathnames input)
           :submission      (user-message-input-submission input)))))
      ((or (input-lisp-p text)
           (input-async-lisp-p text))
       nil)
      ((uiop:string-prefix-p "/" text)
       nil)
      (t
       (user-message-input-copy input)))))

(-> input-work-editable-p (t) boolean)
(defun input-work-editable-p (work)
  "Return true for editable queued messages, commands, or interactive or scheduled Lisp."
  (and (consp work)
       (member (first work) '(:message :command :lisp :native-lisp) :test #'eq)
       (consp (rest work))
       (typep (second work) '(or string user-message-input))))

(-> input-work-from-input
    ((or string user-message-input))
    (option list))
(defun input-work-from-input (input)
  "Return queued work for INPUT, or NIL when INPUT has no effective content."
  (let* ((message (input-message input))
         (text    (user-message-input-text input))
         (work    (cond
                    ((typep input 'command-message-input)
                     (list ':command (user-message-input-copy input)))
                    ((or (input-lisp-p text)
                         (input-async-lisp-p text))
                     (list ':lisp (copy-seq text)))
                    (message
                     (list ':message message))
                    ((non-empty-string-p text)
                     (list ':command (copy-seq text))))))
    (when work
      (append work (when (user-message-input-submission input)
                     (list :submission (user-message-input-submission input)))))))

(-> input-value->form
    ((or string user-message-input))
    (or string list))
(defun input-value->form (input)
  "Return a portable durable form for pending user INPUT."
  (etypecase input
    (string
     (copy-seq input))
    (command-message-input
     (input-work--command->form input))
    (user-message-input
     (list :user-message-input
           :version 1
           :text       (copy-seq (user-message-input-text input))
           :image-pathnames
           (mapcar #'namestring
                    (user-message-input-image-pathnames input))
           :submission (copy-tree (user-message-input-submission input))))))

(-> input-value-from-form
    (t)
    (option (or string user-message-input)))
(defun input-value-from-form (form)
  "Return validated pending input restored from durable FORM."
  (cond
    ((stringp form)
     (copy-seq form))
    ((and (consp form) (eq (first form) ':command-message-input))
     (input-work--command-from-form form))
    ((and (listp form)
          (eq (first form) ':user-message-input)
          (= (or (getf (rest form) :version) 0) 1))
     (let ((text        (getf (rest form) :text))
           (image-names (getf (rest form) :image-pathnames)))
       (when (and (stringp text)
                  (listp image-names)
                  (every #'stringp image-names))
         (handler-case
             (let ((image-pathnames (mapcar #'pathname image-names)))
               (when (every #'uiop:absolute-pathname-p image-pathnames)
                 (user-message-input-create
                  :text             (copy-seq text)
                   :image-pathnames image-pathnames
                   :submission      (getf (rest form) :submission))))
           (error ()
             nil)))))
    (t
     nil)))

(-> input-work--form-shape-p (t) boolean)
(defun input-work--form-shape-p (form)
  "Return true for a work form with optional validated submission provenance."
  (and (proper-list-p form)
       (member (length form) '(2 4))
       (not (null (member (first form) '(:message :command :lisp :native-lisp))))
       (or (= (length form) 2)
           (and (eq (third form) ':submission)
                (user-message-submission-p (fourth form))))))

(-> input-work->form
    (list)
    (option list))
(defun input-work->form (work)
  "Return a portable WORK form, retaining its submitting client and source."
  (when (input-work-editable-p work)
    (append (list (first work) (input-value->form (second work)))
            (when (getf (nthcdr 2 work) :submission)
              (list :submission (copy-tree (getf (nthcdr 2 work) :submission)))))))

(-> input-work-list->forms (list) list)
(defun input-work-list->forms (work-items)
  "Return durable forms for WORK-ITEMS that can be restored after restart."
  (remove nil
          (mapcar #'input-work->form
                  work-items)))

(-> input-work-from-form (t) (option list))
(defun input-work-from-form (form)
  "Return one validated in-memory work item restored from durable FORM."
  (when (input-work--form-shape-p form)
    (let ((input (input-value-from-form (second form))))
      (when (and input (or (not (typep input 'command-message-input))
                           (eq (first form) ':command)))
        (append (list (first form) input) (copy-tree (nthcdr 2 form)))))))

(-> input-steering->form
    (agent-steering-input)
    list)
(defun input-steering->form (entry)
  "Return the durable form for identified in-flight steering ENTRY."
  (append (list :identifier (copy-seq (agent-steering-input-identifier entry))
                :input      (input-value->form (agent-steering-input-content entry)))
          (when (agent-steering-input-operation-request entry)
            (list :operation-request
                  (input-work--copy-native-value (agent-steering-input-operation-request entry))))))

(-> input-steering-from-form
    (t)
    (option agent-steering-input))
(defun input-steering-from-form (form)
  "Return one validated identified steering entry restored from FORM."
  (handler-case
      (when (record-check form :properties-p t :allow-other-keys nil
                         :fields '((:indicator :identifier :required t)
                                   (:indicator :input :required t)
                                   (:indicator :operation-request)))
        (let ((identifier (getf form :identifier))
              (input      (input-value-from-form (getf form :input)))
              (request    (getf form :operation-request)))
          (when (and (non-empty-string-p identifier) input (proper-list-p request))
            (agent-steering-input-create
             :identifier        (copy-seq identifier)
             :content           input
             :operation-request (input-work--copy-native-value request)))))
    (error ()
      nil)))

(-> input-work--native-entry-p (t) boolean)
(defun input-work--native-entry-p (form)
  "Return true for a finite native work entry containing unevaluated source."
  (and (input-work--form-shape-p form)
       (eq (first form) ':native-lisp)
       (stringp (second form))))

(-> input-state--restore-native (list) (option list))
(defun input-state--restore-native (payload)
  "Validate additive native metadata and decode its merged historical v2 view."
  (block restore
    (unless (record-check payload :properties-p t :keyword-keys-p t)
      (return-from restore nil))
    (let* ((properties payload)
           (native     (getf properties :native-work))
           (work       (getf properties :work))
           (active     (getf properties :active-work)))
      (unless
          (and
           (record-check native :properties-p t :allow-other-keys nil :maximum-length 6
                         :fields
                         '((:indicator :active-work :required t)
                           (:indicator :items :required t)
                           (:indicator :steering-promotion-prefix-count :required t)))
           (proper-list-p work)
           (every
            (lambda (entry)
              (and (input-work--form-shape-p entry)
                   (member (first entry) '(:message :command :lisp))))
            work)
           (or (null active)
               (and
                (record-check active :properties-p t :allow-other-keys nil
                              :maximum-length 4 :fields
                              '((:indicator :identifier :required t)
                                (:indicator :work :required t)))
                (input-work--form-shape-p (getf active :work))
                (member (first (getf active :work)) '(:message :command :lisp)))))
        (return-from restore nil))
      (let* ((native-active (getf native :active-work))
             (items         (getf native :items))
             (prefix        (getf native :steering-promotion-prefix-count))
             (compat-prefix (getf properties :steering-promotion-prefix-count 0)))
        (unless
            (and (proper-list-p items) (or native-active items)
                 (not (and native-active active))
                 (or (null native-active)
                     (and
                      (record-check native-active :properties-p t :allow-other-keys nil
                                    :maximum-length 4 :fields
                                    '((:indicator :identifier :required t)
                                      (:indicator :work :required t)))
                      (non-empty-string-p (getf native-active :identifier))
                      (input-work--native-entry-p (getf native-active :work))))
                 (typep prefix '(integer 0)) (typep compat-prefix '(integer 0)))
          (return-from restore nil))
        (let ((total (+ (length work) (length items))) (previous -1) (native-prefix 0))
          (unless (<= prefix total) (return-from restore nil))
          (dolist (item items)
            (unless
                (and
                 (record-check item :properties-p t :allow-other-keys nil :maximum-length
                               4 :fields
                               '((:indicator :position :required t)
                                 (:indicator :work :required t)))
                 (typep (getf item :position) '(integer 0))
                 (< previous (getf item :position) total)
                 (input-work--native-entry-p (getf item :work)))
              (return-from restore nil))
            (setf previous (getf item :position))
            (when (< previous prefix) (incf native-prefix)))
          (unless (= compat-prefix (- prefix native-prefix)) (return-from restore nil))
          (let ((merged
                 (loop for position below total
                       collect (if (and items
                                        (= position (getf (first items) :position)))
                                   (getf (pop items) :work)
                                   (pop work))))
                (inline (copy-list properties)))
            (remf inline :native-work)
            (setf (getf inline :active-work) (or native-active active)
                  (getf inline :work) merged
                  (getf inline :steering-promotion-prefix-count) prefix)
            (input-state-from-payload inline)))))))

(-> input-state-from-form
    (list string)
    (option list))
(defun input-state-from-form (form conversation-identifier)
  "Return normalized validated pending state from durable FORM."
  (unless (record-check form :tag ':pending-inputs :allow-duplicate-keys t)
    (return-from input-state-from-form nil))
  (when
      (and (record-property-present-p form ':native-work)
           (not (eql (record-version form) 2)))
    (return-from input-state-from-form nil))
  (when
      (and (listp form) (eq (first form) ':pending-inputs)
           (stringp (getf (rest form) :conversation-id))
           (string= (getf (rest form) :conversation-id) conversation-identifier))
    (let ((version (or (getf (rest form) :version) 0)))
      (case version
        (1
         (let* ((steering-forms (getf (rest form) :steering))
                (work-forms     (getf (rest form) :work))
                (steering
                 (and (listp steering-forms)
                      (mapcar #'input-value-from-form steering-forms)))
                (work
                 (and (listp work-forms) (mapcar #'input-work-from-form work-forms))))
           (when
               (and (listp steering-forms) (listp work-forms) (every #'identity steering)
                    (every #'identity work))
             (list :snapshot-identifier (make-identifier) :active-work nil
                   :active-work-identifier nil :steering-in-flight-items nil
                   :steering-items steering :work-items work :vault-capture-identifiers
                   nil :steering-promotion-prefix-count 0 :legacy-p t))))
        (2
         (let ((snapshot-identifier (getf (rest form) :snapshot-identifier)))
           (when (non-empty-string-p snapshot-identifier)
             (let ((state (input-state-from-payload (rest form))))
               (when state
                 (list* :snapshot-identifier (copy-seq snapshot-identifier) state))))))
        (otherwise
         nil)))))

(-> input-state-count (list) (integer 0))
(defun input-state-count (state)
  "Return the number of accepted user inputs represented by pending STATE."
  (+ (if (getf state :active-work) 1 0)
     (length (getf state :steering-in-flight-items))
     (length (getf state :steering-items))
     (length (getf state :work-items))))

(-> input-state-filter-persisted
    (list conversation)
    list)
(defun input-state-filter-persisted
    (state conversation)
  "Return STATE after removing input already durable in CONVERSATION."
  (let* ((persisted-identifiers
           (conversation-pending-input-identifiers conversation))
         (active-work            (getf state :active-work))
         (active-work-identifier (getf state :active-work-identifier))
         (active-survives-p
           (and active-work
                (or (typep (second active-work) 'command-message-input)
                    (not (member active-work-identifier persisted-identifiers :test #'string=)))))
         (in-flight
           (remove-if
            (lambda (entry)
              (and (not (typep (agent-steering-input-content entry) 'command-message-input))
                   (member (agent-steering-input-identifier entry)
                           persisted-identifiers :test #'string=)))
            (getf state :steering-in-flight-items)))
         (steering               (getf state :steering-items))
         (work                   (getf state :work-items))
         (steering-promotion-prefix-count
           (min (getf state :steering-promotion-prefix-count)
                (length work)))
         (promoted-work
           (mapcar (lambda (input)
                     (if (typep input 'command-message-input)
                         (input-work-from-input input)
                         (list ':message input)))
                   (append (mapcar #'agent-steering-input-content in-flight) steering))))
    (list :snapshot-identifier      (getf state :snapshot-identifier)
          :active-work              (and active-survives-p active-work)
          :active-work-identifier
          (and active-survives-p active-work-identifier)
          :steering-in-flight-items (and active-survives-p in-flight)
          :steering-items           (and active-survives-p steering)
          :work-items
          (if active-survives-p
              work
              (append (subseq work 0 steering-promotion-prefix-count)
                      promoted-work
                      (nthcdr steering-promotion-prefix-count work)))
          :vault-capture-identifiers
          (getf state :vault-capture-identifiers)
          :steering-promotion-prefix-count
          (if active-survives-p steering-promotion-prefix-count 0)
          :legacy-p                 (getf state :legacy-p))))

(-> input-state->payload (list) list)
(defun input-state->payload (state)
  "Return STATE's portable input payload without snapshot or conversation identity."
  ;; Keep this split until snapshots shared with pre-0.67.9 images are retired.
  (let* ((active-work            (getf state :active-work))
         (active-work-identifier (getf state :active-work-identifier))
         (active-form
           (and active-work active-work-identifier
                (list :identifier active-work-identifier
                      :work       (input-work->form active-work))))
         (work-items             (getf state :work-items))
         (work                   (input-work-list->forms work-items))
         (prefix
           (count-if #'input-work-editable-p
                     (subseq work-items 0
                             (or (getf state :steering-promotion-prefix-count) 0))))
         (native-active          (and (eq (first active-work) ':native-lisp) active-form))
         (native-items
           (loop for entry in work for position from 0
                 when (eq (first entry) ':native-lisp)
                   collect (list :position position :work entry)))
         (form
            (list :active-work (unless native-active active-form)
                 :steering-in-flight
                 (mapcar #'input-steering->form
                         (getf state :steering-in-flight-items))
                 :steering     (mapcar #'input-value->form (getf state :steering-items))
                 :work         (remove ':native-lisp work :key #'first)
                 :vault-capture-identifiers
                 (mapcar #'copy-seq (getf state :vault-capture-identifiers))
                 :steering-promotion-prefix-count
                 (count-if (lambda (entry) (not (eq (first entry) ':native-lisp)))
                           (subseq work 0 prefix)))))
    (if (or native-active native-items)
        (append form
                (list :native-work
                      (list :active-work native-active :items native-items
                            :steering-promotion-prefix-count prefix)))
        form)))

(-> input-state->form (list string) list)
(defun input-state->form (state conversation-identifier)
  "Return the version-two durable pending-input envelope for STATE."
  (append (list :pending-inputs      :version 2
                :snapshot-identifier (getf state :snapshot-identifier)
                :conversation-id     conversation-identifier)
          (input-state->payload state)))

(-> input-state-from-payload (list) (option list))
(defun input-state-from-payload (payload)
  "Return normalized state from a portable input payload, without envelope identities."
  (unless (record-check payload :properties-p t :allow-duplicate-keys t)
    (return-from input-state-from-payload nil))
  (when (record-property-present-p (cons :payload payload) ':native-work)
    (return-from input-state-from-payload (input-state--restore-native payload)))
  (let* ((active-form               (getf payload :active-work))
         (active-work               (and active-form (input-work-from-form (getf active-form :work))))
         (active-work-identifier    (and active-form (getf active-form :identifier)))
         (in-flight-forms           (getf payload :steering-in-flight))
         (in-flight
          (and (listp in-flight-forms)
               (mapcar #'input-steering-from-form in-flight-forms)))
         (steering-forms            (getf payload :steering))
         (steering
          (and (listp steering-forms) (mapcar #'input-value-from-form steering-forms)))
         (work-forms                (getf payload :work))
         (work                      (and (listp work-forms) (mapcar #'input-work-from-form work-forms)))
         (vault-capture-identifiers (or (getf payload :vault-capture-identifiers) nil))
         (steering-promotion-prefix-count
          (or (getf payload :steering-promotion-prefix-count) 0)))
    (when
        (and
         (or (null active-form)
             (and (non-empty-string-p active-work-identifier) active-work))
         (listp in-flight-forms) (every #'identity in-flight) (listp steering-forms)
         (every #'identity steering) (listp work-forms) (every #'identity work)
         (listp vault-capture-identifiers)
         (every #'non-empty-string-p vault-capture-identifiers)
         (= (length vault-capture-identifiers)
            (length (remove-duplicates vault-capture-identifiers :test #'string=)))
         (typep steering-promotion-prefix-count '(integer 0))
         (<= steering-promotion-prefix-count (length work)))
      (list :active-work active-work :active-work-identifier
            (and active-work-identifier (copy-seq active-work-identifier))
            :steering-in-flight-items in-flight :steering-items steering :work-items work
            :vault-capture-identifiers (mapcar #'copy-seq vault-capture-identifiers)
            :steering-promotion-prefix-count steering-promotion-prefix-count :legacy-p
            nil))))

(-> input-state-form-shape-p (t) boolean)
(defun input-state-form-shape-p (form)
  "Return true when FORM has one supported pending-input record shape."
  (values
    (record-check
     form :tag ':pending-inputs :versions '(1 2) :allow-other-keys nil
     :keyword-keys-p t :maximum-length 20
     :fields
     (case (record-version form)
       (1
        '((:indicator :conversation-id :required t)
          (:indicator :steering :required t)
          (:indicator :work :required t)))
       (2
        '((:indicator :snapshot-identifier :required t)
          (:indicator :conversation-id :required t)
          (:indicator :active-work :required t)
          (:indicator :steering-in-flight :required t)
          (:indicator :steering :required t)
          (:indicator :work :required t)
          (:indicator :vault-capture-identifiers)
          (:indicator :steering-promotion-prefix-count)
          (:indicator :native-work)))))))
