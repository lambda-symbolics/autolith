(in-package #:autolith)

;;;; -- Durable Input and Work --

;; Work is a (KIND INPUT) list. Queue snapshots are normalized property lists;
;; their portable payload has no snapshot or conversation identity. Controllers
;; own locks and publication, while this model owns classification and codecs.


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
  "Return INPUT's model message, or NIL for empty, Lisp, or slash input."
  (let ((text (user-message-input-text input)))
    (cond
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
           :image-pathnames (user-message-input-image-pathnames input)))))
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
  (let ((message (input-message input))
        (text    (user-message-input-text input)))
    (cond
      ((or (input-lisp-p text)
           (input-async-lisp-p text))
       (list ':lisp (copy-seq text)))
      (message
       (list ':message message))
      ((non-empty-string-p text)
       (list ':command (copy-seq text)))
      (t
       nil))))

(-> input-value->form
    ((or string user-message-input))
    (or string list))
(defun input-value->form (input)
  "Return a portable durable form for pending user INPUT."
  (etypecase input
    (string
     (copy-seq input))
    (user-message-input
     (list :user-message-input
           :version 1
           :text (copy-seq (user-message-input-text input))
           :image-pathnames
           (mapcar #'namestring
                   (user-message-input-image-pathnames input))))))

(-> input-value-from-form
    (t)
    (option (or string user-message-input)))
(defun input-value-from-form (form)
  "Return validated pending input restored from durable FORM."
  (cond
    ((stringp form)
     (copy-seq form))
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
                  :text            (copy-seq text)
                  :image-pathnames image-pathnames)))
           (error ()
             nil)))))
    (t
     nil)))

(-> input-work->form
    (list)
    (option list))
(defun input-work->form (work)
  "Return the durable form for one restorable WORK item."
  (when (input-work-editable-p work)
    (list (first work)
          (input-value->form (second work)))))

(-> input-work-list->forms (list) list)
(defun input-work-list->forms (work-items)
  "Return durable forms for WORK-ITEMS that can be restored after restart."
  (remove nil
          (mapcar #'input-work->form
                  work-items)))

(-> input-work-from-form (t) (option list))
(defun input-work-from-form (form)
  "Return one validated in-memory work item restored from durable FORM."
  (when (and (listp form)
             (member (first form) '(:message :command :lisp :native-lisp) :test #'eq))
    (let ((input (input-value-from-form (second form))))
      (when input
        (list (first form) input)))))

(-> input-steering->form
    (agent-steering-input)
    list)
(defun input-steering->form (entry)
  "Return the durable form for identified in-flight steering ENTRY."
  (list :identifier (copy-seq (agent-steering-input-identifier entry))
        :input      (input-value->form
                     (agent-steering-input-content entry))))

(-> input-steering-from-form
    (t)
    (option agent-steering-input))
(defun input-steering-from-form (form)
  "Return one validated identified steering entry restored from FORM."
  (when (listp form)
    (let ((identifier (getf form :identifier))
          (input      (input-value-from-form (getf form :input))))
      (when (and (non-empty-string-p identifier) input)
        (agent-steering-input-create
         :identifier (copy-seq identifier)
         :content    input)))))

(-> input-work--native-entry-p (t) boolean)
(defun input-work--native-entry-p (form)
  "Return true for a finite native work entry containing unevaluated source."
  (and (proper-list-p form)
       (= (length form) 2)
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
              (and (proper-list-p entry) (= (length entry) 2)
                   (member (first entry) '(:message :command :lisp))))
            work)
           (or (null active)
               (and
                (record-check active :properties-p t :allow-other-keys nil
                              :maximum-length 4 :fields
                              '((:indicator :identifier :required t)
                                (:indicator :work :required t)))
                (proper-list-p (getf active :work)) (= (length (getf active :work)) 2)
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
                (not
                 (member active-work-identifier
                         persisted-identifiers
                         :test #'string=))))
         (in-flight
           (remove-if
            (lambda (entry)
              (member (agent-steering-input-identifier entry)
                      persisted-identifiers
                      :test #'string=))
            (getf state :steering-in-flight-items)))
         (steering               (getf state :steering-items))
         (work                   (getf state :work-items))
         (steering-promotion-prefix-count
           (min (getf state :steering-promotion-prefix-count)
                (length work)))
         (promoted-work
           (append
            (mapcar (lambda (entry)
                      (list ':message (agent-steering-input-content entry)))
                    in-flight)
            (mapcar (lambda (input)
                      (list ':message input))
                    steering))))
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
