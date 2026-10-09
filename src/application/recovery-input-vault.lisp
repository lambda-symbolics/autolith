(in-package #:autolith)

;;;; -- Recovery Input Vault Conditions --

(define-condition recovery-input-vault-error (autolith-error)
  ((pathname
    :initarg :pathname
    :reader recovery-input-vault-error-pathname
    :type pathname
    :documentation "The pending-input or vault pathname involved in the failure.")
   (operation
    :initarg :operation
    :reader recovery-input-vault-error-operation
    :type keyword
    :documentation "The read, validation, publication, or deletion operation that failed.")
   (cause
    :initarg :cause
    :initform nil
    :reader recovery-input-vault-error-cause
    :type t
    :documentation "The underlying condition, when one was available."))
  (:documentation "A failure preserving or reading recovered interactive input."))

(-> application-recovery-input-vault--signal
    (pathname keyword &key (:message string) (:cause t))
    nil)
(defun application-recovery-input-vault--signal
    (pathname operation &key message cause)
  "Signal a structured recovery-input failure for PATHNAME and OPERATION."
  (error 'recovery-input-vault-error
         :message message
         :pathname pathname
         :operation operation
         :cause cause))


;;;; -- Strict Durable Forms --

(-> application-recovery-input-vault--read-form (pathname keyword) t)
(defun application-recovery-input-vault--read-form (pathname operation)
  "Read one complete snapshot form from PATHNAME or signal a vault error."
  (handler-case
      (multiple-value-bind (form complete-p)
          (snapshot-read pathname)
        (unless complete-p
          (application-recovery-input-vault--signal
           pathname operation
           :message
           (format nil "Recovered input state at ~A is truncated."
                   (namestring pathname))))
        form)
    (recovery-input-vault-error (condition)
      (error condition))
    (error (condition)
      (application-recovery-input-vault--signal
       pathname operation
       :message
       (format nil "Could not read recovered input state at ~A."
               (namestring pathname))
       :cause condition))))

(-> application-recovery-input-vault--pending-form-shape-p (t) boolean)
(defun application-recovery-input-vault--pending-form-shape-p (form)
  "Return true when FORM has one supported pending-input record shape."
  (values
    (record-check
     form :tag ':pending-inputs :versions '(1 2) :allow-other-keys nil
     :keyword-keys-p t :maximum-length 18
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
          (:indicator :steering-promotion-prefix-count)))))))

(-> application-recovery-input-vault--form-shape-p (t) boolean)
(defun application-recovery-input-vault--form-shape-p (form)
  "Return true when FORM has the supported recovery input vault schema."
  (values
    (record-check
     form :tag ':recovery-input-vault :versions '(1) :allow-other-keys nil
     :keyword-keys-p t :maximum-length 6
     :fields (list (list :indicator ':conversation-id :required t :validate #'stringp)
                   (list :indicator ':captures :required t :validate #'proper-list-p)))))

(-> application-recovery-input-vault--pending-payload-form (list) list)
(defun application-recovery-input-vault--pending-payload-form (state)
  "Return STATE's input payload without migration or snapshot identity."
  (let ((form
          (application-input-controller--pending-state-form
           (list :snapshot-identifier "snapshot"
                 :active-work (getf state :active-work)
                 :active-work-identifier (getf state :active-work-identifier)
                 :steering-in-flight-items
                 (getf state :steering-in-flight-items)
                 :steering-items (getf state :steering-items)
                 :work-items (getf state :work-items)
                 :vault-capture-identifiers
                 (getf state :vault-capture-identifiers)
                 :steering-promotion-prefix-count
                 (or (getf state :steering-promotion-prefix-count) 0)
                 :legacy-p nil)
           "conversation")))
    (list :active-work (getf (rest form) :active-work)
          :steering-in-flight (getf (rest form) :steering-in-flight)
          :steering (getf (rest form) :steering)
          :work (getf (rest form) :work)
          :vault-capture-identifiers
          (getf (rest form) :vault-capture-identifiers)
          :steering-promotion-prefix-count
          (getf (rest form) :steering-promotion-prefix-count))))

(-> application-recovery-input-vault--pending-payload-equal-p (list list) boolean)
(defun application-recovery-input-vault--pending-payload-equal-p (left right)
  "Return true when LEFT and RIGHT represent the same accepted input payload."
  (not
   (null
    (equal (application-recovery-input-vault--pending-payload-form left)
           (application-recovery-input-vault--pending-payload-form right)))))

(-> application-recovery-input-vault--read-pending-path
    (application pathname &key (:other-conversation-p boolean))
    (values (option list) keyword))
(defun application-recovery-input-vault--read-pending-path
    (application pathname &key (other-conversation-p nil))
  "Read and normalize PATHNAME for APPLICATION.

The second value is :CURRENT or :OTHER.  :OTHER is returned only when
OTHER-CONVERSATION-P permits a valid legacy record for another conversation."
  (let* ((form
           (application-recovery-input-vault--read-form pathname ':read-pending))
         (conversation (application-conversation application))
         (conversation-identifier (conversation-identifier conversation)))
    (unless (application-recovery-input-vault--pending-form-shape-p form)
      (application-recovery-input-vault--signal
       pathname ':validate-pending
       :message
       (format nil "Recovered pending input at ~A has an unsupported form."
               (namestring pathname))))
    (let ((recorded-identifier (getf (rest form) :conversation-id)))
      (unless (non-empty-string-p recorded-identifier)
        (application-recovery-input-vault--signal
         pathname ':validate-pending
         :message
         (format nil "Recovered pending input at ~A has no conversation identity."
                 (namestring pathname))))
      (unless (string= recorded-identifier conversation-identifier)
        (if other-conversation-p
            (return-from application-recovery-input-vault--read-pending-path
              (values nil ':other))
            (application-recovery-input-vault--signal
             pathname ':validate-pending
             :message
             (format nil "Recovered pending input at ~A belongs to conversation ~A, not ~A."
                     (namestring pathname)
                     recorded-identifier
                     conversation-identifier))))
      (handler-case
          (let ((state
                  (application-input-controller--pending-state
                   form conversation-identifier)))
            (unless state
              (application-recovery-input-vault--signal
               pathname ':validate-pending
               :message
               (format nil "Recovered pending input at ~A is invalid."
                       (namestring pathname))))
            (values
             (application-input-controller--pending-state-filter-persisted
              state conversation)
             ':current))
        (recovery-input-vault-error (condition)
          (error condition))
        (error (condition)
          (application-recovery-input-vault--signal
           pathname ':validate-pending
           :message
           (format nil "Recovered pending input at ~A is invalid."
                   (namestring pathname))
           :cause condition))))))

(-> application-recovery-input-vault--read-pending
    (application)
    (values (option list) (option pathname) (option pathname)))
(defun application-recovery-input-vault--read-pending (application)
  "Return APPLICATION's normalized pending state, source, and legacy cleanup path."
  (let* ((configuration (application-configuration application))
         (conversation (application-conversation application))
         (canonical
           (configuration-pending-inputs-path
            configuration (conversation-pathname conversation)))
         (legacy (configuration-legacy-pending-inputs-path configuration)))
    (cond
      ((probe-file canonical)
       (multiple-value-bind (canonical-state status)
           (application-recovery-input-vault--read-pending-path
            application canonical)
         (declare (ignore status))
         (let ((legacy-cleanup-pathname nil))
           (when (probe-file legacy)
             (let ((legacy-state nil)
                   (legacy-status nil)
                   (legacy-readable-p nil))
               (handler-case
                   (multiple-value-bind (state state-status)
                       (application-recovery-input-vault--read-pending-path
                        application legacy :other-conversation-p t)
                     (setf legacy-state state
                           legacy-status state-status
                           legacy-readable-p t))
                 (recovery-input-vault-error ()
                   ;; A canonical record is authoritative.  An unreadable global
                   ;; legacy record cannot be safely attributed or deleted here.
                   nil))
               (when (and legacy-readable-p (eq legacy-status ':current))
                 (unless
                     (application-recovery-input-vault--pending-payload-equal-p
                      canonical-state legacy-state)
                   (application-recovery-input-vault--signal
                    legacy ':validate-pending
                    :message
                    (format nil "Legacy pending input at ~A conflicts with the conversation-scoped snapshot."
                            (namestring legacy))))
                 (setf legacy-cleanup-pathname legacy))))
           (values canonical-state canonical legacy-cleanup-pathname))))
      ((probe-file legacy)
       (handler-case
           (multiple-value-bind (legacy-state status)
               (application-recovery-input-vault--read-pending-path
                application legacy :other-conversation-p t)
             (if (eq status ':current)
                 (values legacy-state legacy legacy)
                 (values nil nil nil)))
         (recovery-input-vault-error ()
           ;; An unreadable process-global legacy snapshot cannot be safely
           ;; attributed to this conversation.  Preserve and ignore it.
           (values nil nil nil))))
      (t
       (values nil nil nil)))))


;;;; -- Vault Captures --

(-> application-recovery-input-vault--path (application) pathname)
(defun application-recovery-input-vault--path (application)
  "Return APPLICATION's exact conversation-scoped recovery vault pathname."
  (let ((configuration (application-configuration application))
        (conversation (application-conversation application)))
    (configuration-recovery-input-vault-path
     configuration (conversation-pathname conversation))))

(-> application-recovery-input-vault--capture-from-form
    (t string pathname)
    (option list))
(defun application-recovery-input-vault--capture-from-form
    (form conversation-identifier pathname)
  "Return one validated normalized vault capture from FORM."
  (when (record-check
         form :properties-p t :allow-other-keys nil
         :fields '((:indicator :id :required t)
                   (:indicator :captured-at :required t)
                   (:indicator :active-work :required t)
                   (:indicator :steering-in-flight :required t)
                   (:indicator :steering :required t)
                   (:indicator :work :required t)
                   (:indicator :steering-promotion-prefix-count)))
    (let ((identifier (getf form :id))
          (captured-at (getf form :captured-at)))
      (when (and (non-empty-string-p identifier)
                 (typep captured-at '(integer 0)))
        (handler-case
            (let ((state
                    (application-input-controller--pending-state
                     (list :pending-inputs
                           :version 2
                           :snapshot-identifier identifier
                           :conversation-id conversation-identifier
                           :active-work (getf form :active-work)
                           :steering-in-flight (getf form :steering-in-flight)
                           :steering (getf form :steering)
                           :work (getf form :work)
                           :vault-capture-identifiers nil
                           :steering-promotion-prefix-count
                           (or (getf form :steering-promotion-prefix-count) 0))
                     conversation-identifier)))
              (when (and state
                         (plusp
                          (application-input-controller--pending-state-input-count
                           state)))
                (list :id (copy-seq identifier)
                      :captured-at captured-at
                      :active-work (getf state :active-work)
                      :active-work-identifier
                      (getf state :active-work-identifier)
                      :steering-in-flight-items
                      (getf state :steering-in-flight-items)
                      :steering-items (getf state :steering-items)
                      :work-items (getf state :work-items)
                      :vault-capture-identifiers nil
                      :steering-promotion-prefix-count
                      (getf state :steering-promotion-prefix-count)
                      :legacy-p nil)))
          (error (condition)
            (application-recovery-input-vault--signal
             pathname ':validate-vault
             :message
             (format nil "Recovery vault capture ~A at ~A is invalid."
                     identifier (namestring pathname))
             :cause condition)))))))

(-> application-recovery-input-vault--capture-form (list string) list)
(defun application-recovery-input-vault--capture-form
    (capture conversation-identifier)
  "Return the portable durable form for normalized CAPTURE."
  (let* ((identifier (getf capture :id))
         (pending-form
           (application-input-controller--pending-state-form
            (list :snapshot-identifier identifier
                  :active-work (getf capture :active-work)
                  :active-work-identifier
                  (getf capture :active-work-identifier)
                  :steering-in-flight-items
                  (getf capture :steering-in-flight-items)
                  :steering-items (getf capture :steering-items)
                  :work-items (getf capture :work-items)
                  :vault-capture-identifiers nil
                  :steering-promotion-prefix-count
                  (or (getf capture :steering-promotion-prefix-count) 0)
                  :legacy-p nil)
            conversation-identifier)))
    (list :id (copy-seq identifier)
          :captured-at (getf capture :captured-at)
          :active-work (getf (rest pending-form) :active-work)
          :steering-in-flight (getf (rest pending-form) :steering-in-flight)
          :steering (getf (rest pending-form) :steering)
          :work (getf (rest pending-form) :work)
          :steering-promotion-prefix-count
          (getf (rest pending-form) :steering-promotion-prefix-count))))

(-> application-recovery-input-vault--capture-payload-equal-p (list list) boolean)
(defun application-recovery-input-vault--capture-payload-equal-p (left right)
  "Return true when LEFT and RIGHT carry the same captured pending input."
  (not
   (null
    (equal
     (application-recovery-input-vault--pending-payload-form left)
     (application-recovery-input-vault--pending-payload-form right)))))

(-> application-recovery-input-vault--sort-captures (list) list)
(defun application-recovery-input-vault--sort-captures (captures)
  "Return a stable chronological copy of CAPTURES."
  (stable-sort (copy-list captures)
               #'<
               :key (lambda (capture)
                      (getf capture :captured-at))))

(-> application-recovery-input-vault--read-captures (application) list)
(defun application-recovery-input-vault--read-captures (application)
  "Strictly read and validate APPLICATION's conversation-scoped vault captures."
  (let ((pathname (application-recovery-input-vault--path application)))
    (if (not (probe-file pathname))
        nil
        (let* ((form
                 (application-recovery-input-vault--read-form pathname ':read-vault))
               (properties (and (consp form) (rest form)))
               (conversation-identifier
                 (conversation-identifier
                  (application-conversation application))))
          (unless
              (and (application-recovery-input-vault--form-shape-p form)
                   (string= (getf properties :conversation-id)
                            conversation-identifier))
            (application-recovery-input-vault--signal
             pathname ':validate-vault
             :message
             (format nil "Recovery input vault at ~A has an unsupported form."
                     (namestring pathname))))
          (let* ((capture-forms (getf properties :captures))
                 (captures
                   (mapcar
                    (lambda (capture-form)
                      (or
                       (application-recovery-input-vault--capture-from-form
                        capture-form conversation-identifier pathname)
                       (application-recovery-input-vault--signal
                        pathname ':validate-vault
                        :message
                        (format nil "Recovery input vault at ~A contains an invalid capture."
                                (namestring pathname)))))
                    capture-forms))
                 (identifiers (mapcar (lambda (capture)
                                        (getf capture :id))
                                      captures)))
            (unless (= (length identifiers)
                       (length (remove-duplicates identifiers :test #'string=)))
              (application-recovery-input-vault--signal
               pathname ':validate-vault
               :message
               (format nil "Recovery input vault at ~A contains duplicate capture identifiers."
                       (namestring pathname))))
            (application-recovery-input-vault--sort-captures captures))))))

(-> application-recovery-input-vault-captures (application) list)
(defun application-recovery-input-vault-captures (application)
  "Return APPLICATION's validated recovery input captures chronologically."
  (application-recovery-input-vault--read-captures application))

(-> application-recovery-input-vault--form (application list) list)
(defun application-recovery-input-vault--form (application captures)
  "Return the complete version-one vault form for CAPTURES."
  (let ((conversation-identifier
          (conversation-identifier (application-conversation application))))
    (list :recovery-input-vault
          :version 1
          :conversation-id conversation-identifier
          :captures
          (mapcar (lambda (capture)
                    (application-recovery-input-vault--capture-form
                     capture conversation-identifier))
                  (application-recovery-input-vault--sort-captures captures)))))

(-> application-recovery-input-vault--write-captures (application list) null)
(defun application-recovery-input-vault--write-captures (application captures)
  "Atomically replace APPLICATION's vault with CAPTURES, deleting an empty vault."
  (let ((pathname (application-recovery-input-vault--path application)))
    (handler-case
        (if captures
            (progn
              (ensure-directories-exist pathname)
              (snapshot-write
               pathname
               (application-recovery-input-vault--form application captures)
               :mode #o600))
            (when (probe-file pathname)
              (delete-file pathname)))
      (error (condition)
        (application-recovery-input-vault--signal
         pathname ':write-vault
         :message
         (format nil "Could not publish the recovery input vault at ~A."
                 (namestring pathname))
         :cause condition))))
    nil)


(-> application-recovery-input-vault--call-with-publication-lock
    (application function)
    t)
(defun application-recovery-input-vault--call-with-publication-lock
    (application function)
  "Call FUNCTION inside APPLICATION's controller publication boundary.

Callers must not hold the controller lock. Publication precedes controller
locking everywhere that needs both locks."
  (let ((controller
          (and (slot-boundp application 'input-controller)
               (application-input-controller application))))
    (if (typep controller 'application-input-controller)
        (with-lock-held
            ((application-input-controller-publication-lock controller))
          (funcall function))
        (funcall function))))

(-> application-recovery-input-vault--replace-capture-locked
    (application string list)
    null)
(defun application-recovery-input-vault--replace-capture-locked
    (application identifier capture)
  "Replace or remove APPLICATION's vault capture IDENTIFIER.

The caller must hold the active controller's publication lock when one exists."
  (let ((remaining
          (remove identifier
                  (application-recovery-input-vault--read-captures application)
                  :key (lambda (existing)
                         (getf existing :id))
                  :test #'string=)))
    (application-recovery-input-vault--write-captures
     application
     (if capture
         (append remaining (list capture))
         remaining)))
  nil)

(-> application-recovery-input-vault--replace-capture (application string list) null)
(defun application-recovery-input-vault--replace-capture
    (application identifier capture)
  "Replace or remove APPLICATION's vault capture IDENTIFIER atomically."
  (application-recovery-input-vault--call-with-publication-lock
   application
   (lambda ()
     (application-recovery-input-vault--replace-capture-locked
      application identifier capture))))

(-> application-recovery-input-vault--sync-pending-locked
    (application list &key (:create-p boolean))
    null)
(defun application-recovery-input-vault--sync-pending-locked
    (application state &key (create-p t))
  "Mirror pending STATE inside APPLICATION's controller publication boundary."
  (let ((identifier (getf state :snapshot-identifier)))
    (when (non-empty-string-p identifier)
      (let ((existing
              (find identifier
                    (application-recovery-input-vault--read-captures application)
                    :key (lambda (capture)
                           (getf capture :id))
                    :test #'string=)))
        (when (or create-p existing)
          (application-recovery-input-vault--replace-capture-locked
           application
           identifier
           (unless (zerop
                    (application-input-controller--pending-state-input-count state))
             (application-recovery-input-vault--capture-state
              state
              (or (getf existing :captured-at) (get-universal-time)))))))))
  nil)

(-> application-recovery-input-vault-sync-pending
    (application list &key (:create-p boolean))
    null)
(defun application-recovery-input-vault-sync-pending
    (application state &key (create-p t))
  "Mirror live pending STATE into APPLICATION's vault, or remove that capture.

When CREATE-P is false, only an existing capture for the snapshot is updated."
  (application-recovery-input-vault--call-with-publication-lock
   application
   (lambda ()
     (application-recovery-input-vault--sync-pending-locked
      application state :create-p create-p))))

(-> application-recovery-input-vault-capture-message
    (application (or string user-message-input) &key (:identifier string))
    string)
(defun application-recovery-input-vault-capture-message
    (application input &key identifier)
  "Persist INPUT as one vault capture and return its identifier."
  (let* ((capture-identifier
           (or identifier (make-identifier)))
         (state
           (list :snapshot-identifier capture-identifier
                 :active-work nil
                 :active-work-identifier nil
                 :steering-in-flight-items nil
                 :steering-items nil
                 :work-items (list (list ':message (user-message-input-copy input)))
                 :vault-capture-identifiers nil
                 :steering-promotion-prefix-count 0
                 :legacy-p nil)))
    (application-recovery-input-vault--replace-capture
     application
     capture-identifier
     (application-recovery-input-vault--capture-state
      state (get-universal-time)))
    capture-identifier))


;;;; -- Recovery Import Transaction --

(-> application-recovery-input-vault--capture-state (list integer) list)
(defun application-recovery-input-vault--capture-state (state captured-at)
  "Return a vault capture for normalized pending STATE at CAPTURED-AT."
  (list :id (copy-seq (getf state :snapshot-identifier))
        :captured-at captured-at
        :active-work (getf state :active-work)
        :active-work-identifier (getf state :active-work-identifier)
        :steering-in-flight-items (getf state :steering-in-flight-items)
        :steering-items (getf state :steering-items)
        :work-items (getf state :work-items)
        :vault-capture-identifiers nil
        :steering-promotion-prefix-count
        (getf state :steering-promotion-prefix-count)
        :legacy-p nil))

(-> application-recovery-input-vault--replacement-captures
    (application list list)
    list)
(defun application-recovery-input-vault--replacement-captures
    (application captures pending-state)
  "Return the vault state after transactionally importing PENDING-STATE."
  (let* ((vault-pathname (application-recovery-input-vault--path application))
         (represented-identifiers
           (getf pending-state :vault-capture-identifiers))
         (remaining
           (remove-if
            (lambda (capture)
              (member (getf capture :id)
                      represented-identifiers
                      :test #'string=))
            captures)))
    (if (zerop
         (application-input-controller--pending-state-input-count pending-state))
        remaining
        (let* ((identifier (getf pending-state :snapshot-identifier))
               (existing
                 (find identifier remaining
                       :key (lambda (capture) (getf capture :id))
                       :test #'string=))
               (capture
                 (application-recovery-input-vault--capture-state
                  pending-state (get-universal-time))))
          (cond
            ((null existing)
             (append remaining (list capture)))
            ((application-recovery-input-vault--capture-payload-equal-p
              existing capture)
             remaining)
            (t
             (application-recovery-input-vault--signal
              vault-pathname ':validate-vault
              :message
              (format nil "Recovery vault capture ~A conflicts with the pending snapshot at ~A."
                      identifier (namestring vault-pathname)))))))))

(-> application-recovery-input-vault--write-canonical-pending
    (application list pathname)
    null)
(defun application-recovery-input-vault--write-canonical-pending
    (application state pathname)
  "Publish normalized pending STATE at canonical PATHNAME."
  (handler-case
      (progn
        (ensure-directories-exist pathname)
        (snapshot-write
         pathname
         (application-input-controller--pending-state-form
          state
          (conversation-identifier (application-conversation application)))
         :mode #o600))
    (error (condition)
      (application-recovery-input-vault--signal
       pathname ':write-pending
       :message
       (format nil "Could not normalize recovered pending input at ~A."
               (namestring pathname))
       :cause condition)))
  nil)

(-> application-recovery-input-vault--delete (pathname keyword) null)
(defun application-recovery-input-vault--delete (pathname operation)
  "Delete PATHNAME for OPERATION or signal a recovery-input error."
  (handler-case
      (when (probe-file pathname)
        (delete-file pathname))
    (error (condition)
      (application-recovery-input-vault--signal
       pathname operation
       :message
       (format nil "Could not delete recovered input state at ~A."
               (namestring pathname))
       :cause condition)))
  nil)

(-> application-recovery-input-vault--import-locked (application) null)
(defun application-recovery-input-vault--import-locked (application)
  "Import APPLICATION's pending snapshot inside its publication boundary."
  (multiple-value-bind (pending-state source-pathname legacy-cleanup-pathname)
      (application-recovery-input-vault--read-pending application)
    ;; Both durable inputs are fully validated before the first mutation.
    (let ((captures (application-recovery-input-vault--read-captures application)))
      (when pending-state
        (let* ((configuration (application-configuration application))
               (conversation (application-conversation application))
               (canonical-pathname
                 (configuration-pending-inputs-path
                  configuration (conversation-pathname conversation)))
               (replacement-captures
                 (application-recovery-input-vault--replacement-captures
                  application captures pending-state)))
          (when (or (getf pending-state :legacy-p)
                    (not (equal source-pathname canonical-pathname)))
            (application-recovery-input-vault--write-canonical-pending
             application pending-state canonical-pathname))
          (application-recovery-input-vault--write-captures
           application replacement-captures)
          (when legacy-cleanup-pathname
            (application-recovery-input-vault--delete
             legacy-cleanup-pathname ':delete-legacy-pending))
          (application-recovery-input-vault--delete
           canonical-pathname ':delete-pending)))))
  nil)

(-> application-recovery-input-vault--import (application) null)
(defun application-recovery-input-vault--import (application)
  "Import APPLICATION's pending snapshot into its vault without enqueueing it."
  (application-recovery-input-vault--call-with-publication-lock
   application
   (lambda ()
     (application-recovery-input-vault--import-locked application))))

(-> application-recovery-input-vault-import (application) boolean)
(defun application-recovery-input-vault-import (application)
  "Vault recovered pending input for APPLICATION and record any safe failure."
  (handler-case
      (progn
        (application-recovery-input-vault--import application)
        (setf (application-recovery-input-vault-failure application) nil)
        t)
    (recovery-input-vault-error (condition)
      (setf (application-recovery-input-vault-failure application) condition)
      nil)
    (error (cause)
      (let* ((pathname (application-recovery-input-vault--path application))
             (condition
               (make-condition
                'recovery-input-vault-error
                :message
                (format nil "Could not preserve recovered input for conversation ~A."
                        (conversation-identifier
                         (application-conversation application)))
                :pathname pathname
                :operation ':import
                :cause cause)))
        (setf (application-recovery-input-vault-failure application) condition)
        nil))))


;;;; -- Controller Restore Transactions --

(-> application-recovery-input-vault--copy-work (list) list)
(defun application-recovery-input-vault--copy-work (work)
  "Return a detached copy of one restorable WORK item."
  (list (first work) (user-message-input-copy (second work))))

(-> application-recovery-input-vault--capture-work (list) list)
(defun application-recovery-input-vault--capture-work (capture)
  "Return CAPTURE's input as ordered controller work."
  (let* ((work-items (getf capture :work-items))
         (prefix-count
           (min (or (getf capture :steering-promotion-prefix-count) 0)
                (length work-items))))
    (append
     (when (getf capture :active-work)
       (list
        (application-recovery-input-vault--copy-work
         (getf capture :active-work))))
     (mapcar #'application-recovery-input-vault--copy-work
             (subseq work-items 0 prefix-count))
     (mapcar
      (lambda (entry)
        (list ':message
              (user-message-input-copy
               (agent-steering-input-content entry))))
      (getf capture :steering-in-flight-items))
     (mapcar (lambda (input)
              (list ':message (user-message-input-copy input)))
             (getf capture :steering-items))
     (mapcar #'application-recovery-input-vault--copy-work
             (nthcdr prefix-count work-items)))))


(-> application-recovery-input-vault--restore-controller-state
    (application-input-controller list)
    null)
(defun application-recovery-input-vault--restore-controller-state
    (controller state)
  "Restore CONTROLLER from transaction snapshot STATE.

The caller must hold CONTROLLER's lock."
  (let ((work-items (application-input-controller-work-items controller))
        (steering-items (application-input-controller-steering-items controller))
        (steering-in-flight
          (application-input-controller-steering-in-flight-items controller)))
    (mapc #'deque-clear (list work-items steering-items steering-in-flight))
    (deque-append work-items (getf state :work-items))
    (deque-append steering-items (getf state :steering-items))
    (deque-append steering-in-flight (getf state :steering-in-flight-items)))
  (setf (application-input-controller-follow-up-edit-index controller)
        (getf state :follow-up-edit-index)
        (application-input-controller-follow-up-edit-work controller)
        (getf state :follow-up-edit-work)
        (application-input-controller-pending-snapshot-identifier controller)
        (getf state :snapshot-identifier)
        (application-input-controller-vault-capture-identifiers controller)
        (getf state :vault-capture-identifiers)
        (application-input-controller-steering-promotion-prefix-count controller)
        (getf state :steering-promotion-prefix-count))
  nil)

(-> application-recovery-input-vault--pending-path (application) pathname)
(defun application-recovery-input-vault--pending-path (application)
  "Return APPLICATION's exact conversation-scoped pending-input pathname."
  (let ((configuration (application-configuration application))
        (conversation (application-conversation application)))
    (configuration-pending-inputs-path
     configuration (conversation-pathname conversation))))

(-> application-input-controller-vault-restore
    (application-input-controller)
    (integer 0))
(defun application-input-controller-vault-restore (controller)
  "Transactionally restore every vaulted capture before newer controller work."
  (let* ((application
           (application-input-controller-application controller))
         (vault-pathname
           (application-recovery-input-vault--path application))
         (pending-pathname
           (application-recovery-input-vault--pending-path application))
         (restored-count 0))
    (with-lock-held ((application-input-controller-publication-lock controller))
      (let* ((captures
               (application-recovery-input-vault-captures application))
             (represented-identifiers
               (with-lock-held ((application-input-controller-lock controller))
                 (remove nil
                         (cons
                          (application-input-controller-pending-snapshot-identifier
                           controller)
                          (application-input-controller-vault-capture-identifiers
                           controller)))))
             (restore-captures
               (remove-if
                (lambda (capture)
                  (member (getf capture :id)
                          represented-identifiers
                          :test #'equal))
                captures))
             (capture-identifiers
               (mapcar (lambda (capture) (getf capture :id)) restore-captures))
             (restored-work
               (mapcan #'application-recovery-input-vault--capture-work
                       restore-captures)))
        (setf restored-count (length restored-work))
        (unless (zerop restored-count)
          (unless
              (application-input-controller-pending-persistence-enabled-p controller)
            (application-recovery-input-vault--signal
             pending-pathname ':restore
             :message
             "Recovered input storage is unavailable. Inspect (vault) or use (vault-discard) before restoring input."))
          (let ((committed-p nil))
            (handler-case
                (progn
                  (let ((publication
                          (with-lock-held
                              ((application-input-controller-lock controller))
                            (setf
                             (application-input-controller-vault-restore-work-items
                              controller)
                             restored-work
                             (application-input-controller-vault-restore-capture-identifiers
                              controller)
                             capture-identifiers)
                            (application-input-controller--capture-pending-publication-locked
                             controller
                             (application-input-controller--next-publication-generation-locked
                              controller)
                             nil))))
                    (application-input-controller--publish-pending-publication-locked
                     controller publication t))
                  (with-lock-held ((application-input-controller-lock controller))
                    (deque-prepend
                     (application-input-controller-work-items controller)
                     restored-work)
                    (setf
                     (application-input-controller-follow-up-edit-index controller)
                     (let ((index
                             (application-input-controller-follow-up-edit-index
                              controller)))
                       (and index (+ restored-count index)))
                     (application-input-controller-steering-promotion-prefix-count
                      controller)
                     (+ restored-count
                        (application-input-controller-steering-promotion-prefix-count
                         controller))
                     (application-input-controller-vault-capture-identifiers controller)
                     (remove-duplicates
                      (append
                       (application-input-controller-vault-capture-identifiers
                        controller)
                       capture-identifiers)
                      :test #'string=)
                     (application-input-controller-vault-restore-work-items controller)
                     nil
                     (application-input-controller-vault-restore-capture-identifiers
                      controller)
                     nil
                     committed-p t))
                  (application-recovery-input-vault--write-captures application nil)
                  (let ((publication
                          (with-lock-held
                              ((application-input-controller-lock controller))
                            (setf
                             (application-input-controller-vault-capture-identifiers
                              controller)
                             (remove-if
                              (lambda (identifier)
                                (member identifier capture-identifiers :test #'string=))
                              (application-input-controller-vault-capture-identifiers
                               controller)))
                            (application-input-controller--capture-pending-publication-locked
                             controller
                             (application-input-controller--next-publication-generation-locked
                              controller)
                             nil))))
                    (application-input-controller--publish-pending-publication-locked
                     controller publication t)))
              (error (condition)
                (unless committed-p
                  (let ((rollback-publication
                          (with-lock-held
                              ((application-input-controller-lock controller))
                            (setf
                             (application-input-controller-vault-restore-work-items
                              controller)
                             nil
                             (application-input-controller-vault-restore-capture-identifiers
                              controller)
                             nil)
                            (application-input-controller--capture-pending-publication-locked
                             controller
                             (application-input-controller--next-publication-generation-locked
                              controller)
                             nil))))
                    (handler-case
                        (application-input-controller--publish-pending-publication-locked
                         controller rollback-publication t)
                      (error (rollback-condition)
                        (with-lock-held
                            ((application-input-controller-lock controller))
                          (setf
                           (application-input-controller-pending-persistence-enabled-p
                            controller)
                           nil))
                        (let ((rollback-error
                                (make-condition
                                 'recovery-input-vault-error
                                 :message
                                 "Vault restore and rollback failed. Use (vault) to inspect the saved input or (vault-discard) to discard it."
                                 :pathname pending-pathname
                                 :operation ':restore-rollback
                                 :cause (list condition rollback-condition))))
                          (setf (application-recovery-input-vault-failure application)
                                rollback-error)
                          (error rollback-error))))))
                (if (typep condition 'recovery-input-vault-error)
                    (error condition)
                    (application-recovery-input-vault--signal
                     pending-pathname ':restore
                     :message
                     (if committed-p
                         "Could not finish publishing restored recovery input. Its provenance-bearing pending snapshot was preserved."
                         "Could not publish restored recovery input. The staged restore was discarded without replacing concurrent controller state.")
                     :cause condition))))))))
    (unless (zerop restored-count)
      (with-lock-held ((application-input-controller-lock controller))
        (setf (application-input-controller-live-vault-sync-p controller) nil))
      (setf (application-recovery-input-vault-failure application) nil)
      (application-input-controller--publish-counts controller))
    restored-count))


;;;; -- Vault Inspection and Discard --

(defparameter *application-recovery-input-vault-preview-width* 96
  "Maximum display width of one recovered-input preview.")

(-> application-recovery-input-vault--capture-labeled-inputs (list) list)
(defun application-recovery-input-vault--capture-labeled-inputs (capture)
  "Return CAPTURE's ordered inputs paired with concise labels."
  (let* ((work-items (getf capture :work-items))
         (prefix-count
           (min (or (getf capture :steering-promotion-prefix-count) 0)
                (length work-items))))
    (labels ((labeled-work (items)
               (mapcar (lambda (work)
                         (cons (if (eq (first work) ':command)
                                   "command"
                                   "follow-up")
                               (second work)))
                       items)))
      (append
       (when (getf capture :active-work)
         (list (cons "active" (second (getf capture :active-work)))))
       (labeled-work (subseq work-items 0 prefix-count))
       (mapcar (lambda (entry)
                 (cons "steering in flight"
                       (agent-steering-input-content entry)))
               (getf capture :steering-in-flight-items))
       (mapcar (lambda (input) (cons "steering" input))
               (getf capture :steering-items))
       (labeled-work (nthcdr prefix-count work-items))))))

(-> application-recovery-input-vault--preview
    ((or string user-message-input))
    string)
(defun application-recovery-input-vault--preview (input)
  "Return one sanitized bounded preview for recovered INPUT."
  (let* ((text
           (sanitize-text (user-message-input-text input)
                          :single-line-p t))
         (visible
           (text-cell-prefix
            text *application-recovery-input-vault-preview-width*)))
    (if (plusp (length visible)) visible "(empty input)")))

(-> application-recovery-input-vault--input-count (list) (integer 0))
(defun application-recovery-input-vault--input-count (captures)
  "Count the saved messages across CAPTURES, including follow-ups and steering."
  (reduce #'+ captures
          :key (lambda (capture)
                 (length
                  (application-recovery-input-vault--capture-labeled-inputs capture)))
          :initial-value 0))

(-> application-recovery-input-vault-description (application list) string)
(defun application-recovery-input-vault-description (application captures)
  "Return a bounded chronological description of validated CAPTURES."
  (with-output-to-string (stream)
    (let ((input-count (application-recovery-input-vault--input-count captures)))
      (format stream
              "Recovery input vault for ~A: ~D capture~:P, ~D input~:P.~%"
              (conversation-identifier
               (application-conversation application))
              (length captures)
              input-count))
    (loop for capture in captures
          for index from 1
          for labeled-inputs =
            (application-recovery-input-vault--capture-labeled-inputs capture)
          do (format stream "~%~D. ~A, ~D input~:P~%"
                     index
                     (application--calendar-description
                      (getf capture :captured-at))
                     (length labeled-inputs))
             (dolist (entry labeled-inputs)
               (format stream "   ~A: ~A~%"
                       (first entry)
                       (application-recovery-input-vault--preview
                        (rest entry)))))
    (let ((failure (application-recovery-input-vault-failure application)))
      (when failure
        (format stream "~%Storage warning: ~A~%" failure)))))

(-> application-recovery-input-vault--context-p (application) boolean)
(defun application-recovery-input-vault--context-p (application)
  "Return true when APPLICATION has durable configuration and conversation state."
  (and (slot-boundp application 'configuration)
       (typep (application-configuration application) 'configuration)
       (slot-boundp application 'conversation)
       (typep (application-conversation application) 'conversation)))

(-> application-recovery-input-vault-present-startup-warning
    (application)
    boolean)
(defun application-recovery-input-vault-present-startup-warning (application)
  "Warn when APPLICATION has vaulted input or blocked recovery storage."
  (block nil
    (unless (application-recovery-input-vault--context-p application)
      (return nil))
    (let ((failure (application-recovery-input-vault-failure application))
          (captures nil))
      (unless failure
        (handler-case
            (setf captures
                  (application-recovery-input-vault-captures application))
          (recovery-input-vault-error (condition)
            (setf failure condition
                  (application-recovery-input-vault-failure application)
                  condition)
            (let ((controller (application-input-controller application)))
              (when (typep controller 'application-input-controller)
                (setf
                 (application-input-controller-pending-persistence-enabled-p
                  controller)
                 nil))))))
      (unless (or failure captures)
        (return nil))
      (application-present
       application
       (list
        (terminal-span
         (if failure ':failure ':notice)
         (if failure
             (format nil "Vault storage error: ~A~%New submissions are blocked.~%" failure)
             (format nil "~D follow-up/steer message~:P saved to vault.~%"
                     (application-recovery-input-vault--input-count captures))))
        (terminal-span ':dim "Use ")
        (terminal-span ':code "(vault)")
        (terminal-span ':dim " to inspect, ")
        (terminal-span ':code "(vault-restore)")
        (terminal-span ':dim " to restore, or ")
        (terminal-span ':code "(vault-discard)")
        (terminal-span ':dim " to discard.")))
      t)))

(-> application-recovery-input-vault-present (application) null)
(defun application-recovery-input-vault-present (application)
  "Present APPLICATION's vault or its exact validation failure."
  (handler-case
      (let ((captures
              (application-recovery-input-vault-captures application)))
        (application-present
         application
         (if captures
             (application-recovery-input-vault-description
              application captures)
             (let ((failure
                     (application-recovery-input-vault-failure application)))
               (if failure
                   (format nil
                           "No readable input in the vault. Storage error: ~A~%Use (vault-discard) to discard the saved input."
                           failure)
                   "No recovered input is vaulted for this conversation.")))))
    (recovery-input-vault-error (condition)
      (setf (application-recovery-input-vault-failure application) condition)
      (application-present
       application
       (format nil
               "Could not read vault: ~A~%Use (vault-discard) to discard this conversation's vault."
               condition))))
  nil)

(-> application-recovery-input-vault--discard-legacy (application) null)
(defun application-recovery-input-vault--discard-legacy (application)
  "Delete valid legacy pending input only when it belongs to APPLICATION."
  (let* ((configuration (application-configuration application))
         (legacy-pathname
           (configuration-legacy-pending-inputs-path configuration)))
    (when (probe-file legacy-pathname)
      (handler-case
          (multiple-value-bind (state status)
              (application-recovery-input-vault--read-pending-path
               application legacy-pathname :other-conversation-p t)
            (declare (ignore state))
            (when (eq status ':current)
              (application-recovery-input-vault--delete
               legacy-pathname ':discard-legacy-pending)))
        (recovery-input-vault-error ()
          ;; Explicit scoped discard cannot safely remove an unattributed
          ;; process-global legacy file.  Preserve and ignore it.
          nil))))
  nil)

(-> application-input-controller-vault-discard
    (application-input-controller)
    boolean)
(defun application-input-controller-vault-discard (controller)
  "Discard this conversation's vault and any blocked recovery pending state."
  (let* ((application
           (application-input-controller-application controller))
         (vault-pathname
           (application-recovery-input-vault--path application))
         (pending-pathname
           (application-recovery-input-vault--pending-path application))
         (blocked-p
           (with-lock-held ((application-input-controller-lock controller))
             (not
              (application-input-controller-pending-persistence-enabled-p
               controller)))))
    (application-recovery-input-vault--delete
     vault-pathname ':discard-vault)
    (when blocked-p
      (handler-case
          (progn
            (application-recovery-input-vault--delete
             pending-pathname ':discard-pending)
            (application-recovery-input-vault--discard-legacy application)
            (with-lock-held ((application-input-controller-lock controller))
              (setf
               (application-input-controller-pending-snapshot-identifier
                controller)
               nil
               (application-input-controller-vault-capture-identifiers
                controller)
               nil
               (application-input-controller-steering-promotion-prefix-count
                controller)
               0
               (application-input-controller-pending-persistence-enabled-p
                controller)
               t))
            (application-input-controller--persist-pending
             controller :error-p t :sync-vault-p nil))
        (error (condition)
          (with-lock-held ((application-input-controller-lock controller))
            (setf
             (application-input-controller-pending-persistence-enabled-p
              controller)
             nil))
          (if (typep condition 'recovery-input-vault-error)
              (error condition)
              (application-recovery-input-vault--signal
               pending-pathname ':discard
               :message
               "Could not discard all preserved recovery input. New submissions remain blocked."
               :cause condition)))))
    (setf (application-recovery-input-vault-failure application) nil)
    (application-input-controller--publish-counts controller)
    t))


;;;; -- Built-in Recovery Vault Commands --

(define-application-command application--builtin-vault-command
    (:name "/vault"
     :argument nil
     :description "inspect recovered queued input"
     :tip "shows input preserved after an active-image crash."
     :busy-behavior :inspect
     :terminal-behavior :shared
     :callable t)
    (application)
  (application-recovery-input-vault-present application)
  ':continue)

(define-application-command application--builtin-vault-restore-command
    (:name "/vault-restore"
     :argument nil
     :description "restore recovered queued input"
     :tip "queues all vaulted input in its original order without automatic submission."
     ;; Restoring only reorders the follow-up queue, so it must act while
     ;; the agent works instead of waiting behind the queue it manages.
     :busy-behavior :execute
     :terminal-behavior :shared
     :callable t)
    (application)
  (let ((controller (application-input-controller application)))
    (unless (typep controller 'application-input-controller)
      (error 'configuration-error
             :message "Recovery input restore needs the interactive application."))
    (let ((count
            (application-input-controller-vault-restore controller)))
      (application-present
       application
       (if (plusp count)
           (format nil "Restored ~D recovered input~:P before newer queued work."
                   count)
           "No recovered input was available to restore."))))
  ':continue)

(define-application-command application--builtin-vault-discard-command
    (:name "/vault-discard"
     :argument nil
     :description "discard recovered queued input"
     :tip "deletes only this conversation's recovery vault and blocked pending state."
     :busy-behavior :execute
     :terminal-behavior :shared
     :callable t)
    (application)
  (let ((controller (application-input-controller application)))
    (unless (typep controller 'application-input-controller)
      (error 'configuration-error
             :message "Recovery input discard needs the interactive application."))
    (application-input-controller-vault-discard controller)
    (application-present
     application
     "Discarded this conversation's recovered input."))
  ':continue)
