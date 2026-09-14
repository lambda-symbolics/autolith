(in-package #:autolith)

;;;; -- Explicit Queue Storage and Editable Vault Contents --

(-> application-vault--represented-identifiers (application) list)
(defun application-vault--represented-identifiers (application)
  "Return capture identities already represented by the live controller."
  (let ((controller (application-input-controller application)))
    (when controller
      (with-lock-held ((application-input-controller-lock controller))
        (remove nil (cons (application-input-controller-pending-snapshot-identifier controller)
                          (copy-list (application-input-controller-vault-capture-identifiers controller))))))))

(-> application-vault--parked-captures (list list) list)
(defun application-vault--parked-captures (captures represented)
  "Exclude CAPTURES already represented by running or queued input."
  (remove-if (lambda (capture) (member (getf capture :id) represented :test #'equal)) captures))

(-> application-vault--publication (application-input-controller &optional (option string)) list)
(defun application-vault--publication (controller &optional represented)
  "Snapshot pending input, optionally marking a staged vault capture as represented."
  (let* ((publication
           (with-lock-held ((application-input-controller-lock controller))
             (application-input-controller--capture-pending-publication-locked
              controller (application-input-controller--next-publication-generation-locked controller) nil)))
         (state (getf publication :state)))
    (unless publication
      (application-recovery-input-vault--signal
       (application-recovery-input-vault--pending-path (application-input-controller-application controller))
       ':store :message "Vault storage needs writable pending-input persistence."))
    (when (and represented state)
      (pushnew represented (getf state :vault-capture-identifiers) :test #'equal)
      (setf (getf publication :form)
            (application-input-controller--pending-state-form
             state (conversation-identifier
                    (application-conversation (application-input-controller-application controller))))))
    publication))

(-> application-vault--publish (application-input-controller list) boolean)
(defun application-vault--publish (controller publication)
  "Publish one pending snapshot inside the caller's publication boundary."
  (application-input-controller--publish-pending-publication-locked controller publication t))

(-> application-input-controller-vault-store (application-input-controller) (integer 0))
(defun application-input-controller-vault-store (controller)
  "Move queued work and unconsumed steering into the vault immediately.

Active work and steering already in flight continue. A staged pending snapshot
names the new capture before vault publication, so crash import removes either
copy before reconstructing input. No filesystem I/O holds the controller lock."
  (let* ((application (application-input-controller-application controller))
         (identifier (make-identifier))
         (count 0))
    (with-lock-held ((application-input-controller-publication-lock controller))
      (let* ((captures (application-recovery-input-vault-captures application))
             (remaining (application-vault--parked-captures
                         captures (application-vault--represented-identifiers application)))
             (publication nil)
             (queued nil))
        (with-lock-held ((application-input-controller-lock controller))
          (unless (application-input-controller-pending-persistence-enabled-p controller)
            (application-recovery-input-vault--signal
             (application-recovery-input-vault--pending-path application) ':store
             :message "Resolve the existing vault storage failure before storing queued input."))
          (unless (every #'application-input-controller--follow-up-work-p
                         (deque->list (application-input-controller-work-items controller)))
            (application-recovery-input-vault--signal
             (application-recovery-input-vault--pending-path application) ':store
             :message "The queue contains internal work that cannot be stored in the input vault."))
          (setf publication
                (application-input-controller--capture-pending-publication-locked
                 controller (application-input-controller--next-publication-generation-locked controller) nil))
          (unless publication
            (application-recovery-input-vault--signal
             (application-recovery-input-vault--pending-path application) ':store
             :message "Vault storage needs a durable conversation."))
          (let ((state (getf publication :state)))
            (setf queued (copy-list state)
                  (getf queued :active-work) nil
                  (getf queued :active-work-identifier) nil
                  (getf queued :steering-in-flight-items) nil
                  (getf queued :snapshot-identifier) identifier
                  (getf queued :vault-capture-identifiers) nil
                  count (+ (length (getf queued :work-items)) (length (getf queued :steering-items))))
            (when (plusp count)
              (pushnew identifier (getf state :vault-capture-identifiers) :test #'equal)
              (setf (getf publication :form)
                    (application-input-controller--pending-state-form
                     state (conversation-identifier (application-conversation application)))
                    (application-input-controller-follow-up-edit-index controller) nil
                    (application-input-controller-follow-up-edit-work controller) nil
                    (application-input-controller-steering-promotion-prefix-count controller) 0)
              (deque-clear (application-input-controller-work-items controller))
              (deque-clear (application-input-controller-steering-items controller)))))
        (when (plusp count)
          (handler-case
              (progn
                (application-vault--publish controller publication)
                (application-recovery-input-vault--write-captures
                 application (append remaining
                                     (list (application-recovery-input-vault--capture-state
                                            queued (get-universal-time)))))
                (application-vault--publish controller (application-vault--publication controller)))
            (error (condition)
              ;; Restore the drained prefix ahead of submissions accepted meanwhile.
              (with-lock-held ((application-input-controller-lock controller))
                (let* ((work (getf queued :work-items))
                       (old-prefix (or (getf queued :steering-promotion-prefix-count) 0))
                       (new-prefix (application-input-controller-steering-promotion-prefix-count controller))
                       (edit-index (application-input-controller-follow-up-edit-index controller)))
                  ;; Keep both promoted prefixes ahead of ordinary queued input.
                  (loop for item in (nthcdr old-prefix work)
                        for index from new-prefix
                        do (deque-insert (application-input-controller-work-items controller) index item))
                  (deque-prepend (application-input-controller-work-items controller)
                                 (subseq work 0 old-prefix))
                  (deque-prepend (application-input-controller-steering-items controller)
                                 (getf queued :steering-items))
                  (setf (application-input-controller-steering-promotion-prefix-count controller)
                        (+ old-prefix new-prefix))
                  (when edit-index
                    (incf (application-input-controller-follow-up-edit-index controller)
                          (if (< edit-index new-prefix) old-prefix (length work))))))
              (handler-case
                  (progn
                    (application-vault--publish controller (application-vault--publication controller identifier))
                    (application-recovery-input-vault--write-captures application remaining)
                    (application-vault--publish controller (application-vault--publication controller)))
                (error (rollback-error)
                  (with-lock-held ((application-input-controller-lock controller))
                    (setf (application-input-controller-pending-persistence-enabled-p controller) nil))
                  (setf (application-recovery-input-vault-failure application) rollback-error)))
              (application-recovery-input-vault--signal
               (application-recovery-input-vault--path application) ':store
               :message "Could not store queued input; the queue was restored. Inspect the vault if storage remains unavailable."
               :cause condition))))))
    (application-input-controller--publish-counts controller)
    count))

(-> application-vault--active-application () application)
(defun application-vault--active-application ()
  "Resolve the local prompt's application for ordinary generalized-place access."
  (unless (and (boundp '*active-application*) *active-application*
               (application-recovery-input-vault--context-p *active-application*))
    (error 'configuration-error :message "Vault access needs an active conversation."))
  *active-application*)

(-> application-vault--work (application) list)
(defun application-vault--work (application)
  "Read detached parked work in the same order used by vault restore."
  (mapcan #'application-recovery-input-vault--capture-work
          (application-vault--parked-captures
           (application-recovery-input-vault-captures application)
           (application-vault--represented-identifiers application))))

(-> vault-contents (&optional t) list)
(defun vault-contents (&optional (index nil index-p))
  "Return parked (:MESSAGE INPUT), (:COMMAND TEXT), or (:LISP SOURCE) entries.

With INDEX return one zero-based entry. Values are detached snapshots. Use
SETF of this place to publish a replacement, rather than mutating a returned list."
  (let ((application (application-vault--active-application)))
    (application-recovery-input-vault--call-with-publication-lock
     application
     (lambda ()
       (let ((work (application-vault--work application)))
         (if index-p
             (progn
               (application-vault--check-index application index work)
               (nth index work))
             work))))))

(-> application-vault--check-index (application t list) null)
(defun application-vault--check-index (application index work)
  "Reject an index outside the existing parked entries."
  (unless (and (typep index '(integer 0)) (< index (length work)))
    (application-recovery-input-vault--signal
     (application-recovery-input-vault--path application) ':edit
     :message "Vault index must identify an existing entry, starting at zero."))
  nil)

(-> application-vault--validate-work (application t) list)
(defun application-vault--validate-work (application value)
  "Validate and detach a replacement list using the existing pending-input codec."
  (let ((pathname (application-recovery-input-vault--path application)))
    (labels ((invalid ()
               (application-recovery-input-vault--signal
                pathname ':edit :message "Vault entries must be (:message INPUT), (:command TEXT), or (:lisp SOURCE).")))
      (unless (application-recovery-input-vault--proper-list-p value) (invalid))
      (mapcar (lambda (entry)
                (unless (and (application-recovery-input-vault--proper-list-p entry)
                             (= (length entry) 2)
                             (member (first entry) '(:message :command :lisp))
                             (typep (second entry) '(or string user-message-input))
                             (or (eq (first entry) ':message) (stringp (second entry))))
                  (invalid))
                (or (application-input-controller--restore-work-item
                     (application-input-controller--pending-work-entry-form entry))
                    (invalid)))
              value))))

(-> (setf vault-contents) (t &optional t) t)
(defun (setf vault-contents) (value &optional (index nil index-p))
  "Atomically replace parked entries, or just INDEX, and return VALUE.

The live queue, active work and already consumed steering are not edited."
  (let ((application (application-vault--active-application)))
    (application-recovery-input-vault--call-with-publication-lock
     application
     (lambda ()
       (let* ((captures (application-recovery-input-vault-captures application))
              (represented (application-vault--represented-identifiers application))
              (live (remove-if-not (lambda (capture) (member (getf capture :id) represented :test #'equal)) captures))
              (work (if index-p (application-vault--work application) value)))
         (when index-p
           (application-vault--check-index application index work)
           (setf (nth index work) value))
         (setf work (application-vault--validate-work application work))
         (application-recovery-input-vault--write-captures
          application
          (append live
                  (when work
                    (list (list :id (make-identifier) :captured-at (get-universal-time)
                                :work-items work :steering-promotion-prefix-count 0)))))))))
  value)

(define-application-command application--builtin-vault-store-command
    (:name "/vault-store"
     :argument nil
     :description "move queued work and unconsumed steering into the vault immediately"
     :tip "parks queued input immediately, even during an active turn; edit it with (setf (vault-contents 0) '(:message \"new task\"))."
     :busy-behavior :execute
     :terminal-behavior :shared
     :callable t)
    (application)
  (let ((controller (application-input-controller application)))
    (unless (typep controller 'application-input-controller)
      (error 'configuration-error :message "Vault storage needs the interactive application."))
    (let ((count (application-input-controller-vault-store controller)))
      (application-present application (format nil "Stored ~D queued input~:P in the vault." count))))
  ':continue)
