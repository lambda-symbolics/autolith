(in-package #:autolith)

;;;; -- Explicit Vault Editing Tests --

(-> vault-edit-tests--call (function) null)
(defun vault-edit-tests--call (function)
  "Run FUNCTION with a durable conversation and a controller whose turn is active."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (ui (terminal-ui-create :terminal (make-instance 'recording-terminal :columns 80)))
         (conversation (conversation-create configuration :identifier "vault-edit"))
         (application (make-instance 'application :configuration configuration :conversation conversation :ui ui))
         (controller nil)
         (*active-application* application))
    (unwind-protect
         (progn
           (configuration-ensure-directories configuration)
           (conversation-append-user-message conversation "seed")
           (terminal-ui-start ui)
           (setf controller (application-input-controller-create application :load-pending-p nil :start-reader-p nil))
           (setf (application-input-controller-active-p controller) t)
           (funcall function application controller))
      (when controller (application-input-controller-stop controller))
      (terminal-ui-stop ui)
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)

(-> test-vault-store-and-edit () null)
(defun test-vault-store-and-edit ()
  "Store an active turn's queues, edit detached entries, and restore exact work kinds."
  (vault-edit-tests--call
   (lambda (application controller)
     (declare (ignore application))
     (application-input-controller--enqueue controller ':message "first")
     (application-input-controller--enqueue controller ':lisp "(+ 1 2)")
     (with-lock-held ((application-input-controller-lock controller))
       (deque-push-back (application-input-controller-steering-items controller) "steer"))
     (test-assert (= 3 (application-input-controller-vault-store controller)) "store captures all queued kinds")
     (test-assert (application-input-controller-active-p controller) "store does not end the active turn")
     (test-assert (null (application-input-controller--state controller :work-items)) "queued work is parked")
     (test-assert (null (application-input-controller--state controller :steering-items)) "unconsumed steering is parked")
     (test-assert (equal (vault-contents) '((:message "steer") (:message "first") (:lisp "(+ 1 2)"))) "restoration order is exposed as editable work")
     (test-assert (zerop (application-input-controller-vault-store controller)) "empty store preserves the vault")
     (setf (vault-contents 1) '(:message "edited"))
     (test-assert (equal (vault-contents 1) '(:message "edited")) "indexed SETF persists")
     (let ((copy (vault-contents)))
       (setf (second (first copy)) "not published")
       (test-assert (equal (vault-contents 0) '(:message "steer")) "returned lists are detached"))
     (dolist (bad '(((:bogus "x")) ((:message 42)) ((:command "x" :extra)) ("x")))
       (test-assert (handler-case (progn (setf (vault-contents) bad) nil)
                      (recovery-input-vault-error () t)) "invalid edits fail before publication"))
     (test-assert (handler-case (progn (setf (vault-contents 99) '(:message "x")) nil)
                    (recovery-input-vault-error () t)) "out-of-range edits are rejected")
     (application-input-controller--enqueue controller ':message "newer")
     (test-assert (= 3 (application-input-controller-vault-restore controller)) "edited vault restores without waiting for active turn")
     (test-assert (equal (application-input-controller--state controller :work-items)
                         '((:message "steer") (:message "edited") (:lisp "(+ 1 2)") (:message "newer")))
                  "restored edits precede newer input")
     (test-assert (null (vault-contents)) "restored work is no longer parked")
     (setf (vault-contents) '((:command "/info")))
     (test-assert (equal (vault-contents) '((:command "/info"))) "whole-vault SETF works")
     (setf (vault-contents) nil)
     (test-assert (null (vault-contents)) "whole-vault SETF can clear parked input")))
  nil)

(-> vault-edit-tests--publications (function function) t)
(defun vault-edit-tests--publications (checkpoint function)
  "Call CHECKPOINT after each successful pending or vault publication during FUNCTION."
  (let ((publish (symbol-function 'application-vault--publish))
        (write (symbol-function 'application-recovery-input-vault--write-captures)))
    (test-call-with-function-replacements
     (list (list 'application-vault--publish
                 (lambda (&rest arguments)
                   (multiple-value-prog1 (apply publish arguments) (funcall checkpoint))))
           (list 'application-recovery-input-vault--write-captures
                 (lambda (&rest arguments)
                   (multiple-value-prog1 (apply write arguments) (funcall checkpoint)))))
     function)))

(-> test-vault-store-publication-failures () null)
(defun test-vault-store-publication-failures ()
  "Fail after each publication and verify queue rollback and restart deduplication."
  (dolist (failure-point '(1 2 3))
    (vault-edit-tests--call
     (lambda (application controller)
       (application-input-controller--enqueue controller ':message "queued")
       (let ((calls 0))
         (vault-edit-tests--publications
          (lambda ()
            (when (= (incf calls) failure-point)
              (error "Injected publication failure.")))
          (lambda ()
            (test-assert (handler-case (progn (application-input-controller-vault-store controller) nil)
                           (recovery-input-vault-error () t)) "store reports publication failure"))))
       (test-assert (equal (application-input-controller--state controller :work-items) '((:message "queued")))
                    "failed store restores queued input")
       (test-assert (null (vault-contents)) "rollback removes the staged duplicate")
       (test-assert (application-recovery-input-vault-import application) "recovery reads the rolled-back pending state")
       (test-assert (= 1 (length (mapcan #'application-recovery-input-vault--capture-work
                                       (application-recovery-input-vault-captures application))))
                    "restart recovers the input once"))))
  nil)

(-> test-vault-store-crash-boundaries () null)
(defun test-vault-store-crash-boundaries ()
  "Model abrupt interruption after each durable publication, without executing rollback."
  (dolist (crash-point '(1 2 3))
    (vault-edit-tests--call
     (lambda (application controller)
       (application-input-controller--enqueue controller ':message "queued")
       (let ((calls 0))
         (vault-edit-tests--publications
          (lambda () (when (= (incf calls) crash-point) (throw 'crash nil)))
          (lambda () (catch 'crash (application-input-controller-vault-store controller)))))
       (test-assert (application-recovery-input-vault-import application) "interrupted store imports cleanly")
       (test-assert (equal (mapcan #'application-recovery-input-vault--capture-work
                                  (application-recovery-input-vault-captures application))
                           '((:message "queued"))) "each crash boundary recovers exactly one copy"))))
  nil)

(-> test-vault-store-active-routing () null)
(defun test-vault-store-active-routing ()
  "Execute actual terminal submissions without finishing the active turn."
  (vault-edit-tests--call
   (lambda (application controller)
     (application-operation-install-bindings application)
     (setf (application-input-controller-active-work controller) '(:message "running")
           (application-input-controller-active-work-identifier controller) (make-identifier))
     (deque-push-back (application-input-controller-steering-in-flight-items controller)
                      (make-instance 'agent-steering-input :identifier "in-flight" :content "in-flight"))
     (dolist (source '("(vault-store)" "/vault-store"))
       (application-input-controller--enqueue controller ':message "queued")
       (application-input-controller--handle-submission controller source)
       (test-assert (null (application-input-controller--state controller :work-items))
                    "store executes at submission, never queues behind the active turn")
       (test-assert (equal (vault-contents) '((:message "queued"))) "the queued task is parked immediately")
       (application-input-controller--handle-submission
        controller "(setf (vault-contents 0) (list :message (concatenate 'string \"new\" \" task\")))")
       (test-assert (equal (vault-contents 0) '(:message "new task")) "computed indexed SETF executes immediately")
       (application-input-controller--handle-submission controller "(setf (vault-contents) nil)"))
     (test-assert (equal (application-input-controller-active-work controller) '(:message "running"))
                  "active work is not moved or replaced")
     (test-assert (equal (mapcar #'agent-steering-input-content
                                (application-input-controller--state controller :steering-in-flight-items))
                         '("in-flight"))
                  "consumed steering continues with the active turn")
     (test-assert (application-input-controller-active-p controller) "the turn never needed to finish")
     (test-assert (null (application-input-controller--state controller :work-items)) "SETF did not enqueue Lisp")
     (setf (application-input-controller-pending-persistence-enabled-p controller) nil)
     (dolist (source '("(vault)" "(vault-store)" "(vault-restore)" "(vault-discard)" "/vault-store"
                       "(setf (vault-contents) nil)"))
       (test-assert (application-input-controller--submission-storage-ready-p controller source)
                    "vault controls are reachable after a storage failure"))))
  nil)


(-> test-vault-store-concurrent-admission () null)
(defun test-vault-store-concurrent-admission ()
  "Accept newer input during store publication, including a subsequent rollback."
  (dolist (fail-p '(nil t))
    (vault-edit-tests--call
     (lambda (application controller)
       (declare (ignore application))
       (application-input-controller--enqueue controller ':message "earlier")
       (let ((capture (symbol-function 'application-input-controller--capture-pending-publication-locked))
             (admitted (sb-thread:make-semaphore))
             (thread nil)
             (calls 0))
         (unwind-protect
              (test-call-with-function-replacements
               (list (list 'application-input-controller--capture-pending-publication-locked
                           (lambda (target generation sync-vault-p)
                             (let ((publication (funcall capture target generation sync-vault-p)))
                               (when (equal (getf (getf publication :state) :work-items)
                                            '((:message "later")))
                                 (sb-thread:signal-semaphore admitted))
                               publication))))
               (lambda ()
                 (vault-edit-tests--publications
                  (lambda ()
                    (case (incf calls)
                      (1
                       (setf thread (make-thread
                                     (lambda () (application-input-controller--enqueue controller ':message "later"))
                                     :name "Vault concurrent admission"))
                       (test-assert (sb-thread:wait-on-semaphore admitted :timeout 5)
                                    "new input acquires the controller lock during vault I/O"))
                      (2
                       (when fail-p (error "Injected concurrent store failure.")))))
                  (lambda ()
                    (handler-case (application-input-controller-vault-store controller)
                      (recovery-input-vault-error (condition)
                        (unless fail-p (error condition))))))))
           (when thread
             (test-assert (not (eq ':timeout (sb-thread:join-thread thread :timeout 5 :default ':timeout)))
                          "the blocked pending publisher completes after store")))
         (test-assert (equal (application-input-controller--state controller :work-items)
                             (if fail-p '((:message "earlier") (:message "later")) '((:message "later"))))
                      "newer admission survives commit or rollback in FIFO order")
         (test-assert (equal (vault-contents) (unless fail-p '((:message "earlier"))))
                      "only the drained input is parked")))))
  nil)

(-> test-vault-edit-write-failure () null)
(defun test-vault-edit-write-failure ()
  "A rejected durable edit preserves the previously published vault contents."
  (vault-edit-tests--call
   (lambda (application controller)
     (declare (ignore application controller))
     (setf (vault-contents) '((:message "original")))
     (test-call-with-function-replacements
      (list (list 'snapshot-write (lambda (&rest arguments)
                                   (declare (ignore arguments))
                                   (error "Injected edit write failure."))))
      (lambda ()
        (test-assert (handler-case (progn (setf (vault-contents 0) '(:message "replacement")) nil)
                       (recovery-input-vault-error () t)) "failed edits report a vault condition")))
     (test-assert (equal (vault-contents) '((:message "original"))) "failed edit preserves stored input")))
  nil)
