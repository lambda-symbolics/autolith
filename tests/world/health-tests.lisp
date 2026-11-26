(in-package #:autolith)

(-> test-world-host-health () null)
(defun test-world-host-health ()
  "Use the same availability for channel status, named opens and retained ownership."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry      (world-client-entry configuration))
            (workspace  (world-tests--workspace root "health"))
            (identifier (getf (rest (world-server-tests--open entry workspace)) :session-id))
            (host       (world-server--find-host server identifier))
            (owner      (world-host-thread host))
            (dead       (make-thread (constantly nil) :name "Finished health fixture")))
       (join-thread dead)
       (test-assert (world-host-live-p host) "a published endpoint has a serving owner")
       (unwind-protect
            (dolist (kind '(:dead-owner :endpoint-failure :controller-failure :closing :checkpoint))
              (setf (world-host-thread host) (if (eq kind ':dead-owner) dead owner)
                    (world-host-state host) (if (eq kind ':closing) ':closing ':ready)
                    (world-host-exit-reason host) (when (eq kind ':checkpoint) ':checkpoint))
              (test-call-with-function-replacements
               (append
                (when (eq kind ':endpoint-failure)
                  (list (list 'channel-endpoint-endpoint-live-p (constantly nil))))
                (when (eq kind ':controller-failure)
                  (list (list 'application-input-controller-failure
                              (constantly (make-condition 'simple-error))))))
               (lambda ()
                 (let* ((health (world-host-health host))
                        (status (find identifier (world-server-channels server)
                                      :key  (lambda (packet) (getf (rest packet) :session-id))
                                      :test #'equal)))
                   (test-assert (and (not (world-host-live-p host))
                                     (eq (getf (rest status) :host-health) health)
                                     (world-host-reserved-p host)
                                     (eq host (world-server--find-host server identifier)))
                                "unavailable endpoints retain one owner and expose their actual lifecycle")
                   (test-assert
                    (handler-case (progn (world-server--check-host-opening host) nil)
                      (world-error () t))
                    "an unavailable retained host refuses opens")))))
         (setf (world-host-thread host) owner
               (world-host-state host) ':ready
               (world-host-exit-reason host) nil))
       (test-assert (world-host-live-p host) "availability recovers after a legitimate transition"))))
  nil)

(-> test-world-host-cleanup-retry () null)
(defun test-world-host-cleanup-retry ()
  "Keep the lease through failed mandatory cleanup and retry after the owner exits."
  (world-server-tests--call
   (lambda (server configuration root)
     (let* ((entry       (world-client-entry configuration))
            (workspace   (world-tests--workspace root "cleanup-retry"))
            (identifier  (getf (rest (world-server-tests--open entry workspace)) :session-id))
            (host        (world-server--find-host server identifier))
            (application (world-host-application host))
            (lease       (application-conversation-lease application))
            (registry    (application-tool-registry application))
            (close       (symbol-function 'tool-registry-close-runtime-state))
            (fail-p      t)
            (cause       (make-condition 'simple-error :format-control "Injected runtime close failure.")))
       (test-call-with-function-replacements
        (list (list 'tool-registry-close-runtime-state
                    (lambda (candidate)
                      (if (and fail-p (eq candidate registry))
                          (error cause)
                          (funcall close candidate)))))
        (lambda ()
          (unwind-protect
               (progn
                 (world-server--close-host host)
                 (test-assert (task-tests--wait-until
                               (lambda () (not (thread-alive-p (world-host-thread host)))) 30)
                              "the serving thread exits despite a failed cleanup stage")
                 (test-assert (and (eq host (world-server--find-host server identifier))
                                   (conversation-lease-held-p lease)
                                   (eq cause (world-host-failure host))
                                   (not (world-host-live-p host)))
                              "failed cleanup retains routing, the actual lease and the original cause")
                 (setf fail-p nil)
                 (world-server--close-host host)
                 (test-assert (and (eq (world-host-state host) ':retired)
                                   (not (conversation-lease-held-p lease))
                                   (null (world-server--find-host server identifier)))
                              "retry retires the host only after mandatory cleanup and lease release"))
            (setf fail-p nil)
            (world-server--close-host host)))))))
  nil)
