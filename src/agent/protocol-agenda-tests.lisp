;;;; -- Serialized Parent Agenda Adapter Checks --

(in-package #:autolith)

(-> agent-agenda-test--round-trip () null)
(defun agent-agenda-test--round-trip ()
  "Use the shared agenda store through serialized requests and the sole-reader driver."
  (let* ((directory (merge-pathnames (format nil "agent-agenda-~A/" (make-identifier))
                                     (uiop:temporary-directory)))
         (context   (make-instance 'autolith-supervisor::agenda-context
                                   :pathname         (merge-pathnames "agendas.sexp" directory)
                                   :directory        (namestring directory)
                                   :memory-validator (lambda (id) (string= id "memory-a"))))
         (service   (autolith-supervisor::make-agenda-service
                     :context context :session-id "client-session" :max-response-bytes 4096)))
    (ensure-directories-exist directory)
    (unwind-protect
         (agent-protocol-test--peers
          (lambda (parent)
            (let ((welcome (agent-protocol-test--welcome)))
              (setf (agent-wire:welcome.features welcome) '("session.v1" "records.v1"))
              (autolith-supervisor::parent-connection-handshake parent welcome))
            (loop for request = (autolith-supervisor::parent-connection-receive parent)
                  do (case (agent-wire:envelope.body-case request)
                       (agent-wire:output
                        (return))
                       (agent-wire:record-request
                        (let ((response
                                (handler-case
                                    (agent-wire:make-envelope
                                     :record-response
                                     (autolith-supervisor::agenda-service-handle
                                      service (agent-wire:envelope.record-request request)))
                                  (autolith-supervisor::agenda-service-error (condition)
                                    (agent-wire:make-envelope
                                     :failure (agent-wire:make-failure
                                               :code (autolith-supervisor::agenda-service-error-code condition)))))))
                          (setf (agent-wire:envelope.reply-to response)
                                (agent-wire:envelope.message-id request))
                          (autolith-supervisor::parent-connection-send parent response)))
                       (otherwise
                        (assert nil)))))
          (lambda (client)
            (agent-protocol-handshake client :features '("session.v1" "records.v1"))
            (let* ((driver  (agent-protocol-driver-start client))
                   (adapter (make-agent-protocol-agenda driver)))
              (unwind-protect
                   (let* ((original (agent-protocol-agenda-create
                                     adapter :text "first" :memory-identifiers '("memory-a")
                                     :operation-id "create-first"))
                          (updated  (agent-protocol-agenda-replace
                                     adapter original :text "changed" :status ':doing
                                     :operation-id "replace-first"))
                          (retried  (agent-protocol-agenda-create
                                     adapter :text "first" :memory-identifiers '("memory-a")
                                     :operation-id "create-first")))
                     (assert (typep original 'agenda-item))
                     (assert (equal '("memory-a") (agenda-item-memory-identifiers updated)))
                     (assert (eq ':doing (agenda-item-status updated)))
                     (assert (string= (agenda-item-identifier original) (agenda-item-identifier retried)))
                     (assert (string= (agent-protocol-agenda-item-revision original)
                                      (agent-protocol-agenda-item-revision retried)))
                     (assert (not (string= (agent-protocol-agenda-item-revision original)
                                           (agent-protocol-agenda-item-revision updated))))
                     (assert (handler-case
                                 (progn (agent-protocol-agenda-replace
                                         adapter original :text "stale" :operation-id "stale")
                                        nil)
                               (agent-protocol-agenda-error (condition)
                                 (eq ':revision-conflict (agent-protocol-agenda-error-code condition)))))
                     (assert (eq ':ready (autolith-supervisor::parent-connection-phase client)))
                     (agent-protocol-agenda-create adapter :text "second" :operation-id "create-second")
                     (multiple-value-bind (page cursor) (agent-protocol-agenda-query adapter :limit 1)
                       (assert (= 1 (length page)))
                       (assert (string= "changed" (agenda-item-text (first page))))
                       (assert cursor)
                       (multiple-value-bind (next-page next-cursor)
                           (agent-protocol-agenda-query adapter :limit 1 :cursor cursor)
                         (assert (= 1 (length next-page)))
                         (assert (string= "second" (agenda-item-text (first next-page))))
                         (assert (null next-cursor))))
                     (agent-protocol-agenda-delete adapter updated :operation-id "delete-first")
                     (agent-protocol-agenda-delete adapter updated :operation-id "delete-first")
                     (assert (= 1 (length (agent-protocol-agenda-query adapter))))
                     (agent-protocol-output client ':diagnostic "done"))
                (agent-protocol-driver-close driver)
                (agent-protocol-driver-join driver)))))
      (dolist (file (uiop:directory-files directory))
        (delete-file file))
      (uiop:delete-empty-directory directory)))
  nil)

(-> run-agent-protocol-agenda-tests () integer)
(defun run-agent-protocol-agenda-tests ()
  "Check the optional agenda adapter over the real byte channel."
  (agent-agenda-test--round-trip)
  1)
