;;;; -- Serialized Parent Papercut Adapter Checks --

(in-package #:autolith)

(-> agent-papercut-test--round-trip () null)
(defun agent-papercut-test--round-trip ()
  "Check lifecycle, replay, revision guards, and paging over the sole-reader driver."
  (let* ((directory (merge-pathnames (format nil "agent-papercut-~A/" (make-identifier))
                                     (uiop:temporary-directory)))
         (path      (merge-pathnames "papercuts.sexp" directory))
         (context   (make-instance 'autolith-supervisor::papercut-context
                                   :pathname path :workspace "trusted-workspace"))
         (service   (autolith-supervisor::make-papercut-service
                     :context context :session-id "client-session" :max-response-bytes 4096))
         (requests  0))
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
                        (incf requests)
                        (let ((response
                                (handler-case
                                    (agent-wire:make-envelope
                                     :record-response
                                     (autolith-supervisor::papercut-service-handle
                                      service (agent-wire:envelope.record-request request)))
                                  (autolith-supervisor::papercut-service-error (condition)
                                    (agent-wire:make-envelope
                                     :failure (agent-wire:make-failure
                                               :code (autolith-supervisor::papercut-service-error-code
                                                      condition)))))))
                          (setf (agent-wire:envelope.reply-to response)
                                (agent-wire:envelope.message-id request))
                          (autolith-supervisor::parent-connection-send parent response)))
                       (otherwise
                        (error 'agent-protocol-error :reason ':unexpected-papercut-test-request)))))
          (lambda (client)
            (agent-protocol-handshake client :features '("session.v1" "records.v1"))
            (let* ((driver  (agent-protocol-driver-start client))
                   (adapter (make-agent-protocol-papercut driver)))
              (unwind-protect
                   (labels ((fails (code function)
                              (assert
                               (handler-case (progn (funcall function) nil)
                                 (agent-protocol-papercut-error (condition)
                                   (eq code (agent-protocol-papercut-error-code condition)))))))
                     (let* ((original
                              (agent-protocol-papercut-create
                               adapter :title "Pipe reader blocks" :content "A child pipe reader blocks at EOF."
                               :diagnostic "The private byte stream did not close."
                               :issue-kind "broken_tool" :tool "shell.run" :operation-id "create-first"))
                            (assessed
                              (agent-protocol-papercut-assess
                               adapter original :verdict ':unchanged :note "The reader still blocks."
                               :operation-id "assess-first")))
                       (assert (string= "unchanged"
                                        (gethash "verdict" (agent-protocol-papercut-record-fields assessed))))
                       (assert (null (gethash "workspace"
                                             (agent-protocol-papercut-record-fields original))))
                       (assert (null (gethash "telemetry_id"
                                             (agent-protocol-papercut-record-fields original))))
                       (fails ':revision-conflict
                              (lambda ()
                                (agent-protocol-papercut-assess
                                 adapter original :verdict ':improved :note "Stale observation."
                                 :operation-id "assess-stale")))
                       (fails ':invalid-request
                              (lambda ()
                                (agent-protocol-papercut-close
                                 adapter assessed :resolution "Incomplete repair."
                                 :operation-id                "close-unchanged")))
                       (let ((replayed
                               (agent-protocol-papercut-create
                                adapter :title "Pipe reader blocks" :content "A child pipe reader blocks at EOF."
                                :diagnostic "The private byte stream did not close."
                                :issue-kind "broken_tool" :tool "shell.run" :operation-id "create-first")))
                         (assert (string= (agent-protocol-papercut-record-revision original)
                                          (agent-protocol-papercut-record-revision replayed)))
                         (assert (null (gethash "verdict"
                                               (agent-protocol-papercut-record-fields replayed)))))
                       (fails ':invalid-request
                              (lambda ()
                                (agent-protocol-papercut-create
                                 adapter :title "Different operation" :content "A conflicting payload."
                                 :operation-id "create-first")))
                       (let* ((improved
                                (agent-protocol-papercut-assess
                                 adapter assessed :verdict ':improved :note "The EOF check passed."
                                 :operation-id "assess-improved"))
                              (closed
                                (agent-protocol-papercut-close
                                 adapter improved :resolution "Fixed stream cleanup."
                                 :operation-id                "close-first"))
                              (replayed
                                (agent-protocol-papercut-close
                                 adapter improved :resolution "Fixed stream cleanup."
                                 :operation-id                "close-first")))
                         (assert (string= "closed" (gethash "status"
                                                           (agent-protocol-papercut-record-fields closed))))
                         (assert (string= (agent-protocol-papercut-record-revision closed)
                                          (agent-protocol-papercut-record-revision replayed)))))
                     (agent-protocol-papercut-create
                      adapter :title "Window redraw flickers" :content "The terminal window redraw is not stable."
                      :operation-id "create-second")
                     (agent-protocol-papercut-create
                      adapter :title "Account renewal expires" :content "The account renewal returns an expired token."
                      :operation-id "create-third")
                     (multiple-value-bind (page cursor) (agent-protocol-papercut-query adapter :limit 1)
                       (assert (= 1 (length page)))
                       (assert cursor)
                       (let ((first-id (agent-protocol-papercut-record-identifier (first page))))
                         (multiple-value-bind (next next-cursor)
                             (agent-protocol-papercut-query adapter :limit 1 :cursor cursor)
                           (assert (= 1 (length next)))
                           (assert (not (equal first-id (agent-protocol-papercut-record-identifier (first next)))))
                           (assert (null next-cursor)))))
                     (assert (= 2 (length (agent-protocol-papercut-query adapter))))
                     (assert (zerop (hash-table-count (agent-protocol-client-pending client))))
                     (assert (handler-case (progn (agent-protocol-receive client) nil)
                               (agent-protocol-error (condition)
                                 (eq ':reader-owned (agent-protocol-error-reason condition)))))
                     (agent-protocol-output client ':diagnostic "done"))
                (agent-protocol-driver-close driver)
                (agent-protocol-driver-join driver)))))
      (platform-delete-directory-tree *platform* directory :validate t :if-does-not-exist ':ignore))
    ;; Each explicit operation produces one request, including each rejected write.
    (assert (= requests 14)))
  nil)

(-> agent-papercut-test--invalid-view () null)
(defun agent-papercut-test--invalid-view ()
  "Reject incomplete lifecycle fields after one serialized response, without retry."
  (agent-protocol-test--peers
   (lambda (parent)
     (let ((welcome (agent-protocol-test--welcome)))
       (setf (agent-wire:welcome.features welcome) '("session.v1" "records.v1"))
       (autolith-supervisor::parent-connection-handshake parent welcome))
     (let* ((request (autolith-supervisor::parent-connection-receive parent))
            (fields  (agent-protocol-papercut--fields
                      '(("title" . "Valid title") ("content" . "Valid content")
                        ("status" . "active") ("issue_kind" . "other")
                        ("reported_at" . 1) ("verdict" . "improved"))))
            (response
              (agent-wire:make-envelope
               :reply-to (agent-wire:envelope.message-id request)
               :record-response
               (agent-wire:make-record-response
                :records (list (agent-wire:make-stored-record
                                :record-id "report" :revision "revision" :fields fields))))))
       (autolith-supervisor::parent-connection-send parent response)))
   (lambda (client)
     (agent-protocol-handshake client :features '("session.v1" "records.v1"))
     (let* ((driver  (agent-protocol-driver-start client))
            (adapter (make-agent-protocol-papercut driver)))
       (unwind-protect
            (progn
              (assert (handler-case (progn (agent-protocol-papercut-query adapter) nil)
                        (agent-protocol-error () t)))
              (assert (eq ':closed (autolith-supervisor::parent-connection-phase client))))
         (agent-protocol-driver-close driver)
         (agent-protocol-driver-join driver)))))
  nil)

(-> agent-papercut-test--legacy-receipt () null)
(defun agent-papercut-test--legacy-receipt ()
  "Read parent receipts through the legacy facade and replay after legacy closure."
  (let* ((directory     (merge-pathnames (format nil "papercut-cutover-~A/" (make-identifier))
                                         (uiop:temporary-directory)))
         (configuration (make-configuration
                         :working-directory directory
                         :source-root       (asdf:system-source-directory "autolith-agent")
                         :config-root       (merge-pathnames "config/" directory)
                         :data-root         (merge-pathnames "data/" directory)
                         :state-root        (merge-pathnames "state/" directory)
                         :cache-root        (merge-pathnames "cache/" directory)
                         :onboarding-p nil :telemetry-enabled-p nil))
         (service       (autolith-supervisor::make-papercut-service
                         :context (papercut--context configuration) :session-id "cutover-session"))
         (request       (agent-wire:make-record-request
                         :collection ':papercut :operation ':create :operation-id "cutover-create"
                         :fields (agent-protocol-papercut--fields
                                  '(("title" . "A shared report") ("content" . "A shared lifecycle log."))))))
    (ensure-directories-exist directory)
    (unwind-protect
         (let* ((response   (autolith-supervisor::papercut-service-handle service request))
                (bytes      (cl-protobufs:serialize-to-bytes response))
                (identifier (agent-wire:stored-record.record-id
                             (first (agent-wire:record-response.records response))))
                (report     (papercut-find configuration identifier)))
           (assert (typep report 'papercut))
           (assert (= 1 (length (papercut-list configuration))))
           (papercut-mark-closed configuration identifier :resolution "Verified shared closure.")
           (assert (null (papercut-list configuration)))
           (assert (equalp bytes (cl-protobufs:serialize-to-bytes
                                  (autolith-supervisor::papercut-service-handle service request))))
           (assert (null (papercut-list configuration))))
      (platform-delete-directory-tree *platform* directory :validate t :if-does-not-exist ':ignore)))
  nil)

(-> run-agent-protocol-papercut-tests () integer)
(defun run-agent-protocol-papercut-tests ()
  "Check the parent papercut adapter over real serialized connections."
  (agent-papercut-test--round-trip)
  (agent-papercut-test--invalid-view)
  (agent-papercut-test--legacy-receipt)
  3)
