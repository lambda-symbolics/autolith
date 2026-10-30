
(in-package #:autolith)

(-> agent-record-resource-test--field (tool-result string) string)
(defun agent-record-resource-test--field (result label)
  "Return a labelled result line from a successful resource tool call."
  (assert (tool-result-success-p result) () "~A" (tool-result-content result))
  (let* ((text (tool-result-content result)) (position (search label text)))
    (assert position nil "Missing ~A in ~A" label text)
    (let ((start (+ position (length label))))
      (subseq text start (or (position #\Newline text :start start) (length text))))))

(-> agent-record-resource-test--call (tool-registry tool-context string &rest t) tool-result)
(defun agent-record-resource-test--call (registry context name &rest fields)
  "Invoke the real resource tool registry with portable JSON arguments."
  (tool-registry-execute-call registry
                              (json-object "namespace" "resource" "name" name "arguments"
                                           (json-encode (apply #'json-object fields)))
                              context))

(-> agent-record-resource-test--tools (t pathname) null)
(defun agent-record-resource-test--tools (driver private-root)
  "Exercise normal tools while the child private root denotes an ordinary file."
  (let* ((configuration (make-configuration :provider-validation-p nil
                                          :knowledge-mode          "off"
                                          :data-root               private-root
                                          :state-root              private-root
                                          :config-root             private-root))
         (scope
          (agent-record-resources-install driver configuration :session "record-resource-test"))
         (registry      (make-instance 'tool-registry))
         (conversation  (make-instance 'conversation :identifier "record-resource-first"))
         (other         (make-instance 'conversation :identifier "record-resource-second"))
         (context
          (make-instance 'tool-context :configuration configuration :conversation conversation
                         :worker nil))
         (other-context
          (make-instance 'tool-context :configuration configuration :conversation other :worker
                         nil)))
    (default-tools--register-workspace registry)
    (unwind-protect
        (labels ((read-resource (uri &rest filters)
                   (apply #'agent-record-resource-test--call registry context "read" "uri" uri
                          filters))

                 (revision (result)
                   (agent-record-resource-test--field result "Revision: "))

                 (edit-resource (uri alias operation &optional (target context))
                   (agent-record-resource-test--call registry target "edit" "uri" uri
                    "base-revision" alias "operations" (vector operation)))

                 (operation (name &rest fields)
                   (apply #'json-object "op" name fields)))
          (assert (not (slot-boundp conversation 'pathname)))
          (let* ((empty (read-resource "agenda:current"))
                 (base  (revision empty))
                 (add   (operation "agenda-add" "text" "one"))
                 (first (edit-resource "agenda:current" base add))
                 (retry (edit-resource "agenda:current" base add)))
            (assert (tool-result-success-p retry) () "First: ~A~%Retry: ~A" (tool-result-content first) (tool-result-content retry))
            (assert (string= (revision first) (revision retry)))
            (assert
             (not
              (tool-result-success-p
               (edit-resource "agenda:current" base (operation "agenda-add" "text" "different")))))
            (assert
             (not (tool-result-success-p (edit-resource "agenda:current" base add other-context))))
            (let* ((adapter (agent-record-resource-scope-agenda scope))
                   (item    (first (agent-protocol-agenda-query adapter)))
                   (id      (agenda-item-identifier item))
                   (updated
                    (edit-resource "agenda:current" (revision first)
                     (operation "agenda-update" "id" id "status" "doing"))))
              (assert (not (not (tool-result-success-p updated))))
              (assert
               (eq ':doing (agenda-item-status (first (agent-protocol-agenda-query adapter)))))
              (assert
               (not
                (not
                 (tool-result-success-p
                  (edit-resource "agenda:current" (revision updated)
                   (operation "agenda-remove" "id" id))))))))
          (let* ((base    (revision (read-resource "memory:workspace")))
                 (remember
                  (operation "memory-remember" "title" "release" "content" "Build first." "tags"
                   #("release") "sources"
                   (vector (json-object "user" "Use Common Lisp for automation."))))
                 (created (edit-resource "memory:workspace" base remember))
                 (uri     (agent-record-resource-test--field created "Exact URI: "))
                 (item    (read-resource uri))
                 (replacement
                  (operation "memory-replace" "title" "release two" "content" "Build twice."))
                 (changed (edit-resource uri (revision item) replacement)))
            (assert (search "Use Common Lisp for automation." (tool-result-content item)))
            (assert (not (not (tool-result-success-p changed))))
            (assert (search "Use Common Lisp for automation." (tool-result-content changed)))
            (assert (eq ':operation-unsupported
                        (tool-result-error-code
                         (edit-resource uri (revision changed)
                                        (operation "memory-replace" "title" "global" "content" "blocked"
                                                   "scope" "global")))))
            (let* ((record   (first (agent-protocol-memory-query
                                     (agent-record-resource-scope-memory scope))))
                   (metadata (gethash "metadata" (agent-protocol-memory-item-fields record))))
              (assert (typep (gethash "created_at" metadata) 'timestamp))
              (assert (equal "current" (gethash "status" (gethash "assessment" metadata)))))
            (assert
             (not
              (tool-result-success-p
               (edit-resource uri (revision item)
                (operation "memory-replace" "title" "stale" "content" "stale")))))
            (assert
             (not (not (tool-result-success-p (edit-resource uri (revision item) replacement)))))
            (let ((cleared
                   (edit-resource uri (revision changed)
                    (operation "memory-replace" "title" "cleared" "content" "No citations."
                     "sources" #() "premises" #()))))
              (assert (not (not (tool-result-success-p cleared))))
              (assert
               (not (search "Use Common Lisp for automation." (tool-result-content cleared))))
              (assert
               (not
                (not
                 (tool-result-success-p
                  (edit-resource uri (revision cleared) (operation "memory-forget")))))))
            (assert (not (tool-result-success-p (read-resource uri))))
            (assert (not (tool-result-success-p (read-resource "memory:facts"))))
            (assert (not (tool-result-success-p (read-resource "memory:rules"))))
            (assert (eq ':operation-unsupported
                        (tool-result-error-code (read-resource "memory:global"))))
            (assert
             (not (tool-result-success-p (read-resource "memory:workspace" "start-line" 1))))
            (assert
             (not
              (not
               (tool-result-success-p
                (read-resource "memory:relevant" "query" "release" "max-results" 1))))))
          (let* ((base (revision (read-resource "papercut:current")))
                 (created
                  (edit-resource "papercut:current" base
                   (operation "papercut-report" "title" "tool failure" "content"
                    "The call failed.")))
                 (uri  (agent-record-resource-test--field created "Exact URI: "))
                 (item (read-resource uri))
                 (assessed
                  (edit-resource uri (revision item)
                   (operation "papercut-assess" "verdict" "improved" "note" "The check passed."))))
            (assert (not (not (tool-result-success-p assessed))))
            (assert (search "The check passed." (tool-result-content assessed)))
            (assert
             (not
              (not
               (tool-result-success-p
                (edit-resource uri (revision assessed)
                 (operation "papercut-close" "resolution" "Verified."))))))
            (assert (not (tool-result-success-p (read-resource uri)))))
          (let ((old (revision (read-resource "agenda:current"))))
            (agent-record-resources-install driver configuration)
            (assert
             (not
              (tool-result-success-p
               (edit-resource "agenda:current" old
                (operation "agenda-add" "text" "wrong connection")))))))
      (agent-record-resources-uninstall configuration)))
  nil)

(-> agent-record-resource-test--round-trip nil null)
(defun agent-record-resource-test--round-trip ()
  "Exercise the real byte codec, sole-reader driver, services and resource tools."
  (let* ((directory
          (merge-pathnames (format nil "record-resource-~A/" (make-identifier))
                           (uiop/stream:temporary-directory)))
         (memory-context
          (make-memory-store-context :memory-path    (merge-pathnames "memory.sexp" directory)
                                     :knowledge-path (merge-pathnames "knowledge.sexp" directory)
                                     :workspace directory :source-roots (list directory)))
         (memory
          (autolith-supervisor::make-memory-service :context memory-context :session-id
                                                    "client-session" :allow-global-p t
                                                    :maximum-bytes 4096 :input-history
                                                    (constantly
                                                     '("Use Common Lisp for automation."))))
         (agenda
          (autolith-supervisor::make-agenda-service :context
                                                    (make-instance
                                                     'autolith-supervisor::agenda-context :pathname
                                                     (merge-pathnames "agenda.sexp" directory)
                                                     :directory        (namestring directory)
                                                     :memory-validator (constantly t))
                                                    :session-id         "client-session"
                                                    :max-response-bytes 4096))
         (papercut
          (autolith-supervisor::make-papercut-service :context
                                                      (make-instance
                                                       'autolith-supervisor::papercut-context
                                                       :pathname
                                                       (merge-pathnames "papercut.sexp" directory)
                                                       :workspace "attached-workspace")
                                                      :session-id         "client-session"
                                                      :max-response-bytes 4096)))
    (ensure-directories-exist directory)
    (with-open-file (stream (merge-pathnames "child-private" directory)
                            :direction ':output :if-exists ':supersede)
      (write-line "This is a file, not a child private store directory." stream))
    (unwind-protect
        (agent-protocol-test--peers
         (lambda (parent)
           (let ((welcome (agent-protocol-test--welcome)))
             (setf (agent-wire:welcome.features welcome) '("session.v1" "records.v1"))
             (autolith-supervisor::parent-connection-handshake parent welcome))
           (loop for envelope = (autolith-supervisor::parent-connection-receive parent)
                 do (case (agent-wire:envelope.body-case envelope)
                      (agent-wire:output
                       (return))
                      (agent-wire:record-request
                       (let* ((request (agent-wire:envelope.record-request envelope))
                              (response
                               (handler-case
                                (agent-wire:make-envelope :record-response
                                                          (ecase
                                                              (agent-wire:record-request.collection
                                                               request)
                                                            (:agenda
                                                             (autolith-supervisor::agenda-service-handle
                                                              agenda request))
                                                            (:memory
                                                             (autolith-supervisor::memory-service-handle
                                                              memory request))
                                                            (:papercut
                                                             (autolith-supervisor::papercut-service-handle
                                                              papercut request))))
                                (autolith-supervisor::agenda-service-error (condition)
                                 (agent-wire:make-envelope :failure
                                                           (agent-wire:make-failure :code
                                                                                    (autolith-supervisor::agenda-service-error-code
                                                                                     condition))))
                                (memory-record-error (condition)
                                 (agent-wire:make-envelope :failure
                                                           (agent-wire:make-failure :code
                                                                                    (memory-record-error-code
                                                                                     condition))))
                                (autolith-supervisor::papercut-service-error (condition)
                                 (agent-wire:make-envelope :failure
                                                           (agent-wire:make-failure :code
                                                                                    (autolith-supervisor::papercut-service-error-code
                                                                                     condition)))))))
                         (setf (agent-wire:envelope.reply-to response)
                               (agent-wire:envelope.message-id envelope))
                         (autolith-supervisor::parent-connection-send parent response)))
                      (otherwise
                       (assert nil)))))
         (lambda (client)
           (agent-protocol-handshake client :features '("session.v1" "records.v1"))
           (let ((driver (agent-protocol-driver-start client)))
              (unwind-protect (agent-record-resource-test--tools
                               driver (merge-pathnames "child-private/" directory))
               (agent-protocol-output client ':diagnostic "done")
               (agent-protocol-driver-close driver)
               (agent-protocol-driver-join driver)))))
      (platform-delete-directory-tree *platform* directory :validate t :if-does-not-exist ':ignore)))
  nil)

(-> agent-record-resource-test--contract-boundary () null)
(defun agent-record-resource-test--contract-boundary ()
  "Refuse a scope that could read a private knowledge store before dispatch."
  (let ((configuration (make-configuration :provider-validation-p nil :knowledge-mode "active")))
    (assert (handler-case
                (progn (agent-record-resources-install nil configuration) nil)
              (agent-record-resource-contract-unsupported () t)))
    (assert (not (gethash configuration *agent-record-resource-scopes*))))
  nil)

(-> run-agent-protocol-record-resource-tests nil integer)
(defun run-agent-protocol-record-resource-tests ()
  "Run the resource tool integration check over the serialized record boundary."
  (agent-record-resource-test--contract-boundary)
  (agent-record-resource-test--round-trip)
  2)
