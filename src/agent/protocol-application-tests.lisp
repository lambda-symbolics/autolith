;;;; -- Isolated Application Protocol Checks --

(in-package #:autolith)

(-> agent-application-test--exchange (pathname function function) null)
(defun agent-application-test--exchange (path server client)
  "Run serialized application scopes with parent-owned storage."
  (agent-protocol-test--peers
   (lambda (parent)
     (let ((welcome (agent-session-test--welcome)))
       (setf (agent-wire:welcome.features welcome) '("session.v1" "application.v1"))
       (autolith-supervisor::parent-connection-handshake parent welcome))
     (let* ((store   (autolith-supervisor::session-store-open path "client-session"))
            (runtime (autolith-supervisor::session-runtime-open store parent
                       :features '("session.v1" "application.v1"))))
       (unwind-protect
            (funcall server runtime)
         (autolith-supervisor::session-runtime-disconnect runtime)
         (autolith-supervisor::session-runtime-await-close runtime :timeout 3))))
   (lambda (peer)
     (agent-protocol-handshake peer :features '("session.v1" "application.v1"))
     (let ((session (agent-session-test--start peer client)))
       (unwind-protect
            (progn
              (agent-protocol-session-join session :timeout 8)
              (assert (agent-protocol-session-stopped session))
              (assert (null (agent-protocol-session-failure session))))
         (agent-protocol-session-close session)
         (agent-protocol-session-join session :timeout 3)))))
  nil)

(-> agent-application-test--root
    (t string &key (:kind keyword) (:source string)) agent-wire:application-result)
(defun agent-application-test--root (runtime identifier &key kind (source ""))
  "Await one durable root application result."
  (autolith-supervisor::session-runtime-start-turn runtime identifier source
    :application (agent-wire:make-application-action :kind kind :source source))
  (agent-session-test--until-finish runtime)
  (let ((result (autolith-supervisor::session-runtime-application-result runtime)))
    (assert result)
    (assert (eq ':ok (agent-wire:application-result.status result)))
    result))

(-> agent-application-test--scopes () null)
(defun agent-application-test--scopes ()
  "Keep Lisp state in the child and commit exact source and results in parent storage."
  (uiop:with-temporary-file (:pathname path :stream stream)
    (close stream)
    (delete-file path)
    (unwind-protect
         (agent-application-test--exchange
          path
          (lambda (runtime)
             (agent-application-test--root runtime "define" :kind ':lisp
               :source                                            "(defparameter *agent-application-test-state* 40)")
             (let ((result (agent-application-test--root runtime "increment" :kind ':lisp
                             :source                                               "(incf *agent-application-test-state* 2)")))
              (assert (equal '("42") (agent-wire:application-result.values result))))
             (let ((result (agent-application-test--root runtime "complete" :kind ':completion)))
              (assert (agent-wire:application-result.commands result))
              (assert (agent-wire:application-result.completion-entries result)))
            (let* ((store   (autolith-supervisor::session-runtime-store runtime))
                   (page    (autolith-supervisor::session-store-read store
                              (agent-wire:make-session-read :limit 256)))
                   (records (agent-wire:session-page.records page)))
              (assert (= 3 (count ':user-input records :key #'agent-wire:session-record.kind)))
              (assert (= 3 (count-if
                            (lambda (record)
                              (let ((data (autolith-supervisor::protocol-value->data
                                           (agent-wire:session-record.payload record))))
                                (nth-value 1 (gethash "application_result" data)))) records))))
            (agent-session-test--stop runtime))
          (lambda (session observer)
            (declare (ignore session observer))
           (error 'agent-protocol-error :reason ':application-entered-provider-runner)))
      (agent-session-test--cleanup path)))
  nil)

(-> agent-application-test--fifo () null)
(defun agent-application-test--fifo ()
  "Execute active Lisp scopes in order while the provider turn waits."
  (let ((entered nil) (release nil) (lock (bordeaux-threads:make-lock "application test")))
    (uiop:with-temporary-file (:pathname path :stream stream)
      (close stream)
      (delete-file path)
      (unwind-protect
           (agent-application-test--exchange
            path
            (lambda (runtime)
              (autolith-supervisor::session-runtime-start-turn runtime "provider" "ordinary message")
              (loop until (bordeaux-threads:with-lock-held (lock) entered)
                    do (autolith-supervisor::session-runtime-receive runtime))
              (assert entered)
              (autolith-supervisor::session-runtime-application-steer runtime "first"
                (agent-wire:make-application-action :kind ':lisp
                  :source                                 "(defparameter *agent-application-fifo-state* 7)"))
              (autolith-supervisor::session-runtime-application-steer runtime "second"
                (agent-wire:make-application-action :kind ':async-lisp
                  :source                                 "(incf *agent-application-fifo-state*)"))
              (loop until (let ((scope (gethash "second"
                                        (autolith-supervisor::session-runtime-application-scopes runtime))))
                            (autolith-supervisor::session-application-scope-result scope))
                    do (autolith-supervisor::session-runtime-receive runtime))
              (let* ((scope  (gethash "second" (autolith-supervisor::session-runtime-application-scopes runtime)))
                     (result (autolith-supervisor::session-application-scope-result scope)))
                (assert (equal '("8") (agent-wire:application-result.values result))))
              (bordeaux-threads:with-lock-held (lock) (setf release t))
              (agent-session-test--until-finish runtime)
              (agent-session-test--stop runtime))
            (lambda (session observer)
              (declare (ignore observer))
              (assert (equal '("ordinary message")
                             (conversation-input-history (agent-protocol-session-conversation session))))
              (bordeaux-threads:with-lock-held (lock) (setf entered t))
              (agent-protocol-output (agent-protocol-session-client session) ':progress "entered")
              (loop until (bordeaux-threads:with-lock-held (lock) release) do (sleep 0.005))))
        (agent-session-test--cleanup path))))
  nil)

(-> agent-application-test--remote-ui () null)
(defun agent-application-test--remote-ui ()
  "Keep styled stream tails independent of preview and deduplicate finalized output."
  (let* ((events  nil)
         (session (allocate-instance (find-class 'agent-protocol-session)))
         (ui      (agent-protocol-terminal-ui-create
                   session :event-function (lambda (kind payload) (push (cons kind payload) events))))
         (preview (list (terminal-span ':dim "reasoning")))
         (tail    (list (list (terminal-span ':plain "first"))
                        (list (terminal-span ':brand "second")))))
    (terminal-ui-set-preview-rows ui (list preview))
    (terminal-ui-stream-update ui :tail tail)
    (terminal-ui-stream-update ui :tail nil)
    (assert (= 1 (count ':preview events :key #'first)))
    (assert (null (terminal-ui-stream-tail ui)))
    (assert (= 2 (length (getf (rest (second events)) :tail-rows))))
    (assert (terminal-ui-append-finalized ui '(:test 1) "once"))
    (assert (not (terminal-ui-append-finalized ui '(:test 1) "once")))
    (assert (= 1 (count ':finalized events :key #'first)))
    (assert (null (terminal-ui-editor ui)))
    (assert (handler-case
                (progn (terminal--write (terminal-ui-terminal ui) "forbidden") nil)
              (agent-protocol-error () t))))
  (assert (autolith-supervisor::protocol-application-action-validate
           (agent-wire:make-application-action :kind ':pump)))
  (assert (handler-case
              (progn (autolith-supervisor::protocol-application-action-validate
                      (agent-wire:make-application-action :kind ':pump :source "(error :unsafe)")) nil)
            (autolith-supervisor::parent-application-operation-error () t)))
  nil)

(defparameter *agent-application-startup-order* nil
  "Ordered startup effects from the projected replay/init fixture.")

(-> agent-application-test--configure-order () null)
(defun agent-application-test--configure-order ()
  "Execute selected replay before ordinary init, and stop before loads on cancellation."
  (let* ((*agent-application-startup-order* nil)
         (session                           (make-instance 'agent-protocol-session))
         (application                       (make-instance 'agent-protocol-application :session session)))
    (uiop:with-temporary-file (:pathname replay :stream replay-stream :type "lisp")
      (write-string "(push :replay *agent-application-startup-order*)" replay-stream)
      (close replay-stream)
      (uiop:with-temporary-file (:pathname init :stream init-stream :type "lisp")
        (write-string
         "(assert (equal *agent-application-startup-order* '(:replay)))
(push :init *agent-application-startup-order*)" init-stream)
        (close init-stream)
        (let ((source (agentcomms:json-encode
                       (agentcomms:json-object
                        "init_files" (vector (namestring init))
                        "replay_files" (vector (namestring replay))))))
          (agent-application--configure application source)
          (assert (equal *agent-application-startup-order* '(:init :replay)))
          (setf *agent-application-startup-order* nil
                (agent-protocol-session-cancelled session) t)
          (assert (handler-case (progn (agent-application--configure application source) nil)
                    (application-turn-cancelled () t)))
          (assert (null *agent-application-startup-order*))))))
  nil)

(-> agent-application-test--public-settings () null)
(defun agent-application-test--public-settings ()
  "Accept parent image selection state without creating an agent setting."
  (dolist (pristine-p '(nil t))
    (let ((configuration (make-configuration :provider-validation-p nil))
          (settings      (agentcomms:json-object
                          "pristine-p" pristine-p "immutable-p" pristine-p
                          "permission-mode" "ask" "compact-view-p" t)))
      (agent-protocol-application-settings-apply configuration settings)
      (assert (eql pristine-p (config ':immutable-p configuration)))
      (assert (eq ':ask (config ':permission-mode configuration)))
      (assert (config ':compact-view-p configuration))
      (dolist (invalid '("true" 1))
        (setf (gethash "pristine-p" settings) invalid)
        (assert (handler-case
                    (progn
                      (agent-protocol-application-settings-apply configuration settings)
                      nil)
                  (agent-protocol-error () t))))))
  nil)

(-> run-agent-protocol-application-tests () null)
(defun run-agent-protocol-application-tests ()
  "Check root persistence, live Lisp state, completion, and active FIFO execution."
  (agent-application-test--public-settings)
  (agent-application-test--configure-order)
  (agent-application-test--remote-ui)
  (agent-application-test--scopes)
  (agent-application-test--fifo)
  nil)
