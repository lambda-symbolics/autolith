;;;; -- Fresh-Process Parent Launch Checks --

(in-package #:autolith-supervisor)

(declaim (ftype (function (string) string) parent-launch-test--sse))

(defun parent-launch-test--sse (item)
  "Encode one complete Responses item through the real library parser."
  (format nil "data: {\"type\":\"response.created\",\"response\":{\"id\":\"fixture-response\"}}~%~%data: {\"type\":\"response.output_item.done\",\"output_index\":0,\"item\":~A}~%~%data: {\"type\":\"response.completed\",\"response\":{\"id\":\"fixture-response\",\"status\":\"completed\",\"output\":[~A],\"usage\":{\"input_tokens\":1,\"output_tokens\":1}}}~%~%"
          item item))

(declaim (ftype (function () string) parent-launch-test--effect-item))

(defun parent-launch-test--effect-item ()
  "Return a real fixture tool call, not an injected turn runner."
  "{\"id\":\"fc_fixture\",\"type\":\"function_call\",\"call_id\":\"effect-call\",\"name\":\"effect\",\"namespace\":\"fixture\",\"arguments\":\"{}\",\"status\":\"completed\"}")

(declaim (ftype (function () string) parent-launch-test--message-item))

(defun parent-launch-test--message-item ()
  "Return a real assistant message item."
  "{\"id\":\"msg_fixture\",\"type\":\"message\",\"role\":\"assistant\",\"status\":\"completed\",\"content\":[{\"type\":\"output_text\",\"text\":\"fixture answer\"}]}")

(declaim (ftype (function (t string) string) parent-launch-test--http-request))

(defun parent-launch-test--http-request (socket expected-token)
  "Read a bounded local request and check parent authentication without retaining headers."
  (let* ((stream (flexi-streams:make-flexi-stream (usocket:socket-stream socket) :external-format ':utf-8))
         (length nil) (authorized nil))
    (assert (search "POST /responses HTTP/" (read-line stream)))
    (loop for line = (string-right-trim '(#\Return) (read-line stream))
          until (zerop (length line))
          do (when (uiop:string-prefix-p "Content-Length:" line)
               (setf length (parse-integer line :start 15)))
             (when (uiop:string-prefix-p "Authorization: Bearer " line)
               (setf authorized (string= expected-token line :start2 22))))
    (assert authorized)
    (assert (and length (<= 0 length 8388608)))
    (let ((body (make-array length :element-type '(unsigned-byte 8))))
      (assert (= length (read-sequence body stream)))
      (sb-ext:octets-to-string body :external-format ':utf-8))))

(declaim (ftype (function (t string) null) parent-launch-test--http-reply))

(defun parent-launch-test--http-reply (socket body)
  "Write one real chunked SSE response and close its local transport."
  (let* ((raw   (usocket:socket-stream socket))
         (reply (format nil "HTTP/1.1 200 OK~C~CContent-Type: text/event-stream~C~CTransfer-Encoding: chunked~C~CConnection: close~C~C~C~C~X~C~C~A~C~C0~C~C~C~C"
                        #\Return #\Newline #\Return #\Newline #\Return #\Newline
                        #\Return #\Newline #\Return #\Newline (length body)
                        #\Return #\Newline body #\Return #\Newline
                        #\Return #\Newline #\Return #\Newline)))
    (write-sequence (sb-ext:string-to-octets reply :external-format ':utf-8) raw)
    (finish-output raw))
  nil)

(declaim (ftype (function (pathname pathname) list) parent-launch-test--child-command))

(defun parent-launch-test--child-command (runtime core)
  "Return explicit argv for a source-matched fixture core in a fresh process."
  (list (namestring runtime) "--core" (namestring core)
        "--noinform" "--end-runtime-options"))

(declaim (ftype (function (&key (:source-root pathname) (:temporary pathname) (:runtime pathname)
                               (:effect pathname) (:mode string)) list)
                parent-launch-test--environment))

(defun parent-launch-test--environment (&key source-root temporary runtime effect mode)
  "Build the complete child environment without inheriting any parent credential."
  (list (concatenate 'string "AUTOLITH_SOURCE_ROOT=" (namestring source-root))
        (concatenate 'string "AUTOLITH_SBCL=" (namestring runtime))
        (concatenate 'string "AUTOLITH_LAUNCH_TEST_EFFECT=" (namestring effect))
        (concatenate 'string "AUTOLITH_LAUNCH_TEST_MODE=" mode)
        (concatenate 'string "HOME=" (namestring temporary))
        (concatenate 'string "TMPDIR=" (namestring temporary))
        (concatenate 'string "XDG_DATA_HOME=" (namestring temporary))
        (concatenate 'string "XDG_CACHE_HOME=" (namestring temporary))
        (concatenate 'string "XDG_CONFIG_HOME=" (namestring temporary))))

(declaim (ftype (function (pathname) integer) parent-launch-test--effect-count))

(defun parent-launch-test--effect-count (path)
  "Count actual committed effects, not reported tool outcomes."
  (if (probe-file path)
      (with-open-file (stream path)
        (loop for line = (read-line stream nil nil) while line count t))
      0))

(declaim (ftype (function (parent-launch keyword) integer) parent-launch-test--record-count))

(defun parent-launch-test--record-count (launch kind)
  "Count a durable event kind after all runtime workers have joined."
  (count kind (parent-proto:session-page.records
               (session-store-read (parent-launch-store launch) (parent-proto:make-session-read :limit 128)))
         :key #'parent-proto:session-record.kind))

(declaim (ftype (function (pathname) list) parent-launch-test--worker-report))

(defun parent-launch-test--worker-report (effect)
  "Read the bounded worker report and prove that the worker is no longer live."
  (with-open-file (stream (pathname (concatenate 'string (namestring effect) ".worker")))
    (let* ((*read-eval* nil)
           (report      (read stream)))
      (assert (and (listp report) (<= 1 (length report) 5) (integerp (first report))))
      (assert (not (ls-compat.posix:process-alive-p (first report))))
      report)))

(declaim (ftype (function (&key (:runtime pathname) (:core pathname) (:source-root pathname)
                               (:temporary pathname) (:uncertain-p boolean) (:storage-root pathname)
                               (:launch-options function) (:after-first (or null function))
                               (:after-close (or null function)) (:resume-effect-p boolean) (:third-p boolean)) integer)
                parent-launch-test--fresh-process))

(defun parent-launch-test--fresh-process (&key runtime core source-root temporary uncertain-p
                                             (storage-root temporary) (launch-options (constantly nil))
                                             after-first after-close resume-effect-p third-p)
  "Run two fresh children, real HTTP, a real effect, durable outcome, and session resume."
  (let* ((listener      (usocket:socket-listen "127.0.0.1" 0 :reuse-address t :element-type '(unsigned-byte 8)))
         (manager       (make-instance 'provider-executor-test-manager))
         (token         (cl-rfc8628:oauth-credentials-access-token
                         (provider-executor-test-source-credentials
                          (cl-rfc8628:credential-manager-primary-source manager))))
         (profile       (make-model-profile :id "fixture" :model "fixture-model" :family ':responses))
         (profiles      (make-model-profile-registry (list profile)))
         (accounts      (make-hash-table :test #'eq))
         (effect        (merge-pathnames (if uncertain-p "uncertain-effect" "effect") temporary))
         (session-path  (merge-pathnames (if uncertain-p "uncertain-session.sexp" "session.sexp") storage-root))
         (replay-path   (merge-pathnames (if uncertain-p "uncertain-replay.sexp" "replay.sexp") storage-root))
         (configuration (make-hash-table :test #'equal))
         (launch nil) (server nil) (server-error nil) (requests 0)
         (first-turn    (if uncertain-p "uncertain-first" "first"))
         (mode          (if uncertain-p "uncertain" "normal")))
    (setf (gethash profile accounts)
          (make-chatgpt-account :credential-manager manager
                               :endpoint            (format nil "http://127.0.0.1:~D/responses"
                                                            (usocket:get-local-port listener)))
          (gethash "profile_id" configuration) "fixture"
          (gethash "model" configuration) "fixture-model"
          (gethash "workspace" configuration) (namestring temporary)
          (gethash "reasoning_effort" configuration) "none"
          (gethash "worker_core" configuration) (namestring core))
    (unwind-protect
         (progn
           (setf server
                 (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (dotimes (index (cond (third-p
                                               6) (uncertain-p
                                                   2) (resume-effect-p
                                                       4) (t
                                                           3)))
                          (let ((socket (sb-ext:with-timeout 30
                                          (loop until (usocket:wait-for-input listener :timeout 0.1 :ready-only t))
                                          (usocket:socket-accept listener :element-type '(unsigned-byte 8)))))
                            (unwind-protect
                                 (sb-ext:with-timeout 30
                                   (let ((body (parent-launch-test--http-request socket token)))
                                     (incf requests)
                                     (when (plusp index)
                                       (assert (search first-turn body))
                                       (assert (search (if uncertain-p "interrupt" "effect committed") body)))
                                     (parent-launch-test--http-reply
                                      socket (parent-launch-test--sse
                                              (cond ((zerop index)
                                                     (parent-launch-test--effect-item))
                                                    ((and resume-effect-p (= index 2))
                                                     "{\"id\":\"fc_second\",\"type\":\"function_call\",\"call_id\":\"second-effect-call\",\"name\":\"effect\",\"namespace\":\"fixture\",\"arguments\":\"{}\",\"status\":\"completed\"}")
                                                    ((and third-p (= index 4))
                                                     "{\"id\":\"fc_third\",\"type\":\"function_call\",\"call_id\":\"third-effect-call\",\"name\":\"effect\",\"namespace\":\"fixture\",\"arguments\":\"{}\",\"status\":\"completed\"}")
                                                    (t
                                                     (parent-launch-test--message-item)))))))
                              (usocket:socket-close socket))))
                      (error (condition)
                        (setf server-error (type-of condition))
                        (format *error-output* "~&Fixture HTTP request failed: ~A~%" condition))))
                  :name "launch fixture HTTP"))
           (labels ((open-child (instance mode)
                      (apply #'parent-launch-open
                             (append (funcall launch-options instance)
                                     (list
                                      :command     (parent-launch-test--child-command runtime core)
                                      :environment (parent-launch-test--environment :source-root source-root :runtime runtime
                                                                                  :temporary temporary :effect effect :mode mode)
                                      :directory temporary :session-id "launch-fixture" :instance-id instance
                                      :session-path session-path :replay-path replay-path
                                      :configuration (protocol-data->map configuration) :profiles profiles :accounts accounts
                                      :terminal    (parent-launch-test--terminal))))))
             (setf launch (open-child "launch-first-instance" mode))
             (if uncertain-p
                 (assert (parent-launch-test--fails-p
                          (lambda () (parent-launch-turn-run launch first-turn first-turn :timeout 30))
                          ':turn-outcome-uncertain))
                 (assert (eq ':completed (parent-launch-turn-run launch first-turn first-turn :timeout 30))))
             (let ((evidence (parent-launch-close launch)))
               (assert (getf evidence :native-scope-clean-p))
               (assert (eq (not uncertain-p) (getf evidence :graceful-stop-p))))
             (parent-launch-test--worker-report effect)
             (assert (= 1 (parent-launch-test--effect-count effect)))
             (assert (= 1 (parent-launch-test--record-count launch ':tool-started)))
             (assert (= (if uncertain-p 0 1) (parent-launch-test--record-count launch ':tool-finished)))
             (when after-close (funcall after-close 1 launch))
             (when after-first (funcall after-first launch))
             (setf launch (open-child "launch-fresh-resume-instance" "normal"))
             (assert (eq ':completed (parent-launch-turn-run launch "resume" "resume" :timeout 30)))
             (assert (getf (parent-launch-close launch) :graceful-stop-p))
             (parent-launch-test--worker-report effect)
             (when after-close (funcall after-close 2 launch))
             (assert (= (if resume-effect-p 2 1) (parent-launch-test--effect-count effect)))
             (assert (= (if resume-effect-p 2 1) (parent-launch-test--record-count launch ':tool-started)))
             (assert (= (cond (uncertain-p
                               0) (resume-effect-p
                                   2) (t
                                       1))
                        (parent-launch-test--record-count launch ':tool-finished)))
             (when third-p
               (setf launch (open-child "launch-revoked-instance" "normal"))
               (assert (eq ':completed (parent-launch-turn-run launch "revoked" "revoked" :timeout 30)))
               (assert (getf (parent-launch-close launch) :graceful-stop-p))
               (parent-launch-test--worker-report effect)
               (when after-close (funcall after-close 3 launch))
               (assert (= 3 (parent-launch-test--effect-count effect)))
               (assert (= 3 (parent-launch-test--record-count launch ':tool-started)))
               (assert (= 3 (parent-launch-test--record-count launch ':tool-finished)))))
           (multiple-value-bind (value reason) (sb-thread:join-thread server :timeout 5 :default nil)
             (declare (ignore value))
             (assert (null reason)))
           (assert (null server-error))
           (assert (= requests (cond (third-p
                                      6) (uncertain-p
                                          2) (resume-effect-p
                                              4) (t
                                                  3))))
           (assert (null (find-package '#:autolith)))
           1)
      (when launch (parent-launch-close launch :grace-seconds 0))
      (usocket:socket-close listener)
      (when (and server (sb-thread:thread-alive-p server))
        (sb-thread:join-thread server :timeout 35 :default nil)))))

(declaim (ftype (function (&key (:runtime pathname) (:core pathname) (:source-root pathname)
                               (:temporary pathname)) integer) run-parent-launch-e2e-tests))

(defun run-parent-launch-e2e-tests (&key runtime core source-root temporary)
  "Run real fresh-process checks against a previously built, source-matched fixture core."
  (assert (null (find-package '#:autolith)))
  (+ (parent-launch-test--fresh-process :runtime runtime :core core :source-root source-root
                                        :temporary temporary)
     (parent-launch-test--fresh-process :runtime runtime :core core :source-root source-root
                                        :temporary temporary :uncertain-p t)))

(declaim (ftype (function (pathname pathname) record-dispatcher) parent-launch-test--records))

(defun parent-launch-test--records (directory workspace)
  "Keep the real resource services and their claims in parent-private storage."
  (make-record-dispatcher
   :pathname (merge-pathnames "claims.sexp" directory) :session-id "launch-fixture"
   :agenda-service
   (make-agenda-service :session-id "launch-fixture"
                        :context    (make-instance 'agenda-context
                                                   :pathname         (merge-pathnames "agendas.sexp" directory)
                                                   :directory        (namestring workspace)
                                                   :memory-validator (constantly t)))
   :memory-service
   (make-memory-service :session-id "launch-fixture"
                        :context    (make-memory-store-context
                                     :memory-path    (merge-pathnames "memories.sexp" directory)
                                     :knowledge-path (merge-pathnames "knowledge.sexp" directory)
                                     :workspace      workspace))
   :papercut-service
   (make-papercut-service :session-id "launch-fixture"
                          :context    (make-instance 'papercut-context
                                                     :pathname  (merge-pathnames "papercuts.sexp" directory)
                                                     :workspace (namestring workspace)))))

(declaim (ftype (function (&key (:runtime pathname) (:core pathname) (:source-root pathname)
                               (:temporary pathname) (:record-service t) (:runtime-roots list)) integer)
                run-parent-launch-native-e2e-tests))

(defun run-parent-launch-native-e2e-tests (&key runtime core source-root temporary record-service runtime-roots)
  "Run real default workers under native policy, approve read access, and replace the child."
  (unless (uiop:os-macosx-p)
    (return-from run-parent-launch-native-e2e-tests 0))
  (let* ((workspace (merge-pathnames "workspace/" temporary))
         (private   (merge-pathnames "private/" temporary))
         (scratch   (merge-pathnames "scratch/" temporary))
         (secret    (merge-pathnames "secret" private))
         (outside   (merge-pathnames "outside" temporary))
         (effect    (merge-pathnames "effect" workspace))
         (starter nil) (application nil) (backend nil) (policy nil))
    (dolist (directory (list workspace private scratch))
      (ensure-directories-exist directory))
    (dolist (path (list secret outside))
      (with-open-file (stream path :direction ':output :if-exists ':supersede)
        (write-line "fixture data" stream)))
    (unless record-service
      (setf record-service (parent-launch-test--records private workspace)))
    (setf backend
          (make-native-access-backend
           :command       (parent-launch-test--child-command runtime core)
           :environment   (append
                           (parent-launch-test--environment :source-root source-root :temporary scratch :runtime runtime
                                                           :effect effect :mode "normal")
                           (list (concatenate 'string "AUTOLITH_LAUNCH_TEST_PRIVATE=" (namestring secret))
                                 (concatenate 'string "AUTOLITH_LAUNCH_TEST_OUTSIDE=" (namestring outside)))
                           (when record-service (list "AUTOLITH_LAUNCH_TEST_RECORDS=1")))
           :runtime-roots (append runtime-roots
                                  (list "/nix/store/" (namestring source-root)
                                        (namestring (uiop:pathname-directory-pathname core))))
           :temporary-root (namestring scratch) :startup-timeout 30
           :startup       (lambda (started)
                            (setf application started)
                            (funcall starter (native-access-application-process started)))))
    (setf policy
          (make-access-service
           (make-access-context
            :pathname (merge-pathnames "policy.sexp" private) :session-id "launch-fixture"
            :workspace (namestring workspace) :private-roots (list (namestring private))
            :backend  backend
            :terminal (make-parent-terminal
                       :input-source (lambda (seconds) (declare (ignore seconds)) (values "YES" ':line))
                       :renderer     (lambda (region kind payload) (declare (ignore region kind payload)))))))
    (parent-launch-test--fresh-process
     :runtime runtime :core core :source-root source-root :temporary workspace
     :storage-root private :resume-effect-p t :third-p t
     :launch-options
     (lambda (instance)
       (list
        :record-service record-service :access-service (make-access-session-service policy)
        :process-opener
        (lambda (command &key environment directory instance-id startup-timeout startup)
          (declare (ignore command environment directory startup-timeout))
          (assert (equal instance instance-id))
          (setf starter startup)
          (access-service-start policy instance-id)
          (native-access-application-process application))
        :process-closer
        (lambda (process)
          (assert (eq process (native-access-application-process application)))
          (access-service-stop policy)
          (getf (native-access-application-evidence application) :process-scope))
        :startup-policy
        (lambda (process)
          (assert (eq process (native-access-application-process application)))
          (values (access-application-revision application)
                  (mapcar #'access-policy--wire-grant (access-application-grants application))))))
     :after-close
     (lambda (index launch)
       (let ((report (parent-launch-test--worker-report effect)))
         (assert (= (length report) 5))
         (assert (null (second report)))
         (assert (eq (= index 2) (third report)))
         (assert (= -1 (fourth report)))
         (assert (= -1 (fifth report))))
       (assert (getf (getf (native-access-application-evidence application) :process-scope)
                     :native-scope-clean-p))
       (let* ((response (session-record-handle
                         record-service
                         (parent-proto:make-record-request :collection ':agenda :operation ':query
                                                          :query "" :limit 256)))
              (records  (parent-proto:record-response.records response)))
         (assert (= index (length records)))
         (assert (every (lambda (record)
                          (string= "Real child resource edit"
                                   (gethash "text" (protocol-map->data (parent-proto:stored-record.fields record)))))
                        records)))
       (when (= index 2)
         (access-service-revoke policy "native-read-grant")
         (access-service-project-notes policy (parent-launch-store launch))))
     :after-first
     (lambda (launch)
       (let ((decision
               (access-service-handle
                policy
                (parent-proto:make-access-request
                 :operation-id     "native-read-grant"
                 :requested-grants (list (parent-proto:make-grant
                                          :resource ':filesystem :target (namestring outside)
                                          :operations '("read") :duration ':session))))))
         (assert (eq ':approved-pending-restart (parent-proto:access-decision.state decision))))
       (access-service-project-notes policy (parent-launch-store launch))))))
