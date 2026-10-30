;;;; -- Standard Main-Core Production Launch Checks --

(in-package #:autolith-supervisor)

(declaim (ftype (function (string string hash-table) string) parent-launch-production--call))

(defun parent-launch-production--call (namespace name arguments)
  "Encode a real provider tool call with the ordinary default tool name."
  (let ((item     (make-hash-table :test #'equal))
        (identity (parent-application--identifier)))
    (setf (gethash "id" item) (format nil "fc_~A_~A_~A" namespace name identity)
          (gethash "type" item) "function_call"
          (gethash "call_id" item) (format nil "call_~A" identity)
          (gethash "name" item) name
          (gethash "namespace" item) namespace
          (gethash "arguments" item) (argo:json-encode arguments)
          (gethash "status" item) "completed")
    (argo:json-encode item)))

(declaim (ftype (function (&rest t) hash-table) parent-launch-production--object))

(defun parent-launch-production--object (&rest pairs)
  "Build a test-owned JSON object without changing the wire codec."
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by (lambda (tail) (rest (rest tail)))
          do (setf (gethash key object) value))
    object))

(declaim (ftype (function (string) string) parent-launch-production--revision))

(defun parent-launch-production--revision (body)
  "Find the actual resource observation alias in the next model request."
  (block found
    (labels ((visit (value)
               (typecase value
                 (string
                  (let ((start (search "Revision: " value)))
                    (when start
                      (incf start (length "Revision: "))
                      (return-from found
                        (subseq value start (or (position #\Newline value :start start) (length value)))))))
                 (hash-table
                  (maphash (lambda (key item) (declare (ignore key)) (visit item)) value))
                 (vector
                  (map nil #'visit value)))))
      (visit (argo:json-decode body)))
    (error "The real resource.read result has no revision.")))

(declaim (ftype (function (pathname pathname) string) parent-launch-production--worker-form))

(defun parent-launch-production--worker-form (effect secret)
  "Write a real worker effect and a private-root denial report through lisp.eval."
  (with-standard-io-syntax
    (write-to-string
     `(let ((readable (handler-case
                         (with-open-file (stream ,(namestring secret)) (read-char stream nil nil) t)
                       (file-error () nil))))
        (with-open-file (stream ,(namestring effect) :direction ':output :if-exists ':append
                                                   :if-does-not-exist ':create)
          (write-line "effect" stream))
        (with-open-file (stream ,(concatenate 'string (namestring effect) ".worker")
                                :direction ':output :if-exists ':supersede)
          (write (list (uiop:symbol-call '#:autolith '#:current-process-id) readable) :stream stream))
        "production worker committed"))))

(declaim (ftype (function (pathname pathname pathname) string) parent-launch-production--shell-command))

(defun parent-launch-production--shell-command (effect secret outside)
  "Create a shell effect while requiring native denial of private reads and outside writes."
  (format nil "printf 'effect\\n' >> ~A; if IFS= read -r line < ~A; then exit 43; fi; if printf forbidden > ~A; then exit 44; fi; printf 'shell scope verified\\n'"
          (uiop:escape-sh-token (namestring effect))
          (uiop:escape-sh-token (namestring secret))
          (uiop:escape-sh-token (namestring outside))))

(declaim (ftype (function (t string) string) parent-launch-production--failure))

(defun parent-launch-production--failure (condition token)
  "Describe a fixture failure without copying its authentication token."
  (if condition
      (let* ((text (princ-to-string condition))
             (safe (with-output-to-string (stream)
                     (loop with offset = 0
                           for match = (search token text :start2 offset)
                           do (write-string text stream :start offset :end match)
                           while match
                           do (write-string "[redacted]" stream)
                              (setf offset (+ match (length token)))))))
        (subseq safe 0 (min 1024 (length safe))))
      "none"))

(declaim (ftype (function (&key (:runtime pathname) (:core pathname) (:source-root pathname)
                               (:temporary pathname) (:access-p boolean) (:runtime-roots list)) integer)
                parent-launch-production--run))

(defun parent-launch-production--run (&key runtime core source-root temporary access-p runtime-roots)
  "Boot the standard main core through the production parent, real HTTP and default tools."
  (assert (null (find-package '#:autolith)))
  (let* ((workspace    (merge-pathnames "workspace/" temporary))
         (private      (merge-pathnames "private/" temporary))
         (scratch      (merge-pathnames "scratch/" temporary))
         (secret       (merge-pathnames "parent-only" private))
         (effect       (merge-pathnames "production-effect" workspace))
         (shell-effect (merge-pathnames "production-shell-effect" workspace))
         (outside      (merge-pathnames "outside-forbidden" temporary))
         (key-path     (merge-pathnames "accounts/api-keys.sexp" private))
         (listener     (usocket:socket-listen "127.0.0.1" 0 :reuse-address t :element-type '(unsigned-byte 8)))
         (token        (parent-application--identifier))
         (configuration nil) (application nil) (launch nil) (server nil) (server-error nil)
         (requests     0))
    (dolist (directory (list workspace private scratch)) (ensure-directories-exist directory))
    (with-open-file (stream secret :direction ':output :if-exists ':supersede)
      (write-line "parent-only fixture data" stream))
    (cl-rfc8628:credential-source-save
     (make-instance 'api-key-credential-source :pathname key-path :provider-name "openai")
     (make-instance 'cl-rfc8628:oauth-credentials :access-token token :expires-at nil))
    (setf configuration
          (make-parent-configuration
           (list :version 1 :runtime (namestring runtime) :agent-core (namestring core)
                 :worker-core (namestring core) :source-root (namestring source-root)
                 :workspace (namestring workspace) :private-root (namestring private)
                 :temporary-root        (namestring scratch)
                 :runtime-roots         (append runtime-roots
                                                (list "/nix/store/" (namestring source-root)
                                                      (namestring (uiop:pathname-directory-pathname core))))
                 :account-configuration (list :state-root    (namestring (merge-pathnames "accounts/" private))
                                              :api-keys-path (namestring key-path))
                 :profiles              (list (list :id "ordinary" :family ':openai-api :protocol ':responses
                                                    :model "fixture-model" :provider-name "openai"
                                                    :endpoint (format nil "http://127.0.0.1:~D/responses"
                                                                      (usocket:get-local-port listener))))
                 :profile-id "ordinary" :access-p access-p :reasoning-effort "none"))
          application
          (make-parent-application
           configuration :session-id "production-fixture"
           :input-source             (lambda (seconds) (declare (ignore seconds)) (values nil ':eof))
           :renderer                 (lambda (region kind payload) (declare (ignore region kind payload)))))
    (unwind-protect
         (progn
           (setf server
                 (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                      (dotimes (index 7)
                          (let ((socket (sb-ext:with-timeout 60
                                          (loop until (usocket:wait-for-input listener :timeout 0.1 :ready-only t))
                                          (usocket:socket-accept listener :element-type '(unsigned-byte 8)))))
                            (unwind-protect
                                 (sb-ext:with-timeout 60
                                   (let* ((body (parent-launch-test--http-request socket token))
                                          (item
                                            (case index
                                             ((0 5)
                                              (parent-launch-production--call
                                               "resource" "read"
                                               (parent-launch-production--object "uri" "agenda:current")))
                                             (1
                                              (parent-launch-production--call
                                               "resource" "edit"
                                               (parent-launch-production--object
                                                "uri" "agenda:current"
                                                "base-revision" (parent-launch-production--revision body)
                                                "operations" (vector (parent-launch-production--object
                                                                      "op" "agenda-add" "text" "Production resource effect"
                                                                      "status" "note")))))
                                             (2
                                              (parent-launch-production--call
                                               "lisp" "eval"
                                               (parent-launch-production--object
                                                "forms" (vector (parent-launch-production--worker-form effect secret)))))
                                             (3
                                              (parent-launch-production--call
                                               "shell" "run"
                                               (parent-launch-production--object
                                                "command" (parent-launch-production--shell-command shell-effect secret outside))))
                                             (otherwise
                                              (parent-launch-test--message-item)))))
                                     (assert (not (search token body)))
                                     (when (= index 3) (assert (search "production worker committed" body)))
                                     (when (= index 4) (assert (search "shell scope verified" body)))
                                     (when (= index 6) (assert (search "Production resource effect" body)))
                                     (incf requests)
                                     (parent-launch-test--http-reply socket (parent-launch-test--sse item))))
                              (usocket:socket-close socket))))
                      (error (condition) (setf server-error condition))))
                  :name "production fixture HTTP"))
           (dotimes (index 2)
             (setf launch (parent-application-open application))
             (let ((outcome (parent-launch-turn-run
                             launch (format nil "production-~D" index)
                             (if (zerop index) "Use the real production tools." "Resume the durable session.")
                             :timeout 60)))
               (assert (eq ':completed outcome) ()
                       "Production turn ~D failed: ~A; HTTP requests: ~D; fixture failure: ~A."
                       index outcome requests (parent-launch-production--failure server-error token)))
             (let ((evidence (parent-launch-close launch)))
               (assert (getf evidence :graceful-stop-p))
               (assert (getf evidence :native-scope-clean-p)))
             (assert (= 1 (parent-launch-test--effect-count effect)))
             (assert (= 1 (parent-launch-test--effect-count shell-effect)))
             (assert (not (probe-file outside)))
             (let ((report (parent-launch-test--worker-report effect)))
               (assert (= 2 (length report)))
               (assert (null (second report)))))
           (multiple-value-bind (value reason) (sb-thread:join-thread server :timeout 5 :default nil)
             (declare (ignore value))
             (assert (null reason)))
           (when server-error (error server-error))
           (assert (= 7 requests))
           (assert (= 5 (parent-launch-test--record-count launch ':tool-started)))
           (assert (= 5 (parent-launch-test--record-count launch ':tool-finished)))
           (let* ((service  (parent-application-records application))
                  (response (session-record-handle
                             service (parent-proto:make-record-request :collection ':agenda :operation ':query
                                                                      :query "" :limit 256))))
             (assert (= 1 (length (parent-proto:record-response.records response)))))
           (assert (null (find-package '#:autolith)))
           1)
      (when launch (parent-launch-close launch :grace-seconds 0))
      (usocket:socket-close listener)
      (when (and server (sb-thread:thread-alive-p server))
        (sb-thread:join-thread server :timeout 65 :default nil)))))

(declaim (ftype (function (&key (:runtime pathname) (:core pathname) (:source-root pathname)
                               (:temporary pathname) (:runtime-roots list)) integer)
                run-parent-launch-production-e2e-tests))

(defun run-parent-launch-production-e2e-tests (&key runtime core source-root temporary runtime-roots)
  "Exercise real production resource, Lisp and shell tools with access requests enabled and disabled."
  (+ (parent-launch-production--run :runtime runtime :core core :source-root source-root
                                   :runtime-roots runtime-roots
                                   :temporary (merge-pathnames "without-access/" temporary) :access-p nil)
     (parent-launch-production--run :runtime runtime :core core :source-root source-root
                                   :runtime-roots runtime-roots
                                   :temporary (merge-pathnames "with-access/" temporary) :access-p t)))
