(in-package #:autolith-supervisor)

;;;; -- Knowledge Service Boundary Checks --

(serapeum/types:-> knowledge-test-request
    (keyword &key (:fields (option hash-table)) (:view string) (:revision (option string))
                  (:operation-id string) (:scope keyword)) parent-proto:knowledge-request)
(defun knowledge-test-request (operation &key fields (view "") revision (operation-id "")
                                             (scope ':workspace))
  "Build a public request with the same portable conversion as the wire."
  (let ((request (parent-proto:make-knowledge-request
                  :operation operation :operation-id operation-id :scope scope :view view)))
    (when fields
      (setf (parent-proto:knowledge-request.fields request) (protocol-data->map fields)))
    (when revision
      (setf (parent-proto:knowledge-request.expected-revision request) revision))
    request))

(serapeum/types:-> run-knowledge-service-tests () integer)
(defun run-knowledge-service-tests ()
  "Exercise knowledge authority, graph guards, atomic receipts and reopen."
  (let* ((root
          (merge-pathnames (format nil "knowledge-service-~A/" (memory--make-identifier))
                           (uiop/stream:temporary-directory)))
         (workspace (merge-pathnames "workspace/" root))
         (path      (merge-pathnames "private/knowledge.sexp" root))
         (context   nil)
         (checks    0))
    (ensure-directories-exist (merge-pathnames "file.txt" workspace))
    (ensure-directories-exist path)
    (setf context
            (make-memory-store-context :memory-path (merge-pathnames "private/memory.sexp" root)
                                       :knowledge-path path :workspace workspace))
    (unwind-protect
        (labels ((check (value)
                   (incf checks)
                   (assert value))

                 (rejects (code function)
                   (handler-case (progn (funcall function) nil)
                                 (knowledge-service-error (condition)
                                  (eq code (knowledge-service-error-code condition)))))

                 (service (&key global approve)
                   (make-knowledge-service :context context :session-id "knowledge-test"
                    :allow-global-p global :approve (or approve (constantly nil)) :input-history
                    (lambda () '("The server is ready."))))

                 (read-view (service &optional (view "facts") (scope ':workspace))
                   (knowledge-service-request service
                    (knowledge-test-request ':read :view view :scope scope)))

                 (edit (service operations id &key revision (scope ':workspace))
                   (knowledge-service-request service
                    (knowledge-test-request ':edit :view "facts" :scope scope :operation-id id
                     :revision
                     (or revision
                         (parent-proto:knowledge-response.revision
                          (read-view service "facts" scope)))
                     :fields (argo:json-object "operations" operations))))

                 (bytes ()
                   (and (probe-file path) (uiop/stream:read-file-string path))))
          (let* ((first   (service))
                 (other   (service))
                 (initial (parent-proto:knowledge-response.revision (read-view first)))
                 (schema
                  (argo:json-object "op" "predicate-define" "predicate" "server.ready" "arguments"
                                    (vector (argo:json-object "name" "server" "type" "entity"))
                                    "template" "server {server} is ready"))
                 (fact
                  (argo:json-object "op" "fact-assert" "predicate" "server.ready" "arguments"
                                    #("alpha") "sources"
                                    (vector (argo:json-object "user" "The server is ready."))))
                 (request
                  (knowledge-test-request ':edit :view "facts" :operation-id "schema-and-fact"
                   :revision initial :fields (argo:json-object "operations" (vector schema fact))))
                 (result  (knowledge-service-request first request)))
            (check (search "Asserted" (parent-proto:knowledge-response.content result)))
            (check (= 1 (hash-table-count (knowledge-base-claims (knowledge-base-read context)))))
            (check
             (= 1
                (hash-table-count
                 (knowledge-base-operation-receipts (knowledge-base-read context)))))
            (check (search "alpha" (parent-proto:knowledge-response.content (read-view first))))
            (let ((before (bytes)))
              (check
               (equalp (cl-protobufs:serialize-to-bytes result)
                       (cl-protobufs:serialize-to-bytes
                        (knowledge-service-request (service) request))))
              (check (equal before (bytes)))
              (let ((different
                     (cl-protobufs:deserialize-from-bytes 'parent-proto:knowledge-request
                                                          (cl-protobufs:serialize-to-bytes
                                                           request))))
                (setf (parent-proto:knowledge-request.expected-revision different) "different")
                (check
                 (rejects ':invalid-request
                  (lambda () (knowledge-service-request (service) different))))
                (check (equal before (bytes)))))
            (let* ((stale (parent-proto:knowledge-response.revision (read-view other)))
                   (new-fact
                    (argo:json-object "op" "fact-assert" "predicate" "server.ready" "arguments"
                                      #("beta"))))
              (edit first (vector new-fact) "other-write")
              (let ((before (bytes)))
                (check
                 (rejects ':revision-conflict
                  (lambda () (edit other (vector fact) "stale-write" :revision stale))))
                (check (equal before (bytes)))
                (check
                 (null
                  (gethash '("knowledge-test" "stale-write")
                           (knowledge-base-operation-receipts (knowledge-base-read context)))))))
            (let ((before (bytes)))
              (check (rejects ':permission-denied (lambda () (read-view first "facts" ':global))))
              (check
               (rejects ':permission-denied
                (lambda ()
                  (edit first
                   (vector
                    (argo:json-object "op" "fact-assert" "predicate" "server.ready" "arguments"
                                      #("private") "scope" "global"))
                   "forged-global"))))
              (check (equal before (bytes))))
            (let ((before (bytes))
                  (new-schema
                   (argo:json-object "op" "predicate-define" "predicate" "rollback.only"
                                     "arguments"
                                     (vector (argo:json-object "name" "item" "type" "entity")))))
              (check
               (rejects ':unsupported
                (lambda ()
                  (edit first (vector new-schema (argo:json-object "op" "eval" "form" "(+ 1 2)"))
                   "atomic-failure"))))
              (check (equal before (bytes)))
              (check
               (null
                (knowledge-base-schema (knowledge-base-read context) "rollback.only" :workspace
                                       (knowledge--workspace context)))))
            (let* ((global (service :global t))
                   (global-schema
                    (argo:json-object "op" "predicate-define" "predicate" "global.only" "arguments"
                                      (vector (argo:json-object "name" "item" "type" "entity")))))
              (edit global
               (vector global-schema
                       (argo:json-object "op" "fact-assert" "predicate" "global.only" "arguments"
                                         #("secret")))
               "global-create" :scope ':global)
              (check
               (search "secret"
                       (parent-proto:knowledge-response.content
                        (read-view global "facts" ':global))))
              (check
               (not (search "secret" (parent-proto:knowledge-response.content (read-view first)))))
              (check
               (not
                (search "global.only"
                        (parent-proto:knowledge-response.content
                         (read-view first "predicates"))))))
            (let* ((base (knowledge-base-read context))
                   (claim
                    (first
                     (loop for id being the hash-keys of (knowledge-base-claims base)
                           for entry = (gethash id (knowledge-base-claims base))
                           when (and (eq (getf entry :scope) ':workspace)
                                     (equal
                                      (knowledge-proposition-arguments (getf entry :proposition))
                                      '("alpha")))
                           collect id))))
              (check
               (search "alpha"
                       (parent-proto:knowledge-response.content
                        (read-view first (format nil "claim/~A" claim)))))
              (dolist (view '("predicates" "rules" "methods" "contracts" "procedures" "changes"))
                (check (stringp (parent-proto:knowledge-response.content (read-view first view)))))
              (edit first
               (vector
                (argo:json-object "op" "method-define" "id" "prepare-server" "task"
                                  #("prepare" "?server") "steps"
                                  (vector (argo:json-object "step" "Start {?server}"))))
               "method")
              (check
               (search "prepare-server"
                       (parent-proto:knowledge-response.content (read-view first "methods"))))
              (edit first
               (vector
                (argo:json-object "op" "predicate-define" "predicate" "copy.ready" "arguments"
                                  (vector (argo:json-object "name" "server" "type" "entity"))))
               "copy-schema")
              (let ((rule
                     (argo:json-object "op" "rule-propose" "id" "copy-ready" "premises"
                                       #(#("server.ready" "?server")) "conclusion"
                                       #("copy.ready" "copy"))))
                (edit first (vector rule) "rule-proposal")
                (check
                 (not (search "copy" (parent-proto:knowledge-response.content (read-view first)))))
                (let ((approved 0)
                      (trusted
                       (service :approve
                        (lambda (kind scope id version proposal)
                          (declare (ignore scope id version))
                          (when (and (eq kind ':rule) (consp proposal)) t)))))
                  (declare (ignore approved))
                  (edit trusted (vector rule) "rule-approved")
                  (check
                   (search "copy" (parent-proto:knowledge-response.content (read-view trusted))))))
              (let ((before (bytes)))
                (check
                 (rejects ':invalid-request
                  (lambda ()
                    (edit first
                     (vector
                      (argo:json-object "op" "fact-assert" "predicate" "server.ready" "arguments"
                                        #("fake-user") "sources"
                                        (vector (argo:json-object "user" "I did not say this."))))
                     "fake-user"))))
                (check (equal before (bytes))))
              (let ((global (service :global t)))
                (edit global (vector schema) "global-schema" :scope ':global)
                (multiple-value-bind (claim support)
                    (knowledge-assert context '("server.ready" "global-owner")
                                      :scope ':global :basis ':assumption)
                  (check (search "global-owner" (parent-proto:knowledge-response.content
                                                 (read-view global))))
                  (dolist (id (list claim support))
                    (let ((before (bytes)))
                      (check (rejects ':permission-denied
                                      (lambda ()
                                        (edit global
                                              (vector (argo:json-object "op" "fact-retract" "id" id))
                                              (format nil "deny-global-~A" id)))))
                      (check (equal before (bytes)))))
                  (edit global (vector (argo:json-object "op" "fact-retract" "id" support))
                        "retract-global-support" :scope ':global)
                  (check (getf (knowledge-base-support (knowledge-base-read context) support)
                               :retracted-at)))
                (dolist (by-support-p '(t nil))
                  (multiple-value-bind (claim support)
                      (knowledge-assert context
                                        (list "server.ready" (if by-support-p "local-support" "local-claim"))
                                        :basis ':assumption)
                    (let ((id (if by-support-p support claim)))
                      (check (rejects ':permission-denied
                                      (lambda ()
                                        (edit global
                                              (vector (argo:json-object "op" "fact-retract" "id" id))
                                              (format nil "deny-workspace-~A" id) :scope ':global))))
                      (edit global (vector (argo:json-object "op" "fact-retract" "id" id))
                            (format nil "retract-workspace-~A" id))
                      (check (getf (knowledge-base-support (knowledge-base-read context) support)
                                   :retracted-at))))))
              (let* ((trusted  (service :approve (constantly t)))
                     (contract (argo:json-object
                                "op" "contract-define" "id" "inspect-ready" "tool" "probe.run"
                                "observes" (vector (argo:json-object
                                                    "predicate" "server.ready"
                                                    "arguments" #("$result.server"))))))
                (edit trusted (vector contract) "contract-proposal")
                (let ((claims-before (hash-table-count (knowledge-base-claims (knowledge-base-read context)))))
                  (dotimes (index 70)
                    (let* ((checked (knowledge-service-request
                                     trusted (knowledge-test-request ':check :fields
                                              (argo:json-object "tool" "probe.run" "arguments"
                                                                (make-hash-table :test #'equal)))))
                           (data    (protocol-value->data
                                     (common-proto:make-value :map-value
                                                              (parent-proto:knowledge-response.fields checked))))
                           (request (knowledge-test-request
                                     ':record :operation-id (format nil "abandon-~D" index)
                                     :fields                (argo:json-object "handle" (gethash "handle" data)
                                                                             "outcome" "abandoned")))
                           (result  (knowledge-service-request trusted request)))
                      (check (zerop (hash-table-count (knowledge-service-handles trusted))))
                      (when (zerop index)
                        (let ((before (bytes)))
                          (check (equalp (cl-protobufs:serialize-to-bytes result)
                                         (cl-protobufs:serialize-to-bytes
                                          (knowledge-service-request (service) request))))
                          (check (equal before (bytes)))))))
                  (check (= claims-before
                            (hash-table-count (knowledge-base-claims (knowledge-base-read context))))))
                (let* ((checked (knowledge-service-request
                                 trusted (knowledge-test-request ':check :fields
                                          (argo:json-object "tool" "probe.run" "arguments"
                                                            (make-hash-table :test #'equal)))))
                       (data    (protocol-value->data
                                 (common-proto:make-value :map-value
                                                          (parent-proto:knowledge-response.fields checked))))
                       (handle  (gethash "handle" data))
                       (request (knowledge-test-request ':record :operation-id "record-result"
                                 :fields                                       (argo:json-object "handle" handle "result" "{\"server\":\"observed\"}"))))
                  (check (stringp handle))
                  (let ((result (knowledge-service-request trusted request))
                        (before (bytes)))
                    (check (zerop (hash-table-count (knowledge-service-handles trusted))))
                    (check (search "observed" (parent-proto:knowledge-response.content
                                               (read-view trusted))))
                    (setf before (bytes))
                    (check (equalp (cl-protobufs:serialize-to-bytes result)
                                   (cl-protobufs:serialize-to-bytes
                                    (knowledge-service-request (service) request))))
                    (check (equal before (bytes)))
                    (let ((again (knowledge-test-request ':record :operation-id "record-again"
                                  :fields                                       (argo:json-object "handle" handle "result" "{}"))))
                      (check (rejects ':permission-denied
                                      (lambda () (knowledge-service-request trusted again)))))
                    (check (equal before (bytes)))))
                (let* ((checked (knowledge-service-request
                                 trusted (knowledge-test-request ':check :fields
                                          (argo:json-object "tool" "unknown.run" "arguments"
                                                            (make-hash-table :test #'equal)))))
                       (data    (protocol-value->data
                                 (common-proto:make-value :map-value
                                                          (parent-proto:knowledge-response.fields checked)))))
                  (check (eq t (gethash "none" data)))
                  (check (zerop (hash-table-count (knowledge-service-handles trusted))))))
              (check
               (search "alpha" (parent-proto:knowledge-response.content (read-view (service)))))
              (check (plusp checks)))))
      (dolist (directory (list (merge-pathnames "private/" root) workspace))
        (dolist (file (uiop/filesystem:directory-files directory)) (delete-file file))
        (when (probe-file directory) (uiop/filesystem:delete-empty-directory directory)))
      (when (probe-file root) (uiop/filesystem:delete-empty-directory root)))
    checks))
