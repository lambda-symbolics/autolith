;;;; -- Trusted Parent Provider Extension Checks --

(in-package #:autolith-supervisor)

(defvar *parent-provider-fixture-events* nil
  "Parent-only callback execution evidence for extension checks.")

(defvar *parent-provider-fixture-endpoint* "https://fixture.invalid/one"
  "Discovery cache identity controlled by the extension fixture.")

(defvar *parent-provider-fixture-discovery-fails-p* nil
  "Select the fixture's discovery failure path.")

(defclass parent-provider-fixture (cl-llm-provider-api:model-provider)
  ((configuration :initarg :configuration :reader parent-provider-fixture-configuration
                  :documentation "Private fixture configuration, including its synthetic token."))
  (:documentation "Executable custom provider used only in trusted parent tests."))

(serapeum/types:-> parent-provider-fixture--record (keyword) null)
(defun parent-provider-fixture--record (kind)
  "Record the callback kind and actual process identity without its private data."
  (push (list kind (ls-compat.posix:current-process-id)) *parent-provider-fixture-events*)
  nil)

(defmethod cl-llm-provider-api:provider-stream-turn
    ((provider parent-provider-fixture) conversation
     &key tool-namespaces event-callback goal-context compaction-p)
  "Execute a real parent callback and emit canonical, credential-free model output."
  (declare (ignore conversation tool-namespaces goal-context compaction-p))
  (parent-provider-fixture--record ':transport)
  (write-string (getf (parent-provider-fixture-configuration provider) :secret))
  (let ((item    (make-hash-table :test #'equal))
        (content (make-hash-table :test #'equal)))
    (setf (gethash "type" item) "message"
          (gethash "role" item) "assistant"
          (gethash "type" content) "output_text"
          (gethash "text" content) "parent extension completed"
          (gethash "content" item) (vector content))
    (when event-callback
      (funcall event-callback (make-instance 'cl-llm-provider-api:provider-item-event :item item))
      (funcall event-callback
               (make-instance 'cl-llm-provider-api:provider-completed-event :turn-completion ':end)))
    (make-instance 'cl-llm-provider-api:provider-result :output-items (list item) :turn-completion ':end)))

(serapeum/types:-> parent-provider-fixture--write (pathname list) pathname)
(defun parent-provider-fixture--write (path forms)
  "Write executable test extension forms to an owner-selected trusted file."
  (ensure-directories-exist path)
  (with-open-file (stream path :direction ':output :if-exists ':supersede)
    (with-standard-io-syntax
      (let ((*package* (find-package '#:autolith-supervisor)) (*print-case* ':downcase))
        (dolist (form forms) (write form :stream stream) (terpri stream)))))
  path)

(serapeum/types:-> parent-provider-fixture--forms () list)
(defun parent-provider-fixture--forms ()
  "Return the executable custom factory, authentication and discovery registration."
  '((parent-provider-fixture--record ':load)
    (register-parent-provider
     "fixture" :models '("fixed") :protocol ':responses
     :factory         (lambda (configuration &key reasoning-summaries-p)
                        (declare (ignore reasoning-summaries-p))
                        (parent-provider-fixture--record ':factory)
                        (write-string (getf configuration :secret))
                        (make-instance 'parent-provider-fixture :configuration configuration))
     :authenticator   (lambda (provider &key stream open-browser-p)
                        (declare (ignore provider open-browser-p))
                        (parent-provider-fixture--record ':auth)
                        (write-line "parent authentication completed" stream))
     :model-discovery (lambda (configuration)
                        (parent-provider-fixture--record ':discovery)
                        (when *parent-provider-fixture-discovery-fails-p*
                          (error "~A" (getf configuration :secret)))
                        '("dynamic"))
     :model-discovery-endpoint-resolver
     (lambda ()
       (parent-provider-fixture--record ':endpoint)
       *parent-provider-fixture-endpoint*))))

(serapeum/types:-> run-parent-provider-extension-tests (pathname) integer)
(defun run-parent-provider-extension-tests (temporary)
  "Check explicit trusted loading, source layers, cache identity and private callbacks."
  (let* ((trusted                                     (merge-pathnames "trusted/extension.lisp" temporary))
         (workspace                                   (merge-pathnames "workspace/" temporary))
         (scratch                                     (merge-pathnames "scratch/" temporary))
         (site                                        (merge-pathnames "site/" temporary))
         (private                                     (merge-pathnames "private/" temporary))
         (roots                                       (list workspace scratch site private))
         (configuration                               '(:secret "fixture-private-token"))
         (cache nil) (checks 0)
         (*parent-provider-fixture-events*            nil)
         (*parent-provider-fixture-endpoint*          "https://fixture.invalid/one")
         (*parent-provider-fixture-discovery-fails-p* nil)
         (registry
           (make-parent-provider-extension-registry
            :cache-read-function  (lambda (configuration) (declare (ignore configuration)) cache)
            :cache-write-function (lambda (configuration form)
                                    (declare (ignore configuration)) (setf cache form)))))
    (dolist (root roots) (ensure-directories-exist root))
    (labels ((check (value)
               "Count an assertion at its behavioral boundary."
               (incf checks)
               (assert value))

             (fails-p (function reason)
               "Check a fixed failure category without printing callback data."
               (handler-case (progn (funcall function) nil)
                 (parent-provider-extension-error (condition)
                   (and (eq reason (parent-provider-extension-error-reason condition))
                        (not (search (getf configuration :secret) (princ-to-string condition)))))))

             (models ()
               "Read the inert model-name projection only."
               (mapcar (lambda (metadata) (getf metadata :name))
                       (getf (first (parent-provider-projection registry)) :models))))
      (check (fails-p (lambda () (register-parent-provider "forbidden"))
                      ':registration-outside-parent-extension))
      (parent-provider-fixture--write trusted (parent-provider-fixture--forms))
      (check (equal (parent-provider-load-extensions
                     registry (list (list :source ':user :path (namestring trusted)))
                     :writable-roots roots)
                    (list (ls-compat.posix:resolve-pathname trusted))))
      (check (equal (models) '("fixed")))
      (check (null (parent-provider-refresh-models registry configuration)))
      (check (equal (models) '("fixed" "dynamic")))
      (check cache)
      (let ((output (make-string-output-stream)) (events nil))
        (let ((*standard-output* output) (*error-output* output) (*trace-output* output))
          (check (typep (parent-provider-create registry "fixture" configuration)
                        'parent-provider-fixture))
          (check (typep (parent-provider-stream-turn
                         registry "fixture" :configuration configuration :conversation nil
                         :event-callback (lambda (event) (push event events)))
                       'cl-llm-provider-api:provider-result)))
        (check (= 2 (length events)))
        (check (not (search (getf configuration :secret) (get-output-stream-string output)))))
      (let ((output (make-string-output-stream)))
        (check (parent-provider-authenticate registry "fixture" configuration :stream output
                                             :open-browser-p                          nil))
        (check (search "parent authentication completed" (get-output-stream-string output))))
      (let ((*parent-provider-fixture-discovery-fails-p* t))
        (check (equal (parent-provider-refresh-models registry configuration)
                      '((:reason :model-discovery-failed)))))
      (check (equal (models) '("fixed" "dynamic")))
      (check (not (search (getf configuration :secret) (prin1-to-string (parent-provider-projection registry)))))
      (let ((*parent-provider-extension-registry* registry)
            (*parent-provider-extension-source*   ':runtime))
        (register-parent-provider "fixture" :models '("runtime")
                                  :factory          (lambda (configuration &key reasoning-summaries-p)
                                                      (declare (ignore reasoning-summaries-p))
                                                      (make-instance 'parent-provider-fixture :configuration configuration)))
        (check (equal (models) '("runtime")))
        (check (unregister-parent-provider "fixture"))
        (check (equal (models) '("fixed" "dynamic"))))
      (parent-provider-load-extensions registry (list (list :source ':user :path (namestring trusted)))
                                       :writable-roots roots)
      (parent-provider-load-model-cache registry configuration)
      (check (equal (models) '("fixed" "dynamic")))
      (let ((*parent-provider-fixture-endpoint* "https://fixture.invalid/two"))
        (parent-provider-load-extensions registry (list (list :source ':user :path (namestring trusted)))
                                         :writable-roots roots)
        (parent-provider-load-model-cache registry configuration)
        (check (equal (models) '("fixed"))))
      (let ((before (parent-provider-projection registry))
            (broken (merge-pathnames "trusted/broken.lisp" temporary)))
        (parent-provider-fixture--write
         broken '((register-parent-provider "partial" :models '("partial")
                                            :factory          (lambda (configuration &key reasoning-summaries-p)
                                                                (declare (ignore configuration reasoning-summaries-p))
                                                                (make-instance 'parent-provider-fixture)))
                  (error "fixture-private-token")))
        (check (fails-p
                (lambda ()
                  (parent-provider-load-extensions registry (list (list :source ':user :path (namestring broken)))
                                                   :writable-roots roots))
                ':extension-load-failed))
        (check (equal before (parent-provider-projection registry))))
      (dolist (root roots)
        (let ((forbidden (merge-pathnames "extension.lisp" root))
              (before    (copy-tree *parent-provider-fixture-events*)))
          (parent-provider-fixture--write forbidden '((parent-provider-fixture--record ':forbidden)))
          (check (fails-p
                  (lambda ()
                    (parent-provider-load-extensions
                     registry (list (list :source ':user :path (namestring forbidden))) :writable-roots roots))
                  ':extension-in-child-writable-scope))
          (check (equal before *parent-provider-fixture-events*))))
      (check (every (lambda (event) (= (second event) (ls-compat.posix:current-process-id)))
                    *parent-provider-fixture-events*))
      (dolist (kind '(:load :factory :auth :discovery :endpoint :transport))
        (check (find kind *parent-provider-fixture-events* :key #'first)))
      checks)))
