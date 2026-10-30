;;;; -- Executable Parent Provider Native Child Check --

(in-package #:autolith-supervisor)

(defclass parent-provider-native-fixture (parent-provider-fixture) ()
  (:documentation "Trusted custom callback which requests one real child isolation probe."))

(serapeum/types:-> parent-provider-native--record (pathname keyword) null)
(defun parent-provider-native--record (marker kind)
  "Append callback process evidence to parent-private storage."
  (with-open-file (stream marker :direction ':output :if-exists ':append :if-does-not-exist ':create)
    (with-standard-io-syntax
      (write (list kind (ls-compat.posix:current-process-id)) :stream stream)
      (terpri stream)))
  nil)

(serapeum/types:-> parent-provider-native--probe (pathname pathname pathname) string)
(defun parent-provider-native--probe (secret plugin evidence)
  "Make a real lisp.eval submission that proves private input stays outside the child."
  (with-standard-io-syntax
    (let ((*package* (find-package '#:autolith-supervisor)))
      (write-to-string
       `(let ((denied
                (every
                 (lambda (path)
                   (handler-case (with-open-file (stream path) (read-char stream nil nil) nil)
                     (file-error () t)))
                 ',(list (namestring secret) (namestring plugin)))))
          (assert denied)
          (let* ((package (find-package "AUTOLITH-SUPERVISOR"))
                 (symbol  (and package (find-symbol "REGISTER-PARENT-PROVIDER" package))))
            (assert (or (null symbol) (not (fboundp symbol)))))
          (with-open-file (stream ,(namestring evidence) :direction ':output :if-exists ':supersede)
            (write :child-private-denied :stream stream))
          "child-private-denied")))))

(defmethod cl-llm-provider-api:provider-stream-turn
    ((provider parent-provider-native-fixture) (projection cl-llm-provider-api:wire-request)
     &key tool-namespaces event-callback goal-context compaction-p)
  "Request a real child tool call, then verify its result without exposing the private token."
  (declare (ignore tool-namespaces goal-context compaction-p))
  (let* ((configuration (parent-provider-fixture-configuration provider))
         (secret        (getf configuration :secret))
         (state         (getf configuration :state))
         (first-p       (= (incf (first state)) 1))
         (items-json    (argo:json-encode (coerce (cl-llm-provider-api:wire-request-items projection) 'vector)))
         (item          (make-hash-table :test #'equal)))
    (parent-provider-native--record (getf configuration :marker) ':transport)
    (assert (not (search secret items-json)))
    (if first-p
        (let ((arguments (make-hash-table :test #'equal)))
          (setf (gethash "forms" arguments)
                (vector (parent-provider-native--probe
                         (getf configuration :private-file) (getf configuration :plugin-file)
                         (getf configuration :evidence)))
                (gethash "id" item) "fc_parent_extension_probe"
                (gethash "type" item) "function_call"
                (gethash "call_id" item) "call_parent_extension_probe"
                (gethash "name" item) "eval"
                (gethash "namespace" item) "lisp"
                (gethash "arguments" item) (argo:json-encode arguments)
                (gethash "status" item) "completed"))
        (let ((content (make-hash-table :test #'equal)))
          (assert (search "child-private-denied" items-json))
          (setf (gethash "type" item) "message"
                (gethash "role" item) "assistant"
                (gethash "type" content) "output_text"
                (gethash "text" content) "parent callback and child scope verified"
                (gethash "content" item) (vector content))))
    (funcall event-callback (make-instance 'cl-llm-provider-api:provider-item-event :item item))
    (funcall event-callback
             (make-instance 'cl-llm-provider-api:provider-completed-event
                            :turn-completion (if first-p ':continue ':end)))
    (make-instance 'cl-llm-provider-api:provider-result
                   :output-items (list item) :tool-calls (and first-p (list item))
                   :turn-completion (if first-p ':continue ':end))))

(serapeum/types:-> parent-provider-native--forms
    (&key (:secret pathname) (:plugin pathname) (:marker pathname) (:evidence pathname)) list)
(defun parent-provider-native--forms (&key secret plugin marker evidence)
  "Return a complete executable parent plugin. Its closures capture only parent-private values."
  `((let ((token (string-trim '(#\Newline #\Return) (uiop:read-file-string ,secret)))
          (state (list 0)))
      (parent-provider-native--record ,marker ':load)
      (register-parent-provider
       "parent-fixture" :protocol ':responses
       :factory
       (lambda (configuration &key reasoning-summaries-p)
         (declare (ignore configuration reasoning-summaries-p))
         (parent-provider-native--record ,marker ':factory)
         (make-instance 'parent-provider-native-fixture
                        :configuration (list :secret token :state state :marker ,marker
                                             :private-file ,secret :plugin-file ,plugin :evidence ,evidence)))
       :authenticator
       (lambda (provider &key stream open-browser-p)
         (declare (ignore provider stream open-browser-p))
         (parent-provider-native--record ,marker ':auth)
         t)
       :model-discovery
       (lambda (configuration)
         (declare (ignore configuration))
         (parent-provider-native--record ,marker ':discovery)
         '("fixture-model"))
       :model-discovery-endpoint-resolver
       (lambda ()
         (parent-provider-native--record ,marker ':endpoint)
         "https://parent-fixture.invalid/models")))))

(serapeum/types:-> parent-provider-native--startup-files (pathname pathname) null)
(defun parent-provider-native--startup-files (temporary evidence)
  "Write private replay and ordinary init which record ordered child execution."
  (let* ((script (merge-pathnames "data/image-commits/native-startup/replay.lisp" temporary))
         (record `(progn
                    (assert (handler-case
                                (with-open-file (stream ,(namestring (merge-pathnames "private/credential" temporary)))
                                  (read-char stream nil nil)
                                  nil)
                              (file-error () t)))
                    (with-open-file (stream ,(namestring evidence) :direction         ':output
                                                                   :if-exists         ':append
                                                                   :if-does-not-exist ':create)
                      (write (list kind (ls-compat.posix:current-process-id)) :stream stream)
                      (terpri stream)))))
    (parent-provider-fixture--write
     script `((set (intern "*PARENT-NATIVE-REPLAY-LOADED*" "CL-USER") t)
              (let ((kind ':replay)) ,record)))
    (parent-provider-fixture--write
     (merge-pathnames "config/init.lisp" temporary)
     `((unless (probe-file ,(namestring evidence))
         (error "Private replay did not precede ordinary init."))
       (let ((kind ':init)) ,record)))
    (parent-provider-fixture--write
     (merge-pathnames "state/current-image-commit.sexp" temporary)
     '((:current-image-commit :version 1 :id "native-startup")))
    (parent-provider-fixture--write
     (merge-pathnames "manifest.sexp" (uiop:pathname-directory-pathname script))
     `((:image-commit :version 1 :id "native-startup"
        :script ,(make-array (length (namestring script)) :element-type 'character
                                                       :initial-contents (namestring script))))))
  nil)

(serapeum/types:-> run-parent-provider-extension-native-test
    (&key (:runtime pathname) (:core pathname) (:source-root pathname)
          (:temporary pathname) (:runtime-roots list)) integer)
(defun run-parent-provider-extension-native-test (&key runtime core source-root temporary runtime-roots)
  "Run executable custom callbacks in the real parent and prove native child denial.
Use a source-matched standard core. No callback definitions or credentials enter that core."
  (let* ((workspace      (merge-pathnames "workspace/" temporary))
         (private        (merge-pathnames "private/" temporary))
         (scratch        (merge-pathnames "scratch/" temporary))
         (plugin         (merge-pathnames "trusted/provider.lisp" temporary))
         (marker         (merge-pathnames "callback-processes.sexp" private))
         (secret         (merge-pathnames "credential" private))
         (evidence       (merge-pathnames "isolation.sexp" workspace))
         (startup        (merge-pathnames "startup.sexp" workspace))
         (selected-core  (merge-pathnames "selected-image/agent.core" temporary))
         (token          (ironclad:byte-array-to-hex-string (ironclad:random-data 32)))
         (settings       '(:model-provider "parent-fixture" :model "fixture-model"))
         (specifications (list (list :source ':user :path (namestring plugin))))
         (registry       (make-parent-provider-extension-registry
                          :cache-write-function
                          (lambda (configuration form)
                            (declare (ignore configuration))
                            (parent-provider-fixture--write
                             (merge-pathnames "models.sexp" private) (list form)))))
         (configuration nil) (application nil) (launch nil) (rendered nil))
    (dolist (directory (list workspace private scratch)) (ensure-directories-exist directory))
    (with-open-file (stream secret :direction ':output :if-exists ':supersede) (write-line token stream))
    (parent-provider-native--startup-files temporary startup)
    (ensure-directories-exist selected-core)
    (uiop:copy-file core selected-core)
    (parent-provider-fixture--write
     plugin (parent-provider-native--forms :secret secret :plugin plugin :marker marker :evidence evidence))
    (parent-provider-load-extensions registry specifications :writable-roots (list workspace private scratch))
    (assert (null (parent-provider-refresh-models registry settings)))
    (multiple-value-bind (profiles selected-id)
        (parent-provider-configuration-profiles settings :registry registry)
      (setf configuration
            (make-parent-configuration
             (list :version 1 :runtime (namestring runtime) :agent-core (namestring core)
                   :worker-core (namestring core) :source-root (namestring source-root)
                   :workspace (namestring workspace) :private-root (namestring private)
                   :temporary-root        (namestring scratch)
                    :config-root          (namestring (merge-pathnames "config/" temporary))
                    :state-root           (namestring (merge-pathnames "state/" temporary))
                    :data-root            (namestring (merge-pathnames "data/" temporary))
                  :runtime-roots          (append runtime-roots
                                                  (parent-cli-runtime-roots
                                                   source-root runtime :agent-core core :worker-core core))
                   :account-configuration (list :state-root (namestring (merge-pathnames "accounts/" private)))
                   :settings settings :profiles profiles :profile-id selected-id
                   :provider-extensions specifications :access-p nil :reasoning-effort "low"))))
    (parent-application-prepare-configuration configuration)
    (setf application
          (make-parent-application
           configuration :session-id "provider-extension-fixture" :settings settings
           :input-source (lambda (seconds) (declare (ignore seconds)) (values nil ':eof))
           :renderer     (lambda (region kind payload)
                           (declare (ignore region kind)) (push (princ-to-string payload) rendered))))
    (parent-provider-authenticate (parent-application-provider-registry application)
                                  "parent-fixture" settings :stream (make-broadcast-stream)
                                  :open-browser-p                   nil)
    (unwind-protect
        (progn
          (setf launch (parent-application-open application))
          (assert (eq ':completed (parent-application-configure application launch)))
          (parent-launch-close launch)
          (let ((old-roots (copy-list (parent-configuration-get configuration ':runtime-roots))))
            (parent-application--apply-image
             application (list :core (namestring selected-core) :pristine-p nil))
            (let ((roots (parent-configuration-get configuration ':runtime-roots)))
              (assert (every #'uiop:directory-exists-p roots))
              (assert (subsetp old-roots roots :test #'equal))))
          (setf launch (parent-application-open application))
          (assert (eq ':completed (parent-application-configure application launch)))
          (with-open-file (stream startup)
            (let* ((*read-eval* nil)
                   (events      (loop for event = (read stream nil nil) while event collect event)))
              (assert (equal '(:replay :init :init) (mapcar #'first events)))
              (assert (every (lambda (event) (/= (second event) (ls-compat.posix:current-process-id))) events))
              (assert (= (second (first events)) (second (second events))))
              (assert (/= (second (second events)) (second (third events))))))
          (assert (null (find-symbol "*PARENT-NATIVE-REPLAY-LOADED*" "CL-USER")))
          (assert (eq ':completed
                      (parent-launch-turn-run launch "provider-extension-turn"
                                              "Run the child isolation probe." :timeout 60)))
          (let ((isolation (with-open-file (stream evidence)
                             (let ((*read-eval* nil)) (read stream)))))
            (assert (eq ':child-private-denied isolation)))
          (with-open-file (stream marker)
            (let ((*read-eval* nil)
                  (events      (loop for event = (read stream nil nil) while event collect event)))
              (assert (every (lambda (event) (= (second event) (ls-compat.posix:current-process-id))) events))
              (dolist (kind '(:load :factory :auth :discovery :endpoint :transport))
                (assert (find kind events :key #'first)))))
          (assert (notany (lambda (text) (search token text)) rendered))
          18)
      (when launch (parent-launch-close launch)))))
