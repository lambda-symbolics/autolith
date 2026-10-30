;;;; -- Current Parent Controller Checks --

(in-package #:autolith-supervisor)

(defclass parent-interface-network-test-backend (native-access-backend) ()
  (:documentation "Portable network command policy fixture without process authority."))

(defmethod access-native-check-network ((backend parent-interface-network-test-backend) enabled)
  "Admit network policy in the command routing fixture."
  (declare (ignore enabled))
  t)

(serapeum/types:-> run-parent-application-interface-tests () integer)
(defun run-parent-application-interface-tests ()
  "Check live metadata routing, FIFO input, active work, cancellation and modal ownership."
  (let ((checks 0))
    (labels ((check (value)
               (incf checks)
               (unless value (error "Parent interface check ~D failed." checks)))

             (controller ()
               (make-instance 'parent-interface-controller :application nil
                              :frontend                                 (make-instance 'parent-terminal-ui :editor-ui nil :host nil))))
      (dolist (answer '("NO" "YES"))
        (let* ((configuration (make-instance 'parent-configuration :values '(:network-p nil)))
               (events nil) (reads 0)
               (backend       (make-instance 'parent-interface-network-test-backend))
               (launch        (make-instance 'parent-launch))
               (application
                 (make-instance 'parent-application :configuration configuration
                                :input-source                      (lambda (timeout)
                                                                     (declare (ignore timeout))
                                                                     (incf reads)
                                                                     (values answer ':line))
                                :renderer                          (lambda (region kind payload)
                                                                     (declare (ignore payload))
                                                                     (push (list region kind) events))))
               (state         (make-instance 'parent-interface-controller :application application
                                              :frontend                                (make-instance 'parent-terminal-ui :editor-ui nil :host nil))))
          (setf (parent-application-backend application) backend
                (parent-interface-commands state)
                (list (parent-proto:make-application-command
                       :name "/network" :busy-behavior ':inspect :terminal-behavior ':shared)))
          ;; Child metadata cannot route this command into an active child scope.
          (check (not (parent-interface--active-route-p state "/network on")))
          (parent-interface--submit state "/network on" :active-p t)
          (check (= 1 (length (parent-interface-queue state))))
          (check (null (parent-interface-steering-queue state)))
          (check (eq ':hold (parent-interface--child-run state launch "/network on" :active-p t)))
          (check (zerop reads))
          (check (eq ':completed (parent-interface--child-run state launch "/network status")))
          (check (zerop reads))
          (check (not (parent-application--restart-p application)))
          (check (eq ':completed (parent-interface--child-run state launch "/help network")))
          (check (zerop reads))
          (dolist (text '("/network maybe" "/network on extra" "/network #.(error :reader)"))
            (check (handler-case (progn (parent-interface--network-run application text) nil)
                     (parent-application-operation-error (condition)
                       (eq (parent-application-operation-error-code condition) ':invalid-network-command))))
            (check (eq ':completed (parent-interface--child-run state launch text))))
          (check (zerop reads))
          (setf (parent-application-backend application) (make-instance 'native-access-backend))
          (check (eq ':completed (parent-interface--child-run state launch "/network on")))
          (check (zerop reads))
          (check (not (parent-application--restart-p application)))
          (setf (parent-application-backend application) backend)
          (check (eq ':completed (parent-interface--child-run state launch "/network on")))
          (check (= 1 reads))
          (check (member '(:trusted :network-approval) events :test #'equal))
          (check (not (parent-application-network-p application)))
          (check (eq backend (parent-application-backend application)))
          (check (eq (string= answer "YES") (parent-application--restart-p application)))
          (when (string= answer "YES")
            (funcall (parent-application-after-stop application))
            (check (parent-application-network-p application))
            (check (null (parent-application-backend application)))
            (setf (parent-application-after-stop application) nil
                  (parent-application-restart application) nil
                  (parent-application-backend application) backend)
            (check (eq ':completed (parent-interface--child-run state launch "/network off")))
            (check (= 1 reads))
            (check (parent-application-network-p application))
            (check (parent-application--restart-p application))
            (funcall (parent-application-after-stop application))
            (check (not (parent-application-network-p application))))))
      (loop for (text kind source) in
            '(("hello" :message "hello") ("(values 1 2)" :lisp "(values 1 2)")
              ("? (sleep 1)" :async-lisp "(sleep 1)")
              ("/custom" :command "/custom") ("" :message "")
              ("(list #.(error :parent-reader-used))" :lisp "(list #.(error :parent-reader-used))"))
            do (check (eq kind (parent-interface--kind text)))
               (check (string= source (parent-interface--source text))))
      (let* ((state   (controller))
             (command (parent-proto:make-application-command
                       :name "/fresh" :aliases '("/alias") :argument "VALUE"
                       :description "A dynamically registered operation."
                       :options '("one" "two") :busy-behavior ':inspect :terminal-behavior ':shared))
             (result  (parent-proto:make-application-result :turn-id "catalog" :status ':ok
                                                            :commands (list command))))
        (parent-interface--accept-result state result)
        (check (eq command (parent-interface--command state "/alias one")))
        (check (parent-interface--active-route-p state "/fresh one"))
        (check (not (parent-interface--active-route-p state "/unknown")))
        (check (every #'terminal-completion-p (parent-interface-completions state)))
        (check (find "/network on" (parent-interface-completions state)
                     :key (lambda (entry) (getf entry :name)) :test #'string=))
        (check (find "/help network" (parent-interface-completions state)
                     :key (lambda (entry) (getf entry :name)) :test #'string=))
        (check (find "/fresh two" (parent-interface-completions state)
                     :key (lambda (entry) (getf entry :name)) :test #'string=))
        (parent-interface--enqueue state "first")
        (parent-interface--enqueue state "second")
        (let ((entry (first (parent-interface-queue state))))
          (setf (parent-interface-recalled state) entry
                (parent-interface-input-recalled-p entry) t)
          (parent-interface--submit state "edited" :active-p t)
          (check (eq entry (first (parent-interface-queue state))))
          (check (equal '("edited" "second")
                        (mapcar #'parent-interface-input-payload (parent-interface-queue state))))
          (check (null (parent-interface-steering-queue state)))))
      (let ((state (controller))
            (lock  (bordeaux-threads:make-lock "controller check"))
            (executed nil) (steered nil) (step 0) (release nil))
        (setf (parent-interface-commands state)
              (list (parent-proto:make-application-command
                     :name "/inspect" :busy-behavior ':inspect :terminal-behavior ':shared)))
        (check
         (eq ':eof
             (parent-interface-drive
              state
              :poll-function
              (lambda (seconds)
                (sleep (min seconds 0.001))
                (incf step)
                (case step
                  (1
                   (values ':submit "provider"))
                  (2
                   (values ':queue "queued"))
                  (3
                   (values ':submit "/inspect"))
                  (4
                   (values ':submit "later"))
                  (otherwise
                   (bordeaux-threads:with-lock-held (lock)
                     (when steered (setf release t))
                     (if (= 3 (length executed))
                         (values ':eof nil)
                         (values ':timeout nil))))))
              :execute-function
              (lambda (payload)
                (when (string= payload "provider")
                  (loop until (bordeaux-threads:with-lock-held (lock) release) do (sleep 0.001)))
                (bordeaux-threads:with-lock-held (lock) (setf executed (append executed (list payload))))
                (values ':completed nil))
              :active-function
              (lambda (payload)
                (bordeaux-threads:with-lock-held (lock) (push payload steered))
                (values ':handled nil))
              :cancel-function (lambda () (bordeaux-threads:with-lock-held (lock) (setf release t)))
              :stop-function   (constantly nil))))
        (check (equal '("provider" "queued" "later") executed))
        (check (equal '("/inspect") steered))
        (check (null (parent-interface-queue state)))
        (check (null (parent-interface-thread state))))
      (let ((state (controller)) (step 0) (cancelled nil)
            (lock (bordeaux-threads:make-lock "controller cancellation")))
        (check
         (eq ':eof
             (parent-interface-drive
              state
              :poll-function
              (lambda (seconds)
                (sleep (min seconds 0.001))
                (incf step)
                (case step
                  (1
                   (values ':submit "running"))
                  (2
                   (values ':interrupt nil))
                  (otherwise
                   (values ':eof nil))))
              :execute-function
              (lambda (payload)
                (declare (ignore payload))
                (loop until (bordeaux-threads:with-lock-held (lock) cancelled) do (sleep 0.001))
                (values ':cancelled nil))
              :active-function (lambda (payload) (declare (ignore payload)) (values ':handled nil))
              :cancel-function (lambda () (bordeaux-threads:with-lock-held (lock) (setf cancelled t)))
              :stop-function   (constantly nil))))
        (check cancelled)
        (check (null (parent-interface-thread state))))
      (let* ((modal-p nil) (renders 0) (reads 0)
             (terminal
               (make-parent-terminal
                :exclusive-input
                (lambda (function)
                  (check (not modal-p))
                  (let ((old modal-p))
                    (unwind-protect (progn (setf modal-p t) (funcall function))
                      (setf modal-p old))))
                :renderer     (lambda (&rest payload)
                                (declare (ignore payload))
                                (check modal-p) (incf renders))
                :input-source (lambda (seconds)
                                (declare (ignore seconds))
                                (check modal-p) (incf reads) (values "YES" ':line)))))
        (check (parent-terminal-approve-knowledge terminal :kind ':rule :scope ':workspace
                                                 :identifier "rule" :version 1 :proposal '("inert")))
        (check (= renders 1))
        (check (= reads 1))
        (check (not modal-p)))
      (let ((state (controller)) (step 0) (cancelled nil) (release nil)
            (lock (bordeaux-threads:make-lock "uncertain steering check")))
        (check
         (handler-case
             (progn
               (parent-interface-drive
                state
                :poll-function
                (lambda (seconds)
                  (sleep (min seconds 0.001))
                  (incf step)
                  (case step
                    (1
                     (values ':submit "provider"))
                    (2
                     (values ':submit "(+ 1 2)"))
                    (3
                     (values ':queue "unsent"))
                    (otherwise
                     (bordeaux-threads:with-lock-held (lock) (setf release t))
                     (values ':timeout nil))))
                :execute-function
                (lambda (input)
                  (declare (ignore input))
                  (loop until (bordeaux-threads:with-lock-held (lock) cancelled) do (sleep 0.001))
                  (values ':cancelled nil))
                :active-function
                (lambda (input)
                  (declare (ignore input))
                  (loop until (bordeaux-threads:with-lock-held (lock) release) do (sleep 0.001))
                  (error 'parent-application-operation-error :code ':uncertain))
                :cancel-function (lambda () (bordeaux-threads:with-lock-held (lock) (setf cancelled t)))
                :stop-function   (constantly nil))
               nil)
           (parent-application-operation-error () t)))
        (check (equal '("unsent") (mapcar #'parent-interface-input-payload (parent-interface-queue state))))
        (check (null (parent-interface-steering state))))
      (let* ((fields  (make-hash-table :test #'equal))
             (payload (make-hash-table :test #'equal)))
        (setf (gethash "kind" fields) "notice" (gethash "payload" fields) payload
              (gethash "text" payload) "Inert text #.(error :never-read)")
        (multiple-value-bind (kind decoded) (parent-interface-decode-ui fields)
          (check (eq kind ':notice))
          (check (equal (getf decoded :text) (gethash "text" payload))))
        (setf (gethash "unknown-field" payload) "bad")
        (check (handler-case (progn (parent-interface-decode-ui fields) nil)
                 (parent-terminal-error () t))))
      (let ((terminal (make-parent-terminal
                       :input-source  (lambda (seconds) (declare (ignore seconds)) (error "Unexpected poll."))
                       :renderer      (lambda (&rest values) (declare (ignore values)))
                       :read-function (lambda (&key timeout cancelled)
                                        (declare (ignore timeout))
                                        (values (unless (funcall cancelled) "yes")
                                                (if (funcall cancelled) ':cancelled ':line))))))
        (check (equal (multiple-value-list (parent-terminal-read-input terminal)) '("yes" :line)))
        (parent-terminal-close terminal)
        (check (equal (multiple-value-list (parent-terminal-read-input terminal)) '(nil :cancelled))))
      (+ checks (parent-interface-test--startup-projections)
         (parent-interface-test--restart-input)))))

(serapeum/types:-> parent-interface-test--startup-projections () integer)
(defun parent-interface-test--startup-projections ()
  "Check exact inert startup projections, unchanged bytes, and scope exclusion."
  (let* ((root   (merge-pathnames (format nil "parent-interface-~A/" (parent-application--identifier))
                                  (uiop:temporary-directory)))
         (checks 0))
    (labels ((path (name) (namestring (merge-pathnames name root)))

             (check (value)
               (incf checks)
               (unless value (error "Startup projection check ~D failed." checks)))

             (cleanup (directory)
               (when (uiop:directory-exists-p directory)
                 (dolist (file (uiop:directory-files directory)) (delete-file file))
                 (dolist (child (uiop:subdirectories directory)) (cleanup child))
                 (uiop:delete-empty-directory directory))))
      (unwind-protect
           (let* ((data          (list :version 1 :runtime "/usr/bin/false" :agent-core (path "agent.core")
                                       :worker-core (path "agent.core") :source-root (path "source/")
                                       :workspace (path "workspace/") :temporary-root (path "temporary/")
                                       :private-root (path "private/") :runtime-roots (list (path "source/"))
                                       :account-configuration (list :state-root (path "private/accounts/"))
                                       :profiles              '((:id "ordinary" :family :openai-api :model "test-model"))
                                       :profile-id "ordinary" :access-p nil :data-root (path "data/")
                                       :state-root (path "state/") :config-root (path "config/")))
                  (configuration (make-parent-configuration data))
                  (source        (merge-pathnames "init.lisp" (pathname (path "config/"))))
                  (text          "(error \"Must never run in the parent\")"))
             (dolist (directory '("source/" "workspace/" "temporary/" "private/" "data/" "state/" "config/"))
               (ensure-directories-exist (merge-pathnames "probe" (pathname (path directory)))))
             (with-open-file (stream source :direction ':output :if-does-not-exist ':create)
               (write-string text stream))
             (check (eq configuration (parent-application-prepare-configuration configuration)))
             (let* ((files     (gethash configuration *parent-application-code-files*))
                    (projected (first (getf files :init))))
               (check (= 1 (length (getf files :init))))
               (check (equal text (uiop:read-file-string projected)))
               (check (not (equal projected (namestring source))))
               (let ((roots (parent-configuration-get configuration ':runtime-roots)))
                 (check (every #'uiop:directory-exists-p roots))
                 (check (some (lambda (root) (uiop:subpathp (pathname projected) (pathname root))) roots)))
               (check (null (getf files :replay)))
               (with-open-file (stream source :direction ':output :if-exists ':supersede)
                 (write-string "changed" stream))
               (check (equal text (uiop:read-file-string projected)))
               (let ((other (parent-application--project-code configuration source)))
                 (check (not (equal projected other)))
                 (check (notany (lambda (root) (uiop:subpathp (pathname other) (pathname root)))
                                (parent-configuration-get configuration ':runtime-roots))))
               (parent-application--select-code-files configuration t)
               (check (null (getf (gethash configuration *parent-application-code-files*) :init))))
             (let ((original               (symbol-function 'make-parent-terminal-ui))
                   (*terminal-theme*       *terminal-theme*)
                   (*terminal-style-table* *terminal-style-table*))
               (unwind-protect
                    (progn
                      (setf (symbol-function 'make-parent-terminal-ui)
                            (lambda (&rest arguments)
                              (declare (ignore arguments))
                              (throw 'installed-theme (terminal-theme-name *terminal-theme*))))
                      (dolist (theme '(:almighty "almighty"))
                        (setf (getf (getf (slot-value configuration 'values) :settings) :terminal-theme) theme)
                        (check (eq ':almighty
                                   (catch 'installed-theme
                                     (parent-application-start
                                      configuration :input  (make-string-input-stream "")
                                                    :output (make-string-output-stream)))))))
                 (setf (symbol-function 'make-parent-terminal-ui) original)))
             (let ((unsafe (make-parent-configuration
                            (let ((copy (copy-tree data)))
                              (setf (getf copy :data-root) (path "workspace/")) copy))))
               (check (handler-case
                          (progn (parent-application--project-code unsafe source) nil)
                        (parent-configuration-error () t)))))
        (cleanup root)))
    checks))

(serapeum/types:-> parent-interface-test--restart-input () integer)
(defun parent-interface-test--restart-input ()
  "Check inert restart argument transfer and cancellation at each input boundary."
  (let* ((application     (make-instance 'parent-application))
         (frontend        (make-instance 'parent-terminal-ui :editor-ui nil :host nil))
         (arguments       (make-hash-table :test #'equal))
         (entry           (make-hash-table :test #'equal))
         (reads           nil)
         (cancelled-p     nil)
         (checks          0)
         (original-read   (symbol-function 'parent-terminal-ui-read))
         (original-render (symbol-function 'parent-terminal-ui-render)))
    (labels ((check (value)
               "Count a restart assertion."
               (incf checks)
               (assert value))

             (run (answers &key cancelled)
               "Submit modal answers without evaluating their source."
               (setf reads answers cancelled-p cancelled)
               (funcall (parent-interface-restart-handler application frontend)
                        arguments (lambda () cancelled-p))))
      (setf (gethash "name" entry) "USE-VALUE"
            (gethash "report" entry) "Supply a replacement value."
            (gethash "restarts" arguments) (vector entry))
      (unwind-protect
           (progn
             (setf (symbol-function 'parent-terminal-ui-render)
                   (lambda (&rest values) (declare (ignore values)) nil)
                   (symbol-function 'parent-terminal-ui-read)
                   (lambda (&rest values)
                     (declare (ignore values))
                     (let ((answer (pop reads)))
                       (when (third answer) (setf cancelled-p t))
                       (values (first answer) (second answer)))))
             (let* ((source "#.(error :parent-evaluated-restart-source) :inert")
                    (result (run (list (list "0" ':line) (list source ':line)))))
               (check (eql 0 (gethash "index" result)))
               (check (equal source (gethash "source" result)))
               (check (not (parent-application-restart application))))
             (let ((result (run '(("0" :line) ("" :line)))))
               (check (eql 0 (gethash "index" result)))
               (check (not (nth-value 1 (gethash "source" result)))))
             (dolist (answers '(((nil :cancelled))
                                (("0" :line) (nil :cancelled))
                                (("0" :line) ("42" :line t))
                                (("-1" :line)) (("1" :line)) (("bad" :line))))
               (check (zerop (hash-table-count (run answers)))))
             (check (zerop (hash-table-count (run nil :cancelled t))))
             (check (null reads)))
        (setf (symbol-function 'parent-terminal-ui-read) original-read
              (symbol-function 'parent-terminal-ui-render) original-render)))
    checks))
