;;;; -- Parent Shared Domain Adapter Checks --

(in-package #:autolith-supervisor)

(serapeum:-> family-domain-test--with-functions (list function) t)
(defun family-domain-test--with-functions (replacements function)
  "Call FUNCTION with isolated parent transport replacements, then restore them."
  (let ((originals (mapcar (lambda (entry)
                             (cons (first entry) (symbol-function (first entry))))
                           replacements)))
    (unwind-protect
         (progn
           (dolist (entry replacements)
             (setf (symbol-function (first entry)) (second entry)))
           (funcall function))
      (dolist (entry originals)
        (setf (symbol-function (first entry)) (rest entry))))))

(serapeum:-> family-domain-test--executor () chatgpt-executor)
(defun family-domain-test--executor ()
  "Return the minimal parent executor needed by transport adapter fixtures."
  (make-instance 'chatgpt-executor :clock #'session-runtime--clock))

(serapeum:-> family-domain-test--gemini
    (&key (:project (or null string)) (:rpc function) (:operation function)
     (:wait function) (:check function) (:fail function)) (values string string))
(defun family-domain-test--gemini (&key project rpc operation wait check fail)
  "Run shared Gemini fixtures through the real parent setup adapter."
  (declare (ignore check))
  (multiple-value-bind (profile account invocation)
      (provider-family-test--binding '(:gemini "gemini-fixture" :gemini)
                                     :options (list :project project))
    (declare (ignore profile))
    (family-domain-test--with-functions
     (list (list 'provider-family--rpc
                 (lambda (account &key endpoint method request &allow-other-keys)
                   (declare (ignore account))
                   (if (eq method ':get)
                       (funcall operation (subseq endpoint (1+ (search "/operations/" endpoint))))
                       (let ((method (subseq endpoint (1+ (position #\: endpoint :from-end t)))))
                         (funcall rpc method
                                  (if (equal method "loadCodeAssist") ':load ':onboard)
                                  request)))))
           (list 'provider-http--sleep
                 (lambda (state seconds clock)
                   (declare (ignore state clock))
                   (funcall wait seconds))))
     (lambda ()
       (handler-case
           (progn
             (provider-family--gemini-setup
              account :executor (family-domain-test--executor)
              :state (make-instance 'provider-http-state) :invocation invocation
              :credentials      (provider-executor-test--credentials))
             (values (getf (provider-account-state account) :project)
                     (getf (provider-account-state account) :tier)))
         (provider-executor-error (condition)
           (assert (null (provider-account-state account)))
           (funcall fail (provider-executor-error-reason condition))))))))

(serapeum:-> family-domain-test--session
    (&key (:acquire function) (:now function) (:check function) (:fail function)) hash-table)
(defun family-domain-test--session (&key acquire now check fail)
  "Run session fixtures through parent Auto, stopping before model selection."
  (declare (ignore now check))
  (multiple-value-bind (profile account invocation)
      (provider-family-test--binding '(:copilot "copilot/auto" :auto))
    (declare (ignore profile))
    (family-domain-test--with-functions
     (list (list 'provider-family--rpc
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (funcall acquire)))
           (list 'family-domain-copilot-model
                 (lambda (&key session &allow-other-keys)
                   (throw 'session-result session))))
     (lambda ()
       (handler-case
           (catch 'session-result
             (provider-family--copilot-auto
              account (first (model-invocation-inputs invocation))
              :executor    (family-domain-test--executor)
              :state (make-instance 'provider-http-state) :invocation invocation
              :credentials (provider-executor-test--credentials)))
         (provider-executor-error (condition)
           (funcall fail (provider-executor-error-reason condition))))))))

(serapeum:-> family-domain-test--model
    (&key (:session hash-table) (:catalog function) (:refresh function)
     (:vision-p boolean) (:preferred-model (or null string)) (:check function) (:fail function))
    (values list string))
(defun family-domain-test--model
    (&key session catalog refresh vision-p preferred-model check fail)
  "Run selection fixtures through parent Auto and its trusted account metadata."
  (declare (ignore check))
  (assert (not vision-p))
  (multiple-value-bind (profile account invocation)
      (provider-family-test--binding '(:copilot "copilot/auto" :auto)
                                     :options (list :catalog         (funcall catalog)
                                                    :catalog-refresh refresh))
    (declare (ignore profile))
    (setf (provider-account-state account) (list :preferred-model preferred-model))
    (family-domain-test--with-functions
     (list (list 'provider-family--rpc
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   session)))
     (lambda ()
       (handler-case
           (multiple-value-bind (replacement projection token)
               (provider-family--copilot-auto
                account (first (model-invocation-inputs invocation))
                :executor    (family-domain-test--executor)
                :state (make-instance 'provider-http-state) :invocation invocation
                :credentials (provider-executor-test--credentials))
             (declare (ignore replacement))
             (values (list :name (cl-llm-provider-api:wire-request-model projection)) token))
         (provider-executor-error (condition)
           (funcall fail (provider-executor-error-reason condition))))))))

(serapeum:-> run-parent-family-domain-adapter-tests () integer)
(defun run-parent-family-domain-adapter-tests ()
  "Run the common fixture matrix through the parent Gemini and Auto adapters."
  (run-family-domain-fixtures :gemini #'family-domain-test--gemini
                             :session #'family-domain-test--session
                             :model   #'family-domain-test--model))
