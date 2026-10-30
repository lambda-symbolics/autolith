;;;; -- Shared Provider Family Domain Checks --

(in-package #:autolith-supervisor)

(serapeum:-> run-family-domain-fixtures
    (&key (:gemini function) (:session function) (:model function)) integer)
(defun run-family-domain-fixtures
    (&key (gemini #'family-domain-gemini-setup)
          (session #'family-domain-copilot-session)
          (model #'family-domain-copilot-model))
  "Run the same setup and Auto cases through domain or host adapter callbacks."
  (let ((checks 0))
    (labels ((check (value)
               (incf checks)
               (assert value))

             (fail (reason &key stage project response)
               (declare (ignore stage project response))
               (throw 'fixture-failure reason))

             (setup (&key project loaded onboard polled)
               (let ((requests nil)
                     (waits    nil))
                 (let ((result
                         (catch 'fixture-failure
                           (multiple-value-list
                            (funcall gemini
                                     :project   project
                                     :rpc       (lambda (method stage request)
                                                  (declare (ignore stage))
                                                  (push (cons method request) requests)
                                                  (if (equal method "loadCodeAssist") loaded onboard))
                                     :operation (lambda (name)
                                                  (check (equal name "operations/fixture"))
                                                  polled)
                                     :wait      (lambda (seconds) (push seconds waits))
                                     :check     (lambda () nil)
                                     :fail      #'fail)))))
                   (values result (reverse requests) waits)))))
      (dolist (entry '(("auto" . "gemini-3-pro-preview")
                       ("pro" . "gemini-3-pro-preview")
                       ("flash" . "gemini-3-flash-preview")
                       ("flash-lite" . "gemini-3.1-flash-lite")
                       ("auto-gemini-3" . "gemini-3-pro-preview")
                       ("auto-gemini-2.5" . "gemini-2.5-pro")
                       ("gemini-auto" . "gemini-3-pro-preview")
                       ("gemini-pro" . "gemini-3-pro-preview")
                       ("gemini-flash" . "gemini-3-flash-preview")
                       ("custom-model" . "custom-model")))
        (check (equal (family-domain-gemini-model-name (first entry)) (rest entry))))
      (dolist (paid (list nil (cl-rfc8628:json-object)
                         (cl-rfc8628:json-object "id" "paid-tier")))
        (check (equal (setup :loaded (cl-rfc8628:json-object
                                      "paidTier" paid
                                      "currentTier" (cl-rfc8628:json-object "id" "current-tier")
                                      "cloudaicompanionProject" "loaded-project"))
                      (list "loaded-project" (if (and paid (gethash "id" paid))
                                                 "paid-tier" "current-tier")))))
      (check (equal (setup :project "configured-project"
                          :loaded   (cl-rfc8628:json-object
                                     "currentTier" (cl-rfc8628:json-object)))
                    '("configured-project" "standard-tier")))
      (check (eq (setup :project "12345") ':numeric-gemini-project))
      (check (eq (setup :loaded (cl-rfc8628:json-object
                                 "currentTier" (cl-rfc8628:json-object)))
                 ':gemini-project-required))
      (dolist (tier-id '("free-tier" "standard-tier" "legacy-tier"))
        (multiple-value-bind (result requests waits)
            (setup :project "configured-project"
                   :loaded  (if (equal tier-id "legacy-tier")
                                (cl-rfc8628:json-object)
                                (cl-rfc8628:json-object
                                 "allowedTiers" (vector (cl-rfc8628:json-object
                                                         "id" tier-id "isDefault" t))))
                   :onboard (cl-rfc8628:json-object "name" "operations/fixture")
                   :polled  (cl-rfc8628:json-object
                             "done" t "response"
                             (cl-rfc8628:json-object "cloudaicompanionProject"
                                                     (cl-rfc8628:json-object "id" "onboard-project"))))
          (check (equal result (list "onboard-project" tier-id)))
          (check (equal waits '(5)))
          (let* ((request (rest (second requests)))
                 (project (unless (equal tier-id "free-tier") "configured-project")))
            (check (equal (gethash "cloudaicompanionProject" request) project))
            (check (equal (gethash "duetProject" (gethash "metadata" request)) project)))))
      (check (eq (setup :loaded  (cl-rfc8628:json-object)
                        :onboard (cl-rfc8628:json-object "done" t))
                 ':gemini-project-required))
      (dolist (name '(nil "operations/" "../operations/fixture" "operations/../fixture"
                      "operations/fixture?query" "operations/fixture#fragment"))
        (check (eq (setup :project "configured-project"
                          :loaded  (cl-rfc8628:json-object)
                          :onboard (cl-rfc8628:json-object "name" name))
                   ':invalid-gemini-operation)))
      (dolist (expiry '(0 9999999999 nil))
        (let* ((document (cl-rfc8628:json-object "expires_at" expiry))
               (result   (catch 'fixture-failure
                           (funcall session :acquire (lambda () document)
                                            :now     (lambda () 100)
                                            :check (lambda () nil) :fail #'fail))))
          (check (if (eql expiry 0)
                     (eq result ':expired-auto-session)
                     (eq result document)))))
      (let* ((document (cl-rfc8628:json-object
                        "selected_model" "responses" "session_token" "fixture-token"))
             (catalog  (cl-rfc8628:json-object
                        "data" (vector (cl-rfc8628:json-object
                                        "id" "responses" "supported_endpoints" #("/responses")))))
             (empty    (cl-rfc8628:json-object "data" #())))
        (dolist (initial (list catalog empty))
          (let ((refreshes 0))
            (multiple-value-bind (spec token)
                (funcall model :session document :catalog (lambda () initial)
                               :refresh (lambda () (incf refreshes) catalog)
                               :vision-p nil :preferred-model nil
                               :check (lambda () nil) :fail #'fail)
              (check (equal (getf spec :name) "copilot/responses"))
              (check (equal token "fixture-token"))
              (check (= refreshes (if (eq initial catalog) 0 1))))))
        (let ((refreshes 0))
          (check (eq (catch 'fixture-failure
                       (funcall model :session document :catalog (lambda () empty)
                                      :refresh (lambda () (incf refreshes) empty)
                                      :vision-p nil :preferred-model nil
                                      :check (lambda () nil) :fail #'fail))
                     ':unusable-auto-session))
          (check (= refreshes 1))))
      checks)))

(serapeum:-> run-family-domain-tests () integer)
(defun run-family-domain-tests ()
  "Check shared setup, Auto selection and cancellation between host operations."
  (let ((checks (run-family-domain-fixtures)))
    (dolist (cancel-at '(1 2 3 4 5 6))
      (let ((calls  0)
            (polled nil))
        (assert
         (eq (catch 'cancelled
               (family-domain-gemini-setup
                :project   "fixture-project"
                :rpc       (lambda (method stage request)
                             (declare (ignore stage request))
                             (if (equal method "loadCodeAssist")
                                 (cl-rfc8628:json-object)
                                 (cl-rfc8628:json-object "name" "operations/fixture")))
                :operation (lambda (name)
                             (declare (ignore name))
                             (setf polled t)
                             (cl-rfc8628:json-object "done" t))
                :wait      (lambda (seconds) (declare (ignore seconds)))
                :check     (lambda ()
                             (when (= (incf calls) cancel-at)
                               (throw 'cancelled ':cancelled)))
                :fail      (lambda (&rest arguments)
                             (declare (ignore arguments))
                             (error "Unexpected setup failure."))))
             ':cancelled))
        (assert (eq polled (= cancel-at 6)))
        (incf checks 2)))
    checks))
