;;;; -- Trusted Parent Editor Checks --

(in-package #:autolith-supervisor)

(defclass parent-ui-test-terminal (terminal)
  ((events :initarg :events :initform nil :accessor parent-ui-test-terminal-events
           :documentation "Deterministic input events.")
   (output :initform (make-string-output-stream) :reader parent-ui-test-terminal-output
           :documentation "Captured renderer output."))
  (:documentation "In-memory terminal for the real shared editor and renderer."))

(defmethod terminal--write ((terminal parent-ui-test-terminal) (text string))
  "Capture trusted terminal output."
  (write-string text (parent-ui-test-terminal-output terminal))
  nil)

(defmethod clinedi:terminal-write ((terminal parent-ui-test-terminal) text)
  "Capture controls and presentation through the same transport method."
  (terminal--write terminal text))

(defmethod clinedi:terminal-flush ((terminal parent-ui-test-terminal))
  "Keep captured output in memory."
  (declare (ignore terminal))
  nil)

(defmethod clinedi:terminal-start ((terminal parent-ui-test-terminal))
  "Start an in-memory terminal without native input changes."
  terminal)

(defmethod clinedi:terminal-stop ((terminal parent-ui-test-terminal))
  "Stop an in-memory terminal without closing its capture stream."
  terminal)

(defmethod clinedi:terminal-input-ready-p ((terminal parent-ui-test-terminal))
  "Report whether one deterministic event is queued."
  (not (null (parent-ui-test-terminal-events terminal))))

(defmethod clinedi:terminal-read-event ((terminal parent-ui-test-terminal))
  "Consume one deterministic event."
  (pop (parent-ui-test-terminal-events terminal)))

(defmethod clinedi:terminal-interactive-p ((terminal parent-ui-test-terminal))
  "Exercise the interactive editor and fullscreen path."
  (declare (ignore terminal))
  t)

(defmethod clinedi:terminal-styled-p ((terminal parent-ui-test-terminal))
  "Use plain spans while testing real terminal layout."
  (declare (ignore terminal))
  nil)

(serapeum/types:-> run-parent-terminal-ui-tests () integer)
(defun run-parent-terminal-ui-tests ()
  "Check the shared editor, semantic events, modal ownership and secret input."
  (let ((checks 0))
    (labels ((check (value)
               (incf checks)
               (unless value (error "Parent editor check ~D failed." checks)))

             (rejects (function)
               (check (handler-case (progn (funcall function) nil)
                        (parent-terminal-error () t))))

             (transport (&rest events)
               (make-instance 'parent-ui-test-terminal :rows 30 :columns 90 :events events)))
      (let* ((terminal (transport '(:insert "(+ 1 2)") ':submit))
             (entered 0) (restored nil)
             (frontend
               (make-parent-terminal-ui
                :terminal terminal :history '("earlier")
                :fullscreen-enable-function  (lambda () (incf entered) ':native-token)
                :fullscreen-restore-function (lambda (token) (push token restored)))))
        (unwind-protect
             (progn
               (check (terminal-ui-fullscreen-p (parent-terminal-ui-editor-ui frontend)))
               (multiple-value-bind (action input) (parent-terminal-ui-poll-event frontend 0.1)
                 (check (eq action ':submit))
                 (check (string= input "(+ 1 2)")))
               (check (= entered 1))
               (let* ((widget   (termdown:make-widget ':code-copy "copy" '(:copy "one\ntwo")))
                      (content  (list (terminal-span ':tool "▸ (test)") widget))
                      (portable (terminal-ui-content->portable content))
                      (decoded  (terminal-ui-portable->content portable)))
                 (check (equal (first content) (first decoded)))
                 (check (equal (termdown:widget-action widget)
                               (termdown:widget-action (second decoded)))))
               (parent-terminal-ui-event
                frontend ':status
                (list :status "Thinking" :details
                      (terminal-ui-content->portable (list (terminal-span ':status-model "model")))
                      :worked-seconds 3))
               (check (string= "Thinking" (terminal-ui-status
                                           (parent-terminal-ui-editor-ui frontend))))
               (parent-terminal-ui-event frontend ':context
                                         '(:used 100 :window 1000 :compaction-limit 800))
               (check (= 100 (terminal-ui-context-used
                              (parent-terminal-ui-editor-ui frontend))))
               (parent-terminal-ui-event frontend ':stream
                                         (list :rows (list (terminal-ui-content->portable
                                                            (list (terminal-span ':tool "tool card"))))
                                               :tail "partial"))
               (check (search "partial" (terminal-ui-stream-tail
                                         (parent-terminal-ui-editor-ui frontend))))
               (parent-terminal-ui-event frontend ':pending-inputs
                                         '(:steering ("now") :queued ("later")))
               (check (equal '("later") (terminal-ui-queued-input-previews
                                         (parent-terminal-ui-editor-ui frontend))))
               (let* ((ui     (parent-terminal-ui-editor-ui frontend))
                      (before (terminal-ui-status ui)))
                 (rejects (lambda () (parent-terminal-ui-event frontend ':status
                                                                '(:status :not-a-string))))
                 (check (equal before (terminal-ui-status ui))))
               (rejects (lambda () (terminal-ui-portable->content
                                    '((:kind :widget :role :code-copy :label "run"
                                       :action (:eval "(error :bad)"))))))
               (rejects (lambda () (terminal-ui-portable->content
                                    '((:kind :widget :role :code-copy :label "open"
                                       :action (:open-url "file:///private"))))))
               (check (not (terminal-ui-portable-value-p #'identity)))
               (check (not (terminal-ui-portable-value-p #P"/private")))
               (check (not (terminal-ui-portable-value-p 'ordinary-symbol)))
               (let ((cycle (list ':cycle)))
                 (setf (rest cycle) cycle)
                 (check (not (terminal-ui-portable-value-p cycle))))
               (let ((*terminal-ui-event-character-limit* 4))
                 (check (not (terminal-ui-event-valid-p ':notice '(:text "too long"))))))
          (parent-terminal-ui-close frontend)
          (parent-terminal-ui-close frontend))
        (check (equal '(:native-token) restored)))
      (let* ((terminal (transport '(:insert "secret-value") ':submit))
             (frontend (make-parent-terminal-ui :terminal terminal :fullscreen-p nil)))
        (unwind-protect
             (let* ((ui       (parent-terminal-ui-editor-ui frontend))
                    (original (terminal-ui-editor ui)))
               (terminal-ui-set-input ui "ordinary draft")
               (multiple-value-bind (text state)
                   (parent-terminal-ui-read frontend "Credential" :secret-p t :timeout 1)
                 (check (eq state ':line))
                 (check (string= "secret-value" text)))
               (check (eq original (terminal-ui-editor ui)))
               (check (string= "ordinary draft" (clinedi:line-editor-text original)))
               (check (not (search "secret-value"
                                    (get-output-stream-string
                                     (parent-ui-test-terminal-output terminal)))))
               (multiple-value-bind (text state)
                   (parent-terminal-ui-read frontend "Timeout" :timeout 0)
                 (check (null text))
                 (check (eq state ':timeout))))
          (parent-terminal-ui-close frontend)))
      (let* ((terminal (transport ':down ':submit))
             (frontend (make-parent-terminal-ui :terminal terminal :fullscreen-p nil)))
        (unwind-protect
             (check (string= "two"
                              (parent-terminal-ui-choose
                               frontend "Model"
                               '((:name "one" :value "one" :description "first")
                                 (:name "two" :value "two" :description "second"))
                               :search-p nil)))
          (parent-terminal-ui-close frontend))))
    checks))
