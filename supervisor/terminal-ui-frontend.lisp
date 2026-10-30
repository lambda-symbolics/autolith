;;;; -- Trusted Parent Editor Frontend --

(in-package #:autolith-supervisor)

(defclass parent-terminal-ui ()
  ((editor-ui :initarg :editor-ui :reader parent-terminal-ui-editor-ui
              :documentation "The shared Clinedi presentation and composer.")
   (host :initarg :host :reader parent-terminal-ui-host
         :documentation "Trusted host callbacks for this frontend.")
   (input-lock :initform      (bordeaux-threads:make-recursive-lock "parent editor input")
               :reader        parent-terminal-ui-input-lock
               :documentation "Serializes polling and modal input ownership.")
   (state-lock :initform      (bordeaux-threads:make-lock "parent editor lifetime")
               :reader        parent-terminal-ui-state-lock
               :documentation "Protects the close latch without waiting for modal input.")
   (closed-p :initform nil :accessor parent-terminal-ui--closed-p
             :documentation "Whether terminal shutdown has been requested.")
   (sequence :initform 0 :accessor parent-terminal-ui-sequence
             :documentation "Local identity for finalized transcript entries.")
   (active-p :initform nil :accessor parent-terminal-ui-active-p
             :documentation "Whether submissions can target an active turn.")
   (queue-editing-p :initform nil :accessor parent-terminal-ui-queue-editing-p
                    :documentation "Whether the operator edits a recalled queued submission."))
  (:documentation "Trusted parent adapter over the current terminal editor and renderer."))

(serapeum/types:-> make-parent-terminal-ui
    (&key (:input stream) (:output stream) (:terminal (option terminal))
     (:fullscreen-p boolean) (:history list) (:completion-function (option function))
     (:completion-root (option pathname)) (:path-search-function (option function))
     (:action-function (option function)) (:image-path-function function)
     (:startup-tip-function function) (:inference-tool-function function)
     (:fullscreen-enable-function function) (:fullscreen-restore-function function))
    parent-terminal-ui)
(defun make-parent-terminal-ui
    (&key (input *standard-input*) (output *standard-output*) terminal
          (fullscreen-p t) history completion-function completion-root path-search-function
          action-function (image-path-function #'terminal-ui-recognize-image-path)
          (startup-tip-function (constantly nil))
          (inference-tool-function (lambda (name)
                                     (member name '("rlm.infer" "rlm.map" "rlm.complete")
                                             :test #'string=)))
          (fullscreen-enable-function #'parent-terminal-native-enable-fullscreen)
          (fullscreen-restore-function #'parent-terminal-native-restore-fullscreen))
  "Start the current editor without loading the agent system.

COMPLETION-FUNCTION takes no arguments and returns inert completion plists.
ACTION-FUNCTION receives a transcript click action; its return values are ignored.
Use PARENT-TERMINAL-UI-POLL-EVENT to retain active-input editor actions."
  (let* ((host               (make-instance 'terminal-ui-host
                                            :image-path-function         image-path-function
                                            :startup-tip-function        startup-tip-function
                                            :inference-tool-function     inference-tool-function
                                            :fullscreen-enable-function  fullscreen-enable-function
                                            :fullscreen-restore-function fullscreen-restore-function))
         (*terminal-ui-host* host)
         (transport          (or terminal (stream-terminal-create :input-stream  input
                                                                  :output-stream output)))
         (ui                 (terminal-ui-create :terminal transport :fullscreen-p fullscreen-p
                                                 :completion-function     completion-function
                                                 :completion-root         completion-root
                                                 :path-search-function    path-search-function
                                                 :message-header-prefixes '("You" "Autolith" "PARENT")))
         (frontend           (make-instance 'parent-terminal-ui :editor-ui ui :host host)))
    (setf (terminal-ui-action-function ui) action-function
          (terminal-ui-exclusive-input-function ui)
          (lambda (function)
            (parent-terminal-ui-call-with-exclusive-input frontend function)))
    (terminal-ui-load-history ui history)
    (handler-case
        (progn (terminal-ui-start ui) frontend)
      (error (condition)
        (ignore-errors (terminal-ui-stop ui))
        (error condition)))))

(serapeum/types:-> parent-terminal-ui-closed-p (parent-terminal-ui) boolean)
(defun parent-terminal-ui-closed-p (frontend)
  "Read the close latch without waiting for a modal input owner."
  (bordeaux-threads:with-lock-held ((parent-terminal-ui-state-lock frontend))
    (not (null (parent-terminal-ui--closed-p frontend)))))

(serapeum/types:-> parent-terminal-ui-close (parent-terminal-ui) null)
(defun parent-terminal-ui-close (frontend)
  "Cancel modal input, then restore native terminal state exactly once."
  (let ((close-p
          (bordeaux-threads:with-lock-held ((parent-terminal-ui-state-lock frontend))
            (unless (parent-terminal-ui--closed-p frontend)
              (setf (parent-terminal-ui--closed-p frontend) t)))))
    (when close-p
      (bordeaux-threads:with-recursive-lock-held ((parent-terminal-ui-input-lock frontend))
        (let ((*terminal-ui-host* (parent-terminal-ui-host frontend)))
          (terminal-ui-stop (parent-terminal-ui-editor-ui frontend))))))
  nil)

(serapeum/types:-> parent-terminal-ui-call-with-exclusive-input (parent-terminal-ui function) t)
(defun parent-terminal-ui-call-with-exclusive-input (frontend function)
  "Call FUNCTION while no other frontend poll can consume modal input.

Wrap the complete trusted approval or picker operation, not each individual poll."
  (bordeaux-threads:with-recursive-lock-held ((parent-terminal-ui-input-lock frontend))
    (let ((*terminal-ui-host* (parent-terminal-ui-host frontend)))
      (funcall function))))

(serapeum/types:-> parent-terminal-ui-choose
    (parent-terminal-ui string list &key (:initial-name (option string))
     (:initial-value (option string)) (:search-p boolean) (:hint (option string)))
    (option string))
(defun parent-terminal-ui-choose
    (frontend title choices &key initial-name initial-value (search-p t) hint)
  "Choose a trusted parent value with the current filtered modal picker."
  (unless (and (every #'terminal-completion-p choices)
               (terminal-ui-portable-value-p choices))
    (terminal-ui--reject-event))
  (parent-terminal-ui-call-with-exclusive-input
   frontend
   (lambda ()
     (terminal-ui-select (parent-terminal-ui-editor-ui frontend)
                         :title           (concatenate 'string "PARENT | " title)
                         :items choices :initial-name initial-name :initial-value initial-value
                         :search-p search-p :hint hint
                         :resize-callback #'terminal-relayed-resize-consume))))

(serapeum/types:-> parent-terminal-ui-read
    (parent-terminal-ui string &key (:initial-value string) (:secret-p boolean)
     (:timeout (option real)) (:cancelled (option function)))
    (values (option string) keyword))
(defun parent-terminal-ui-read
    (frontend prompt &key (initial-value "") secret-p timeout cancelled)
  "Read a trusted parent value in a separate modal editor.

Return text and :LINE, or NIL and :CANCELLED, :EOF or :TIMEOUT. Preserve the
ordinary composer and its history. Secret text is never passed to the renderer."
  (unless (and (stringp initial-value)
               (<= (length initial-value) *terminal-ui-event-character-limit*)
               (or (null timeout) (and (realp timeout) (not (minusp timeout)))))
    (terminal-ui--reject-event))
  (parent-terminal-ui-call-with-exclusive-input
   frontend
   (lambda ()
     (let* ((ui                           (parent-terminal-ui-editor-ui frontend))
            (terminal                     (terminal-ui-terminal ui))
            (original-editor              (terminal-ui-editor ui))
            (original-prompt              (terminal-ui-prompt ui))
            (original-placeholder         (terminal-ui-placeholder ui))
            (original-completions         (terminal-ui-completions ui))
            (original-completion-function (terminal-ui-completion-function ui))
            (original-selector            (terminal-ui-selector ui))
            (original-lisp-p              (terminal-ui-lisp-input-p ui))
            (editor                       (clinedi:line-editor-create :text initial-value :auto-pair-p nil))
            (display-editor               (if secret-p (clinedi:line-editor-create :auto-pair-p nil) editor))
            (deadline                     (and timeout (+ (parent-terminal--clock) timeout))))
       (unwind-protect
            (progn
              (with-terminal-ui-locked (ui)
                (setf (slot-value ui 'editor) display-editor
                      (slot-value ui 'prompt) (concatenate 'string "PARENT | " prompt " ")
                      (slot-value ui 'placeholder) ""
                      (slot-value ui 'completions) nil
                      (slot-value ui 'completion-function) nil
                      (terminal-ui-selector ui) nil
                      (terminal-ui-lisp-input-p ui) nil
                      (terminal-ui-prompt-render-cache ui) nil))
              (loop
                (when (or (parent-terminal-ui-closed-p frontend)
                          (and cancelled (funcall cancelled)))
                  (return (values nil ':cancelled)))
                (when (and deadline (>= (parent-terminal--clock) deadline))
                  (return (values nil ':timeout)))
                (with-terminal-ui-locked (ui)
                  (when secret-p
                    (clinedi:line-editor-set-text display-editor
                                          (make-string (length (clinedi:line-editor-text editor))
                                                       :initial-element #\*)
                                          :cursor (clinedi:line-editor-cursor editor)))
                  (terminal-ui--repaint-live ui))
                (terminal-ui-refresh-size ui #'terminal-relayed-resize-consume)
                (if (clinedi:terminal-input-ready-p terminal)
                    (let ((event (terminal-ui-read-event ui)))
                      (case event
                        (:stream-end
                         (return (values nil ':eof)))
                        ((:interrupt :escape)
                         (return (values nil ':cancelled)))
                        (otherwise
                         (unless (member event '(:history-previous :history-next :up :down
                                                  :complete :complete-previous))
                           (multiple-value-bind (action value)
                               (clinedi:line-editor-handle-event editor event)
                             (when (> (length (clinedi:line-editor-text editor))
                                      *terminal-ui-event-character-limit*)
                               (terminal-ui--reject-event))
                             (case action
                               (:submit
                                (return (values (copy-seq value) ':line)))
                               ((:eof :stream-end)
                                (return (values nil ':eof)))
                               (:interrupt
                                (return (values nil ':cancelled)))))))))
                    (sleep 0.005))))
         (clinedi:line-editor-clear editor)
         (with-terminal-ui-locked (ui)
           (setf (slot-value ui 'editor) original-editor
                 (slot-value ui 'prompt) original-prompt
                 (slot-value ui 'placeholder) original-placeholder
                 (slot-value ui 'completions) original-completions
                 (slot-value ui 'completion-function) original-completion-function
                 (terminal-ui-selector ui) original-selector
                 (terminal-ui-lisp-input-p ui) original-lisp-p
                 (terminal-ui-prompt-render-cache ui) nil)
           (terminal-ui--repaint-live ui)))))))

(serapeum/types:-> parent-terminal-ui-poll-event
    (parent-terminal-ui real) (values keyword (option (or string user-message-input))))
(defun parent-terminal-ui-poll-event (frontend seconds)
  "Poll the current composer and return its action and submission.

Actions include :SUBMIT, :QUEUE, :INTERRUPT, :EOF, :TIMEOUT, :EDIT-QUEUE,
:CYCLE-QUEUE and :KEPT. No Lisp or application command executes in this adapter."
  (unless (and (realp seconds) (not (minusp seconds)))
    (error 'parent-terminal-error :reason ':invalid-configuration))
  (bordeaux-threads:with-recursive-lock-held ((parent-terminal-ui-input-lock frontend))
    (let* ((*terminal-ui-host* (parent-terminal-ui-host frontend))
           (ui                 (parent-terminal-ui-editor-ui frontend))
           (terminal           (terminal-ui-terminal ui))
           (deadline           (+ (parent-terminal--clock) seconds)))
      (loop
        (when (parent-terminal-ui-closed-p frontend)
          (return (values ':eof nil)))
        (terminal-ui-refresh-size ui #'terminal-relayed-resize-consume)
        (terminal-ui-refresh-status ui)
        (when (clinedi:terminal-input-ready-p terminal)
          (let ((event (terminal-ui-read-event ui)))
            (when (eq event ':stream-end)
              (return (values ':eof nil)))
            (multiple-value-bind (action payload)
                (terminal-ui-process-event
                 ui event :queue-completion-p (parent-terminal-ui-active-p frontend)
                          :queue-editing-p    (parent-terminal-ui-queue-editing-p frontend))
              (unless (member action '(:changed :cleared :continue))
                (return (values action payload))))))
        (when (>= (parent-terminal--clock) deadline)
          (return (values ':timeout nil)))
        (sleep (min 0.005 (max 0 (- deadline (parent-terminal--clock)))))))))

(serapeum/types:-> parent-terminal-ui-input-source (parent-terminal-ui) function)
(defun parent-terminal-ui-input-source (frontend)
  "Return the PARENT-TERMINAL polling adapter for submitted lines and approvals.

Use POLL-EVENT directly in the active controller to retain queue-edit actions."
  (lambda (seconds)
    (multiple-value-bind (action input) (parent-terminal-ui-poll-event frontend seconds)
      (case action
        ((:submit :queue)
         (values (user-message-input-text input) ':line))
        (:interrupt
         (values nil ':cancelled))
        (:eof
         (values nil ':eof))
        (otherwise
         (values nil ':timeout))))))

(serapeum/types:-> parent-terminal-ui--append (parent-terminal-ui list) null)
(defun parent-terminal-ui--append (frontend spans)
  "Append one parent-produced presentation under a fresh local identity."
  (terminal-ui-append-finalized (parent-terminal-ui-editor-ui frontend)
                                (format nil "parent-ui-~D"
                                        (incf (parent-terminal-ui-sequence frontend)))
                                spans)
  nil)

(serapeum/types:-> parent-terminal-ui--approval-spans (keyword list) list)
(defun parent-terminal-ui--approval-spans (kind payload)
  "Build trusted approval text from the exact parent-owned snapshot."
  (list
   (terminal-span ':notice
                  (with-output-to-string (stream)
                    (case kind
                      (:approval
                       (format stream "PARENT | ACCESS ~A~%" (getf payload :operation-id))
                       (loop for grant across (getf payload :grants)
                             do (format stream "~A target=~A duration=~A operations=["
                                        (getf grant :resource)
                                        (parent-terminal--quoted-field (getf grant :target))
                                        (getf grant :duration))
                                (loop for operation across (getf grant :operations)
                                      for first-p = t then nil
                                      do (unless first-p (write-string "," stream))
                                         (write-string
                                          (parent-terminal--quoted-field operation) stream))
                                (write-line "]" stream))
                       (write-string "Type YES to approve these exact grants; any other answer denies."
                                     stream))
                      (:network-approval
                       (format stream "PARENT | NETWORK ~A" (getf payload :text)))
                      (:knowledge-approval
                       (format stream "PARENT | KNOWLEDGE ~A~%" (getf payload :text))
                       (write-string "Type YES to admit this exact proposal; any other answer denies."
                                     stream)))))))

(serapeum/types:-> parent-terminal-ui--agent-content (list) list)
(defun parent-terminal-ui--agent-content (content)
  "Keep agent text and widgets inside a fixed visible agent region."
  (let ((result nil) (line-start-p t) (first-line-p t))
    (labels ((begin-line ()
               (when line-start-p
                 (push (terminal-span ':dim (if first-line-p "AGENT | " "      | ")) result)
                 (setf line-start-p nil first-line-p nil))))
      (dolist (entry content)
        (if (termdown:widget-p entry)
            (progn (begin-line) (push entry result))
            (let ((text (terminal-span-text entry)) (start 0))
              (loop for newline = (position #\Newline text :start start)
                    for end = (if newline (1+ newline) (length text))
                    do (when (> end start)
                         (begin-line)
                         (push (terminal-span (terminal-span-style entry)
                                              (subseq text start end)) result))
                    when newline do (setf line-start-p t)
                    while newline do (setf start end)))))
      (nreverse result))))

(serapeum/types:-> parent-terminal-ui--agent-row (list) list)
(defun parent-terminal-ui--agent-row (portable)
  "Decode one semantic row and add the trusted origin marker."
  (parent-terminal-ui--agent-content (terminal-ui-portable->content portable)))

(serapeum/types:-> parent-terminal-ui-event (parent-terminal-ui keyword list) null)
(defun parent-terminal-ui-event (frontend kind payload)
  "Apply one bounded semantic event through the current renderer.

This entry accepts agent presentation only. Trusted approval prompts use the
separate parent renderer and cannot be requested through these event kinds."
  (unless (terminal-ui-event-valid-p kind payload)
    (terminal-ui--reject-event))
  (let* ((*terminal-ui-host* (parent-terminal-ui-host frontend))
         (ui                 (parent-terminal-ui-editor-ui frontend)))
    (case kind
      (:stream
       (terminal-ui-stream-update
        ui :rows  (mapcar #'parent-terminal-ui--agent-row (getf payload :rows))
            :tail (if (getf payload :tail-rows)
                      (mapcar #'parent-terminal-ui--agent-row (getf payload :tail-rows))
                      (let ((tail (getf payload :tail)))
                        (and tail (concatenate 'string "AGENT | " tail))))))
      (:finalized
       (terminal-ui-append-finalized
        ui (concatenate 'string "agent/" (getf payload :id))
        (parent-terminal-ui--agent-row (getf payload :content))))
      (:epilogue
       (terminal-ui-set-epilogue ui (parent-terminal-ui--agent-row (getf payload :content))))
      (:preview
       (terminal-ui-set-preview-rows
        ui (mapcar #'parent-terminal-ui--agent-row (getf payload :rows))))
      (:status
       (terminal-ui-set-status
        ui (getf payload :status)
        :details        (terminal-ui-portable->content (getf payload :details))
        :worked-seconds (getf payload :worked-seconds)))
      (:context
       (terminal-ui-set-context-usage
        ui :used (getf payload :used) :window (getf payload :window)
           :compaction-limit (getf payload :compaction-limit)
           :estimated-p      (getf payload :estimated-p)))
      (:notice
       (terminal-ui-set-notice ui (getf payload :text)
                               :duration-seconds (or (getf payload :duration-seconds) 4)))
      (:agent-activities
       (terminal-ui-set-agent-activities ui (getf payload :activities)))
      (:command-activities
       (terminal-ui-set-command-activities ui (getf payload :activities)))
      (:pending-inputs
       (terminal-ui-set-pending-inputs ui (getf payload :steering) (getf payload :queued)))
      (:compacting
       (terminal-ui-set-compacting ui (getf payload :active-p)))
      (:local-activity
       (terminal-ui-set-local-activity ui (getf payload :text)))
      (:idle-status
       (terminal-ui-set-idle-status-details
        ui (terminal-ui-portable->content (getf payload :details))))
      (:reset-transcript
       (terminal-ui-reset-transcript ui))))
  nil)

(serapeum/types:-> parent-terminal-ui-render (parent-terminal-ui &key (:region keyword)
                                              (:kind keyword) (:payload list)) null)
(defun parent-terminal-ui-render (frontend &key region kind payload)
  "Render bounded inert events with distinct parent and agent presentation.

Only fixed parent event kinds receive the trusted approval style. All text
from agent events remains in the agent region, including claimed parent labels."
  (let* ((*terminal-ui-host* (parent-terminal-ui-host frontend))
         (ui                 (parent-terminal-ui-editor-ui frontend)))
    (cond
      ((and (eq region ':trusted) (member kind '(:approval :knowledge-approval :network-approval)))
       (parent-terminal-ui--append frontend (parent-terminal-ui--approval-spans kind payload)))
      ((eq region ':trusted)
       (parent-terminal-ui--append
        frontend (list (terminal-span ':notice "PARENT | ")
                       (terminal-span ':plain (getf payload :text "")))))
      ((eq region ':agent)
       (parent-terminal-ui--append
        frontend (list (terminal-span ':dim "AGENT | ")
                       (terminal-span ':plain (getf payload :text "")))))
      (t
       (error 'parent-terminal-error :reason ':invalid-configuration)))
    (terminal-ui-refresh-status ui))
  nil)

(serapeum/types:-> parent-terminal-ui-renderer (parent-terminal-ui) function)
(defun parent-terminal-ui-renderer (frontend)
  "Return the synchronous callback expected by MAKE-PARENT-TERMINAL."
  (lambda (region kind payload)
    (parent-terminal-ui-render frontend :region region :kind kind :payload payload)))
