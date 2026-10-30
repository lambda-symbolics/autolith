;;;; -- Remote Terminal Presentation --

(in-package #:autolith)

(defclass agent-protocol-terminal (terminal) ()
  (:documentation "Terminal geometry used by child renderers without a local transport."))

(defclass agent-protocol-terminal-ui (terminal-ui)
  ((session
    :initarg       :session
    :reader        agent-protocol-terminal-ui-session
    :documentation "The child session receiving semantic presentation events.")
   (event-function
    :initarg       :event-function
    :reader        agent-protocol-terminal-ui-event-function
    :documentation "The function sending each fixed event kind and inert payload.")
   (counter
    :initform      0
    :accessor      agent-protocol-terminal-ui-counter
    :documentation "The monotonically increasing presentation identifier.")
   (identifiers
    :initform      (make-hash-table :test #'equal)
    :reader        agent-protocol-terminal-ui-identifiers
    :documentation "The child identifiers whose finalized entries were sent successfully."))
  (:documentation "Child-side semantic UI with no editor, live region, or terminal I/O."))

(defmethod terminal--write ((terminal agent-protocol-terminal) (text string))
  "Reject accidental local writes from a child renderer."
  (declare (ignore terminal text))
  (error 'agent-protocol-error :reason ':remote-terminal-write))

(defmethod terminal-ui-editor ((ui agent-protocol-terminal-ui))
  "Return no editor because only the parent owns terminal input."
  nil)

(defmethod terminal-ui-live-region ((ui agent-protocol-terminal-ui))
  "Return no local live region for remote presentation."
  nil)

(defmethod terminal-ui-fullscreen-p ((ui agent-protocol-terminal-ui))
  "Report that the child owns no fullscreen terminal."
  nil)

(defmethod terminal-ui-reset-transcript ((ui agent-protocol-terminal-ui))
  "Reset parent transcript state and child deduplication state together."
  (with-terminal-ui-locked (ui)
    (agent-protocol-terminal-ui--emit ui ':reset-transcript nil)
    (clrhash (agent-protocol-terminal-ui-identifiers ui)))
  ui)

(defmethod terminal-ui--finalized-content ((ui agent-protocol-terminal-ui) entry)
  "Return plain capture text without local display rendering."
  (declare (ignore ui))
  (let* ((text       (if (stringp entry) entry (terminal--spans-text entry)))
         (newline    (string #\Newline))
         (terminated (if (and (plusp (length text))
                              (char= (char text (1- (length text))) #\Newline))
                         text
                         (concatenate 'string text newline)))
         (plain      (if (autolith-supervisor::terminal-ui--compact-entry-group entry)
                         terminated
                         (concatenate 'string terminated newline))))
    (values plain plain nil)))

(defmethod terminal-ui-append-finalized ((ui agent-protocol-terminal-ui) identifier entry)
  "Send a finalized entry once, recording its identifier only after successful send."
  (with-terminal-ui-locked (ui)
    (unless (gethash identifier (agent-protocol-terminal-ui-identifiers ui))
      (agent-protocol-terminal-ui--emit
       ui ':finalized
       (list :id      (agent-protocol-terminal-ui--next-id ui)
             :content (autolith-supervisor::terminal-ui-content->portable entry)))
      (setf (gethash identifier (agent-protocol-terminal-ui-identifiers ui)) t)
      t)))

(defmethod terminal-ui-append-finalized-batch ((ui agent-protocol-terminal-ui) entries)
  "Send each ordered entry through the same deduplicated presentation path."
  (with-terminal-ui-locked (ui)
    (loop for (identifier entry) in entries
          count (terminal-ui-append-finalized ui identifier entry))))

(defmethod terminal-ui-set-epilogue ((ui agent-protocol-terminal-ui) entry)
  "Send epilogue content through the semantic parent renderer."
  (with-terminal-ui-locked (ui)
    (agent-protocol-terminal-ui--emit
     ui ':epilogue
     (list :id      (agent-protocol-terminal-ui--next-id ui)
           :content (autolith-supervisor::terminal-ui-content->portable entry))))
  nil)

(defmethod terminal-ui-stream-update ((ui agent-protocol-terminal-ui) &key rows tail)
  "Send completed rows and styled unfinished content independently of preview state."
  (with-terminal-ui-locked (ui)
    (agent-protocol-terminal-ui--emit
     ui ':stream
     (list :rows      (agent-protocol-terminal-ui--rows rows)
           :tail-rows (agent-protocol-terminal-ui--rows
                       (cond
                         ((null tail)
                          nil)
                         ((or (stringp tail) (terminal-styled-text-p tail))
                          (list tail))
                         (t
                          tail)))))
    (setf (terminal-ui-stream-tail ui) tail))
  ui)

(defmethod terminal-ui-set-preview-rows ((ui agent-protocol-terminal-ui) rows)
  "Send transient styled preview rows without painting the child terminal."
  (agent-protocol-terminal-ui--emit
   ui ':preview (list :rows (agent-protocol-terminal-ui--rows rows))))

(defmethod terminal-ui-set-notice ((ui agent-protocol-terminal-ui) notice &key duration-seconds)
  "Send a transient notice and report successful application."
  (agent-protocol-terminal-ui--emit
   ui ':notice (list :text notice :duration-seconds duration-seconds))
  (values ui t))

(defmethod terminal-ui-set-context-usage
    ((ui agent-protocol-terminal-ui) &key used window compaction-limit (estimated-p nil))
  "Send bounded context usage and measurement provenance."
  (agent-protocol-terminal-ui--emit
   ui ':context (list :used used :window window :compaction-limit compaction-limit
                     :estimated-p estimated-p)))

(defmethod terminal-ui-set-compacting ((ui agent-protocol-terminal-ui) compacting-p)
  "Send the current compaction state."
  (agent-protocol-terminal-ui--emit ui ':compacting (list :active-p compacting-p)))

(defmethod terminal-ui-set-idle-status-details ((ui agent-protocol-terminal-ui) details)
  "Send styled idle session details."
  (agent-protocol-terminal-ui--emit
   ui ':idle-status (list :details (autolith-supervisor::terminal-ui-content->portable details))))

(defmethod terminal-ui-set-status
    ((ui agent-protocol-terminal-ui) status &key details worked-seconds)
  "Send activity status and its initial accumulated working time."
  (agent-protocol-terminal-ui--emit
   ui ':status (list :status        status
                    :details        (autolith-supervisor::terminal-ui-content->portable details)
                    :worked-seconds worked-seconds)))

(defmethod terminal-ui-set-local-activity ((ui agent-protocol-terminal-ui) activity)
  "Send the local Lisp activity label."
  (agent-protocol-terminal-ui--emit ui ':local-activity (list :text activity)))

(defmethod terminal-ui-set-agent-activities ((ui agent-protocol-terminal-ui) activities)
  "Send raw activity snapshots for parent-side validation and time anchoring."
  (agent-protocol-terminal-ui--emit ui ':agent-activities (list :activities activities)))

(defmethod terminal-ui-set-command-activities ((ui agent-protocol-terminal-ui) activities)
  "Send raw command snapshots without child-side animation state."
  (agent-protocol-terminal-ui--emit ui ':command-activities (list :activities activities)))

(defmethod terminal-ui-clear-command-activities ((ui agent-protocol-terminal-ui))
  "Clear command snapshots through the same remote presentation protocol."
  (terminal-ui-set-command-activities ui nil))

(defmethod terminal-ui-refresh-status ((ui agent-protocol-terminal-ui))
  "Leave animation timing to the parent presentation loop."
  nil)

(defmethod terminal-ui-set-pending-inputs
    ((ui agent-protocol-terminal-ui) steering-inputs queued-inputs)
  "Send pending input previews without constructing an active child editor."
  (agent-protocol-terminal-ui--emit
   ui ':pending-inputs (list :steering steering-inputs :queued queued-inputs)))

(-> agent-protocol-terminal-ui-create
    (agent-protocol-session &key (:rows (integer 1)) (:columns (integer 1))
                                (:event-function (option function)))
    agent-protocol-terminal-ui)
(defun agent-protocol-terminal-ui-create
    (session &key (rows 24) (columns 80) event-function)
  "Create a remote semantic UI using SESSION and approved terminal geometry."
  (make-instance 'agent-protocol-terminal-ui
                 :session  session
                 :terminal (make-instance 'agent-protocol-terminal :rows rows :columns columns)
                 :event-function
                 (or event-function
                     (lambda (kind payload)
                       (let ((fields (make-hash-table :test #'equal)))
                         (setf (gethash "kind" fields) (string-downcase kind)
                               (gethash "payload" fields)
                               (autolith-supervisor::terminal-ui-event->wire payload))
                         (agent-protocol-application-event session ':ui fields))))))

(-> agent-protocol-terminal-ui--next-id (agent-protocol-terminal-ui) string)
(defun agent-protocol-terminal-ui--next-id (ui)
  "Return a compact monotonically increasing remote entry identifier."
  (format nil "child-ui/~D" (incf (agent-protocol-terminal-ui-counter ui))))

(-> agent-protocol-terminal-ui--rows (list) list)
(defun agent-protocol-terminal-ui--rows (rows)
  "Encode each styled semantic row using the shared content codec."
  (mapcar #'autolith-supervisor::terminal-ui-content->portable rows))

(-> agent-protocol-terminal-ui--emit (agent-protocol-terminal-ui keyword t) agent-protocol-terminal-ui)
(defun agent-protocol-terminal-ui--emit (ui kind payload)
  "Validate each inert event before sending it under the UI presentation lock."
  (unless (autolith-supervisor::terminal-ui-event-valid-p kind payload)
    (error 'agent-protocol-error :reason ':invalid-terminal-ui-event))
  (with-terminal-ui-locked (ui)
    (funcall (agent-protocol-terminal-ui-event-function ui) kind payload))
  ui)
