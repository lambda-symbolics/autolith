(in-package #:autolith)

;;;; -- Channel Presentation --

;;; A channel's server has no terminal and composes no rows. Its application
;;; talks to a CHANNEL-PRESENTATION, a UI that holds what the channel shows
;;; and tells the clients that present it. Each change is one event
;;;
;;;   (:ui :operation OPERATION FIELD VALUE ...)
;;;
;;; which a client applies to its own terminal UI with PRESENTATION-APPLY.
;;; The client chooses the colors, the width and the wrapping. Styled text
;;; travels as (STYLE TEXT) spans and (STYLE TEXT ACTION) widgets, times as
;;; ages in seconds. A client of another version ignores the operations and
;;; fields it does not know.

(defparameter *channel-presentation-log-character-limit* 1048576
  "The most characters of transcript operations a channel keeps for a client that attaches later.")

(defparameter *channel-presentation-progress-seconds* 1
  "The least seconds between two progress notes a channel sends its clients.")

(defparameter *presentation-entry-transforms*
  '((:resource-edit-failure . application--resource-edit-failure-entry))
  "The functions a record may name to rewrite its call entry for an outcome, by wire name.")


;;;; -- Wire Forms --

(-> presentation-entry->wire ((or string list)) (or string list))
(defun presentation-entry->wire (entry)
  "Return styled ENTRY as plain lists a client can read.

A string stays a string. A span becomes (STYLE TEXT) and a widget
(STYLE TEXT ACTION)."
  (if (stringp entry)
      entry
      (loop for item in entry
            collect (if (termdown:widget-p item)
                        (list (termdown:widget-role item)
                              (termdown:widget-label item)
                              (termdown:widget-action item))
                        (list (first item) (rest item))))))

(-> presentation-wire->entry (t) (or string list))
(defun presentation-wire->entry (wire)
  "Return the styled entry WIRE describes, dropping what is not a span or a widget.

A style this version does not know shows as plain text."
  (if (stringp wire)
      wire
      (loop for item in (and (listp wire) wire)
            when (and (consp item) (keywordp (first item))
                      (consp (rest item)) (stringp (second item)))
              collect (let ((style (if (typep (first item) 'terminal-style) (first item) ':plain)))
                        (if (consp (rest (rest item)))
                            (termdown:make-widget style (second item) (third item))
                            (terminal-span style (second item)))))))

(-> presentation-rows->wire (list) list)
(defun presentation-rows->wire (rows)
  "Return styled ROWS as wire entries."
  (mapcar #'presentation-entry->wire rows))

(-> presentation-wire->rows (t) list)
(defun presentation-wire->rows (wire)
  "Return the styled rows WIRE describes."
  (mapcar #'presentation-wire->entry (and (listp wire) wire)))

(-> presentation--weight (t) (integer 0))
(defun presentation--weight (value)
  "Estimate retained size in character units, charging four for each wire cons as well as its strings."
  (cond
    ((stringp value)
     (length value))
    ((consp value)
     (loop for tail = value then (rest tail)
           while (consp tail)
           sum (+ 4 (presentation--weight (first tail)))))
    (t
     0)))

(-> presentation--source-input-p (t) boolean)
(defun presentation--source-input-p (description)
  "Recognize this version's data-only source input recipe."
  (handler-case
      (and (listp description) (eql 1 (getf description :version))
           (stringp (getf description :header))
           (listp (getf description :sources)) (every #'stringp (getf description :sources))
           (listp (getf description :fields))
           (every (lambda (field)
                    (and (listp field) (stringp (getf field :label))
                         (stringp (getf field :value)) (typep (getf field :style) 'terminal-style)))
                  (getf description :fields))
           (typep (getf description :restart-value) '(or null string))
           (typep (getf description :invocation) '(or null string))
           (typep (getf description :detail) '(or null string))
           (typep (getf description :line-limit) '(integer 1))
           (typep (getf description :lisp-columns) '(integer 1)))
    (type-error () nil)))

(-> presentation--tool-entry-p (t) boolean)
(defun presentation--tool-entry-p (description)
  "Recognize a versioned bounded pre-clipping ordinary tool entry."
  (handler-case
      (and (listp description) (eql 1 (getf description :version))
           (stringp (getf description :header)) (typep (getf description :style) 'terminal-style)
           (typep (getf description :detail) '(or null string))
           (typep (getf description :invocation) '(or null string))
           (typep (getf description :provider-call-p) 'boolean)
           (typep (getf description :inline-p) 'boolean)
           (listp (getf description :rows)) (every #'listp (getf description :rows)))
    (type-error () nil)))

(-> presentation--wire-tool-rows (list vector) list)
(defun presentation--wire-tool-rows (rows actions)
  "Restore styled ROWS whose widget references name the shared ACTIONS table."
  (loop for row in rows
        collect (presentation-wire->entry
                 (loop for span in row
                       collect (if (and (consp span) (consp (rest span)))
                                   (let ((index (third span)))
                                     (if (and (typep index '(integer 0)) (< index (length actions)))
                                         (list (first span) (second span) (aref actions index))
                                         (list (first span) (second span))))
                                   span)))))

(-> presentation--tool-entry->wire (list list) (values list list))
(defun presentation--tool-entry->wire (description entry)
  "Share ENTRY's actions once beside its display data, retaining legacy fallback spans."
  (let ((actions nil)
        (fields  (copy-list description)))
    (labels ((action-index (action)
               "Intern one action in this record's small action table."
               (or (position action actions :test #'equal)
                   (prog1 (length actions)
                     (setf actions (append actions (list action)))))))
      (remf fields :invocation)
      (setf (getf fields :header-action)
            (when (getf description :invocation)
              (action-index (list ':insert (getf description :invocation))))
            (getf fields :entry-actions)
            (loop for span in entry for index from 0
                  when (termdown:widget-p span)
                    collect (list index (action-index (termdown:widget-action span)))))
      (when (getf description :rows)
        (setf (getf fields :rows)
              (loop for row in (getf description :rows)
                    collect (loop for span in row
                                  collect (if (termdown:widget-p span)
                                              (list (termdown:widget-role span) (termdown:widget-label span)
                                                    (action-index (termdown:widget-action span)))
                                              (list (terminal-span-style span) (terminal-span-text span)))))))
      (setf (getf fields :actions) actions)
      (values fields
              (loop for span in entry
                    collect (list (terminal-span-style span) (terminal-span-text span)))))))

(-> presentation--wire-tool-entry
    (t (or string list)) (values (option list) (or string list)))
(defun presentation--wire-tool-entry (wire entry)
  "Restore recognized display data and actions, or retain only collapsed ENTRY."
  (handler-case
      (if (and (listp entry)
               (or (presentation--source-input-p wire) (presentation--tool-entry-p wire))
               (listp (getf wire :actions)) (listp (getf wire :entry-actions)))
          (let* ((description   (copy-list wire))
                 (actions       (coerce (getf wire :actions) 'vector))
                 (header-index  (getf wire :header-action))
                 (header-action (when (and (typep header-index '(integer 0))
                                           (< header-index (length actions)))
                                  (aref actions header-index)))
                 (restored      (coerce entry 'vector)))
            (dolist (key '(:actions :entry-actions :header-action))
              (remf description key))
            (setf (getf description :invocation)
                  (when (and (listp header-action) (= 2 (length header-action))
                             (eq (first header-action) ':insert) (stringp (second header-action)))
                    (second header-action)))
            (when (getf wire :rows)
              (setf (getf description :rows) (presentation--wire-tool-rows (getf wire :rows) actions)))
            (dolist (reference (getf wire :entry-actions))
              (when (and (listp reference) (= 2 (length reference))
                         (typep (first reference) '(integer 0)) (< (first reference) (length restored))
                         (typep (second reference) '(integer 0)) (< (second reference) (length actions)))
                (let ((span (aref restored (first reference))))
                  (setf (aref restored (first reference))
                        (termdown:make-widget (terminal-span-style span) (terminal-span-text span)
                                              (aref actions (second reference)))))))
            (values description (coerce restored 'list)))
          (values nil entry))
    (type-error () (values nil entry))))

(-> presentation--source-input-layouts (list) list)
(defun presentation--source-input-layouts (description)
  "Reconstruct local layouts around source data without rendering or evaluating it."
  (let ((cache (make-hash-table :test #'equal)))
    (flet ((layout (columns expanded-p)
             "Render only the variant requested by the local viewport."
             (let ((*application-transcript-source-cache* cache))
               (declare (special *application-transcript-source-cache*))
               (application--source-input-entry description columns :expanded-p expanded-p))))
      (list :source-input          description
            :entry-layout          (lambda (columns) (layout columns nil))
            :expanded-entry-layout (lambda (columns) (layout columns t))
            :source-characters
            (lambda ()
              (let ((*application-transcript-source-cache* cache))
                (declare (special *application-transcript-source-cache*))
                (application--record-source-characters description)))))))

(-> presentation-record->wire (list (integer 1) &key (:lay-out-p boolean)) list)
(defun presentation-record->wire (presentation columns &key lay-out-p)
  "Return (IDENTIFIER ENTRY METADATA) PRESENTATION as data, laid out at COLUMNS.

ENTRY is taken as its producer just laid it out; with LAY-OUT-P the record's
layout function lays it out again, as for a client of another width.

A source input travels as a versioned data recipe, rendered only by the local
client. Other layouts become their rendered data. A transform travels by its
name in *PRESENTATION-ENTRY-TRANSFORMS*."
  (destructuring-bind (identifier entry metadata) presentation
    (let ((wire nil))
      (loop for (key value) on metadata by (partial #'nthcdr 2)
            do (case key
                 ((:entry-layout :schema-layout)
                  nil)
                 ((:source-input :tool-entry)
                  nil)
                 (:expanded-entry-layout
                  (when (and value (not (getf metadata :source-input)) (not (getf metadata :tool-entry)))
                    (setf (getf wire :expanded-entry)
                          (presentation-entry->wire (funcall value columns)))))
                 (:body-layout
                  (when value
                    (setf (getf wire :body) (presentation-entry->wire (funcall value columns)))))
                 (:body
                  (unless (getf metadata :body-layout)
                    (setf (getf wire :body) (and value (presentation-entry->wire value)))))
                 ((:handoff :annotations)
                  (setf (getf wire key) (and value (presentation-entry->wire value))))
                 ((:source-characters :body-source-characters)
                  (setf (getf wire key) (if (functionp value) (funcall value) value)))
                 (:entry-transform
                  (let ((named (find-if (lambda (transform)
                                          (and (fboundp (rest transform))
                                               (eq value (fdefinition (rest transform)))))
                                        *presentation-entry-transforms*)))
                    (when named
                      (setf (getf wire key) (first named)))))
                 (:origin
                  (when value
                    (setf (getf wire key)
                          (presentation-record->wire value columns :lay-out-p lay-out-p))))
                 (:markdown
                  (setf (getf wire key) (presentation--markdown->wire value)))
                 (otherwise
                  (setf (getf wire key) value))))
      (let ((entry      (if (and lay-out-p (getf metadata :entry-layout))
                            (funcall (getf metadata :entry-layout) columns)
                            entry))
            (source     (getf metadata :source-input))
            (tool-entry (getf metadata :tool-entry)))
        (let ((key (cond ((presentation--source-input-p source)
                          ':source-input)
                         ((presentation--tool-entry-p tool-entry)
                          ':tool-entry))))
          (if key
              (multiple-value-bind (description collapsed)
                  (presentation--tool-entry->wire (getf metadata key) entry)
                (setf (getf wire key) description)
                (list identifier
                      (if (eq key ':tool-entry) (presentation-entry->wire entry) collapsed)
                      wire))
              (list identifier (and entry (presentation-entry->wire entry)) wire)))))))

(defparameter *terminal-compaction-label* " CONTEXT COMPACTED "
  "The label centered in the transcript rule marking a context compaction.")

(-> terminal-compaction-divider ((integer 1)) terminal-styled-text)
(defun terminal-compaction-divider (columns)
  "Return a centered compaction divider spanning exactly COLUMNS cells."
  (let* ((label (subseq *terminal-compaction-label* 0
                        (min columns (length *terminal-compaction-label*))))
         (left  (floor (- columns (length label)) 2))
         (right (- columns (length label) left)))
    (list (terminal-span ':success (make-string left :initial-element #\━))
          (terminal-span ':failure label)
          (terminal-span ':success (make-string right :initial-element #\━)))))

(-> presentation-wire->record (t) (option list))
(defun presentation-wire->record (wire)
  "Return the (IDENTIFIER ENTRY METADATA) presentation WIRE describes, or NIL when it is none.

The layouts a resident record needs are rebuilt from its data."
  (when (and (listp wire) (= (length wire) 3) (listp (third wire)))
    (destructuring-bind (identifier entry fields) wire
      (let ((metadata nil))
        (loop for (key value) on fields by (partial #'nthcdr 2)
              do (case key
                   ((:source-input :tool-entry)
                    nil)
                   (:expanded-entry
                    (let ((expanded (presentation-wire->entry value)))
                      (setf (getf metadata :expanded-entry-layout) (constantly expanded))))
                   ((:body :handoff :annotations)
                    (setf (getf metadata key) (and value (presentation-wire->entry value))))
                   (:entry-transform
                    (let ((function (rest (assoc value *presentation-entry-transforms*))))
                      (when (and function (fboundp function))
                        (setf (getf metadata key) (fdefinition function)))))
                   (:origin
                    (setf (getf metadata key) (presentation-wire->record value)))
                   (:markdown
                    (setf (getf metadata key) (presentation--wire->markdown value)))
                   (otherwise
                    (setf (getf metadata key) value))))
        (when (and (eq (getf metadata :entry-kind) ':tool-schemas)
                   (every #'stringp (getf metadata :schema-names)))
          (let ((names (getf metadata :schema-names)))
            (setf (getf metadata :schema-layout) #'terminal-tool-schema-entry
                  (getf metadata :entry-layout)
                  (lambda (columns) (terminal-tool-schema-entry names columns)))))
        (multiple-value-bind (description entry)
            (presentation--wire-tool-entry (or (getf fields :source-input) (getf fields :tool-entry))
                                           (and entry (presentation-wire->entry entry)))
          (cond
            ((presentation--source-input-p description)
             (setf metadata (append (presentation--source-input-layouts description) metadata)))
            ((presentation--tool-entry-p description)
             (setf (getf metadata :tool-entry) description
                   (getf metadata :entry-layout)
                   (lambda (columns) (application--tool-entry-from-description description columns)))))
          (when (and (eq (getf metadata :entry-kind) ':summary) (consp entry))
            ;; The rendered first line is width dependent. Keep the styled notice below it.
            (let ((notice (member (terminal-span ':plain (string #\Newline)) entry :test #'equal)))
              (setf (getf metadata :entry-layout)
                    (lambda (columns) (append (terminal-compaction-divider columns) notice)))))
          (list identifier entry metadata))))))

(-> terminal-tool-schema-entry (list (integer 1)) (option list))
(defun terminal-tool-schema-entry (names columns)
  "Return one row naming the tool schema namespaces NAMES, cut to fit COLUMNS."
  (when names
    (let ((maximum-width (max 1 (1- columns)))
          (row           (cons (terminal-span ':tool "▸ tool schemas: ")
                               (loop for name in names
                                     for first-p = t then nil
                                     unless first-p
                                       collect (terminal-span ':dim ", ")
                                     collect (terminal-span ':syntax-function name)))))
      (if (<= (terminal--spans-width row) maximum-width)
          row
          (append (terminal--clip-spans row (max 0 (1- maximum-width)))
                  (list (terminal-span ':dim "…")))))))

(-> presentation--markdown->wire (list) list)
(defun presentation--markdown->wire (markdown)
  "Return a Markdown publication's plist with its header and marker as wire entries."
  (list :source     (getf markdown :source)
        :finished-p (getf markdown :finished-p)
        :stream-p   (getf markdown :stream-p)
        :header     (presentation-entry->wire (getf markdown :header))
        :marker     (presentation-entry->wire (getf markdown :marker))))

(-> presentation--wire->markdown (t) list)
(defun presentation--wire->markdown (wire)
  "Return the Markdown publication WIRE describes."
  (let ((fields (and (listp wire) wire)))
    (list :source     (if (stringp (getf fields :source)) (getf fields :source) "")
          :finished-p (not (null (getf fields :finished-p)))
          :stream-p   (not (null (getf fields :stream-p)))
          :header     (presentation-wire->entry (getf fields :header))
          :marker     (presentation-wire->entry (getf fields :marker)))))


;;;; -- The Server's Model --

(defclass presentation-terminal (terminal)
  ((interactive-function
    :initform      nil
    :accessor      presentation-terminal-interactive-function
    :type          (option function)
    :documentation "The function saying whether a client presents the channel now, or NIL.")
   (event-lock
    :initform      (make-lock "Autolith presentation input")
    :reader        presentation-terminal-event-lock
    :documentation "The lock guarding the events a modal reader waits for.")
   (event-condition
    :initform      (make-condition-variable :name "Autolith presentation input")
    :reader        presentation-terminal-event-condition
    :documentation "Signaled when a client's event arrives for a modal reader.")
   (events
    :initform      (make-deque)
    :reader        presentation-terminal-events
    :documentation "The events a client sent for the open modal reader, oldest first.")
   (modal-depth
    :initform      0
    :accessor      presentation-terminal-modal-depth
    :type          (integer 0)
    :documentation "How many modal readers, such as a picker inside a command, are open.")
   (text-function
    :initform      nil
    :accessor      presentation-terminal-text-function
    :type          (option function)
    :documentation "The function given each line a program prints for the user, such as a provider's login, or NIL.")
   (page-function
    :initform      nil
    :accessor      presentation-terminal-page-function
    :type          (option function)
    :documentation "The function given the rows and cursor of a page that takes the whole pane, or NIL."))
  (:documentation "The terminal of a channel no process writes to.

It holds the size and styling of the client that presents the channel, so
entries are laid out for that client. Nothing written to it goes anywhere.
It has no keyboard: while a picker or another modal reader is open on the
channel, the client sends its events, and they are read here."))

(defparameter *presentation-terminal-event-limit* 4096
  "The most events a modal reader may have waiting before a client's further ones are dropped.")

(defparameter *presentation-terminal-poll-seconds* 0.25
  "How often a modal reader that waits for an event checks that a client still presents the channel.")

(-> presentation-terminal-offer-event (presentation-terminal t) boolean)
(defun presentation-terminal-offer-event (terminal event)
  "Give a client's EVENT to TERMINAL's open modal reader and return true when one takes it."
  (with-lock-held ((presentation-terminal-event-lock terminal))
    (when (and (plusp (presentation-terminal-modal-depth terminal))
               (< (deque-count (presentation-terminal-events terminal))
                  *presentation-terminal-event-limit*))
      (deque-push-back (presentation-terminal-events terminal) event)
      (condition-notify (presentation-terminal-event-condition terminal))
      t)))

(defclass channel-presentation (fullscreen-terminal-ui)
  ((publish-function
    :initform      nil
    :accessor      channel-presentation-publish-function
    :type          (option function)
    :documentation "The function given each (:ui ...) event to send the channel's clients, or NIL.")
   (transcript-log
    :initform      (make-deque)
    :reader        channel-presentation-transcript-log
    :documentation "Cells holding the transcript events a later client replays, oldest first.")
   (log-characters
    :initform      0
    :accessor      channel-presentation-log-characters
    :type          (integer 0)
    :documentation "The characters the transcript log holds.")
   (streams
    :initform      (make-hash-table :test #'eq)
    :reader        channel-presentation-streams
    :documentation "Each unfinished Markdown stream's (WIRE-IDENTIFIER SOURCE LOG-CELL), by its identifier.")
   (stream-counter
    :initform      0
    :accessor      channel-presentation-stream-counter
    :type          (integer 0)
    :documentation "The number the next Markdown stream's wire identifier takes.")
   (published-state
    :initform      (make-hash-table :test #'eq)
    :reader        channel-presentation-published-state
    :documentation "What each state operation last told the clients, by operation.")
   (published-completion-revision
    :initform      nil
    :accessor      channel-presentation-published-completion-revision
    :documentation "The provider revision of the last complete catalog publication.")
   (agent-activity-arguments
    :initform      nil
    :accessor      channel-presentation-agent-activity-arguments
    :type          list
    :documentation "The child-agent activities last set, as their producer described them.")
   (command-activity-arguments
    :initform      nil
    :accessor      channel-presentation-command-activity-arguments
    :type          list
    :documentation "The command activities last set, as their producer described them.")
   (progress-published-at
    :initform      nil
    :accessor      channel-presentation-progress-published-at
    :type          (option real)
    :documentation "When the clients last heard of progress, on the UI's clock.")
   (turn-state-function
    :initform      nil
    :accessor      channel-presentation-turn-state-function
    :type          (option function)
    :documentation "The function returning the :TURN fields: whether the channel works, cancels, or edits a follow-up.")
   (history-function
    :initform      nil
    :accessor      channel-presentation-history-function
    :type          (option function)
    :documentation "The function returning the newest durable inputs, the offset of the first, and whether the rest pages in.")
   (record-boundary-function
    :initform      nil
    :accessor      channel-presentation-record-boundary-function
    :type          (option function)
    :documentation "The function returning the durable conversation identity and sequence preceding local output.")
   (window-title
    :initform      nil
    :accessor      channel-presentation-window-title
    :type          (option string)
    :documentation "The title the clients' terminal windows take.")
   (text-counter
    :initform      0
    :accessor      channel-presentation-text-counter
    :type          (integer 0)
    :documentation "The number the next line a program printed takes in the transcript.")
   (page
    :initform      nil
    :accessor      channel-presentation-page
    :type          list
    :documentation "The :PAGE fields of the page that takes the clients' panes now, or NIL."))
  (:documentation "What one hosted channel shows, kept as records and state for its clients to render.

It is a UI without a screen: the application sets it as it sets any terminal
UI, and every change reaches the clients as a (:ui ...) event."))

(-> channel-presentation-create
    (&key (:rows (option integer)) (:columns (option integer)) (:placeholder string)
          (:prompt string) (:prompt-padding (integer 0 2)))
    channel-presentation)
(defun channel-presentation-create
    (&key rows columns (prompt "> ") (placeholder "") (prompt-padding 1))
  "Create the presentation of a hosted channel, laid out at ROWS by COLUMNS until a client says otherwise."
  (let ((terminal (make-instance 'presentation-terminal
                                 :rows          (or rows *terminal-default-rows*)
                                 :columns       (or columns *terminal-default-columns*)
                                 :interactive-p nil
                                 :styled-p      t)))
    (let ((ui (make-instance
               'channel-presentation
               :terminal            terminal
               :editor              (line-editor-create
                                     :history-limit           *terminal-history-limit*
                                     :history-character-limit *terminal-history-character-limit*)
               :live-region         (make-live-region
                                     :columns        (terminal-columns terminal)
                                     :maximum-rows   1
                                     :write-function (constantly nil)
                                     :flush-function (constantly nil))
               :clock-function      #'terminal-ui--monotonic-seconds
               :prompt              prompt
               :prompt-padding      prompt-padding
               :placeholder         placeholder
               :completion-selector (make-selector :visible-count 1 :arrangement ':vertical))))
      ;; What a program prints for the user becomes transcript lines, and a
      ;; page's rows go to the clients' panes.
      (setf (presentation-terminal-text-function terminal)
            (lambda (line)
              (terminal-ui-append-finalized
               ui (list ':printed (incf (channel-presentation-text-counter ui))) line))
            (presentation-terminal-page-function terminal)
            (lambda (page)
              (with-terminal-ui-locked (ui)
                (setf (channel-presentation-page ui) page)
                (channel-presentation--publish-state ui ':page)))
            (fullscreen-terminal-ui-active-p ui) t)
      ui)))

(defclass presentation-output-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((terminal
    :initarg       :terminal
    :reader        presentation-output-stream-terminal
    :type          presentation-terminal
    :documentation "The terminal whose channel shows what is written.")
   (line
    :initform      (make-string-output-stream)
    :reader        presentation-output-stream-line
    :documentation "The text of the line being written."))
  (:documentation "A character stream whose lines a channel shows in its transcript.

A program that prints for the user, such as a provider's login, writes here
on a hosted channel, where there is no terminal to print on."))

(-> presentation-output-stream--flush (presentation-output-stream) null)
(defun presentation-output-stream--flush (stream)
  "Show the text written to STREAM so far as one line, without its terminal styling."
  (let ((text     (clinedi:ansi-strip
                   (get-output-stream-string (presentation-output-stream-line stream))))
        (function (presentation-terminal-text-function
                   (presentation-output-stream-terminal stream))))
    (when (and function (plusp (length text)))
      (funcall function text)))
  nil)

(defmethod trivial-gray-streams:stream-write-char
    ((stream presentation-output-stream) character)
  "Add CHARACTER to STREAM's line, showing the line when it ends."
  (if (char= character #\Newline)
      (presentation-output-stream--flush stream)
      (write-char character (presentation-output-stream-line stream)))
  character)

(defmethod trivial-gray-streams:stream-line-column ((stream presentation-output-stream))
  "Return NIL: the column of a line a client lays out is not known here."
  nil)

(defmethod trivial-gray-streams:stream-force-output ((stream presentation-output-stream))
  "Show an unfinished line, such as a prompt, at once."
  (presentation-output-stream--flush stream))

(defmethod trivial-gray-streams:stream-finish-output ((stream presentation-output-stream))
  "Show an unfinished line, such as a prompt, at once."
  (presentation-output-stream--flush stream))

(defmethod terminal-interactive-p ((terminal presentation-terminal))
  "Return true while a client presents the channel, so somebody can answer it."
  (let ((function (presentation-terminal-interactive-function terminal)))
    (and function (not (null (funcall function))))))

(defmethod terminal-start ((terminal presentation-terminal))
  "Count TERMINAL as started; there is nothing to prepare."
  (setf (terminal-started-p terminal) t)
  terminal)

(defmethod terminal-stop ((terminal presentation-terminal))
  "Count TERMINAL as stopped; there is nothing to restore."
  (setf (terminal-started-p terminal) nil)
  terminal)

(defmethod terminal--write ((terminal presentation-terminal) text)
  "Drop TEXT: a channel's server writes to no terminal."
  (declare (ignore text))
  nil)

(defmethod terminal-flush ((terminal presentation-terminal))
  "Do nothing: TERMINAL holds no output."
  nil)

(defmethod terminal-plain-output-p ((terminal presentation-terminal))
  "Return NIL: TERMINAL shows no output of its own."
  nil)

(defmethod terminal-input-ready-p ((terminal presentation-terminal))
  "Return whether a client's event waits for the open modal reader."
  (with-lock-held ((presentation-terminal-event-lock terminal))
    (not (deque-empty-p (presentation-terminal-events terminal)))))

(defmethod terminal-read-event ((terminal presentation-terminal))
  "Return the next event a client sent the open modal reader, waiting for one.

Without a client that presents the channel nobody can answer, and the read
ends as the end of input."
  (with-lock-held ((presentation-terminal-event-lock terminal))
    (loop
      (cond
        ((not (deque-empty-p (presentation-terminal-events terminal)))
         (return (deque-pop-front (presentation-terminal-events terminal))))
        ((not (terminal-interactive-p terminal))
         (return ':stream-end))
        (t
         (condition-wait (presentation-terminal-event-condition terminal)
                         (presentation-terminal-event-lock terminal)
                         :timeout *presentation-terminal-poll-seconds*))))))

(defmethod terminal-screen-enter ((terminal presentation-terminal))
  "Take no screen and return no token."
  nil)

(defmethod terminal-screen-leave ((terminal presentation-terminal) token)
  "Give back no screen."
  (declare (ignore token))
  nil)

(defmethod terminal-screen-paint
    ((terminal presentation-terminal) painter rows
     &key (cursor-row 0) (cursor-column 0) (cursor-visible-p t) whole-screen-p)
  "Give ROWS to the channel's clients as a page that takes their whole pane.

Only a page paints rows here, such as the memory manager or the onboarding
tour: the transcript, the live rows and the composer are the clients' own to
compose."
  (declare (ignore painter))
  (let ((function (presentation-terminal-page-function terminal)))
    (when function
      (funcall function (list :open-p           t
                              :rows             rows
                              :cursor-row       cursor-row
                              :cursor-column    cursor-column
                              :cursor-visible-p (not (null cursor-visible-p))
                              :whole-screen-p   (not (null whole-screen-p))))))
  nil)

(defmethod terminal-screen-invalidate ((terminal presentation-terminal) painter)
  "Forget nothing: TERMINAL shows no frame."
  (declare (ignore painter))
  nil)


;;;; -- Publishing --

(-> channel-presentation--columns (channel-presentation) (integer 1))
(defun channel-presentation--columns (ui)
  "Return the width UI's records are laid out at."
  (max 1 (terminal-columns (terminal-ui-terminal ui))))

(-> channel-presentation--send (channel-presentation list) null)
(defun channel-presentation--send (ui event)
  "Give EVENT to UI's publisher, when it has one."
  (let ((function (channel-presentation-publish-function ui)))
    (when function
      (funcall function event)))
  nil)

(-> channel-presentation--forget-cell (channel-presentation cons) null)
(defun channel-presentation--forget-cell (ui cell)
  "Let the records of evicted log CELL be presented again and note how far history was dropped."
  (let ((fields (rest (first cell))))
    (case (getf fields :operation)
      (:records
       (dolist (presentation (getf fields :presentations))
         (terminal-ui-fullscreen--forget-sequence ui (first presentation))))
      ((:append :mark)
       (terminal-ui-fullscreen--forget-sequence ui (getf fields :identifier)))
      (:markdown
       (maphash (lambda (identifier stream)
                  (when (eq (third stream) cell)
                    (setf (third stream) nil))
                  identifier)
                (channel-presentation-streams ui)))))
  nil)

(-> channel-presentation--record-weight (list list) (integer 0))
(defun channel-presentation--record-weight (event presentations)
  "Return the characters a :RECORDS EVENT charges: its text and the sources its PRESENTATIONS retain."
  (+ (presentation--weight event)
     (loop for presentation in presentations
           sum (loop for key in '(:source-characters :body-source-characters)
                     for value = (getf (third presentation) key)
                     sum (cond
                           ((functionp value)
                            (funcall value))
                           ((integerp value)
                            value)
                           (t
                            0))))))

(-> channel-presentation--cell-group (cons) t)
(defun channel-presentation--cell-group (cell)
  "Return the identity of CELL's logical publication for replay retention."
  (or (fourth cell) cell))

(-> channel-presentation--trim-log (channel-presentation) null)
(defun channel-presentation--trim-log (ui)
  "Evict oldest logical publications until the budget fits, retaining the newest."
  (when (> (channel-presentation-log-characters ui) *channel-presentation-log-character-limit*)
    (let* ((log          (channel-presentation-transcript-log ui))
           (cells        (deque->list log))
           (newest-group (and cells (channel-presentation--cell-group (first (last cells))))))
      (loop while (and cells
                       (> (channel-presentation-log-characters ui) *channel-presentation-log-character-limit*)
                       (not (eq newest-group (channel-presentation--cell-group (first cells)))))
            do (let ((group (channel-presentation--cell-group (first cells))))
                 (loop while (and cells (eq group (channel-presentation--cell-group (first cells))))
                       do (let ((evicted (deque-pop-front log)))
                            (pop cells)
                            (decf (channel-presentation-log-characters ui) (second evicted))
                            (channel-presentation--forget-cell ui evicted)))))))
  nil)

(-> channel-presentation--log
    (channel-presentation list &key (:presentations list) (:retention-group t)) cons)
(defun channel-presentation--log (ui event &key presentations retention-group)
  "Keep transcript EVENT for a client that attaches later and return its log cell.

  A cell holds (EVENT CHARACTERS PRESENTATIONS RETENTION-GROUP SUPERSEDED-P BOUNDARY).
  BOUNDARY identifies the durable record preceding this publication. Source
  presentations permit layout at another width. Cells of one logical publication
  share RETENTION-GROUP and leave the log together."
  (let ((cell (list event
                    (if presentations
                        (channel-presentation--record-weight event presentations)
                        (presentation--weight event))
                    presentations retention-group nil
                    (when (channel-presentation-record-boundary-function ui)
                      (funcall (channel-presentation-record-boundary-function ui)))))
        (log  (channel-presentation-transcript-log ui)))
    (deque-push-back log cell)
    (incf (channel-presentation-log-characters ui) (second cell))
    (channel-presentation--trim-log ui)
    cell))

(-> channel-presentation--publish
    (channel-presentation keyword list &key (:log-p boolean) (:presentations list)
                                           (:retention-group t)) null)
(defun channel-presentation--publish (ui operation fields &key log-p presentations retention-group)
  "Tell UI's clients of OPERATION with FIELDS, keeping a transcript operation when LOG-P.

PRESENTATIONS are the records a logged :RECORDS operation was made from."
  ;; Whatever changed, the clients first hear whether the channel still works.
  (unless (eq operation ':turn)
    (channel-presentation--publish-state ui ':turn))
  (let ((event (list* :ui :operation operation fields)))
    (when log-p
      (channel-presentation--log ui event :presentations  presentations
                                         :retention-group retention-group))
    (channel-presentation--send ui event))
  nil)

(defgeneric channel-presentation-state (ui operation)
  (:documentation "Return the fields that tell a client UI's state OPERATION as it is now.

The second value is what the fields were made from, without their ages, so a
state that did not change is not sent again."))

(defparameter *channel-presentation-state-operations*
  '(:turn :title :idle-details :context :status :local-activity :compacting :judgments
    :agent-activities :command-activities :preview :pending-inputs :prompt-padding :program-status
    :lisp-input :tail :page :selector :draft :modal :history :completions)
  "The state operations a client that attaches is told, in this order.")

(-> presentation--completion-entry (list) (option list))
(defun presentation--completion-entry (entry)
  "Return ENTRY's bounded display metadata without altering its operation name."
  (let ((name    (getf entry :name))
        (primary (getf entry :primary)))
    (when (and (stringp name) (<= (length name) 4096))
      (append (list :name        name
                    :argument    (let ((value (getf entry :argument)))
                                   (and (stringp value) (subseq value 0 (min 512 (length value)))))
                    :description (let ((value (getf entry :description)))
                                   (if (stringp value)
                                       (subseq value 0 (min 512 (length value)))
                                       "")))
              (when (and (stringp primary) (<= (length primary) 4096))
                (list :primary primary))))))

(-> channel-presentation--completion-entries (t) list)
(defun channel-presentation--completion-entries (snapshot)
  "Validate and normalize the captured catalog for the client protocol."
  (remove nil (mapcar #'presentation--completion-entry
                     (terminal-ui--validate-completions snapshot))))

(defmethod channel-presentation-state ((ui channel-presentation) (operation (eql :completions)))
  "Describe the current catalog, including when a new client attaches."
  (let ((entries (channel-presentation--completion-entries (terminal-ui--completion-snapshot ui))))
    (values (list :entries entries) entries)))

(-> channel-presentation--completion-events (list) list)
(defun channel-presentation--completion-events (entries)
  "Split a logical catalog into bounded events, including an empty reset."
  (loop with remaining = (length entries)
        for tail = entries then (nthcdr 32 tail)
        for first-p = t then nil
        collect (list :ui      :operation ':completions
                      :entries (subseq tail 0 (min 32 remaining))
                      :reset-p first-p :complete-p (<= remaining 32))
        while (> remaining 32)
        do (decf remaining 32)))

(-> channel-presentation--state-events (channel-presentation keyword) (values list t))
(defun channel-presentation--state-events (ui operation)
  "Return bounded state events and the signature of their complete logical state."
  (multiple-value-bind (fields signature) (channel-presentation-state ui operation)
    (values (if (eq operation ':completions)
                (channel-presentation--completion-events (getf fields :entries))
                (list (list* :ui :operation operation fields)))
            signature)))

(defmethod terminal-ui--refresh-completions ((ui channel-presentation))
  "Poll the provider and publish a changed catalog, retrying failed publication."
  (multiple-value-bind (snapshot revision) (terminal-ui--completion-snapshot ui)
    (unless (and revision (eq revision (channel-presentation-published-completion-revision ui)))
      (let ((entries   (channel-presentation--completion-entries snapshot))
            (published (channel-presentation-published-state ui)))
        (multiple-value-bind (previous present-p) (gethash ':completions published)
          (unless (and present-p (equal previous entries))
            ;; A failed batch may have sent some chunks. Retry from the reset.
            (remhash ':completions published)
            (setf (channel-presentation-published-completion-revision ui) nil)
            (dolist (event (channel-presentation--completion-events entries))
              (channel-presentation--publish ui ':completions (nthcdr 3 event)))
            (setf (gethash ':completions published) entries)))
        (setf (channel-presentation-published-completion-revision ui) revision))))
  nil)

(-> channel-presentation--publish-state (channel-presentation keyword) null)
(defun channel-presentation--publish-state (ui operation)
  "Tell UI's clients state OPERATION when it differs from what they were told last."
  (multiple-value-bind (events signature) (channel-presentation--state-events ui operation)
    (let ((published (channel-presentation-published-state ui)))
      (multiple-value-bind (previous present-p) (gethash operation published)
        (unless (and present-p (equal previous signature))
          (setf (gethash operation published) signature)
          (dolist (event events)
            (channel-presentation--publish ui operation (nthcdr 3 event)))))))
  nil)

(-> channel-presentation--call-publishing (channel-presentation keyword function) t)
(defun channel-presentation--call-publishing (ui operation function)
  "Call FUNCTION, which changes UI, then tell the clients state OPERATION, and return its values.

Both happen under UI's lock, so a client that attaches meanwhile sees the
change exactly once."
  (with-terminal-ui-locked (ui)
    (multiple-value-prog1 (funcall function)
      (channel-presentation--publish-state ui operation))))

(-> channel-presentation--age (channel-presentation (option real)) real)
(defun channel-presentation--age (ui time)
  "Return the seconds since TIME on UI's clock, or zero without one."
  (if time
      (max 0 (- (funcall (terminal-ui-clock-function ui)) time))
      0))

(defmethod channel-presentation-state ((ui channel-presentation) (operation (eql :turn)))
  "Describe whether the channel works, cancels its turn, or edits a recalled follow-up."
  (let* ((function (channel-presentation-turn-state-function ui))
         (state    (and function (funcall function))))
    (values state state)))

(-> channel-presentation-note-turn (channel-presentation) null)
(defun channel-presentation-note-turn (ui)
  "Tell UI's clients whether the channel works now, when that changed."
  (with-terminal-ui-locked (ui)
    (channel-presentation--publish-state ui ':turn))
  nil)

(defmethod channel-presentation-state ((ui channel-presentation) (operation (eql :title)))
  "Describe the title of the clients' terminal windows."
  (let ((title (channel-presentation-window-title ui)))
    (values (list :title title) title)))

(defmethod terminal-ui-set-window-title ((ui channel-presentation) title)
  "Tell the clients the title their terminal windows take."
  (with-terminal-ui-locked (ui)
    (setf (channel-presentation-window-title ui) title)
    (channel-presentation--publish-state ui ':title))
  t)

(-> channel-presentation-replay (channel-presentation &key (:transcript list)) list)
(defun channel-presentation-replay (ui &key (transcript nil transcript-p))
  "Return the events that bring a client that just attached to what UI shows.

The transcript is cleared and replayed from the log, then every state follows,
and :REPLAYED ends them, so the client knows where its own notes go. Call
this under UI's lock, before the client receives live events. A supplied
TRANSCRIPT replaces the transient log's events without changing state replay."
  (append
   (list (list :ui :operation ':reset))
   (if transcript-p
       transcript
       (loop for cell in (deque->list (channel-presentation-transcript-log ui))
             collect (first cell)))
   (loop for operation in *channel-presentation-state-operations*
         append (nth-value 0 (channel-presentation--state-events ui operation)))
   (list (list :ui :operation ':replayed))))

(-> channel-presentation-set-geometry
    (channel-presentation &key (:rows (option integer)) (:columns (option integer)) (:styled-p t))
    null)
(defun channel-presentation-set-geometry (ui &key rows columns (styled-p t))
  "Lay UI's later records out for a client of ROWS by COLUMNS.

A page or reader that owns the pane is told with a (:RESIZE COLUMNS ROWS)
event, as a terminal tells it, so it lays itself out again."
  (with-terminal-ui-locked (ui)
    (let* ((terminal         (terminal-ui-terminal ui))
           (previous-rows    (terminal-rows terminal))
           (previous-columns (terminal-columns terminal)))
      (when (and (integerp columns) (plusp columns))
        (terminal-set-dimensions terminal columns
                                 :rows (if (and (integerp rows) (plusp rows))
                                           rows
                                           previous-rows))
        (unless (= columns previous-columns)
          (channel-presentation--lay-out-log ui))
        (unless (and (= columns previous-columns) (= (terminal-rows terminal) previous-rows))
          (presentation-terminal-offer-event
           terminal (list :resize columns (terminal-rows terminal)))))
      (setf (terminal-styled-p terminal) (not (null styled-p)))))
  nil)


;;;; -- Transcript Operations --

(defmethod terminal-ui-pages-p ((ui channel-presentation))
  "A channel's page takes the panes of the clients that present it."
  t)

(defmethod channel-presentation-state ((ui channel-presentation) (operation (eql :page)))
  "Describe the page that takes the clients' panes, or that none does."
  (let ((fields (or (channel-presentation-page ui) (list :open-p nil))))
    (values fields fields)))

(defmethod terminal-ui--note-finalized-output ((ui channel-presentation) identifiers)
  "Tag nothing: the clients keep the transcript."
  (declare (ignore identifiers))
  nil)

(defmethod terminal-ui--present-live
    ((ui channel-presentation) &key status-now appended-text appended-display appended-regions)
  "Publish the hosted live state and acknowledge its status signature."
  (declare (ignore appended-text appended-display appended-regions))
  (let ((now (or status-now
                 (and (or (terminal-ui--status-row-visible-p ui)
                          (terminal-ui-notice ui))
                      (funcall (terminal-ui-clock-function ui))))))
    (terminal-ui--expire-notice-at ui now)
    ;; A page lasts while its owner keeps the live rows suspended.
    (unless (terminal-ui-live-output-suspended-p ui)
      (setf (channel-presentation-page ui) nil))
    (channel-presentation--publish-state ui ':page)
    (channel-presentation--publish-state ui ':selector)
    (channel-presentation--publish-state ui ':draft)
    (terminal-ui--refresh-completions ui)
    (setf (terminal-ui-status-rendered-signature ui)
          (and now (terminal-ui--animation-signature-at ui now))))
  nil)


;;;; -- Modal Readers --

;;; A picker, or any other reader that takes the keyboard for a while, runs
;;; where the work that opened it runs. The client shows what it shows and
;;; sends it the events it would have read.

(-> presentation--item->wire (list) list)
(defun presentation--item->wire (item)
  "Return picker ITEM with its styled description as a wire entry."
  (loop for (key value) on item by (partial #'nthcdr 2)
        append (list key (if (eq key ':description-spans)
                             (presentation-entry->wire value)
                             value))))

(-> presentation--wire->item (t) (option list))
(defun presentation--wire->item (wire)
  "Return the picker item WIRE describes, or NIL when it names nothing."
  (when (and (listp wire) (stringp (getf wire :name)))
    (loop for (key value) on wire by (partial #'nthcdr 2)
          append (list key (if (eq key ':description-spans)
                               (presentation-wire->entry value)
                               value)))))

(defmethod channel-presentation-state ((ui channel-presentation) (operation (eql :selector)))
  "Describe the open picker: its title, hint, and the candidates in view."
  (let ((selector (terminal-ui-selector ui)))
    (if (null selector)
        (values (list :open-p nil) nil)
        (multiple-value-bind (items start selection) (selector-window selector)
          (declare (ignore start))
          (let* ((hint   (terminal-ui-selector-hint ui))
                 (fields (list :open-p    t
                               :title     (terminal-ui-selector-title ui)
                               :hint      (if (stringp hint) hint (presentation-entry->wire hint))
                               :items     (mapcar #'presentation--item->wire items)
                               :selection selection)))
            (values fields fields))))))

(defmethod channel-presentation-state ((ui channel-presentation) (operation (eql :draft)))
  "Describe the line a modal reader edits, such as a restart's argument; outside one there is none."
  (let* ((editor (terminal-ui-editor ui))
         (fields (and (plusp (presentation-terminal-modal-depth (terminal-ui-terminal ui)))
                      (null (terminal-ui-selector ui))
                      (list :text   (line-editor-text editor)
                            :cursor (line-editor-cursor editor)))))
    (values fields fields)))

(defmethod channel-presentation-state ((ui channel-presentation) (operation (eql :modal)))
  "Describe whether a modal reader waits for the client's events."
  (let ((open-p (plusp (presentation-terminal-modal-depth (terminal-ui-terminal ui)))))
    (values (list :open-p open-p) open-p)))

(defmethod terminal-ui-call-with-exclusive-input :around
    ((ui channel-presentation) function &key &allow-other-keys)
  "Have the client send its events to FUNCTION's modal reader while it runs."
  (declare (ignore function))
  (let ((terminal (terminal-ui-terminal ui)))
    (with-terminal-ui-locked (ui)
      (with-lock-held ((presentation-terminal-event-lock terminal))
        (incf (presentation-terminal-modal-depth terminal)))
      (channel-presentation--publish-state ui ':modal))
    (unwind-protect
         (call-next-method)
      (with-terminal-ui-locked (ui)
        (with-lock-held ((presentation-terminal-event-lock terminal))
          (when (zerop (decf (presentation-terminal-modal-depth terminal)))
            (deque-clear (presentation-terminal-events terminal))))
        (channel-presentation--publish-state ui ':selector)
        (channel-presentation--publish-state ui ':draft)
        (channel-presentation--publish-state ui ':modal)))))

(defmethod terminal-ui--append-output :around
    ((ui channel-presentation) text display &key regions compact-first-group compact-last-group)
  "Send already rendered TEXT as a plain entry, since UI keeps no rows of its own."
  (declare (ignore display regions compact-last-group))
  (when (plusp (length text))
    (channel-presentation--publish
     ui ':append (list :entry (string-right-trim '(#\Newline) text) :group compact-first-group)
     :log-p t))
  nil)

(defmethod terminal-ui-append-finalized
    ((ui channel-presentation) identifier entry &key group)
  "Send ENTRY to the clients once for IDENTIFIER."
  (with-terminal-ui-locked (ui)
    (unless (terminal-ui--finalized-p ui identifier)
      (terminal-ui--remember-finalized ui (list identifier))
      (channel-presentation--publish
       ui ':append (list :identifier identifier
                         :entry      (presentation-entry->wire entry)
                         :group      group)
       :log-p t)
      t)))

(defmethod terminal-ui-mark-finalized :around ((ui channel-presentation) identifier)
  "Tell the clients that IDENTIFIER names their newest output, the first time it does."
  (with-terminal-ui-locked (ui)
    (when (call-next-method)
      (channel-presentation--publish ui ':mark (list :identifier identifier) :log-p t)
      t)))

(defmethod terminal-ui-present-records ((ui channel-presentation) presentations)
  "Send the PRESENTATIONS not yet shown to the clients, which merge calls and outcomes themselves."
  (with-terminal-ui-locked (ui)
    (let ((seen    (make-hash-table :test #'equal))
          (pending nil))
      (dolist (presentation presentations)
        (let ((identifier (first presentation)))
          (unless (or (terminal-ui--finalized-p ui identifier) (gethash identifier seen))
            (setf (gethash identifier seen) t)
            (push presentation pending))))
      (setf pending (nreverse pending))
      (dolist (cell (channel-presentation--record-cells ui pending))
        (let ((batch (third cell)))
          (channel-presentation--publish
           ui ':records (list :presentations (getf (rest (first cell)) :presentations))
           :log-p t :presentations batch :retention-group (fourth cell))
          (terminal-ui--remember-finalized ui (mapcar #'first batch))))
      (length pending))))

(-> channel-presentation--records-fields
    (channel-presentation list &key (:lay-out-p boolean)) list)
(defun channel-presentation--records-fields (ui presentations &key lay-out-p)
  "Return the fields of a :RECORDS operation for PRESENTATIONS at UI's width.

With LAY-OUT-P each record is laid out again for that width."
  (let ((columns (channel-presentation--columns ui)))
    (list :presentations
          (mapcar (lambda (presentation)
                    (presentation-record->wire presentation columns :lay-out-p lay-out-p))
                  presentations))))

(-> presentation--validate-transport-shape (t) null)
(defun presentation--validate-transport-shape (value)
  "Check cycles and depth without copying an oversized rendered VALUE."
  (let ((active (make-hash-table :test #'eq)))
    (labels ((enter (item)
               "Enter an active container without retaining completed subtrees."
               (when (gethash item active)
                 (error 'portable-value-error :reason ':cycle))
               (setf (gethash item active) t))

             (walk (item depth)
               "Check ITEM at the transport mapper's structural DEPTH."
               (when (> depth 128)
                 (error 'portable-value-error :reason ':depth))
               (cond
                 ((consp item)
                  (let ((tails nil))
                    (unwind-protect
                         (loop for tail = item then (rest tail)
                               while (consp tail)
                               do (enter tail)
                                  (push tail tails)
                                  (walk (first tail) (1+ depth))
                               finally (walk tail depth))
                      (dolist (tail tails) (remhash tail active)))))
                 ((or (hash-table-p item) (and (vectorp item) (not (stringp item))))
                  (enter item)
                  (unwind-protect
                       (if (hash-table-p item)
                           (maphash (lambda (key value)
                                      (walk key (1+ depth))
                                      (walk value (1+ depth))) item)
                           (map nil (lambda (value) (walk value (1+ depth))) item))
                    (remhash item active))))))
      (walk value 0)))
  nil)

(-> presentation--small-field-p (t) boolean)
(defun presentation--small-field-p (value)
  "Return whether VALUE fits bounded lifecycle metadata in an overflow notice."
  (block nil
    (handler-case
        (progn
          (portable-value-map
           value (lambda (item child)
                   (declare (ignore child))
                   (unless (typep item '(or null integer keyword (eql t) character string))
                     (return nil))
                   (when (or (and (stringp item) (> (length item) 512))
                             (and (symbolp item) (> (length (symbol-name item)) 512))
                             (and (integerp item) (> (integer-length item) 64)))
                     (return nil))
                   item)
           :maximum-nodes 256)
          t)
      (portable-value-error (condition)
        (if (eq (portable-value-error-reason condition) ':nodes)
            nil
            (error condition))))))

(-> presentation--overflow-record (list) list)
(defun presentation--overflow-record (record)
  "Replace oversized rendered content with a width-independent lifecycle notice."
  (let* ((notice   "[Rendered record exceeds the transport budget. Full content is retained in conversation history.]")
         (metadata (list :body notice :annotations (list (list ':dim notice)))))
    (dolist (key '(:entry-kind :kind :state :call-key :job-key :group :retain-output-p))
      (let ((value (getf (third record) key)))
        (when (presentation--small-field-p value)
          (setf (getf metadata key) value))))
    (list (first record) notice metadata)))

(-> channel-presentation--record-cells
    (channel-presentation list &key (:lay-out-p boolean) (:retention-group t)) list)
(defun channel-presentation--record-cells (ui presentations &key lay-out-p retention-group)
  "Partition PRESENTATIONS into replay cells accepted by the client transport.

Check the publisher's cursor fields and the actual packet character limit.
Split oversized batches; replace an oversized singleton's rendered content
with a notice. Keep its source presentation for later layout and history."
  (let ((records (getf (channel-presentation--records-fields
                        ui presentations :lay-out-p lay-out-p)
                      :presentations))
        (group   (or retention-group (list nil))))
    (labels ((partition (records presentations)
               "Return bounded cells for matching wire RECORDS and PRESENTATIONS."
               (let* ((event (list :ui :operation ':records :presentations records))
                      (split-p
                        (handler-case
                            (progn
                              (image-daemon:daemon-packet-string
                               (client-portable
                                (append event (list :sequence        most-positive-fixnum
                                                    :record-sequence most-positive-fixnum))))
                              nil)
                          (portable-value-error (condition)
                            (if (eq (portable-value-error-reason condition) ':nodes)
                                t
                                (error condition)))
                          (image-daemon:daemon-error (condition)
                            (if (eq (image-daemon:daemon-error-operation condition) ':write-packet)
                                t
                                (error condition))))))
                (cond
                  ((and split-p (null (rest records)))
                   (presentation--validate-transport-shape event)
                   (let ((notice (presentation--overflow-record (first records))))
                     (image-daemon:daemon-packet-string
                      (client-portable (list :ui :operation ':records :presentations (list notice)
                                             :sequence        most-positive-fixnum
                                             :record-sequence most-positive-fixnum)))
                     (let ((event (list :ui :operation ':records :presentations (list notice))))
                       (list (list event (channel-presentation--record-weight event presentations)
                                   presentations group)))))
                  (split-p
                   (let ((middle (floor (length records) 2)))
                     (nconc (partition (subseq records 0 middle)
                                       (subseq presentations 0 middle))
                            (partition (nthcdr middle records)
                                       (nthcdr middle presentations)))))
                  (t
                   (list (list event
                               (channel-presentation--record-weight event presentations)
                               presentations group)))))))
      (when records
        (partition records presentations)))))

(-> channel-presentation--lay-out-log (channel-presentation) null)
(defun channel-presentation--lay-out-log (ui)
  "Rebuild replay cells at UI's width, splitting any that outgrow transport bounds."
  (let* ((log (channel-presentation-transcript-log ui))
         (cells
           (loop for cell in (deque->list log)
                 append (if (third cell)
                            (mapcar (lambda (replacement)
                                      (append replacement (list (fifth cell) (sixth cell))))
                                    (channel-presentation--record-cells
                                     ui (third cell) :lay-out-p t
                                     :retention-group           (channel-presentation--cell-group cell)))
                            (list cell)))))
    (deque-clear log)
    (setf (channel-presentation-log-characters ui) 0)
    (dolist (cell cells)
      (deque-push-back log cell)
      (incf (channel-presentation-log-characters ui) (second cell))))
  (channel-presentation--trim-log ui))

(defmethod terminal-ui-stream-update ((ui channel-presentation) &key rows tail)
  "Send streamed ROWS and the unfinished TAIL to the clients."
  (with-terminal-ui-locked (ui)
    (when rows
      (channel-presentation--publish
       ui ':stream (list :rows (presentation-rows->wire rows)) :log-p t))
    (setf (terminal-ui-stream-tail ui) tail)
    (channel-presentation--publish-state ui ':tail))
  ui)

(-> channel-presentation--tail->wire (t) list)
(defun channel-presentation--tail->wire (tail)
  "Return an unfinished TAIL, which is text, one row or rows, as (KIND VALUE)."
  (cond
    ((null tail)
     nil)
    ((stringp tail)
     (list ':text tail))
    ((terminal-styled-text-p tail)
     (list ':row (presentation-entry->wire tail)))
    (t
     (list ':rows (presentation-rows->wire tail)))))

(defmethod channel-presentation-state ((ui channel-presentation) (operation (eql :tail)))
  "Describe the unfinished streamed text continuing the transcript."
  (let ((tail (and (null (fullscreen-terminal-ui-markdown-stream ui))
                   (channel-presentation--tail->wire (terminal-ui-stream-tail ui)))))
    (values (list :tail tail) tail)))

(-> channel-presentation--stream-wire-identifier (channel-presentation t) t)
(defun channel-presentation--stream-wire-identifier (ui identifier)
  "Return the wire identity of UI's active stream, before it is finalized."
  (with-terminal-ui-locked (ui)
    (first (gethash identifier (channel-presentation-streams ui)))))

(-> channel-presentation--supersede-transient (channel-presentation keyword t) null)
(defun channel-presentation--supersede-transient (ui operation identifier)
  "Mark a retained publication as superseded by durable records for future attachments.

Clients that already saw the stream keep it, with the durable record marks."
  (with-terminal-ui-locked (ui)
    (dolist (cell (deque->list (channel-presentation-transcript-log ui)))
      (let ((fields (rest (first cell))))
        (when (and (eq (getf fields :operation) operation)
                   (equal (getf fields :identifier) identifier))
          (setf (fifth cell) t)))))
  nil)

(defmethod terminal-ui-stream-markdown
    ((ui channel-presentation) identifier &key source header marker finished-p)
  "Send one streamed Markdown block's new source to the clients, which render it.

Only what was added since the last update travels; a source that changed
otherwise is sent whole."
  (with-terminal-ui-locked (ui)
    (let* ((streams  (channel-presentation-streams ui))
           (stream   (or (gethash identifier streams)
                         (setf (gethash identifier streams)
                               (list (list ':stream (incf (channel-presentation-stream-counter ui)))
                                     nil nil))))
           (previous (second stream))
           (delta-p  (and previous (string-prefix-p previous source)))
           (shared   (list :identifier (first stream)
                           :header     (presentation-entry->wire header)
                           :marker     (presentation-entry->wire marker)
                           :finished-p finished-p))
           (whole    (list* :ui :operation ':markdown :source source shared))
           (cell     (third stream)))
      (if cell
          (let ((weight (presentation--weight whole)))
            (incf (channel-presentation-log-characters ui) (- weight (second cell)))
            (setf (first cell) whole (second cell) weight)
            (channel-presentation--trim-log ui))
          (setf (third stream) (channel-presentation--log ui whole)))
      (channel-presentation--send
       ui (if delta-p
              (list* :ui :operation ':markdown :delta (subseq source (length previous)) shared)
              whole))
      (if finished-p
          (remhash identifier streams)
          (setf (second stream) source))
      (setf (fullscreen-terminal-ui-markdown-stream ui) (unless finished-p identifier))))
  ui)

(defmethod terminal-ui-set-epilogue ((ui channel-presentation) entry)
  "Send the clients ENTRY to leave behind when the channel ends."
  (with-terminal-ui-locked (ui)
    (channel-presentation--publish ui ':epilogue (list :entry (presentation-entry->wire entry))))
  nil)

(defmethod terminal-ui-reset-transcript ((ui channel-presentation))
  "Forget the transcript and tell the clients to clear theirs."
  (with-terminal-ui-locked (ui)
    (deque-clear (channel-presentation-transcript-log ui))
    (setf (channel-presentation-log-characters ui) 0
          (terminal-ui-stream-tail ui) nil
          (fullscreen-terminal-ui-markdown-stream ui) nil)
    (clrhash (channel-presentation-streams ui))
    (terminal-ui--clear-finalized ui)
    (clrhash (fullscreen-terminal-ui-evicted-sequences ui))
    (channel-presentation--publish ui ':reset nil))
  nil)


;;;; -- State Operations --

(defmethod channel-presentation-state ((ui channel-presentation) (operation (eql :status)))
  "Describe the timed activity and its modeline details."
  (let ((signature (list (terminal-ui-status ui) (terminal-ui-status-label-p ui)
                         (terminal-ui-status-details ui) (terminal-ui-status-worked-seconds ui)
                         (terminal-ui-status-started-at ui))))
    (values (list :status         (terminal-ui-status ui)
                  :label-p        (terminal-ui-status-label-p ui)
                  :details        (presentation-entry->wire (terminal-ui-status-details ui))
                  :worked-seconds (terminal-ui-status-worked-seconds ui)
                  :age            (channel-presentation--age ui (terminal-ui-status-started-at ui))
                  :progress-age   (channel-presentation--age ui (terminal-ui-status-progress-at ui)))
            signature)))

(defmethod terminal-ui-set-status :around
    ((ui channel-presentation) status &key &allow-other-keys)
  "Tell the clients the activity the channel now shows."
  (declare (ignore status))
  (channel-presentation--call-publishing ui ':status #'call-next-method))

(defmethod terminal-ui-note-status-progress :around ((ui channel-presentation))
  "Tell the clients of progress, at most once in *CHANNEL-PRESENTATION-PROGRESS-SECONDS*."
  (with-terminal-ui-locked (ui)
    (multiple-value-prog1 (call-next-method)
      (let ((now  (funcall (terminal-ui-clock-function ui)))
            (last (channel-presentation-progress-published-at ui)))
        (when (and (terminal-ui-status ui)
                   (or (null last) (>= (- now last) *channel-presentation-progress-seconds*)))
          (setf (channel-presentation-progress-published-at ui) now)
          (channel-presentation--publish ui ':progress nil))))))

(defmethod channel-presentation-state ((ui channel-presentation) (operation (eql :local-activity)))
  "Describe the explicit local Lisp work."
  (let ((activity (terminal-ui-local-activity ui)))
    (values (list :activity activity
                  :age      (channel-presentation--age ui (terminal-ui-local-activity-started-at ui)))
            (list activity (terminal-ui-local-activity-started-at ui)))))

(defmethod terminal-ui-set-local-activity :around ((ui channel-presentation) activity)
  "Tell the clients the local Lisp work the channel now shows."
  (declare (ignore activity))
  (channel-presentation--call-publishing ui ':local-activity #'call-next-method))

(defmethod channel-presentation-state ((ui channel-presentation) (operation (eql :compacting)))
  "Describe whether the conversation is being compacted."
  (values (list :compacting-p (terminal-ui-compacting-p ui)
                :age          (channel-presentation--age ui (terminal-ui-compaction-started-at ui)))
          (list (terminal-ui-compacting-p ui) (terminal-ui-compaction-started-at ui))))

(defmethod terminal-ui-set-compacting :around ((ui channel-presentation) compacting-p)
  "Tell the clients whether the channel is compacting."
  (declare (ignore compacting-p))
  (channel-presentation--call-publishing ui ':compacting #'call-next-method))

(defmethod channel-presentation-state ((ui channel-presentation) (operation (eql :context)))
  "Describe the context meter."
  (let ((fields (list :used             (terminal-ui-context-used ui)
                      :window           (terminal-ui-context-window ui)
                      :compaction-limit (terminal-ui-context-compaction-limit ui)
                      :estimated-p      (terminal-ui-context-estimated-p ui))))
    (values fields fields)))

(defmethod terminal-ui-set-context-usage :around
    ((ui channel-presentation) &key &allow-other-keys)
  "Tell the clients the context usage the channel now shows."
  (channel-presentation--call-publishing ui ':context #'call-next-method))

(defmethod channel-presentation-state ((ui channel-presentation) (operation (eql :idle-details)))
  "Describe the static details of the idle modeline."
  (let ((details (terminal-ui-idle-status-details ui)))
    (values (list :details (presentation-entry->wire details)) details)))

(defmethod terminal-ui-set-idle-status-details :around ((ui channel-presentation) details)
  "Tell the clients the idle modeline details."
  (declare (ignore details))
  (channel-presentation--call-publishing ui ':idle-details #'call-next-method))

(defmethod channel-presentation-state ((ui channel-presentation) (operation (eql :judgments)))
  "Describe the permission evaluations in flight."
  (let ((judgments (terminal-ui-judgments ui)))
    (values (list :judgments
                  (loop for judgment in judgments
                        collect (list (getf judgment :label)
                                      (channel-presentation--age ui (getf judgment :started-at)))))
            (copy-list judgments))))

(defmethod terminal-ui--note-judgments ((ui channel-presentation))
  "Tell the clients which permission evaluations are in flight."
  (channel-presentation--publish-state ui ':judgments))

(defmethod channel-presentation-state ((ui channel-presentation) (operation (eql :agent-activities)))
  "Describe the child agents at work, as their producer described them."
  (let ((activities (channel-presentation-agent-activity-arguments ui)))
    (values (list :activities activities) activities)))

(defmethod terminal-ui-set-agent-activities :around ((ui channel-presentation) activities)
  "Tell the clients the child agents at work."
  (with-terminal-ui-locked (ui)
    (multiple-value-prog1 (call-next-method)
      (setf (channel-presentation-agent-activity-arguments ui) (copy-tree activities))
      (channel-presentation--publish-state ui ':agent-activities))))

(defmethod channel-presentation-state ((ui channel-presentation) (operation (eql :command-activities)))
  "Describe the commands at work, as their producer described them."
  (let ((activities (channel-presentation-command-activity-arguments ui)))
    (values (list :activities activities) activities)))

(defmethod terminal-ui-set-command-activities :around ((ui channel-presentation) activities)
  "Tell the clients the commands at work."
  (with-terminal-ui-locked (ui)
    (multiple-value-prog1 (call-next-method)
      (setf (channel-presentation-command-activity-arguments ui) (copy-tree activities))
      (channel-presentation--publish-state ui ':command-activities))))

(defmethod terminal-ui-clear-command-activities :around ((ui channel-presentation))
  "Tell the clients to drop every command row."
  (with-terminal-ui-locked (ui)
    (multiple-value-prog1 (call-next-method)
      (setf (channel-presentation-command-activity-arguments ui) nil)
      (remhash ':command-activities (channel-presentation-published-state ui))
      (channel-presentation--publish ui ':command-activities (list :activities nil :clear-p t)))))

(defmethod channel-presentation-state ((ui channel-presentation) (operation (eql :preview)))
  "Describe the transient rows above the composer."
  (let ((rows (terminal-ui-preview-rows ui)))
    (values (list :rows (presentation-rows->wire rows)) rows)))

(defmethod terminal-ui-set-preview-rows :around ((ui channel-presentation) rows)
  "Tell the clients the transient rows the channel now shows."
  (declare (ignore rows))
  (channel-presentation--call-publishing ui ':preview #'call-next-method))

(defmethod channel-presentation-state ((ui channel-presentation) (operation (eql :pending-inputs)))
  "Describe the steering and queued inputs that wait for the agent."
  (let ((fields (list :steering (terminal-ui-steering-input-previews ui)
                      :queued   (terminal-ui-queued-input-previews ui))))
    (values fields fields)))

(defmethod terminal-ui-set-pending-inputs :around
    ((ui channel-presentation) steering-inputs queued-inputs)
  "Tell the clients the inputs that wait for the agent."
  (declare (ignore steering-inputs queued-inputs))
  (channel-presentation--call-publishing ui ':pending-inputs #'call-next-method))

(defmethod channel-presentation-state ((ui channel-presentation) (operation (eql :prompt-padding)))
  "Describe the blank rows around the prompt."
  (let ((padding (terminal-ui-prompt-padding ui)))
    (values (list :padding padding) padding)))

(defmethod terminal-ui-set-prompt-padding :around ((ui channel-presentation) padding)
  "Tell the clients the prompt padding."
  (declare (ignore padding))
  (channel-presentation--call-publishing ui ':prompt-padding #'call-next-method))

(defmethod channel-presentation-state ((ui channel-presentation) (operation (eql :program-status)))
  "Describe the root program status a terminal is told."
  (let ((status (terminal-ui-program-status ui)))
    (values (list :state (first status) :kind (second status)) (copy-list status))))

(defmethod terminal-ui-report-program-status :around
    ((ui channel-presentation) state &key &allow-other-keys)
  "Tell the clients the root program status."
  (declare (ignore state))
  (channel-presentation--call-publishing ui ':program-status #'call-next-method))

(defmethod channel-presentation-state ((ui channel-presentation) (operation (eql :lisp-input)))
  "Describe whether the composer explicitly reads Common Lisp."
  (let ((lisp-input-p (not (null (terminal-ui-lisp-input-p ui)))))
    (values (list :lisp-input-p lisp-input-p) lisp-input-p)))

(defmethod terminal-ui-set-lisp-input :around ((ui channel-presentation) lisp-input-p)
  "Tell the clients whether the composer reads Common Lisp."
  (declare (ignore lisp-input-p))
  (channel-presentation--call-publishing ui ':lisp-input #'call-next-method))

(defmethod terminal-ui-set-notice :around
    ((ui channel-presentation) notice &key duration-seconds)
  "Tell the clients the transient notice, when the channel took it."
  (multiple-value-bind (result applied-p) (call-next-method)
    (when applied-p
      ;; Like the notice itself, its publication never waits for the UI's lock.
      (terminal-ui--call-with-lock-if-available
       ui (lambda ()
            (channel-presentation--publish
             ui ':notice (list :notice notice :duration-seconds duration-seconds)))))
    (values result applied-p)))

(defmethod terminal-ui-set-theme :around ((ui channel-presentation) name)
  "Tell the clients the color theme to install."
  (multiple-value-prog1 (call-next-method)
    (channel-presentation--publish ui ':theme (list :name name))))


;;;; -- Composer Operations --

(-> presentation--input->wire ((or null string user-message-input)) list)
(defun presentation--input->wire (input)
  "Return INPUT's text and image pathnames as fields."
  (if (typep input 'user-message-input)
      (list :text   (user-message-input-text input)
            :images (mapcar #'namestring (user-message-input-image-pathnames input)))
      (list :text (or input ""))))

(-> presentation--wire->input (t t) (or string user-message-input))
(defun presentation--wire->input (text images)
  "Return the composer input with TEXT and the IMAGES that are pathnames."
  (let ((text   (if (stringp text) text ""))
        (images (and (listp images) (remove-if-not #'stringp images))))
    (if images
        (user-message-input-create :text text :image-pathnames (mapcar #'pathname images))
        text)))

(defmethod terminal-ui-set-input :around ((ui channel-presentation) input)
  "Tell the clients to replace their composer's input with INPUT."
  (multiple-value-prog1 (call-next-method)
    (channel-presentation--publish ui ':input (presentation--input->wire input))))

(defmethod terminal-ui-insert-input :around ((ui channel-presentation) text)
  "Tell the clients to insert TEXT into their composer."
  (multiple-value-prog1 (call-next-method)
    (channel-presentation--publish ui ':insert (list :text text))))

(defmethod terminal-ui-edit-input :around ((ui channel-presentation) event)
  "Tell the clients to apply editor EVENT, when it is a key they can read."
  (multiple-value-prog1 (call-next-method)
    (when (keywordp event)
      (channel-presentation--publish ui ':edit (list :event event)))))

(defmethod terminal-ui-recall-follow-up :around
    ((ui channel-presentation) input &key &allow-other-keys)
  "Tell the clients to recall INPUT into their composer."
  (with-terminal-ui-locked (ui)
    (multiple-value-prog1 (call-next-method)
      (channel-presentation--publish-state ui ':pending-inputs)
      (channel-presentation--publish ui ':input (presentation--input->wire input)))))

(-> presentation--history-fields (list (integer 0) t) list)
(defun presentation--history-fields (entries offset paged-p)
  "Return the fields that give a client input history ENTRIES, the first at OFFSET, paged when PAGED-P."
  (list :entries entries :offset offset :paged-p (not (null paged-p))))

(defmethod terminal-ui-load-history :around
    ((ui channel-presentation) entries &key (offset 0) older-function newer-function)
  "Tell the clients the newest input history, which they page through the channel."
  (multiple-value-prog1 (call-next-method)
    (channel-presentation--publish
     ui ':history (presentation--history-fields entries offset (and older-function newer-function)))))

(defmethod channel-presentation-state ((ui channel-presentation) (operation (eql :history)))
  "Describe the newest input history as the channel keeps it now.

A client edits its own history after this, so one that attaches later is told
the durable inputs, not what the channel loaded when it opened."
  (let* ((function (channel-presentation-history-function ui))
         (fields   (multiple-value-call #'presentation--history-fields
                     (if function
                         (funcall function)
                         (values nil 0 nil)))))
    (values fields fields)))


;;;; -- Applying Operations in a Client --

(defgeneric presentation-apply (ui operation &key &allow-other-keys)
  (:documentation "Apply the channel's presentation OPERATION with its fields to the client's UI.

An operation this version does not know is ignored, and so is every field a
method does not name."))

(defmethod presentation-apply ((ui terminal-ui) operation &key &allow-other-keys)
  "Ignore an OPERATION this version does not know."
  (declare (ignore operation))
  nil)

(defmethod presentation-apply ((ui terminal-ui) (operation (eql :reset)) &key &allow-other-keys)
  "Clear the transcript."
  (terminal-ui-reset-transcript ui)
  nil)

(defmethod presentation-apply
    ((ui terminal-ui) (operation (eql :append)) &key identifier entry group &allow-other-keys)
  "Append one entry, once for its identifier."
  (let ((entry (presentation-wire->entry entry)))
    (when entry
      (terminal-ui-append-finalized
       ui (or identifier (list ':anonymous (make-identifier))) entry :group group)))
  nil)

(defmethod presentation-apply
    ((ui terminal-ui) (operation (eql :mark)) &key identifier &allow-other-keys)
  "Count an identifier as shown by the newest output."
  (when identifier
    (terminal-ui-mark-finalized ui identifier))
  nil)

(defmethod presentation-apply
    ((ui terminal-ui) (operation (eql :records)) &key presentations &allow-other-keys)
  "Present transcript records."
  (let ((records (remove nil (mapcar #'presentation-wire->record
                                     (and (listp presentations) presentations)))))
    (when records
      (terminal-ui-present-records ui records)))
  nil)

(defmethod presentation-apply
    ((ui terminal-ui) (operation (eql :stream)) &key rows &allow-other-keys)
  "Append streamed rows, keeping the unfinished tail."
  (with-terminal-ui-locked (ui)
    (terminal-ui-stream-update ui :rows (presentation-wire->rows rows)
                                  :tail (terminal-ui-stream-tail ui)))
  nil)

(defmethod presentation-apply
    ((ui terminal-ui) (operation (eql :tail)) &key tail &allow-other-keys)
  "Show the unfinished streamed text, unless a Markdown block is streaming and shows its own."
  (unless (and (null tail)
               (typep ui 'fullscreen-terminal-ui)
               (fullscreen-terminal-ui-markdown-stream ui))
    (terminal-ui-stream-update
     ui :tail (and (consp tail)
                   (case (first tail)
                     (:text
                      (and (stringp (second tail)) (second tail)))
                     (:row
                      (presentation-wire->entry (second tail)))
                     (:rows
                      (presentation-wire->rows (second tail)))))))
  nil)

(defmethod presentation-apply
    ((ui fullscreen-terminal-ui) (operation (eql :markdown))
     &key identifier source delta header marker finished-p &allow-other-keys)
  "Render one streamed Markdown block again from its whole source.

A DELTA extends the source this UI already holds for the block."
  (with-terminal-ui-locked (ui)
    (let* ((record   (gethash identifier (fullscreen-terminal-ui-presentation-records ui)))
           (previous (or (getf (getf record :markdown) :source) ""))
           (source   (cond
                       ((stringp source)
                        source)
                       ((stringp delta)
                        (concatenate 'string previous delta))
                       (t
                        previous))))
      (terminal-ui-stream-markdown ui identifier
                                   :source     source
                                   :header     (presentation-wire->entry header)
                                   :marker     (presentation-wire->entry marker)
                                   :finished-p (not (null finished-p)))
      (terminal-ui-note-status-progress ui)))
  nil)

(defmethod presentation-apply
    ((ui terminal-ui) (operation (eql :epilogue)) &key entry &allow-other-keys)
  "Keep the entry to leave behind when the channel ends."
  (terminal-ui-set-epilogue ui (presentation-wire->entry entry))
  nil)

(defmethod presentation-apply
    ((ui terminal-ui) (operation (eql :status))
     &key status details worked-seconds (label-p t) (age 0) (progress-age 0) &allow-other-keys)
  "Show the timed activity, begun AGE seconds ago."
  (with-terminal-ui-locked (ui)
    (terminal-ui-set-status ui (and (stringp status) status)
                            :details        (presentation-wire->entry details)
                            :worked-seconds (and (integerp worked-seconds) worked-seconds)
                            :label-p        (not (null label-p)))
    (when (terminal-ui-status ui)
      (let ((now (funcall (terminal-ui-clock-function ui))))
        (setf (terminal-ui-status-started-at ui)  (- now (if (realp age) age 0))
              (terminal-ui-status-progress-at ui) (- now (if (realp progress-age) progress-age 0))))
      (terminal-ui--paint-live ui)))
  nil)

(defmethod presentation-apply ((ui terminal-ui) (operation (eql :progress)) &key &allow-other-keys)
  "Note progress within the activity."
  (terminal-ui-note-status-progress ui)
  nil)

(defmethod presentation-apply
    ((ui terminal-ui) (operation (eql :local-activity)) &key activity (age 0) &allow-other-keys)
  "Show the local Lisp work, begun AGE seconds ago."
  (with-terminal-ui-locked (ui)
    (terminal-ui-set-local-activity ui (and (stringp activity) activity))
    (when (terminal-ui-local-activity ui)
      (setf (terminal-ui-local-activity-started-at ui)
            (- (funcall (terminal-ui-clock-function ui)) (if (realp age) age 0)))))
  nil)

(defmethod presentation-apply
    ((ui terminal-ui) (operation (eql :compacting)) &key compacting-p (age 0) &allow-other-keys)
  "Show or clear the compaction indicator, begun AGE seconds ago."
  (with-terminal-ui-locked (ui)
    (terminal-ui-set-compacting ui (not (null compacting-p)))
    (when (terminal-ui-compacting-p ui)
      (setf (terminal-ui-compaction-started-at ui)
            (- (funcall (terminal-ui-clock-function ui)) (if (realp age) age 0)))))
  nil)

(defmethod presentation-apply
    ((ui terminal-ui) (operation (eql :context))
     &key used window compaction-limit estimated-p &allow-other-keys)
  "Show the context meter, once the channel measured one."
  (when (and (typep used '(integer 0)) (typep window '(integer 1))
             (typep compaction-limit '(integer 1)) (<= compaction-limit window))
    (terminal-ui-set-context-usage ui :used             used
                                      :window           window
                                      :compaction-limit compaction-limit
                                      :estimated-p      (not (null estimated-p))))
  nil)

(defmethod presentation-apply
    ((ui terminal-ui) (operation (eql :idle-details)) &key details &allow-other-keys)
  "Show the idle modeline details."
  (terminal-ui-set-idle-status-details ui (presentation-wire->entry details))
  nil)

(defmethod presentation-apply
    ((ui terminal-ui) (operation (eql :judgments)) &key judgments &allow-other-keys)
  "Show the permission evaluations in flight."
  (with-terminal-ui-locked (ui)
    (let ((now (funcall (terminal-ui-clock-function ui))))
      (setf (terminal-ui-judgments ui)
            (loop for judgment in (and (listp judgments) judgments)
                  when (and (consp judgment) (stringp (first judgment)))
                    collect (list :label      (first judgment)
                                  :started-at (- now (if (realp (second judgment))
                                                         (second judgment)
                                                         0))))))
    (terminal-ui--paint-live ui))
  nil)

(defmethod presentation-apply
    ((ui terminal-ui) (operation (eql :agent-activities)) &key activities &allow-other-keys)
  "Show the child agents at work."
  (when (and (listp activities) (every #'terminal-agent-activity-p activities))
    (terminal-ui-set-agent-activities ui activities))
  nil)

(defmethod presentation-apply
    ((ui terminal-ui) (operation (eql :command-activities))
     &key activities clear-p &allow-other-keys)
  "Show the commands at work, or drop every command row."
  (cond
    (clear-p
     (terminal-ui-clear-command-activities ui))
    ((and (listp activities) (every #'terminal-command-activity-p activities))
     (terminal-ui-set-command-activities ui activities)))
  nil)

(defmethod presentation-apply
    ((ui terminal-ui) (operation (eql :preview)) &key rows &allow-other-keys)
  "Show the transient rows above the composer."
  (terminal-ui-set-preview-rows ui (presentation-wire->rows rows))
  nil)

(defmethod presentation-apply
    ((ui terminal-ui) (operation (eql :pending-inputs)) &key steering queued &allow-other-keys)
  "Show the inputs that wait for the agent."
  (terminal-ui-set-pending-inputs
   ui
   (and (listp steering) (remove-if-not #'stringp steering))
   (and (listp queued) (remove-if-not #'stringp queued)))
  nil)

(defmethod presentation-apply
    ((ui terminal-ui) (operation (eql :prompt-padding)) &key padding &allow-other-keys)
  "Set the blank rows around the prompt."
  (when (typep padding '(integer 0 2))
    (terminal-ui-set-prompt-padding ui padding))
  nil)

(defmethod presentation-apply
    ((ui terminal-ui) (operation (eql :program-status)) &key state kind &allow-other-keys)
  "Report the root program status to the terminal."
  (when (keywordp state)
    (terminal-ui-report-program-status ui state :kind (and (keywordp kind) kind)))
  nil)

(defmethod presentation-apply
    ((ui terminal-ui) (operation (eql :lisp-input)) &key lisp-input-p &allow-other-keys)
  "Set whether the composer reads Common Lisp."
  (terminal-ui-set-lisp-input ui (not (null lisp-input-p)))
  nil)

(defmethod presentation-apply
    ((ui terminal-ui) (operation (eql :notice)) &key notice duration-seconds &allow-other-keys)
  "Show or clear the transient notice."
  (if (and (stringp notice) (realp duration-seconds) (plusp duration-seconds))
      (terminal-ui-set-notice ui notice :duration-seconds duration-seconds)
      (terminal-ui-set-notice ui nil))
  nil)

(defmethod presentation-apply
    ((ui terminal-ui) (operation (eql :theme)) &key name &allow-other-keys)
  "Install a color theme this version knows."
  (when (and (keywordp name) (terminal-theme-find name))
    (terminal-ui-set-theme ui name))
  nil)

(defmethod presentation-apply
    ((ui terminal-ui) (operation (eql :selector))
     &key open-p title hint items (selection 0) &allow-other-keys)
  "Show the candidates of the channel's open picker, or close it."
  (with-terminal-ui-locked (ui)
    (let ((items (and open-p (remove nil (mapcar #'presentation--wire->item
                                                 (and (listp items) items))))))
      (if (and open-p (every #'terminal-completion-p items))
          (let ((selector (make-selector :items         items
                                         :visible-count (max 1 (length items))
                                         :arrangement   ':vertical)))
            (when (and (integerp selection) (plusp selection))
              (selector-move selector selection))
            (setf (terminal-ui-selector ui)       selector
                  (terminal-ui-selector-title ui) (if (stringp title) title "select")
                  (terminal-ui-selector-hint ui)  (if (stringp hint)
                                                      hint
                                                      (presentation-wire->entry hint))))
          (setf (terminal-ui-selector ui)       nil
                (terminal-ui-selector-title ui) nil
                (terminal-ui-selector-hint ui)  nil)))
    (terminal-ui--repaint-live ui))
  nil)

(defmethod presentation-apply
    ((ui terminal-ui) (operation (eql :draft)) &key text cursor &allow-other-keys)
  "Show the line the channel's modal reader edits, with its cursor."
  (when (stringp text)
    (with-terminal-ui-locked (ui)
      (line-editor-set-text (terminal-ui-editor ui) (sanitize-text text)
                            :cursor (if (typep cursor '(integer 0))
                                        (min cursor (length text))
                                        (length text)))
      (terminal-ui--repaint-live ui)))
  nil)

(defmethod presentation-apply
    ((ui terminal-ui) (operation (eql :input)) &key text images &allow-other-keys)
  "Replace the composer's input."
  (terminal-ui-set-input ui (presentation--wire->input text images))
  nil)

(defmethod presentation-apply
    ((ui terminal-ui) (operation (eql :insert)) &key text &allow-other-keys)
  "Insert text into the composer."
  (when (stringp text)
    (terminal-ui-insert-input ui text))
  nil)

(defmethod presentation-apply
    ((ui terminal-ui) (operation (eql :edit)) &key event &allow-other-keys)
  "Apply one editor key to the composer."
  (when (keywordp event)
    (terminal-ui-edit-input ui event))
  nil)
