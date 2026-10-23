(in-package #:autolith)

;;;; -- Fullscreen Memory Manager --

(defclass memory-manager ()
  ((application :initarg :application :reader memory-manager-application
                :documentation "The application whose visible memories are managed.")
   (items :initform nil :accessor memory-manager-items
          :documentation "The complete captured inventory, before local filtering.")
   (session :initarg :session :reader memory-manager-session
            :documentation "Clinedi's stable-identity search and selection state.")
   (kind :initform ':all :accessor memory-manager-kind
         :documentation "The selected All, Notes, or Facts tab.")
   (scope :initform ':all :accessor memory-manager-scope
          :documentation "The selected All, Workspace, or Global scope.")
   (focus :initform ':list :accessor memory-manager-focus
          :documentation "Whether navigation applies to the list, search, or detail pane.")
   (mode :initform ':browse :accessor memory-manager-mode
         :documentation "The current browser, editor, or confirmation state.")
   (detail-item :initform nil :accessor memory-manager-detail-item
                :documentation "The captured item whose explanation is cached.")
   (detail :initform nil :accessor memory-manager-detail
           :documentation "The selected item's terminal-safe explanation spans.")
   (scroll :initform 0 :accessor memory-manager-scroll
           :documentation "The first visible physical detail row.")
   (detail-height :initform 1 :accessor memory-manager-detail-height
                  :documentation "The detail pane's most recently rendered height.")
   (message :initform nil :accessor memory-manager-message
            :documentation "A short styled result or recoverable error.")
   (edit-item :initform nil :accessor memory-manager-edit-item
              :documentation "The captured note being edited, or NIL for a new note.")
   (editors :initform nil :accessor memory-manager-editors
            :documentation "Independent title, tags, and body editors for the note form.")
   (field :initform 0 :accessor memory-manager-field
          :documentation "The active title, tags, scope, or body form field.")
   (edit-scope :initform ':workspace :accessor memory-manager-edit-scope
               :documentation "The note form's selected persistence scope.")
   (initial-form :initform nil :accessor memory-manager-initial-form
                 :documentation "The initial form values used to detect unsaved changes."))
  (:documentation "A temporary fullscreen notes-and-facts workspace, separate from the transcript."))

(-> memory-manager--selected (memory-manager) (option memory-manager-item))
(defun memory-manager--selected (manager)
  "Return MANAGER's selected captured item, or NIL for an empty result."
  (clinedi:selector-selected-item
   (clinedi:selection-session-selector (memory-manager-session manager))))

(-> memory-manager--filter (memory-manager) null)
(defun memory-manager--filter (manager)
  "Apply kind and scope without discarding the query or selected identity."
  (clinedi:selection-session-replace-items
   (memory-manager-session manager)
   (remove-if-not
    (lambda (item)
      (and (or (eq (memory-manager-kind manager) ':all)
               (eq (memory-manager-kind manager) (memory-manager-item-kind item)))
           (or (eq (memory-manager-scope manager) ':all)
               (eq (memory-manager-scope manager) (memory-manager-item-scope item)))))
    (memory-manager-items manager))
   :reset-query-p nil)
  (setf (memory-manager-detail-item manager) nil
        (memory-manager-detail manager) nil
        (memory-manager-scroll manager) 0)
  nil)

(-> memory-manager--reload (memory-manager &key (:selected-id (option string))) null)
(defun memory-manager--reload (manager &key selected-id)
  "Reload durable state, preserving local filters and the selected identity."
  (setf (memory-manager-items manager)
        (application-memory-manager-items (memory-manager-application manager)))
  (memory-manager--filter manager)
  (when selected-id
    (clinedi:selection-session-select-id (memory-manager-session manager) selected-id))
  nil)

(-> memory-manager--notice (memory-manager keyword string) null)
(defun memory-manager--notice (manager style text)
  "Present a safe one-line result inside the manager."
  (setf (memory-manager-message manager)
        (list (terminal-span style (sanitize-text text :single-line-p t))))
  nil)

(-> memory-manager--cycle (keyword list) keyword)
(defun memory-manager--cycle (current choices)
  "Return the choice following CURRENT, wrapping at the end of CHOICES."
  (or (second (member current choices)) (first choices)))


;;;; -- Screen Composition --

(-> memory-manager--wrap (terminal list integer) list)
(defun memory-manager--wrap (terminal spans width)
  "Wrap safe SPANS into display rows bounded by WIDTH cells."
  (mapcar #'second
          (clinedi:wrap-styled-text (terminal--spans-text spans)
                                   (terminal--render-spans terminal spans)
                                   (max 1 width))))

(-> memory-manager--line (terminal string integer &key (:style keyword)) string)
(defun memory-manager--line (terminal text width &key (style ':plain))
  "Render one padded, terminal-safe TEXT row with STYLE."
  (terminal--render-spans
   terminal (list (terminal-span style
                                 (layout-fit-text (sanitize-text text :single-line-p t)
                                                  (max 1 width))))))

(-> memory-manager--pad (string integer) string)
(defun memory-manager--pad (display width)
  "Pad one trusted DISPLAY row to WIDTH terminal cells."
  (concatenate 'string display
               (make-string (max 0 (- width (text-cell-width (clinedi:ansi-strip display))))
                            :initial-element #\Space)))

(-> memory-manager--legend (memory-manager terminal integer) list)
(defun memory-manager--legend (manager terminal width)
  "Return a pinned legend with distinct key labels for the current mode."
  (let ((hints
          (case (memory-manager-mode manager)
            (:edit
             '(("Tab" "field") ("Space" "scope") ("Enter" "save")
               ("Shift-Enter" "newline") ("Esc" "cancel")))
            (:confirm-delete
             '(("y" "forget") ("n/Esc" "cancel")))
            (:confirm-discard
             '(("y" "discard changes") ("n/Esc" "keep editing")))
            (otherwise
             (case (memory-manager-focus manager)
               (:search
                '(("Type" "to search") ("↑↓" "select") ("Ctrl-U" "clear")
                  ("Enter" "done") ("Esc" "back")))
               (:detail
                '(("↑↓/PgUp/PgDn" "scroll") ("←→" "item") ("e" "edit")
                  ("d" "forget") ("r" "reload") ("Esc" "list")))
               (otherwise
                '(("↑↓" "select") ("Enter" "read") ("/" "search") ("Tab" "kind")
                  ("Shift-Tab" "scope") ("n" "new") ("e" "edit") ("d" "forget")
                  ("r" "reload") ("Esc" "close"))))))))
    (memory-manager--wrap
     terminal
     (loop for (key description) in hints for first-p = t then nil
           append (append (unless first-p (list (terminal-span ':hint "  ")))
                          (list (terminal-span ':code key)
                                (terminal-span ':hint (format nil " ~A" description)))))
     width)))

(-> memory-manager--query-line (memory-manager integer) string)
(defun memory-manager--query-line (manager width)
  "Show the query's editable tail without splitting a grapheme."
  (let* ((query (clinedi:selection-session-query (memory-manager-session manager)))
         (available (max 1 (- width 9)))
         (shown query))
    (when (> (text-cell-width query) available)
      (let ((start (length query)))
        (loop while (plusp start)
              for previous = (clinedi:grapheme-previous-boundary query start)
              while (<= (text-cell-width (subseq query previous)) (1- available))
              do (setf start previous))
        (setf shown (concatenate 'string "…" (subseq query start)))))
    (format nil "Search ~A ~A" (if (eq (memory-manager-focus manager) ':search) ">" "/") shown)))

(-> memory-manager--detail-rows (memory-manager terminal &key (:width integer) (:height integer)) list)
(defun memory-manager--detail-rows (manager terminal &key width height)
  "Return a bounded, scrollable explanation of the selected captured item."
  (let ((item (memory-manager--selected manager)))
    (unless (eq item (memory-manager-detail-item manager))
      (setf (memory-manager-detail-item manager) item
            (memory-manager-scroll manager) 0
            (memory-manager-detail manager)
            (when item
              (application-memory-manager-detail
               (memory-manager-application manager) item))))
    (let* ((rows (memory-manager--wrap
                  terminal
                  (or (memory-manager-detail manager)
                      (list (terminal-span ':hint "No matching memories.
Use / to search, Tab to change kind, or n to create a note.")))
                  width))
           (body-height (max 1 (1- height)))
           (offset (min (max 0 (memory-manager-scroll manager))
                        (max 0 (- (length rows) body-height)))))
      (setf (memory-manager-scroll manager) offset
            (memory-manager-detail-height manager) body-height)
      (cons (memory-manager--line
             terminal
             (if item
                 (format nil "~A  ~D-~D/~D"
                         (if (eq (memory-manager-focus manager) ':detail) "DETAILS" "PREVIEW")
                         (1+ offset) (min (length rows) (+ offset body-height)) (length rows))
                 "DETAILS")
             width :style ':brand)
            (subseq rows offset (min (length rows) (+ offset body-height)))))))

(-> memory-manager--list-rows (memory-manager terminal &key (:width integer) (:height integer)) list)
(defun memory-manager--list-rows (manager terminal &key width height)
  "Render a window of compact two-row items, not a transcript dump."
  (let* ((selector (clinedi:selection-session-selector (memory-manager-session manager)))
         (visible (max 1 (floor (1- height) 2))))
    (unless (= visible (selector-visible-count selector))
      (reinitialize-instance selector :visible-count visible))
    (multiple-value-bind (items start selected) (clinedi:selector-window selector)
      (declare (ignore start))
      (append
       (list (memory-manager--line
              terminal
              (format nil "~D~:[~;/~D~] matching"
                      (length (selector-items selector))
                      (/= (length (selector-items selector)) (length (memory-manager-items manager)))
                      (length (memory-manager-items manager)))
              width :style ':brand))
       (if items
           (loop for item in items for index from 0
                 append
                 (list
                  (memory-manager--line
                   terminal
                   (format nil "~A ~A" (if (eql index selected) "›" " ")
                           (memory-manager-item-title item))
                   width :style (if (eql index selected) ':selected ':strong))
                  (memory-manager--line
                   terminal
                   (format nil "  ~(~A~)  ~(~A~)  ~(~A~)"
                           (memory-manager-item-kind item) (memory-manager-item-scope item)
                           (memory-manager-item-status item))
                   width :style (case (memory-manager-item-status item)
                                  ((:current :supported) ':dim)
                                  ((:stale :conflicted :expired) ':notice)
                                  (otherwise ':hint)))))
           (list (memory-manager--line terminal "No matches" width :style ':hint)))))))

(-> memory-manager--browse-frame (memory-manager terminal &key (:width integer) (:height integer)) list)
(defun memory-manager--browse-frame (manager terminal &key width height)
  "Compose a split browser on wide screens and a list/detail stack on narrow ones."
  (let* ((legend (memory-manager--legend manager terminal width))
         (header
           (list
            (memory-manager--line
             terminal
             (format nil "MEMORY MANAGER  ~D notes  ~D facts"
                     (count ':note (memory-manager-items manager) :key #'memory-manager-item-kind)
                     (count ':fact (memory-manager-items manager) :key #'memory-manager-item-kind))
             width :style ':brand)
            (memory-manager--line
             terminal
             (format nil "Kind  ~A"
                     (ecase (memory-manager-kind manager)
                       (:all "[All] Notes Facts")
                       (:note "All [Notes] Facts")
                       (:fact "All Notes [Facts]")))
             width :style ':strong)
            (memory-manager--line
             terminal (format nil "Scope  ~A" (string-downcase (symbol-name (memory-manager-scope manager))))
             width :style ':dim)
            (memory-manager--line
             terminal
             (memory-manager--query-line manager width)
             width :style (if (eq (memory-manager-focus manager) ':search) ':code ':dim))))
         (body-height (max 1 (- height (length header) (length legend) 1)))
         (split-p (>= width 76))
         (list-width (min 40 (floor (* width 0.4))))
         (body
           (if split-p
              (let ((left (memory-manager--list-rows manager terminal :width list-width :height body-height))
                    (right (memory-manager--detail-rows manager terminal :width (- width list-width 3) :height body-height)))
                 (loop for index below body-height
                       collect (concatenate 'string
                                            (memory-manager--pad (or (nth index left) "") list-width)
                                            (terminal--render-spans terminal (list (terminal-span ':dim " │ ")))
                                            (or (nth index right) ""))))
               (let ((rows (if (eq (memory-manager-focus manager) ':detail)
                              (memory-manager--detail-rows manager terminal :width width :height body-height)
                              (memory-manager--list-rows manager terminal :width width :height body-height))))
                 (loop for index below body-height collect (or (nth index rows) ""))))))
    (append header body
            (list (if (memory-manager-message manager)
                      (first (memory-manager--wrap terminal (memory-manager-message manager) width))
                      (memory-manager--line
                       terminal
                       (cond
                         ((eq (memory-manager-focus manager) ':detail)
                          "Esc to return to the list")
                         (split-p
                          "Enter to focus details")
                         (t
                          "Enter to read the selected item"))
                       width :style ':dim)))
            legend)))


;;;; -- Note Form --

(-> memory-manager--form-values (memory-manager) list)
(defun memory-manager--form-values (manager)
  "Return the current note form's exact values for saving and dirty detection."
  (append (mapcar #'line-editor-text (memory-manager-editors manager))
          (list (memory-manager-edit-scope manager))))

(-> memory-manager--begin-edit (memory-manager &key (:new-p boolean)) null)
(defun memory-manager--begin-edit (manager &key new-p)
  "Open a separate note form without touching the conversation composer."
  (let* ((item (unless new-p (memory-manager--selected manager)))
         (memory (and item (memory-manager-item-data item))))
    (cond
      ((and item (not (application-memory-manager-editable-p item)))
       (memory-manager--notice manager ':notice "Facts are inspected here; edit their supporting evidence or rules."))
      ((or new-p item)
       (setf (memory-manager-edit-item manager) item
             (memory-manager-editors manager)
             (mapcar (lambda (text) (line-editor-create :text text))
                     (if memory
                         (list (memory-title memory) (format nil "~{~A~^, ~}" (memory-tags memory))
                               (memory-content memory))
                         (list "" "" "")))
             (memory-manager-edit-scope manager) (if memory (memory-scope memory) ':workspace)
             (memory-manager-field manager) 0
             (memory-manager-mode manager) ':edit
             (memory-manager-message manager) nil
             (memory-manager-initial-form manager) (memory-manager--form-values manager)))))
  nil)

(-> memory-manager--form-frame (memory-manager terminal &key (:width integer) (:height integer)) (values list integer integer))
(defun memory-manager--form-frame (manager terminal &key width height)
  "Render the note form, scrolling its fields to keep the active editor visible."
  (let ((rows nil) (cursor-row 0) (cursor-column 0)
        (legend (memory-manager--legend manager terminal width)))
    (push (memory-manager--line terminal
                                (if (memory-manager-edit-item manager) "EDIT NOTE" "NEW NOTE")
                                width :style ':brand) rows)
    (loop for field from 0 below 4
          for label in '("Title" "Tags (comma separated)" "Scope (Space to change)" "Content")
          do (push (memory-manager--line terminal label width
                                         :style (if (= field (memory-manager-field manager)) ':brand ':dim)) rows)
             (if (= field 2)
                 (progn
                   (when (= field (memory-manager-field manager))
                     (setf cursor-row (length rows) cursor-column 0))
                   (push (memory-manager--line terminal
                                              (string-downcase (symbol-name (memory-manager-edit-scope manager)))
                                              width :style ':code) rows))
                 (let* ((editor (nth (if (= field 3) 2 field) (memory-manager-editors manager)))
                        (text (sanitize-text (line-editor-text editor)))
                        (display (terminal--render-spans terminal (list (terminal-span ':code text)))))
                   (multiple-value-bind (wrapped row column)
                       (clinedi:wrap-styled-editor-rows text display
                                                       :cursor (min (length text) (line-editor-cursor editor))
                                                       :columns width)
                     (when (= field (memory-manager-field manager))
                       (setf cursor-row (+ (length rows) row) cursor-column column))
                     (dolist (line wrapped) (push (second line) rows))))))
    (let* ((ordered (nreverse rows))
           (available (max 1 (- height (length legend) 1)))
           (offset (max 0 (- cursor-row (1- available))))
           (visible (subseq ordered offset (min (length ordered) (+ offset available)))))
      (values
       (append visible (make-list (max 0 (- available (length visible))) :initial-element "")
               (list (if (memory-manager-message manager)
                         (first (memory-manager--wrap terminal (memory-manager-message manager) width)) ""))
               legend)
       (- cursor-row offset) cursor-column))))

(-> memory-manager--save (memory-manager) null)
(defun memory-manager--save (manager)
  "Save a complete validated note, keeping the draft on any recoverable failure."
  (destructuring-bind (title tags content scope) (memory-manager--form-values manager)
    (let* ((arguments (list :title title :content content
                            :tags (remove "" (mapcar (lambda (tag) (string-trim '(#\Space #\Tab) tag))
                                                      (uiop:split-string tags :separator '(#\,)))
                                          :test #'string=)
                            :scope scope))
           (memory (if (memory-manager-edit-item manager)
                       (apply #'application-memory-manager-save
                              (memory-manager-application manager) (memory-manager-edit-item manager) arguments)
                       (apply #'application-memory-manager-create (memory-manager-application manager) arguments))))
      (setf (memory-manager-mode manager) ':browse
            (memory-manager-focus manager) ':list
            (memory-manager-kind manager) ':note
            (memory-manager-scope manager) (memory-scope memory))
      (clinedi:selection-session-replace-items (memory-manager-session manager) nil)
      (memory-manager--reload manager :selected-id (concatenate 'string "note:" (memory-identifier memory)))
      (memory-manager--notice manager ':success "Note saved.")))
  nil)


;;;; -- Interaction and Ownership --

(-> memory-manager--scroll (memory-manager t) null)
(defun memory-manager--scroll (manager event)
  "Apply line or page movement to the detail pane."
  (let ((page (memory-manager-detail-height manager)))
    (case event
      ((:up :scroll-up) (decf (memory-manager-scroll manager) 1))
      ((:down :scroll-down) (incf (memory-manager-scroll manager) 1))
      (:page-up (decf (memory-manager-scroll manager) page))
      (:page-down (incf (memory-manager-scroll manager) page))
      (:home (setf (memory-manager-scroll manager) 0))
      (:end (setf (memory-manager-scroll manager) most-positive-fixnum))))
  nil)

(-> memory-manager--edit-event (memory-manager t) null)
(defun memory-manager--edit-event (manager event)
  "Apply a form event without exposing draft text to the application composer."
  (cond
    ((eq event ':escape)
     (setf (memory-manager-mode manager)
           (if (equal (memory-manager-initial-form manager) (memory-manager--form-values manager))
               ':browse ':confirm-discard)))
    ((member event '(:complete :complete-previous))
     (setf (memory-manager-field manager)
           (mod (+ (memory-manager-field manager) (if (eq event ':complete) 1 -1)) 4)))
    ((eq event ':submit)
     (memory-manager--save manager))
    ((= (memory-manager-field manager) 2)
     (when (or (member event '(:left :right)) (equal event '(:insert " ")))
       (setf (memory-manager-edit-scope manager)
             (memory-manager--cycle (memory-manager-edit-scope manager) '(:workspace :global)))))
    ((or (not (eq event ':insert-newline)) (= (memory-manager-field manager) 3))
     (line-editor-handle-event
      (nth (if (= (memory-manager-field manager) 3) 2 (memory-manager-field manager))
           (memory-manager-editors manager)) event)))
  nil)

(-> memory-manager--browse-event (memory-manager t) boolean)
(defun memory-manager--browse-event (manager event)
  "Handle browser navigation and actions; return NIL only when closing."
  (let ((session (memory-manager-session manager))
        (focus (memory-manager-focus manager)))
    (cond
      ((eq focus ':search)
       (if (member event '(:escape :submit))
           (setf (memory-manager-focus manager) ':list)
           (clinedi:selection-session-handle-event session event)))
      ((eq event ':escape)
       (if (eq focus ':detail)
           (setf (memory-manager-focus manager) ':list)
           (return-from memory-manager--browse-event nil)))
      ((member event '(:complete :complete-previous))
       (if (eq event ':complete)
           (setf (memory-manager-kind manager)
                 (memory-manager--cycle (memory-manager-kind manager) '(:all :note :fact)))
           (setf (memory-manager-scope manager)
                 (memory-manager--cycle (memory-manager-scope manager) '(:all :workspace :global))))
       (memory-manager--filter manager))
      ((eq event ':submit)
       (when (memory-manager--selected manager)
         (setf (memory-manager-focus manager) ':detail)))
      ((and (consp event) (eq (first event) ':insert))
       (cond
         ((string= (second event) "/") (setf (memory-manager-focus manager) ':search))
         ((string-equal (second event) "n") (memory-manager--begin-edit manager :new-p t))
         ((string-equal (second event) "e") (memory-manager--begin-edit manager))
         ((string-equal (second event) "r")
          (memory-manager--reload manager)
          (memory-manager--notice manager ':success "Memories reloaded."))
         ((string-equal (second event) "d")
          (let ((item (memory-manager--selected manager)))
            (when item
              (if (application-memory-manager-deletable-p item)
                  (setf (memory-manager-mode manager) ':confirm-delete)
                  (memory-manager--notice manager ':notice "This fact is derived. Inspect its premises or retire its rule.")))))))
      ((and (eq focus ':detail) (member event '(:left :right)))
       (clinedi:selection-session-handle-event session (if (eq event ':left) ':up ':down)))
      ((or (eq focus ':detail) (member event '(:scroll-up :scroll-down)))
       (memory-manager--scroll manager event))
      (t
       (clinedi:selection-session-handle-event session event))))
  t)

(-> memory-manager--event (memory-manager t) boolean)
(defun memory-manager--event (manager event)
  "Handle one event, showing typed recoverable failures inside the manager."
  (when (eq event ':stream-end)
    (return-from memory-manager--event nil))
  (when (member event '(:interrupt :end-of-input))
    (if (member (memory-manager-mode manager) '(:edit :confirm-discard))
        (setf event ':escape)
        (return-from memory-manager--event nil)))
  (when (and (eq (memory-manager-mode manager) ':browse)
             (consp event) (eq (first event) ':scroll) (member (second event) '(-1 1)))
    (let ((movement (if (minusp (second event)) ':up ':down)))
      (loop repeat 3
            do (if (eq (memory-manager-focus manager) ':detail)
                   (memory-manager--scroll manager movement)
                   (clinedi:selection-session-handle-event (memory-manager-session manager) movement))))
    (return-from memory-manager--event t))
  (handler-case
      (case (memory-manager-mode manager)
        (:edit
         (memory-manager--edit-event manager event)
         t)
        ((:confirm-delete :confirm-discard)
         (cond
           ((equalp event '(:insert "y"))
            (when (eq (memory-manager-mode manager) ':confirm-delete)
              (let ((result (application-memory-manager-forget
                             (memory-manager-application manager) (memory-manager--selected manager))))
                (memory-manager--reload manager)
                (setf (memory-manager-message manager) result)))
            (setf (memory-manager-mode manager) ':browse))
           ((or (eq event ':escape) (equalp event '(:insert "n")))
            (setf (memory-manager-mode manager)
                  (if (eq (memory-manager-mode manager) ':confirm-discard) ':edit ':browse))))
         t)
        (otherwise
         (memory-manager--browse-event manager event)))
    (autolith-error (condition)
      (memory-manager--notice manager ':failure (princ-to-string condition))
      (when (eq (memory-manager-mode manager) ':confirm-delete)
        (setf (memory-manager-mode manager) ':browse))
      t)))

(-> memory-manager--frame (memory-manager) (values list integer integer boolean))
(defun memory-manager--frame (manager)
  "Return a bounded manager frame and its cursor without changing the transcript."
  (let* ((terminal (terminal-ui-terminal (application-ui (memory-manager-application manager))))
         (width (max 1 (terminal-columns terminal)))
         (height (max 1 (terminal-rows terminal))))
    (cond
      ((or (< width 24) (< height 12))
       (let ((rows (memory-manager--wrap
                    terminal
                    (list (terminal-span ':notice (format nil "Memory manager~%Enlarge the terminal.~%Esc to close."))) width)))
         (values (subseq rows 0 (min height (length rows))) 0 0 nil)))
      ((eq (memory-manager-mode manager) ':edit)
       (multiple-value-bind (rows row column) (memory-manager--form-frame manager terminal :width width :height height)
         (values rows row column (/= (memory-manager-field manager) 2))))
      ((member (memory-manager-mode manager) '(:confirm-delete :confirm-discard))
       (let* ((legend (memory-manager--legend manager terminal width))
              (rows (memory-manager--wrap
                     terminal
                     (if (eq (memory-manager-mode manager) ':confirm-delete)
                         (list (terminal-span
                                ':failure
                                (format nil "FORGET ~A?~2%"
                                        (sanitize-text
                                         (memory-manager-item-title (memory-manager--selected manager))
                                         :single-line-p t)))
                               (terminal-span ':plain "Dependent memories may become stale. Derived facts may stop holding."))
                         (list (terminal-span ':notice "Discard unsaved note changes?")))
                     width)))
         (values (append (subseq rows 0 (min (length rows) (max 1 (- height (length legend) 1))))
                         (list "") legend)
                 0 0 nil)))
      (t
       (values (memory-manager--browse-frame manager terminal :width width :height height)
               3 (min (1- width)
                      (text-cell-width (memory-manager--query-line manager width)))
               (eq (memory-manager-focus manager) ':search))))))

(-> application-memory-manager (application &key (:query (option string)) (:identifier (option string))) null)
(defun application-memory-manager (application &key query identifier)
  "Manage visible notes and facts in a temporary fullscreen workspace.

The transcript, scroll anchor, composer, attachments, and pending input are not
used as manager state. Restore the ordinary fullscreen frame on every exit."
  (let* ((ui (application-ui application))
         (terminal (terminal-ui-terminal ui))
         (manager (make-instance
                   'memory-manager :application application
                   :session (clinedi:make-selection-session
                             :identity-key #'memory-manager-item-key :identity-test #'string=
                             :search-key #'memory-manager-item-search-text :search-p t :visible-count 8)))
         (painter (clinedi:make-frame-painter)))
    (memory-manager--reload manager)
    (when (non-empty-string-p query)
      (clinedi:selection-session-handle-event (memory-manager-session manager) (list ':insert query)))
    (when identifier
      (let ((matches (remove-if-not
                      (lambda (item)
                        (let ((id (memory-manager-item-identifier item)))
                          (and (<= (length identifier) (length id))
                               (string-equal identifier id :end2 (length identifier)))))
                      (memory-manager-items manager))))
        (cond
          ((= (length matches) 1)
           (clinedi:selection-session-select-id
            (memory-manager-session manager) (memory-manager-item-key (first matches)))
           (setf (memory-manager-focus manager) ':detail))
          (t
           (clinedi:selection-session-handle-event (memory-manager-session manager) (list ':insert identifier))
           (memory-manager--notice manager ':notice
                                   (if matches "Ambiguous ID. Use / to enter more characters." "No memory matches this ID."))))))
    (terminal-ui-call-with-exclusive-input
     ui
     (lambda ()
       (let ((suspended-p (terminal-ui-live-output-suspended-p ui))
             (cursor-visible-p (fullscreen-terminal-ui-cursor-visible-p ui)))
         (unwind-protect
              (progn
                (with-terminal-ui-locked (ui)
                  (setf (terminal-ui-live-output-suspended-p ui) t))
                (loop
                  (terminal-ui-refresh-size ui nil)
                  (with-terminal-ui-locked (ui)
                    (multiple-value-bind (rows row column visible-p) (memory-manager--frame manager)
                      (clinedi:frame-painter-paint
                       painter rows
                       (lambda (text) (terminal--write terminal text) (terminal-flush terminal))
                       :height (terminal-rows terminal) :width (terminal-columns terminal)
                       :cursor-row row :cursor-column column :cursor-visible-p visible-p)))
                  (let ((event (if (terminal-input-ready-p terminal)
                                   (terminal-read-event terminal)
                                   (progn (sleep *terminal-ui-picker-poll-interval*) ':poll))))
                    (cond
                      ((and (consp event) (eq (first event) ':resize))
                       (terminal-ui-resize ui (second event) :rows (third event)))
                      ((eq event ':clear-screen)
                       (clinedi:frame-painter-invalidate painter))
                      ((not (memory-manager--event manager event))
                       (return))))))
           (with-terminal-ui-locked (ui)
             (setf (terminal-ui-live-output-suspended-p ui) suspended-p
                   (fullscreen-terminal-ui-cursor-visible-p ui) cursor-visible-p)
             (terminal-ui-fullscreen-invalidate ui)
             (when (and (terminal-ui-started-p ui) (not suspended-p))
               (terminal-ui--paint-live ui))))))))
  nil)
