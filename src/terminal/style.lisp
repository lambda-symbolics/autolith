(in-package #:autolith)

;;;; -- Semantic Terminal Styles --

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *terminal-style-names*
    '(:plain :brand
      :brand-gradient-1 :brand-gradient-2 :brand-gradient-3
      :brand-gradient-4 :brand-gradient-5 :brand-gradient-6
      :recovery-gradient-1 :recovery-gradient-2 :recovery-gradient-3
      :recovery-gradient-4 :recovery-gradient-5 :recovery-gradient-6
      :lambda-logo-1 :lambda-logo-2 :lambda-logo-3
      :lambda-logo-4 :lambda-logo-5 :lambda-logo-6
      :user :tool :success :failure :notice :dim :annotation :hint :selected
      :surface :surface-code :surface-diff-add :surface-diff-remove
      :strong :emphasis :code :code-copy :link :lisp-prompt :plan-active :timestamp-time
      :mermaid-box :mermaid-edge :mermaid-arrow :mermaid-label
      :agent-spinner :agent-name :child-name :agent-role :agent-tool
      :command-spinner :command-id :command-tool
      :status-plain :status-dim :status-accent
      :legend-plain :legend-dim :legend-accent
      :status-model :status-effort :status-branch
      :compaction-label :compaction-track :compaction-head
      :syntax-comment :syntax-keyword :syntax-string :syntax-escape
      :syntax-number :syntax-type :syntax-function :syntax-property
      :syntax-heading :heading-emphasis :syntax-link)
    "Every semantic style a theme must render."))

(deftype terminal-style ()
  "A semantic terminal style resolved to color and emphasis by the renderer."
  `(member ,@*terminal-style-names*))

(defparameter *terminal-brand-gradient-names*
  '(:brand-gradient-1 :brand-gradient-2 :brand-gradient-3
    :brand-gradient-4 :brand-gradient-5 :brand-gradient-6)
  "The startup mark's row styles, top to bottom.")

(defparameter *terminal-recovery-gradient-names*
  '(:recovery-gradient-1 :recovery-gradient-2 :recovery-gradient-3
    :recovery-gradient-4 :recovery-gradient-5 :recovery-gradient-6)
  "The recovery startup mark's row styles, top to bottom.")

(defparameter *terminal-lambda-logo-names*
  '(:lambda-logo-1 :lambda-logo-2 :lambda-logo-3
    :lambda-logo-4 :lambda-logo-5 :lambda-logo-6)
  "The Lambda Symbolics logo's green row styles, top to bottom.")

(-> terminal--style-table (list) list)
(defun terminal--style-table (specifications)
  "Return an alist of style names to Colorist styles from SPECIFICATIONS.

Each specification is (NAME MAKE-STYLE-ARGUMENTS)."
  (loop for (name arguments) in specifications
        collect (cons name (apply #'make-style arguments))))

(-> terminal--recovery-gradient-styles () list)
(defun terminal--recovery-gradient-styles ()
  "Return the red indexed ramp every theme uses after recovery starts Autolith."
  (loop for name in *terminal-recovery-gradient-names*
        for index in '(224 217 210 203 197 196)
        collect (cons name (make-style
                            :foreground (indexed-color index :fallback ':red)
                            :bold       t))))

(-> terminal--lambda-logo-styles () list)
(defun terminal--lambda-logo-styles ()
  "Return NanoBSD's six green logo shades with basic green fallbacks."
  (loop for name in *terminal-lambda-logo-names*
        for index in '(157 121 85 84 78 72)
        collect (cons name (make-style
                            :foreground (indexed-color index :fallback ':green)
                            :bold       t))))

(-> terminal-style-table-autolith () list)
(defun terminal-style-table-autolith ()
  "Return the default style table.

General interface styles use the basic ANSI palette so Autolith follows the
terminal's own theme. The startup marks, Lambda Symbolics logo, promoted child
name, and status background opt into indexed colors. Surface fills use the
neutral bright black because basic terminals cannot tint a row."
  (append
   (terminal--style-table
    '((:plain ())
      (:brand (:foreground :magenta :bold t))
      (:user (:foreground :cyan :bold t))
      (:tool (:foreground :yellow :bold t))
      (:success (:foreground :green))
      (:failure (:foreground :red :bold t))
      (:notice (:foreground :yellow))
      (:dim (:faint t))
      (:annotation (:faint t))
      (:hint (:faint t :italic t))
      (:selected (:reverse t))
      (:surface (:background :bright-black))
      (:surface-code (:background :bright-black))
      (:surface-diff-add (:background :bright-black))
      (:surface-diff-remove (:background :bright-black))
      (:strong (:bold t))
      (:emphasis (:italic t))
      (:code (:foreground :cyan))
      (:code-copy (:faint t :underline t))
      (:mermaid-box (:foreground :cyan))
      (:mermaid-edge (:foreground :yellow))
      (:mermaid-arrow (:foreground :magenta :bold t))
      (:mermaid-label (:foreground :green))
      (:lisp-prompt (:foreground :red :bold t))
      (:plan-active (:foreground :cyan :bold t))
      (:timestamp-time (:foreground :cyan))
      (:agent-spinner (:foreground :bright-green :bold t))
      (:agent-name (:foreground :bright-cyan :bold t))
      (:agent-role (:foreground :bright-magenta))
      (:agent-tool (:foreground :bright-yellow))
      (:command-spinner (:foreground :bright-green :bold t))
      (:command-id (:foreground :bright-cyan :bold t))
      (:command-tool (:foreground :bright-yellow))
      (:legend-plain (:foreground :bright-white))
      (:legend-dim (:foreground :white))
      (:legend-accent (:foreground :bright-magenta :bold t))
      (:syntax-comment (:faint t))
      (:syntax-keyword (:foreground :magenta))
      (:syntax-string (:foreground :green))
      (:syntax-escape (:foreground :yellow))
      (:syntax-number (:foreground :yellow))
      (:syntax-type (:foreground :cyan))
      (:syntax-function (:foreground :blue))
      (:syntax-property (:foreground :cyan))
      (:syntax-heading (:foreground :magenta :bold t))
      (:heading-emphasis (:foreground :magenta :bold t :italic t))
      (:link (:foreground :cyan :underline t))
      (:syntax-link (:foreground :cyan :underline t))))
   (list
    (cons ':child-name
          (make-style
           :foreground (indexed-color 78 :fallback ':green))))
   (loop for name in *terminal-brand-gradient-names*
         for index in '(193 157 121 85 84 83)
         collect (cons name (make-style
                             :foreground (indexed-color index :fallback ':green)
                             :bold       t)))
   (terminal--recovery-gradient-styles)
   (terminal--lambda-logo-styles)
   (loop for (name foreground arguments) in
         '((:status-plain :bright-white ())
           (:status-dim :white ())
           (:status-accent :bright-magenta (:bold t))
           (:status-model :bright-cyan (:bold t))
           (:status-effort :bright-red (:bold t))
           (:status-branch :bright-green (:bold t))
           (:compaction-label :bright-yellow (:bold t))
           (:compaction-track :white ())
           (:compaction-head :bright-yellow (:bold t)))
         collect (cons name
                       (apply #'make-style
                              :foreground foreground
                              :background
                              (indexed-color 236 :fallback ':black)
                              arguments)))))

(defparameter *almighty-palette*
  '((:ink . "#eff6ff")
    (:soft . "#8ec5ff")
    (:accent . "#fce04d")
    (:accent-soft . "#f6c91f")
    (:accent-light . "#fefce8")
    (:secondary . "#53eafd")
    (:safe . "#7bf1a8")
    (:danger . "#ff6467")
    (:canvas . "#193cb8")
    (:canvas-darker . "#162456"))
  "Micah's Almighty Lisp palette from almightylisp.com: near-white ink, yellow
accent, and cyan secondary on a blue canvas.")

(defparameter *almighty-gradient*
  '("#fefce8" "#fef9c2" "#fff085" "#fce04d" "#f6c91f" "#ebb300")
  "Yellow shades from light to deep for the Almighty startup mark.")

(-> almighty-color (keyword) color)
(defun almighty-color (name)
  "Return palette color NAME as a 24-bit color with derived fallbacks."
  (hex-color (rest (assoc name *almighty-palette*))))

(-> terminal-style-table-almighty () list)
(defun terminal-style-table-almighty ()
  "Return the Almighty Lisp style table.

Plain text keeps the terminal default, which the theme sets to the site's
near-white. Strong text uses the yellow accent, code and strings use cyan, and
quiet text uses soft blue."
  (let ((ink           (almighty-color ':ink))
        (soft          (almighty-color ':soft))
        (accent        (almighty-color ':accent))
        (accent-soft   (almighty-color ':accent-soft))
        (accent-light  (almighty-color ':accent-light))
        (secondary     (almighty-color ':secondary))
        (safe          (almighty-color ':safe))
        (danger        (almighty-color ':danger))
        (canvas-darker (almighty-color ':canvas-darker)))
    (append
     (terminal--style-table
      `((:plain ())
        (:brand (:foreground ,accent :bold t))
        (:user (:foreground ,secondary :bold t))
        (:tool (:foreground ,accent :bold t))
        (:success (:foreground ,safe))
        (:failure (:foreground ,danger :bold t))
        (:notice (:foreground ,accent))
        (:dim (:foreground ,soft))
        (:annotation (:foreground ,soft))
        (:hint (:foreground ,soft :italic t))
        (:selected (:reverse t))
        (:surface (:background ,canvas-darker))
        (:surface-code (:background ,canvas-darker))
        (:surface-diff-add (:background ,canvas-darker))
        (:surface-diff-remove (:background ,canvas-darker))
        (:strong (:foreground ,accent :bold t))
        (:emphasis (:italic t))
        (:code (:foreground ,secondary))
        (:code-copy (:foreground ,soft :underline t))
        (:mermaid-box (:foreground ,secondary))
        (:mermaid-edge (:foreground ,accent-soft))
        (:mermaid-arrow (:foreground ,accent :bold t))
        (:mermaid-label (:foreground ,ink))
        (:lisp-prompt (:foreground ,accent :bold t))
        (:plan-active (:foreground ,secondary :bold t))
        (:timestamp-time (:foreground ,soft))
        (:agent-spinner (:foreground ,accent :bold t))
        (:agent-name (:foreground ,secondary :bold t))
        (:agent-role (:foreground ,soft))
        (:agent-tool (:foreground ,accent))
        (:command-spinner (:foreground ,accent :bold t))
        (:command-id (:foreground ,secondary :bold t))
        (:command-tool (:foreground ,accent))
        (:child-name (:foreground ,safe :bold t))
        (:legend-plain (:foreground ,ink))
        (:legend-dim (:foreground ,soft))
        (:legend-accent (:foreground ,accent :bold t))
        (:syntax-comment (:foreground ,soft))
        (:syntax-keyword (:foreground ,accent))
        (:syntax-string (:foreground ,secondary))
        (:syntax-escape (:foreground ,accent-soft))
        (:syntax-number (:foreground ,ink))
        (:syntax-type (:foreground ,secondary))
        (:syntax-function (:foreground ,accent-light))
        (:syntax-property (:foreground ,secondary))
        (:syntax-heading (:foreground ,accent :bold t))
        (:heading-emphasis (:foreground ,accent :bold t :italic t))
        (:link (:foreground ,secondary :underline t))
        (:syntax-link (:foreground ,secondary :underline t))))
     (loop for name in *terminal-brand-gradient-names*
           for hex in *almighty-gradient*
           collect (cons name (make-style :foreground (hex-color hex) :bold t)))
     (terminal--recovery-gradient-styles)
     (terminal--lambda-logo-styles)
     (loop for (name foreground arguments) in
           `((:status-plain ,ink ())
             (:status-dim ,soft ())
             (:status-accent ,accent (:bold t))
             (:status-model ,secondary (:bold t))
             (:status-effort ,danger (:bold t))
             (:status-branch ,safe (:bold t))
             (:compaction-label ,accent (:bold t))
             (:compaction-track ,soft ())
             (:compaction-head ,accent (:bold t)))
           collect (cons name
                         (apply #'make-style
                                :foreground foreground
                                :background canvas-darker
                                arguments))))))


;;;; -- Themes --

(defstruct (terminal-theme
            (:constructor make-terminal-theme
                (&key name style-table foreground background))
            (:copier nil))
  "A named presentation of semantic styles plus optional terminal default colors.

FOREGROUND and BACKGROUND are 24-bit colors imposed on a styled interactive
terminal while its UI is active, or NIL to leave the terminal's own defaults."
  (name        ':autolith :type keyword :read-only t)
  (style-table nil :type list :read-only t)
  (foreground  nil :type (option color) :read-only t)
  (background  nil :type (option color) :read-only t))

(defparameter *terminal-themes*
  (list (make-terminal-theme :name        ':autolith
                             :style-table (terminal-style-table-autolith))
        (make-terminal-theme :name        ':almighty
                             :style-table (terminal-style-table-almighty)
                             :foreground  (almighty-color ':ink)
                             :background  (almighty-color ':canvas)))
  "The presentation themes, default first.")

(defvar *terminal-theme* (first *terminal-themes*)
  "The installed presentation theme.")

(defparameter *terminal-style-table* (terminal-theme-style-table *terminal-theme*)
  "Colorist style objects for Autolith's semantic styles, from the installed theme.")

(-> terminal-theme-find (keyword) terminal-theme)
(defun terminal-theme-find (name)
  "Return the theme called NAME."
  (or (find name *terminal-themes* :key #'terminal-theme-name)
      (error 'terminal-theme-error
             :name    name
             :message (format nil "~S is not a terminal theme; choose one of ~{~S~^, ~}."
                              name (mapcar #'terminal-theme-name *terminal-themes*)))))

(-> terminal-theme-install (keyword) terminal-theme)
(defun terminal-theme-install (name)
  "Make the theme called NAME current for every later render, returning it."
  (let ((theme (terminal-theme-find name)))
    (setf *terminal-theme* theme
          *terminal-style-table* (terminal-theme-style-table theme))
    theme))

(-> terminal-theme-enter-sequence (terminal-theme) string)
(defun terminal-theme-enter-sequence (theme)
  "Return the controls imposing THEME's default colors, or an empty string."
  (concatenate
   'string
   (if (terminal-theme-foreground theme)
       (default-color-sequence ':foreground (terminal-theme-foreground theme))
       "")
   (if (terminal-theme-background theme)
       (default-color-sequence ':background (terminal-theme-background theme))
       "")))

(-> terminal-theme-leave-sequence (terminal-theme) string)
(defun terminal-theme-leave-sequence (theme)
  "Return the controls restoring the terminal's own defaults after THEME."
  (concatenate
   'string
   (if (terminal-theme-foreground theme)
       (default-color-reset-sequence ':foreground)
       "")
   (if (terminal-theme-background theme)
       (default-color-reset-sequence ':background)
       "")))

(defparameter *terminal-style-reset*
  (reset-sequence :level ':basic)
  "The trusted control that restores default terminal rendition.")

(-> terminal-style-reset-sequence () string)
(defun terminal-style-reset-sequence ()
  "Return the trusted control that restores default terminal rendition."
  *terminal-style-reset*)

(-> terminal-environment-indexed-color-p () boolean)
(defun terminal-environment-indexed-color-p ()
  "Return true when the process environment advertises indexed or 24-bit colors."
  (not (null (member (effective-color-level) '(:indexed :truecolor)))))

(-> terminal-style--level (boolean) keyword)
(defun terminal-style--level (indexed-color-p)
  "Return the Colorist level for INDEXED-COLOR-P.

Indexed color renders at 24 bits when the environment advertises true color,
which only theme colors carrying RGB values make use of."
  (cond ((not indexed-color-p)
         ':basic)
        ((eq (effective-color-level) ':truecolor)
         ':truecolor)
        (t
         ':indexed)))

(-> terminal-style-sequence
    (terminal-style &optional boolean)
    (option string))
(defun terminal-style-sequence
    (style &optional (indexed-color-p (terminal-environment-indexed-color-p)))
  "Return STYLE's trusted control, using INDEXED-COLOR-P for brand gradients and theme colors.

A theme that does not define STYLE renders it plainly instead of failing the row."
  (let ((entry (assoc style *terminal-style-table*)))
    (when entry
      (let ((sequence (sgr-sequence (rest entry)
                                    :level (terminal-style--level indexed-color-p))))
        (and (plusp (length sequence)) sequence)))))

(-> terminal-environment-styling-p () boolean)
(defun terminal-environment-styling-p ()
  "Return true when the process environment permits color and emphasis output."
  (not (eq (effective-color-level) ':none)))


;;;; -- Styled Spans --

(-> terminal-span-p (t) boolean)
(defun terminal-span-p (value)
  "Return true when VALUE pairs a known terminal style with untrusted text."
  (and (consp value)
       (typep (first value) 'terminal-style)
       (stringp (rest value))))

(-> terminal-span (terminal-style string) cons)
(defun terminal-span (style text)
  "Return one styled span pairing STYLE with untrusted TEXT."
  (cons style text))

(-> terminal-span-style ((or cons termdown:widget)) terminal-style)
(defun terminal-span-style (span)
  "Return SPAN's semantic style, or the role styling a widget's label."
  (if (termdown:widget-p span)
      (termdown:widget-role span)
      (first span)))

(-> terminal-span-text ((or cons termdown:widget)) string)
(defun terminal-span-text (span)
  "Return SPAN's untrusted text, or a widget's label."
  (if (termdown:widget-p span)
      (termdown:widget-label span)
      (rest span)))

(-> terminal-styled-text-p (t) boolean)
(defun terminal-styled-text-p (value)
  "Return true when VALUE is a proper list of styled spans and widgets."
  (loop for tail = value then (rest tail)
        while tail
        always (and (consp tail)
                    (or (terminal-span-p (first tail))
                        (and (termdown:widget-p (first tail))
                             (typep (termdown:widget-role (first tail))
                                    'terminal-style))))))

(deftype terminal-styled-text ()
  "A proper list of styled spans and widgets rendered in order."
  '(satisfies terminal-styled-text-p))

(defstruct (terminal-rendered-row
            (:constructor terminal--make-rendered-row (text display))
            (:copier nil))
  "One fully rendered terminal row with matched plain and styled content."
  (text    "" :type string :read-only t)
  (display "" :type string :read-only t))

(-> terminal--spans-width (list) (integer 0))
(defun terminal--spans-width (spans)
  "Return the single-row cell width of sanitized SPANS."
  (text-cell-width (termdown:spans-text spans :single-line-p t)))

(-> terminal--clip-spans (list integer) list)
(defun terminal--clip-spans (spans maximum-width)
  "Fit semantic SPANS to one terminal row."
  (termdown:fit-spans spans maximum-width))

(-> terminal--pad-spans (list (integer 0) &key (:style terminal-style)) list)
(defun terminal--pad-spans (spans width &key (style ':plain))
  "Return SPANS clipped or padded with STYLE to exactly WIDTH cells."
  (let* ((clipped   (terminal--clip-spans spans width))
         (remaining (- width (terminal--spans-width clipped))))
    (if (plusp remaining)
        (append clipped
                (list (terminal-span style
                                     (make-string remaining :initial-element #\Space))))
        clipped)))


;;;; -- Markdown Heading Styles --

(-> terminal--markdown-heading-p (string) boolean)
(defun terminal--markdown-heading-p (line)
  "Return true when LINE begins an ATX heading of level one through six."
  (let* ((start (or (position-if-not (lambda (character) (char= character #\Space))
                                    line)
                    (length line)))
         (end   (or (position-if-not (lambda (character) (char= character #\#))
                                    line :start start)
                    (length line))))
    (and (<= start 3)
         (<= 1 (- end start) 6)
         (or (= end (length line))
             (not (null (find (char line end) '(#\Space #\Tab))))))))

(-> terminal-markdown-render-line (termdown:markdown-renderer string) list)
(defun terminal-markdown-render-line (renderer line)
  "Render LINE through RENDERER with theme colors for Markdown headings.

Color heading spans while retaining widget labels and actions."
  ;; The pinned renderer exposes fence state only through this internal accessor.
  (let* ((heading-p (and (not (termdown::markdown-renderer-code-open-p renderer))
                         (terminal--markdown-heading-p line)))
         (rows      (termdown:markdown-render-line renderer line)))
    (if heading-p
        (loop for row in rows
              collect (loop for span in row
                            collect (if (termdown:widget-p span)
                                        span
                                        (terminal-span
                                         (case (terminal-span-style span)
                                           ((:plain :strong)
                                            ':syntax-heading)
                                           (:emphasis
                                            ':heading-emphasis)
                                           (otherwise
                                            (terminal-span-style span)))
                                         (terminal-span-text span)))))
        rows)))


;;;; -- Conditions --

(define-condition terminal-theme-error (autolith-error)
  ((name
    :initarg       :name
    :reader        terminal-theme-error-name
    :type          keyword
    :documentation "The requested theme name that names no installed theme."))
  (:documentation "A requested terminal presentation theme does not exist."))
