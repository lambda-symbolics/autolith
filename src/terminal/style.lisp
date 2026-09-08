(in-package #:autolith)

;;;; -- Semantic Terminal Styles --

(deftype terminal-style ()
  "A semantic terminal style resolved to color and emphasis by the renderer."
  '(member :plain :brand
           :brand-gradient-1 :brand-gradient-2 :brand-gradient-3
           :brand-gradient-4 :brand-gradient-5 :brand-gradient-6
           :recovery-gradient-1 :recovery-gradient-2 :recovery-gradient-3
           :recovery-gradient-4 :recovery-gradient-5 :recovery-gradient-6
           :user :tool :success :failure :notice :dim :hint :selected
           :strong :emphasis :code :lisp-prompt :plan-active :timestamp-time
           :agent-spinner :agent-name :child-name :agent-role :agent-tool
           :command-spinner :command-id :command-tool
           :status-plain :status-dim :status-accent
           :status-model :status-effort :status-branch
           :compaction-label :compaction-track :compaction-head
           :syntax-comment :syntax-keyword :syntax-string :syntax-escape
           :syntax-number :syntax-type :syntax-function :syntax-property
           :syntax-heading :syntax-link))

(defparameter *terminal-style-table*
  ;; General interface styles use the basic ANSI palette so Autolith follows
  ;; the terminal theme. Only the startup mark, promoted child name, and status
  ;; background opt into indexed colors.
  (append
   (loop for (name arguments) in
         '((:plain ())
           (:brand (:foreground :magenta :bold t))
           (:user (:foreground :cyan :bold t))
           (:tool (:foreground :yellow :bold t))
           (:success (:foreground :green))
           (:failure (:foreground :red :bold t))
           (:notice (:foreground :yellow))
           (:dim (:faint t))
           (:hint (:faint t :italic t))
           (:selected (:reverse t))
           (:strong (:bold t))
           (:emphasis (:italic t))
           (:code (:foreground :cyan))
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
           (:syntax-comment (:faint t))
           (:syntax-keyword (:foreground :magenta))
           (:syntax-string (:foreground :green))
           (:syntax-escape (:foreground :yellow))
           (:syntax-number (:foreground :yellow))
           (:syntax-type (:foreground :cyan))
           (:syntax-function (:foreground :blue))
           (:syntax-property (:foreground :cyan))
           (:syntax-heading (:foreground :magenta :bold t))
           (:syntax-link (:foreground :cyan :underline t)))
         collect (cons name (apply #'make-style arguments)))
   (list
    (cons ':child-name
          (make-style
           :foreground (indexed-color 78 :fallback ':green))))
   (loop for name in '(:brand-gradient-1 :brand-gradient-2 :brand-gradient-3
                       :brand-gradient-4 :brand-gradient-5 :brand-gradient-6)
         for index in '(193 157 121 85 84 83)
         collect (cons name (make-style
                             :foreground (indexed-color index :fallback ':green)
                             :bold t)))
   (loop for name in '(:recovery-gradient-1 :recovery-gradient-2
                       :recovery-gradient-3 :recovery-gradient-4
                       :recovery-gradient-5 :recovery-gradient-6)
         for index in '(224 217 210 203 197 196)
         collect (cons name (make-style
                             :foreground (indexed-color index :fallback ':red)
                             :bold t)))
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
                              arguments))))
  "Colorist style objects for Autolith's semantic styles.")

(defparameter *terminal-style-reset*
  (reset-sequence :level ':basic)
  "The trusted control that restores default terminal rendition.")

(-> terminal-style-reset-sequence () string)
(defun terminal-style-reset-sequence ()
  "Return the trusted control that restores default terminal rendition."
  *terminal-style-reset*)

(-> terminal-environment-indexed-color-p () boolean)
(defun terminal-environment-indexed-color-p ()
  "Return true when the process environment advertises indexed colors."
  (eq (effective-color-level) ':indexed))

(-> terminal-style-sequence
    (terminal-style &optional boolean)
    (option string))
(defun terminal-style-sequence
    (style &optional (indexed-color-p (terminal-environment-indexed-color-p)))
  "Return STYLE's trusted control, using INDEXED-COLOR-P for brand gradients."
  (let ((sequence
          (sgr-sequence (rest (assoc style *terminal-style-table*))
                        :level (if indexed-color-p ':indexed ':basic))))
    (and (plusp (length sequence)) sequence)))

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

(-> terminal-span-style (cons) terminal-style)
(defun terminal-span-style (span)
  "Return SPAN's semantic style."
  (first span))

(-> terminal-span-text (cons) string)
(defun terminal-span-text (span)
  "Return SPAN's untrusted text."
  (rest span))

(-> terminal-styled-text-p (t) boolean)
(defun terminal-styled-text-p (value)
  "Return true when VALUE is a proper list of styled spans."
  (loop for tail = value then (rest tail)
        while tail
        always (and (consp tail)
                    (terminal-span-p (first tail)))))

(deftype terminal-styled-text ()
  "A proper list of styled spans rendered in order."
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
