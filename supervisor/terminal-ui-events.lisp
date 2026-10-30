;;;; -- Inert Terminal Presentation Events --

(in-package #:autolith-supervisor)

(defparameter *terminal-ui-event-character-limit* 262144
  "Maximum text characters in one semantic presentation event.")

(defparameter *terminal-ui-event-node-limit* 32768
  "Maximum inert value nodes in one semantic presentation event.")

(serapeum/types:-> terminal-ui--reject-event () nil)
(defun terminal-ui--reject-event ()
  "Reject a malformed or excessive semantic event before changing the UI."
  (error 'parent-terminal-error :reason ':invalid-semantic-event))

(serapeum/types:-> terminal-ui-portable-value-p (t) boolean)
(defun terminal-ui-portable-value-p (value)
  "Accept bounded inert values without functions, pathnames or package symbols."
  (let ((nodes 0) (characters 0) (seen (make-hash-table :test #'eq)))
    (labels ((visit (item depth)
               (when (or (> (incf nodes) *terminal-ui-event-node-limit*) (> depth 16))
                 (return-from visit nil))
               (typecase item
                 (null
                  t)
                 (string
                  (<= (incf characters (length item)) *terminal-ui-event-character-limit*))
                 (symbol
                  (or (eq item t) (keywordp item)))
                 (integer
                  (<= (- (expt 2 63)) item (1- (expt 2 63))))
                 (real
                  (and (= item item) (<= (- most-positive-single-float)
                                         item most-positive-single-float)))
                 (cons
                  (when (gethash item seen) (return-from visit nil))
                  (setf (gethash item seen) t)
                  (and (visit (first item) (1+ depth))
                       (visit (rest item) depth)))
                 (t
                  nil))))
      (handler-case (not (null (visit value 0)))
        (error () nil)))))

(serapeum/types:-> terminal-ui--event-fields-p (t list list) boolean)
(defun terminal-ui--event-fields-p (value allowed required)
  "Check a proper, duplicate-free plist against fixed ALLOWED and REQUIRED keys."
  (handler-case
      (and (listp value) (evenp (length value))
           (loop with seen = nil
                 for key in value by (lambda (tail) (rest (rest tail)))
                 always (and (member key allowed) (not (member key seen))
                             (progn (push key seen) t))
                 finally (return (every (lambda (key) (member key seen)) required))))
    (error () nil)))

(serapeum/types:-> terminal-ui--portable-action-p (t) boolean)
(defun terminal-ui--portable-action-p (action)
  "Accept only inert copy and HTTP browser actions from transcript widgets."
  (and (listp action) (= (length action) 2) (stringp (second action))
       (case (first action)
         (:copy
          t)
         (:open-url
          (or (uiop:string-prefix-p "https://" (second action))
              (uiop:string-prefix-p "http://" (second action))))
         (otherwise
          nil))))

(serapeum/types:-> terminal-ui-content->portable ((or string list)) list)
(defun terminal-ui-content->portable (content)
  "Encode current renderer spans and widgets as bounded inert descriptors."
  (let ((result
          (mapcar
           (lambda (entry)
             (cond
               ((terminal-span-p entry)
                (list :kind ':span :style (terminal-span-style entry)
                      :text (copy-seq (terminal-span-text entry))))
               ((and (termdown:widget-p entry)
                     (typep (termdown:widget-role entry) 'terminal-style)
                     (terminal-ui--portable-action-p (termdown:widget-action entry)))
                (list :kind ':widget :role (termdown:widget-role entry)
                      :label  (copy-seq (termdown:widget-label entry))
                      :action (copy-tree (termdown:widget-action entry))))
               (t
                (terminal-ui--reject-event))))
           (if (stringp content) (list (terminal-span ':plain content)) content))))
    (unless (terminal-ui-portable-value-p result) (terminal-ui--reject-event))
    result))

(serapeum/types:-> terminal-ui-portable->content (list) list)
(defun terminal-ui-portable->content (content)
  "Validate and rebuild spans and fixed widget actions without evaluating data."
  (unless (and (listp content) (terminal-ui-portable-value-p content))
    (terminal-ui--reject-event))
  (mapcar
   (lambda (entry)
     (case (and (listp entry) (getf entry :kind))
       (:span
        (unless (and (terminal-ui--event-fields-p entry '(:kind :style :text)
                                                  '(:kind :style :text))
                     (typep (getf entry :style) 'terminal-style)
                     (stringp (getf entry :text)))
          (terminal-ui--reject-event))
        (terminal-span (getf entry :style) (copy-seq (getf entry :text))))
       (:widget
        (unless (and (terminal-ui--event-fields-p entry '(:kind :role :label :action)
                                                  '(:kind :role :label :action))
                     (typep (getf entry :role) 'terminal-style)
                     (stringp (getf entry :label))
                     (terminal-ui--portable-action-p (getf entry :action)))
          (terminal-ui--reject-event))
        (termdown:make-widget (getf entry :role) (copy-seq (getf entry :label))
                             (copy-tree (getf entry :action))))
       (otherwise
        (terminal-ui--reject-event))))
   content))

(serapeum/types:-> terminal-ui-event-valid-p (keyword t) boolean)
(defun terminal-ui-event-valid-p (kind payload)
  "Check the fixed semantic event grammar without changing presentation state."
  (handler-case
      (and
       (terminal-ui-portable-value-p payload)
       (case kind
         (:stream
           (and (terminal-ui--event-fields-p payload '(:rows :tail :tail-rows) nil)
                (listp (getf payload :rows))
                (every (lambda (row) (terminal-ui-portable->content row) t)
                       (getf payload :rows))
                (typep (getf payload :tail) '(or null string))
                (listp (getf payload :tail-rows))
                (every (lambda (row) (terminal-ui-portable->content row) t)
                       (getf payload :tail-rows))))
         ((:finalized :epilogue)
          (and (terminal-ui--event-fields-p payload '(:id :content) '(:id :content))
               (terminal-ui-non-empty-string-p (getf payload :id))
               (<= (length (getf payload :id)) 256)
               (progn (terminal-ui-portable->content (getf payload :content)) t)))
         (:preview
          (and (terminal-ui--event-fields-p payload '(:rows) '(:rows))
               (listp (getf payload :rows))
               (every (lambda (row) (terminal-ui-portable->content row) t)
                      (getf payload :rows))))
         (:status
          (and (terminal-ui--event-fields-p payload '(:status :details :worked-seconds)
                                             '(:status))
               (typep (getf payload :status) '(or null string))
               (progn (terminal-ui-portable->content (getf payload :details)) t)
               (typep (getf payload :worked-seconds) '(or null (integer 0)))))
         (:context
          (and (terminal-ui--event-fields-p payload
                                             '(:used :window :compaction-limit :estimated-p)
                                             '(:used :window :compaction-limit))
               (typep (getf payload :used) '(integer 0))
               (typep (getf payload :window) '(integer 1))
               (typep (getf payload :compaction-limit) '(integer 1))
               (<= (getf payload :compaction-limit) (getf payload :window))
               (typep (getf payload :estimated-p) 'boolean)))
         (:notice
          (and (terminal-ui--event-fields-p payload '(:text :duration-seconds) '(:text))
               (typep (getf payload :text) '(or null string))
               (typep (getf payload :duration-seconds) '(or null (real 0)))))
         (:agent-activities
          (and (terminal-ui--event-fields-p payload '(:activities) '(:activities))
               (listp (getf payload :activities))
               (every #'terminal-agent-activity-p (getf payload :activities))))
         (:command-activities
          (and (terminal-ui--event-fields-p payload '(:activities) '(:activities))
               (listp (getf payload :activities))
               (every #'terminal-command-activity-p (getf payload :activities))))
         (:pending-inputs
          (and (terminal-ui--event-fields-p payload '(:steering :queued) nil)
               (listp (getf payload :steering)) (listp (getf payload :queued))
               (every #'stringp (getf payload :steering))
               (every #'stringp (getf payload :queued))))
         (:compacting
          (and (terminal-ui--event-fields-p payload '(:active-p) '(:active-p))
               (typep (getf payload :active-p) 'boolean)))
         (:local-activity
          (and (terminal-ui--event-fields-p payload '(:text) '(:text))
               (typep (getf payload :text) '(or null string))))
         (:idle-status
          (and (terminal-ui--event-fields-p payload '(:details) '(:details))
               (progn (terminal-ui-portable->content (getf payload :details)) t)))
         (:reset-transcript
          (null payload))
         (otherwise
          nil))
       t)
    (error () nil)))


;;;; -- Protocol Projection --

(serapeum/types:-> terminal-ui-event->wire (t) t)
(defun terminal-ui-event->wire (value)
  "Encode an inert UI plist as vectors with explicit keyword tags."
  (unless (terminal-ui-portable-value-p value) (terminal-ui--reject-event))
  (labels ((encode (item)
             (typecase item
               (null
                nil)
               (keyword
                (let ((map (make-hash-table :test #'equal)))
                  (setf (gethash "keyword" map) (string-downcase item))
                  map))
               ((or string real)
                item)
               (cons
                (map 'vector #'encode item))
               (symbol
                (if (eq item t) t (terminal-ui--reject-event)))
               (t
                (terminal-ui--reject-event)))))
    (encode value)))
