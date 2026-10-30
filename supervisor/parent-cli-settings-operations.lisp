;;;; -- Owner Settings Operations --

(in-package #:autolith-supervisor)

(serapeum/types:-> parent-cli-public-setting-key (string) keyword)
(defun parent-cli-public-setting-key (name)
  "Resolve one public setting without interning agent-controlled symbols."
  (or (loop for (key) on *parent-cli-setting-defaults* by (lambda (tail) (rest (rest tail)))
            when (string-equal name (symbol-name key)) return key)
      (parent-cli--reject ':unknown-public-setting)))

(serapeum/types:-> parent-cli-public-setting-value-p (keyword t) boolean)
(defun parent-cli-public-setting-value-p (key value)
  "Validate inert public values. Paths and executable extension settings are excluded."
  (not
   (null
    (case key
      ((:reasoning-traces-p :compact-view-p :turn-timestamps-p :cache-miss-notices-p
        :diagnosis-p :idle-compaction-p :simple-technical-english-p :session-title-generation-p
        :hurry-up-p :relax-p :fullscreen-p :onboarding-p :boot-screen-p :boot-screen-linger-p
        :codex-fast-mode-p :prefer-codex-context-window-p)
       (member value '(nil t)))
      (:compaction-threshold-percent
       (typep value '(integer 1 95)))
      (:prompt-cache-retention-seconds
       (typep value '(integer 0 86400)))
      (:boot-screen-seconds
       (and (realp value) (<= 0 value 3600)))
      (:boot-screen-tip-seconds
       (typep value '(integer 0 86400)))
      (:knowledge-mode
       (member value '("active" "passive" "off") :test #'equal))
      (:web-search-mode
       (member value '("disabled" "cached" "live") :test #'equal))
      (:terminal-theme
       (member value '(:autolith :almighty)))
      ((:model-provider :model :reasoning-effort :reasoning-summary :openrouter-provider-routing)
       (and (stringp value) (<= (length value) 1024)
            (every (lambda (character) (<= 32 (char-code character) 126)) value)))
      (otherwise
       nil)))))

(serapeum/types:-> parent-cli-public-settings (parent-configuration) hash-table)
(defun parent-cli-public-settings (configuration)
  "Return only approved public settings in a fresh map."
  (let ((result   (make-hash-table :test #'equal))
        (settings (parent-configuration-get configuration ':settings)))
    (loop for (key default) on *parent-cli-setting-defaults* by (lambda (tail) (rest (rest tail)))
          unless (member key '(:immutable-p :pristine-p :permission-mode))
            do (let ((value (getf settings key default)))
                 (setf (gethash (string-downcase key) result)
                       (cond
                         ((keywordp value)
                          (string-downcase value))
                         ((and (realp value) (not (integerp value)))
                          (coerce value 'double-float))
                         (t
                          value)))))
    result))

(serapeum/types:-> parent-cli--preferences-form-p (t) boolean)
(defun parent-cli--preferences-form-p (form)
  "Check the version 8 snapshot shape before a private transactional update."
  (and (parent-configuration--data-p form)
       (consp form) (eq (first form) ':preferences)
       (let ((values (rest form)))
         (and (list-length values) (evenp (length values))
              (loop for (key) on values by (lambda (tail) (rest (rest tail))) always (keywordp key))
              (eql (getf values :version) 8))) t))

(serapeum/types:-> parent-cli--decode-preferences (list) list)
(defun parent-cli--decode-preferences (form)
  "Keep first-key semantics and preserve unknown owner settings during a write."
  (loop with seen = nil
        for (key value) on (rest form) by (lambda (tail) (rest (rest tail)))
        unless (member key seen)
          do (push key seen)
          and append (unless (eq key ':version) (list key value))))

(serapeum/types:-> parent-cli--encode-preferences (list) list)
(defun parent-cli--encode-preferences (values)
  "Encode a complete version 8 owner preference snapshot."
  (list* ':preferences ':version 8 values))

(serapeum/types:-> parent-cli-store-settings (parent-configuration list &key (:persistent-p boolean)) null)
(defun parent-cli-store-settings (configuration changes &key (persistent-p t))
  "Validate public CHANGES, atomically persist them, then replace in-memory owner values.
Callers serialize application changes with the application lock. This function never
accepts runtime paths, provider extension files, or credentials from the child."
  (when (getf (parent-configuration-get configuration ':settings) :immutable-p)
    (parent-cli--reject ':immutable-configuration))
  (unless (and (parent-configuration--data-p changes) (listp changes)
               (list-length changes) (evenp (length changes)))
    (parent-cli--reject ':invalid-setting-changes))
  (loop for (key value) on changes by (lambda (tail) (rest (rest tail)))
        unless (parent-cli-public-setting-value-p key value)
          do (parent-cli--reject ':invalid-public-setting-value))
  (let* ((values   (copy-tree (parent-configuration-values configuration)))
         (settings (copy-tree (getf values :settings)))
         (path     (parent-configuration-get configuration ':preferences-path)))
    (loop for (key value) on changes by (lambda (tail) (rest (rest tail)))
          do (setf (getf settings key) value))
    (setf (getf values :settings) settings)
    (when (member :knowledge-mode changes)
      (setf (getf values :knowledge-mode) (getf changes :knowledge-mode)))
    (when (member :reasoning-effort changes)
      (setf (getf values :reasoning-effort) (getf changes :reasoning-effort)))
    (let ((validated (make-parent-configuration values)))
      (when persistent-p
        (unless path (parent-cli--reject ':missing-preferences-path))
        (parent-cli-read-preferences path)
        (ensure-directories-exist path)
        (sexp-store:store-transact
         (make-instance 'sexp-store:snapshot-store
                        :pathname       path
                        :lock-pathname  (merge-pathnames "preferences.lock"
                                                          (uiop:pathname-directory-pathname (pathname path)))
                        :initial-state #'list :validator #'parent-cli--preferences-form-p
                        :decoder #'parent-cli--decode-preferences :encoder #'parent-cli--encode-preferences
                        :duplicate-keys ':first)
         (lambda (current)
           (let ((replacement (copy-list current)))
             (loop for (key value) on changes by (lambda (tail) (rest (rest tail)))
                   do (setf (getf replacement key) value))
             (values replacement nil t)))))
      (setf (slot-value configuration 'values) (parent-configuration-values validated))))
  nil)
