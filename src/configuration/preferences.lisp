(in-package #:autolith)

;;;; -- Global Preferences --

(defparameter *preferences-version* 6
  "The readable global preferences file format version.")

(defclass preference-state ()
  ((model
    :initarg :model
    :initform nil
    :reader preference-state-model
    :type (option non-empty-string)
    :documentation "The last interactively selected provider model, if any.")
   (reasoning-effort
    :initarg :reasoning-effort
    :initform nil
    :reader preference-state-reasoning-effort
    :type (option non-empty-string)
    :documentation "The last interactively selected reasoning effort, if any.")
   (codex-fast-mode-p
    :initarg :codex-fast-mode-p
    :initform nil
    :reader preference-state-codex-fast-mode-p
    :type boolean
    :documentation "Whether Codex Fast mode is enabled for future requests.")
   (reasoning-traces-p
    :initarg :reasoning-traces-p
    :initform nil
    :reader preference-state-reasoning-traces-p
    :type boolean
    :documentation "Whether provider reasoning summaries are requested and shown.")
   (compact-view-p
    :initarg :compact-view-p
    :initform t
    :reader preference-state-compact-view-p
    :type boolean
    :documentation
    "Whether verbose tool calls are condensed and successful routine results hidden.")
   (turn-timestamps-p
    :initarg :turn-timestamps-p
    :initform nil
    :reader preference-state-turn-timestamps-p
    :type boolean
    :documentation "Whether transcript turn headers include local timestamps.")
   (cache-miss-notices-p
    :initarg :cache-miss-notices-p
    :initform nil
    :reader preference-state-cache-miss-notices-p
    :type boolean
    :documentation
    "Whether provider requests that re-read uncached context are reported.")
   (simple-technical-english-p
    :initarg :simple-technical-english-p
    :initform nil
    :reader preference-state-simple-technical-english-p
    :type boolean
    :documentation "Whether natural-language replies use Simple Technical English.")
   (session-title-generation-p
    :initarg :session-title-generation-p
    :initform t
    :reader preference-state-session-title-generation-p
    :type boolean
    :documentation
    "Whether the provider may refresh locally derived session titles.")
   (permission-mode
    :initarg :permission-mode
    :initform nil
    :reader preference-state-permission-mode
    :type (option (member :ask :auto))
    :documentation
    "The last saved durable command-permission mode, or NIL when unset."))
   (:documentation "Validated global choices restored across Autolith processes."))

(defparameter *preferences-record-fields*
  (list (list :indicator ':reasoning-traces-p
              :validate (lambda (value) (typep value 'boolean))
              :required t)
        (list :indicator ':model
              :validate (lambda (value)
                          (or (null value) (non-empty-string-p value)))
              :required '(2 3 4 5 6))
        ;; Model-specific effort names may come from executable user
        ;; initialization. Validate them when applying preferences to the
        ;; active model.
        (list :indicator ':reasoning-effort
              :validate (lambda (value)
                          (or (null value) (non-empty-string-p value)))
              :required '(2 3 4 5 6))
        (list :indicator ':compact-view-p
              :validate (lambda (value) (typep value 'boolean))
              :required '(3 4 5 6))
        (list :indicator ':turn-timestamps-p
              :validate (lambda (value) (typep value 'boolean)))
        (list :indicator ':cache-miss-notices-p
              :validate (lambda (value) (typep value 'boolean))
              :required '(6))
        (list :indicator ':simple-technical-english-p
              :validate (lambda (value) (typep value 'boolean)))
        (list :indicator ':codex-fast-mode-p
              :validate (lambda (value) (typep value 'boolean))
              :required '(5 6))
        (list :indicator ':session-title-generation-p
              :validate (lambda (value) (typep value 'boolean))
              :required '(4 5 6))
        (list :indicator ':permission-mode
              :validate (lambda (value)
                          (member value '(nil :ask :auto) :test #'eq))))
  "Versioned field descriptions for the durable preferences record.")

(-> preferences--form-p (t) boolean)
(defun preferences--form-p (form)
  "Return true when FORM is one complete supported preferences record."
  (values (record-check form
                        :tag ':preferences
                        :versions '(1 2 3 4 5 6)
                        :fields *preferences-record-fields*)))

(-> preferences--form->state (list) preference-state)
(defun preferences--form->state (form)
  "Return the validated preference state represented by FORM."
  (let ((properties (rest form)))
      (make-instance 'preference-state
                     :model (getf properties :model)
                     :reasoning-effort (getf properties :reasoning-effort)
                     :codex-fast-mode-p
                     (getf properties :codex-fast-mode-p nil)
                     :reasoning-traces-p
                     (getf properties :reasoning-traces-p)
                     :compact-view-p (getf properties :compact-view-p t)
                     :turn-timestamps-p
                     (getf properties :turn-timestamps-p nil)
                     :cache-miss-notices-p
                     (getf properties :cache-miss-notices-p nil)
                     :simple-technical-english-p
                     (getf properties :simple-technical-english-p nil)
                     :session-title-generation-p
                     (getf properties :session-title-generation-p t)
                     :permission-mode
                     (getf properties :permission-mode))))

  (-> preferences--state-form (preference-state) list)
  (defun preferences--state-form (preferences)
    "Return the backward-compatible durable record for PREFERENCES."
    (list :preferences
          :version *preferences-version*
          :model (preference-state-model preferences)
          :reasoning-effort
          (preference-state-reasoning-effort preferences)
          :codex-fast-mode-p
          (preference-state-codex-fast-mode-p preferences)
          :reasoning-traces-p
          (preference-state-reasoning-traces-p preferences)
          :compact-view-p
          (preference-state-compact-view-p preferences)
          :turn-timestamps-p
          (preference-state-turn-timestamps-p preferences)
          :cache-miss-notices-p
          (preference-state-cache-miss-notices-p preferences)
          :simple-technical-english-p
          (preference-state-simple-technical-english-p preferences)
          :session-title-generation-p
          (preference-state-session-title-generation-p preferences)
          :permission-mode
          (preference-state-permission-mode preferences)))

(-> preferences--file-signature (pathname) (values t boolean))
(defun preferences--file-signature (pathname)
  "Return a cheap change signature for PATHNAME and whether it is usable."
  (handler-case
      (if (probe-file pathname)
          (with-open-file (stream pathname
                                  :direction ':input
                                  :element-type '(unsigned-byte 8))
            (values (list (file-write-date pathname)
                          (file-length stream))
                    t))
          (values nil t))
    (error ()
      (values nil nil))))

(-> preferences--read
    (configuration)
    (values preference-state integer))
(defun preferences--read (configuration)
  "Read CONFIGURATION's preference state and its durable format version."
  (block nil
    (let ((pathname (configuration-preferences-path configuration)))
      (unless (probe-file pathname)
        (return (values (make-instance 'preference-state)
                        *preferences-version*)))
      (handler-case
          (multiple-value-bind (form sole-form-p)
              (snapshot-read pathname)
            (unless (and sole-form-p (preferences--form-p form))
              (error 'preferences-error
                     :message (format nil
                                      "Preferences at ~A are malformed or unsupported."
                                      pathname)
                     :pathname pathname
                     :operation ':read
                     :cause nil))
            (values (preferences--form->state form)
                    (getf (rest form) :version)))
        (preferences-error (condition)
          (error condition))
        (error (cause)
          (error 'preferences-error
                 :message (format nil "Could not read preferences at ~A: ~A"
                                  pathname
                                  cause)
                 :pathname pathname
                 :operation ':read
                 :cause cause))))))

(-> preferences--load-uncached (configuration) preference-state)
(defun preferences--load-uncached (configuration)
  "Return freshly validated preferences, warning and defaulting after corruption."
  (handler-case
      (multiple-value-bind (preferences version)
          (preferences--read configuration)
        (when (/= version *preferences-version*)
          (ignore-errors
            (snapshot-write
             (configuration-preferences-path configuration)
             (preferences--state-form preferences))))
        preferences)
    (preferences-error (condition)
      (warn 'preferences-load-warning
            :pathname (preferences-error-pathname condition)
            :cause condition)
      (make-instance 'preference-state))))

(-> preferences-load (configuration) preference-state)
(defun preferences-load (configuration)
  "Return cached validated preferences, reloading them after the file changes."
  (with-lock-held ((configuration-preferences-lock configuration))
    (multiple-value-bind (signature signature-p)
        (preferences--file-signature
         (configuration-preferences-path configuration))
      (let ((cache (configuration-preferences-cache configuration)))
        (if (and signature-p
                 cache
                 (equal signature (first cache)))
            (second cache)
            (let ((preferences (preferences--load-uncached configuration)))
              (multiple-value-bind (current-signature current-signature-p)
                  (preferences--file-signature
                   (configuration-preferences-path configuration))
                (setf (configuration-preferences-cache configuration)
                      (and current-signature-p
                           (list current-signature preferences))))
              preferences))))))

(-> preferences-reasoning-traces-p (configuration) boolean)
(defun preferences-reasoning-traces-p (configuration)
  "Return the persisted reasoning-summary setting, defaulting safely to false."
  (preference-state-reasoning-traces-p (preferences-load configuration)))

(-> preferences-compact-view-p (configuration) boolean)
(defun preferences-compact-view-p (configuration)
  "Return the persisted compact tool-presentation setting, defaulting to true."
  (preference-state-compact-view-p (preferences-load configuration)))

(-> preferences-turn-timestamps-p (configuration) boolean)
(defun preferences-turn-timestamps-p (configuration)
  "Return the persisted turn-timestamp setting, defaulting to false."
  (preference-state-turn-timestamps-p (preferences-load configuration)))

(-> preferences-cache-miss-notices-p (configuration) boolean)
(defun preferences-cache-miss-notices-p (configuration)
  "Return the persisted prompt-cache miss notice setting, defaulting to false."
  (preference-state-cache-miss-notices-p (preferences-load configuration)))

(-> preferences-simple-technical-english-p (configuration) boolean)
(defun preferences-simple-technical-english-p (configuration)
  "Return the persisted Simple Technical English setting, defaulting to false."
  (preference-state-simple-technical-english-p
   (preferences-load configuration)))

(-> preferences-session-title-generation-p (configuration) boolean)
(defun preferences-session-title-generation-p (configuration)
  "Return whether provider-generated session-title refreshes are enabled."
  (preference-state-session-title-generation-p
   (preferences-load configuration)))

(-> preferences-permission-mode (configuration) (option (member :ask :auto)))
(defun preferences-permission-mode (configuration)
  "Return the persisted durable command-permission mode, or NIL when unset."
  (preference-state-permission-mode (preferences-load configuration)))

(-> preferences-apply-model-selection (configuration) configuration)
(defun preferences-apply-model-selection (configuration)
  "Apply saved model, effort, and Codex Fast mode choices when they remain valid.

A saved model that no effective provider registration serves is dropped rather
than applied, so removing a provider cannot leave the configuration naming a
model no provider can serve."
  (let* ((preferences (preferences-load configuration))
         (saved-model (preference-state-model preferences))
         (saved-effort (preference-state-reasoning-effort preferences))
         (saved-codex-fast-mode-p
           (preference-state-codex-fast-mode-p preferences))
         (selected configuration))
    (when (and saved-model
               (not (non-empty-string-p (uiop:getenv "AUTOLITH_MODEL")))
               (configuration--model-supported-p saved-model))
      (setf selected (configuration-with-model selected saved-model)))
    (when (and saved-effort
               (not (non-empty-string-p
                     (uiop:getenv "AUTOLITH_REASONING_EFFORT")))
               (member saved-effort
                       (configuration--reasoning-efforts-for
                        (configuration-model selected))
                       :test #'string=))
      (setf selected
            (configuration-with-reasoning-effort selected saved-effort)))
    (unless (non-empty-string-p (uiop:getenv "AUTOLITH_CODEX_FAST_MODE"))
      (setf selected
            (configuration-with-codex-fast-mode
             selected
             saved-codex-fast-mode-p)))
    selected))

(-> preferences--write (configuration preference-state) null)
(defun preferences--write (configuration preferences)
  "Atomically write and cache PREFERENCES under CONFIGURATION's state root."
  (let ((pathname (configuration-preferences-path configuration)))
    (with-lock-held ((configuration-preferences-lock configuration))
      (handler-case
          (progn
            (snapshot-write pathname (preferences--state-form preferences))
            (multiple-value-bind (signature signature-p)
                (preferences--file-signature pathname)
              (setf (configuration-preferences-cache configuration)
                    (and signature-p (list signature preferences)))))
        (error (cause)
          (error 'preferences-error
                 :message (format nil "Could not persist preferences at ~A: ~A"
                                  pathname
                                  cause)
                 :pathname pathname
                 :operation ':write
                 :cause cause)))))
  nil)

(-> preferences--copy
    (preference-state
     &key
     (:model (option non-empty-string))
     (:reasoning-effort (option non-empty-string))
     (:codex-fast-mode-p boolean)
     (:reasoning-traces-p boolean)
     (:compact-view-p boolean)
     (:turn-timestamps-p boolean)
     (:cache-miss-notices-p boolean)
     (:simple-technical-english-p boolean)
     (:session-title-generation-p boolean)
     (:permission-mode (option (member :ask :auto))))
    preference-state)
(defun preferences--copy
    (previous &key (model (preference-state-model previous))
                   (reasoning-effort (preference-state-reasoning-effort previous))
                   (codex-fast-mode-p
                    (preference-state-codex-fast-mode-p previous))
                   (reasoning-traces-p
                    (preference-state-reasoning-traces-p previous))
                   (compact-view-p (preference-state-compact-view-p previous))
                   (turn-timestamps-p
                    (preference-state-turn-timestamps-p previous))
                   (cache-miss-notices-p
                    (preference-state-cache-miss-notices-p previous))
                   (simple-technical-english-p
                    (preference-state-simple-technical-english-p previous))
                   (session-title-generation-p
                    (preference-state-session-title-generation-p previous))
                   (permission-mode (preference-state-permission-mode previous)))
  "Return a replacement preference state based on PREVIOUS."
  (make-instance 'preference-state
                 :model model
                 :reasoning-effort reasoning-effort
                 :codex-fast-mode-p codex-fast-mode-p
                 :reasoning-traces-p reasoning-traces-p
                 :compact-view-p compact-view-p
                 :turn-timestamps-p turn-timestamps-p
                 :cache-miss-notices-p cache-miss-notices-p
                 :simple-technical-english-p simple-technical-english-p
                 :session-title-generation-p session-title-generation-p
                 :permission-mode permission-mode))

(-> preferences-set-model-selection (configuration) null)
(defun preferences-set-model-selection (configuration)
  "Persist CONFIGURATION's model and reasoning effort as global defaults."
  (preferences--write
   configuration
   (preferences--copy
    (preferences-load configuration)
    :model (configuration-model configuration)
    :reasoning-effort (configuration-reasoning-effort configuration)))
  nil)

(-> preferences-set-codex-fast-mode (configuration boolean) null)
(defun preferences-set-codex-fast-mode (configuration enabled-p)
  "Atomically persist Codex Fast mode without discarding other global choices."
  (preferences--write
   configuration
   (preferences--copy (preferences-load configuration)
                      :codex-fast-mode-p enabled-p))
  nil)

(-> preferences-set-reasoning-traces (configuration boolean) null)
(defun preferences-set-reasoning-traces (configuration enabled-p)
  "Atomically persist ENABLED-P without discarding saved model choices."
  (preferences--write
   configuration
   (preferences--copy (preferences-load configuration)
                      :reasoning-traces-p enabled-p))
  nil)

(-> preferences-set-compact-view (configuration boolean) null)
(defun preferences-set-compact-view (configuration enabled-p)
  "Atomically persist ENABLED-P without discarding other global choices."
  (preferences--write
   configuration
   (preferences--copy (preferences-load configuration)
                      :compact-view-p enabled-p))
  nil)

(-> preferences-set-turn-timestamps (configuration boolean) null)
(defun preferences-set-turn-timestamps (configuration enabled-p)
  "Atomically persist ENABLED-P without discarding other global choices."
  (preferences--write
   configuration
   (preferences--copy (preferences-load configuration)
                      :turn-timestamps-p enabled-p))
  nil)

(-> preferences-set-cache-miss-notices (configuration boolean) null)
(defun preferences-set-cache-miss-notices (configuration enabled-p)
  "Atomically persist ENABLED-P without discarding other global choices."
  (preferences--write
   configuration
   (preferences--copy (preferences-load configuration)
                      :cache-miss-notices-p enabled-p))
  nil)

(-> preferences-set-simple-technical-english (configuration boolean) null)
(defun preferences-set-simple-technical-english (configuration enabled-p)
  "Atomically persist ENABLED-P without discarding other global choices."
  (preferences--write
   configuration
   (preferences--copy (preferences-load configuration)
                      :simple-technical-english-p enabled-p))
  nil)

(-> preferences-set-session-title-generation (configuration boolean) null)
(defun preferences-set-session-title-generation (configuration enabled-p)
  "Persist whether providers may refresh locally derived session titles."
  (preferences--write
   configuration
   (preferences--copy (preferences-load configuration)
                      :session-title-generation-p enabled-p))
  nil)

(-> preferences-set-permission-mode
    (configuration (option (member :ask :auto)))
    null)
(defun preferences-set-permission-mode (configuration mode)
  "Persist MODE as the durable command-permission choice."
  (unless (or (null mode) (member mode '(:ask :auto) :test #'eq))
    (error 'preferences-error
           :message "Only ask and auto command-permission modes can be saved."
           :pathname (configuration-preferences-path configuration)
           :operation ':validate
           :cause nil))
  (preferences--write
   configuration
   (preferences--copy (preferences-load configuration)
                      :permission-mode mode))
  nil)
