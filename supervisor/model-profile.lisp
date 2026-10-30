;;;; -- Trusted Model Profiles --

(in-package #:autolith-supervisor)

(define-condition model-projection-error (error)
  ((reason :initarg :reason :reader model-projection-error-reason
           :documentation "A redacted semantic error category.")
   (field :initarg :field :reader model-projection-error-field
          :documentation "The fixed schema field name, never its contents."))
  (:documentation "A model request cannot be projected without loss or excess authority.")
  (:report (lambda (condition stream)
             (format stream "Invalid model request (~a, ~a)."
                     (model-projection-error-field condition)
                     (model-projection-error-reason condition)))))

(declaim (ftype (function (keyword keyword) nil) model-projection--fail))

(defun model-projection--fail (field reason)
  "Signal a redacted semantic failure."
  (error 'model-projection-error :field field :reason reason))

;; Make the class type available when compiling its function declarations.
(eval-when (:compile-toplevel :load-toplevel :execute)
 (defclass model-profile ()
   ((id :initarg :id :reader model-profile-id :type string
        :documentation "The exact parent-installed profile ID.")
    (model :initarg :model :reader model-profile-model :type string
           :documentation "The actual parent-selected model.")
    (family :initarg :family :reader model-profile-family :type keyword
            :documentation "The installed library encoder family.")
    (capabilities :initarg :capabilities :reader model-profile-capabilities :type list
                  :documentation "Explicitly enabled logical capabilities.")
    (options :initarg :options :reader model-profile-options :type list
             :documentation "Trusted library wire options, never agent Value data.")
    (hosted-tools :initarg :hosted-tools :reader model-profile-hosted-tools :type hash-table
                  :documentation "Parent capability IDs mapped to installed declarations.")
    (maximum-output-tokens :initarg       :maximum-output-tokens
                           :reader model-profile-maximum-output-tokens :type (integer 1)
                           :documentation "The allowed output token ceiling.")
    (maximum-request-bytes :initarg       :maximum-request-bytes
                           :reader model-profile-maximum-request-bytes :type (integer 1)
                           :documentation "The serialized logical input byte limit.")
    (reasoning-efforts :initarg :reasoning-efforts :reader model-profile-reasoning-efforts :type list
                       :documentation "Allowed logical reasoning effort strings.")
    (reasoning-summaries :initarg :reasoning-summaries :reader model-profile-reasoning-summaries :type list
                         :documentation "Allowed logical reasoning summary strings.")
    (media-types :initarg :media-types :reader model-profile-media-types :type list
                 :documentation "Allowed inline image MIME types."))
   (:documentation "Parent-installed model policy. Never construct this from agent values.
Keep the registry and profile objects private to the parent. No field selects
an account, endpoint, header, credential, or provider factory from wire data.")))

(declaim (ftype (function (&key (:id string) (:model string) (:family keyword)
                              (:capabilities list) (:options list)
                              (:hosted-tools hash-table)
                              (:maximum-output-tokens (integer 1))
                              (:maximum-request-bytes (integer 1))
                              (:reasoning-efforts list) (:reasoning-summaries list)
                              (:media-types list)) model-profile)
                make-model-profile))

(defun make-model-profile (&key id model family capabilities options
                               (hosted-tools (make-hash-table :test #'equal))
                               (maximum-output-tokens 32000)
                               (maximum-request-bytes (* 16 1024 1024))
                               reasoning-efforts reasoning-summaries
                               (media-types '("image/png" "image/jpeg" "image/gif" "image/webp")))
  "Install trusted model settings, independent of any agent configuration."
  (unless (and (stringp id) (plusp (length id))
               (stringp model) (plusp (length model))
               (member family '(:responses :chat-completions :anthropic :gemini))
               (typep maximum-output-tokens '(integer 1))
               (typep maximum-request-bytes '(integer 1))
               (every #'stringp reasoning-efforts)
               (every #'stringp reasoning-summaries)
               (every #'stringp media-types)
               (evenp (length options))
               (every (lambda (capability)
                        (member capability '(:media :deferred-tools :native-compaction
                                             :continuation :turn-state)))
                      capabilities))
    (model-projection--fail ':profile ':invalid-policy))
  (when (and (or (intersection capabilities '(:deferred-tools :native-compaction))
                 (plusp (hash-table-count hosted-tools)))
             (not (eq family ':responses)))
    (model-projection--fail ':profile ':unsupported-family))
  (make-instance 'model-profile :id (copy-seq id) :model (copy-seq model) :family family
                                :capabilities (copy-list capabilities) :options (copy-list options)
                                :hosted-tools hosted-tools :maximum-output-tokens maximum-output-tokens
                                :maximum-request-bytes maximum-request-bytes
                                :reasoning-efforts     (copy-list reasoning-efforts)
                                :reasoning-summaries   (copy-list reasoning-summaries)
                                :media-types           (copy-list media-types)))

(declaim (ftype (function (list) hash-table) make-model-profile-registry))

(defun make-model-profile-registry (profiles)
  "Build a parent-owned exact, case-sensitive profile lookup table."
  (let ((registry (make-hash-table :test #'equal)))
    (dolist (profile profiles registry)
      (unless (typep profile 'model-profile)
        (model-projection--fail ':profile ':invalid-policy))
      (when (gethash (model-profile-id profile) registry)
        (model-projection--fail ':profile ':duplicate))
      (setf (gethash (model-profile-id profile) registry) profile))))

(declaim (ftype (function (hash-table string) model-profile) model-profile-lookup))

(defun model-profile-lookup (registry id)
  "Select only an installed parent profile; never interpret ID as a path."
  (let ((profile (gethash id registry)))
    (unless (typep profile 'model-profile)
      (model-projection--fail ':profile-id ':unknown-profile))
    profile))

(declaim (ftype (function (model-profile) hash-table) model-profile-reasoning-capabilities))

(defun model-profile-reasoning-capabilities (profile)
  "Project only reasoning settings admitted by this profile and its wire dialect."
  (let* ((family   (model-profile-family profile))
         (effort-p (or (eq family ':responses)
                       (and (eq family ':chat-completions)
                            (getf (model-profile-options profile) :reasoning-parameter))))
         (map      (make-hash-table :test #'equal)))
    (setf (gethash "efforts" map)
          (coerce (when effort-p (model-profile-reasoning-efforts profile)) 'vector)
          (gethash "summaries" map)
          (coerce (when (and effort-p (eq family ':responses)
                             (model-profile-reasoning-efforts profile))
                    (model-profile-reasoning-summaries profile)) 'vector))
    map))
