(in-package #:autolith)

;;;; -- User Initialization --

(defvar *user-init-configuration* nil
  "The configuration whose executable initialization files are loading.")

(defvar *user-init-pathname* nil
  "The executable initialization pathname currently being loaded.")

(defvar *user-init-layer* nil
  "The site, directory, or user initialization layer currently being loaded.")

(-> user-init--registry-lifecycle (symbol symbol symbol) registry-lifecycle)
(defun user-init--registry-lifecycle
    (snapshot-function restore-function remove-source-function)
  "Adapt named registry operations, resolving their current definitions on each call."
  (make-registry-lifecycle
   :snapshot-function      (lambda () (funcall snapshot-function))
   :restore-function       (lambda (snapshot) (funcall restore-function snapshot))
   :remove-source-function (lambda (source) (funcall remove-source-function source))))

(defparameter *extension-registry-lifecycles*
  (list
   (user-init--registry-lifecycle
    'context--registry-snapshot
    'context--registry-restore
    'context--remove-registration-source)
   (user-init--registry-lifecycle
    'application-command--registry-snapshot
    'application-command--registry-restore
    'application-command--remove-registration-source)
   (user-init--registry-lifecycle
    'mcp--registry-snapshot
    'mcp--registry-restore
    'mcp--remove-registration-source)
   (user-init--registry-lifecycle
    'provider--registry-snapshot
    'provider--registry-restore
    'provider--remove-registration-source))
  "Ordered participants in executable initialization's registration transaction.
Each participant retains its own locks, validation, and derived-state publication.")

(-> user-init--present-entry (pathname keyword string) list)
(defun user-init--present-entry (pathname layer description)
  "Return one LAYER entry when PATHNAME is a present regular file."
  (handler-case
      (when (mcp-configuration--source-present-p
             pathname :description description)
        (list (list layer pathname)))
    (mcp-configuration-error (cause)
      (let ((pathname
              (or (mcp-configuration-error-pathname cause)
                  pathname)))
        (error 'user-init-error
               :message
               (format nil
                       "Could not inspect ~A executable initialization at ~A: ~A"
                       (string-downcase (symbol-name layer)) pathname cause)
               :pathname pathname
               :layer    layer
               :cause    cause)))))

(-> user-init--directory-entries (configuration) list)
(defun user-init--directory-entries (configuration)
  "Return trusted directory initialization entries in inheritance order."
  (handler-case
      (mapcar (lambda (pathname)
                (list ':directory pathname))
              (directory-configuration-active-init-paths configuration))
    (mcp-configuration-error (cause)
      (let ((pathname
              (or (mcp-configuration-error-pathname cause)
                  (configuration-directory-scopes-path configuration))))
        (error 'user-init-error
               :message
               (format nil
                       "Could not inspect directory executable initialization at ~A: ~A"
                       pathname cause)
               :pathname pathname
               :layer    ':directory
               :cause    cause)))))

(defparameter *user-init-layers* '(:site :directory :user)
  "The executable initialization layers in load order.")

(-> user-init--layers-valid-p (t) boolean)
(defun user-init--layers-valid-p (value)
  "Return whether VALUE names a nonempty set of initialization layers without duplicates."
  (and (consp value)
       (= (length value) (length (remove-duplicates value)))
       (every (lambda (layer) (member layer *user-init-layers*)) value)))

(-> user-init--check-layers (list) list)
(defun user-init--check-layers (layers)
  "Return LAYERS after refusing names outside *USER-INIT-LAYERS*."
  (unless (user-init--layers-valid-p layers)
    (error 'configuration-error
           :message (format nil "~S is not a list of executable initialization layers."
                            layers)))
  layers)

(-> user-init--registry-sources (list) list)
(defun user-init--registry-sources (layers)
  "Return the registration sources that loading LAYERS replaces."
  (remove-duplicates
   (mapcar (lambda (layer) (if (eq layer ':site) ':site ':user)) layers)))

(-> user-init--entries (configuration &key (:layers list)) list)
(defun user-init--entries (configuration &key (layers *user-init-layers*))
  "Return the LAYERS initialization entries in load order."
  (let ((site (configuration-site-init-path configuration))
        (user (configuration-user-init-path configuration)))
    (append
     (when (and site (member ':site layers))
       (user-init--present-entry site ':site "site initialization file"))
     (when (member ':directory layers)
       (user-init--directory-entries configuration))
     (when (and user (member ':user layers))
       (user-init--present-entry user ':user "user initialization file")))))

(-> user-init-load (configuration &key (:layers list)) (option pathname))
(defun user-init-load (configuration &key (layers *user-init-layers*))
  "Load CONFIGURATION's executable initialization files and return the last path.

LAYERS selects the layers to load, by default all of them: the optional site
file first, trusted directory files from outermost to nearest, then the global
user file. The files are read in the AUTOLITH package after tracked and
privately committed definitions have loaded, with the user's full privileges.
Registration changes roll back after failure, but arbitrary Lisp side effects
do not generally have reversible semantics."
  (let ((layers (user-init--check-layers layers)))
    (with-extension-registry-transaction
      (let* ((entries         (user-init--entries configuration :layers layers))
             (registries      (copy-list *extension-registry-lifecycles*))
             (snapshots       (mapcar #'registry-snapshot registries))
             (loaded-pathname nil)
             (current-entry   nil))
        (handler-case
            (progn
              (dolist (source (user-init--registry-sources layers))
                (dolist (registry registries)
                  (registry-remove-source registry source)))
              (dolist (entry entries loaded-pathname)
                (setf current-entry entry)
                (let* ((layer                     (first entry))
                       (pathname                  (second entry))
                       (*package*                 (find-package '#:autolith))
                       (*user-init-loading-p*     t)
                       (*user-init-configuration* configuration)
                       (*configuration*           configuration)
                       (*user-init-pathname*      pathname)
                       (*user-init-layer*         layer)
                       (*extension-registration-source*
                         (if (eq layer ':site) ':site ':user)))
                  (load pathname :verbose nil :print nil)
                  (setf loaded-pathname pathname))))
          (serious-condition (cause)
            (mapc #'registry-restore registries snapshots)
            (let* ((entry    (or current-entry
                                 (first entries)
                                 (list ':user
                                       (configuration-user-init-path configuration))))
                   (layer    (first entry))
                   (pathname (second entry)))
              (error 'user-init-error
                     :message
                     (format nil
                             "Could not load ~A executable initialization at ~A: ~A"
                             (string-downcase (symbol-name layer)) pathname cause)
                     :pathname pathname
                     :layer    layer
                     :cause    cause))))))))

(-> user-init-load-site (configuration &key (:stream stream)) (option pathname))
(defun user-init-load-site (configuration &key (stream *error-output*))
  "Load only CONFIGURATION's site initialization layer, degrading on failure.

The terminal client paints its own theme table and screen, so it loads the site
layer in its own process before that presentation is built. Only the site layer
loads: trusted directory and personal initialization belong to the world server.
A failing site file restores its registrations, reports one bounded line on
STREAM, and leaves the caller with the stock presentation. A repeated call
replaces its registrations, so a resumed application may load it again."
  (handler-case
      (user-init-load configuration :layers '(:site))
    (sb-sys:interactive-interrupt (condition)
      (error condition))
    (serious-condition (condition)
      (user-init--report-site-failure configuration stream condition))))

(-> user-init--report-site-failure (configuration stream serious-condition) null)
(defun user-init--report-site-failure (configuration stream condition)
  "Report a failed client site load in one bounded line on STREAM."
  (let ((pathname (or (ignore-errors (configuration-site-init-path configuration))
                      "the site root")))
    (format stream "~&Autolith: site initialization failed at ~A: ~A~%"
            pathname
            (bounded-string (princ-to-string condition) :limit 400))
    (finish-output stream))
  nil)
