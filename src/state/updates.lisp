(in-package #:autolith)

;;;; -- Release Versions --

(defparameter *update-state-version* 1
  "The readable cached release-availability format version.")

(defparameter *update-check-interval* (* 20 60 60)
  "Seconds between nonblocking release-availability attempts.")

(defparameter *update-latest-url*
  "https://sh.lambda-symbolics.com/releases/latest"
  "The default redirect identifying the newest complete release.")

(-> release-tag->version (string) (option list))
(defun release-tag->version (tag)
  "Return TAG's three integer semantic-version components, or NIL."
  (block nil
    (unless (and (> (length tag) 1)
                 (char= (char tag 0) #\v))
      (return nil))
    (let ((parts (uiop:split-string (subseq tag 1) :separator '(#\.))))
      (unless (and (= (length parts) 3)
                   (every (lambda (part)
                            (and (plusp (length part))
                                 (every #'digit-char-p part)))
                          parts))
        (return nil))
      (mapcar (lambda (part)
                (parse-integer part :junk-allowed nil))
              parts))))

(-> release-tag-valid-p (string) boolean)
(defun release-tag-valid-p (tag)
  "Return true when TAG is a strict three-component release tag."
  (not (null (release-tag->version tag))))

(-> release-tag< (string string) boolean)
(defun release-tag< (left right)
  "Return true when semantic release tag LEFT precedes RIGHT."
  (let ((left-version  (release-tag->version left))
        (right-version (release-tag->version right)))
    (unless (and left-version right-version)
      (error 'type-error
             :datum (list left right)
             :expected-type '(cons string (cons string null))))
    (loop for left-part in left-version
          for right-part in right-version
          when (< left-part right-part)
            return t
          when (> left-part right-part)
            return nil
          finally (return nil))))


;;;; -- Installation Provenance --

(defclass installation-provenance ()
  ((method
    :initarg :method
    :reader installation-provenance-method
    :type (member :source :nix :release)
    :documentation "The structurally validated source, Nix, or release method.")
   (current-tag
    :initarg :current-tag
    :initform nil
    :reader installation-provenance-current-tag
    :type (option non-empty-string)
    :documentation "The currently running release tag, when versioned.")
   (release-root
    :initarg :release-root
    :initform nil
    :reader installation-provenance-release-root
    :type (option pathname)
    :documentation "The validated selected packaged release root, when present."))
  (:documentation
   "A startup installation identity derived from environment and immutable layout."))

(-> installation--canonical-directory ((or pathname string)) (option pathname))
(defun installation--canonical-directory (value)
  "Return VALUE's canonical existing directory, or NIL."
  (handler-case
      (let ((directory (uiop:ensure-directory-pathname (pathname value))))
        (and (uiop:directory-exists-p directory)
             (uiop:ensure-directory-pathname (platform-truename *platform* directory))))
    (error ()
      nil)))

(-> installation--same-directory-p
    ((or pathname string) (or pathname string))
    boolean)
(defun installation--same-directory-p (left right)
  "Return true when existing directory pathnames LEFT and RIGHT are identical.

The platform's file identity decides, so a symbolic link and its target compare
equal even where TRUENAME leaves directory links unresolved, as on Windows."
  (flet ((directory-identity (value)
           "Return VALUE's directory identity, or NIL when it is not an existing directory."
           (let ((directory (installation--canonical-directory value)))
             (and directory
                  (handler-case
                      (platform-file-status-identity
                       (platform-path-status *platform* directory
                                             :follow-links-p t))
                    (platform-error ()
                      nil))))))
    (let ((left-identity  (directory-identity left))
          (right-identity (directory-identity right)))
      (and left-identity
           right-identity
           (equal left-identity right-identity)
           t))))

(-> installation--release-fields (pathname) list)
(defun installation--release-fields (pathname)
  "Return the strict legacy key-value release record at PATHNAME."
  (handler-case
      (let ((fields nil))
        (dolist (line (uiop:read-file-lines pathname))
          (let ((separator (position #\= line)))
            (unless (and separator
                         (plusp separator)
                         (< separator (1- (length line))))
              (return-from installation--release-fields nil))
            (let ((key   (subseq line 0 separator))
                  (value (subseq line (1+ separator))))
              (when (assoc key fields :test #'string=)
                (return-from installation--release-fields nil))
              (push (cons key value) fields))))
        (let ((version (rest (assoc "version" fields :test #'string=)))
              (tag     (rest (assoc "tag" fields :test #'string=)))
              (commit  (rest (assoc "commit" fields :test #'string=))))
          (and (= (length fields) 3)
               version
               tag
               commit
               (release-tag-valid-p tag)
               (string= tag (format nil "v~A" version))
               (= (length commit) 40)
               (every (lambda (character)
                        (or (digit-char-p character)
                            (find character "abcdef")))
                      commit)
               fields)))
    (error ()
      nil)))

(-> installation--release-provenance
    (configuration (option string) (option string))
    (option installation-provenance))
(defun installation--release-provenance (configuration kind release-root-value)
  "Return validated packaged release provenance, or NIL."
  (block nil
    (unless (and (string= (or kind "") "release")
                 (non-empty-string-p release-root-value))
      (return nil))
    (let* ((release-root
             (installation--canonical-directory release-root-value))
           (source-root
             (installation--canonical-directory
              (configuration-source-root configuration)))
           (record-path (and release-root
                             (merge-pathnames "RELEASE" release-root)))
           (fields (and record-path
                        (probe-file record-path)
                        (installation--release-fields record-path)))
           (tag (and fields (rest (assoc "tag" fields :test #'string=))))
           (version
             (and fields (rest (assoc "version" fields :test #'string=))))
           (release-directory-name
             (and release-root (first (last (pathname-directory release-root)))))
           (releases-root
             (and release-root
                  (uiop:pathname-parent-directory-pathname release-root)))
           (install-root
             (and releases-root
                  (uiop:pathname-parent-directory-pathname releases-root)))
           (current (and install-root (merge-pathnames "current/" install-root))))
      (when (and release-root
                 source-root
                 tag
                 (string= version *autolith-version*)
                 (stringp release-directory-name)
                 (string= release-directory-name tag)
                 (installation--same-directory-p
                  source-root
                  (merge-pathnames "libexec/autolith/" release-root))
                 (installation--same-directory-p current release-root))
        (make-instance 'installation-provenance
                       :method ':release
                       :current-tag tag
                       :release-root release-root)))))

(-> installation--nix-store-directory-p (pathname) boolean)
(defun installation--nix-store-directory-p (directory)
  "Return true when DIRECTORY is a concrete Nix store path."
  (let ((name (namestring directory)))
    (and (uiop:string-prefix-p "/nix/store/" name)
         (> (length name) (length "/nix/store/"))
         t)))

(-> installation--nix-provenance
    (configuration (option string) (option string))
    (option installation-provenance))
(defun installation--nix-provenance (configuration kind nix-source-root-value)
  "Return validated Nix provenance, or NIL."
  (let ((source-root
          (installation--canonical-directory
           (configuration-source-root configuration)))
        (nix-source-root
          (and (non-empty-string-p nix-source-root-value)
               (installation--canonical-directory nix-source-root-value))))
    (when (and (string= (or kind "") "nix")
               source-root
               nix-source-root
               (installation--nix-store-directory-p source-root)
               (equal source-root nix-source-root))
      (make-instance 'installation-provenance
                     :method ':nix
                     :current-tag (format nil "v~A" *autolith-version*)))))

(-> installation-provenance-detect
    (configuration &key (:kind (option string))
                        (:release-root (option string))
                        (:nix-source-root (option string)))
    installation-provenance)
(defun installation-provenance-detect
    (configuration &key
                     (kind (uiop:getenv "AUTOLITH_INSTALLATION_KIND"))
                     (release-root (uiop:getenv "AUTOLITH_RELEASE_ROOT"))
                     (nix-source-root (uiop:getenv "AUTOLITH_NIX_SOURCE_ROOT")))
  "Return structurally validated installation provenance for CONFIGURATION."
  (or (installation--release-provenance configuration kind release-root)
      (installation--nix-provenance configuration kind nix-source-root)
      (make-instance 'installation-provenance :method ':source)))


;;;; -- Cached Update State --

(defclass update-state ()
  ((last-attempt-at
    :initarg :last-attempt-at
    :initform nil
    :reader update-state-last-attempt-at
    :type (option (integer 0))
    :documentation "The universal time of the newest bounded network attempt.")
   (last-success-at
    :initarg :last-success-at
    :initform nil
    :reader update-state-last-success-at
    :type (option (integer 0))
    :documentation "The universal time of the newest valid response.")
   (latest-tag
    :initarg :latest-tag
    :initform nil
    :reader update-state-latest-tag
    :type (option non-empty-string)
    :documentation "The newest strictly validated release tag, if known.")
   (dismissed-tag
    :initarg :dismissed-tag
    :initform nil
    :reader update-state-dismissed-tag
    :type (option non-empty-string)
    :documentation "The exact release tag the user chose to skip."))
  (:documentation "Validated cached release availability and dismissal state."))

(defvar *update-state-lock* (make-lock "Autolith update state")
  "The in-process lock serializing cached update-state replacement.")

(defvar *update-check-fetch-function* nil
  "Optional test replacement for the bounded newest-release request.")

(-> update-state--optional-time-p (t) boolean)
(defun update-state--optional-time-p (value)
  "Return true when VALUE is NIL or a nonnegative integer timestamp."
  (or (null value) (typep value '(integer 0))))

(-> update-state--optional-tag-p (t) boolean)
(defun update-state--optional-tag-p (value)
  "Return true when VALUE is NIL or a strict release tag."
  (or (null value)
      (and (stringp value) (release-tag-valid-p value))))

(-> update-state--form-p (t) boolean)
(defun update-state--form-p (form)
  "Return true when FORM is one complete supported update-state record."
  (values
   (record-check form
                 :tag ':update-state
                 :versions (list *update-state-version*)
                 :fields (list (list :indicator ':last-attempt-at
                                     :validate #'update-state--optional-time-p
                                     :required t)
                               (list :indicator ':last-success-at
                                     :validate #'update-state--optional-time-p
                                     :required t)
                               (list :indicator ':latest-tag
                                     :validate #'update-state--optional-tag-p
                                     :required t)
                               (list :indicator ':dismissed-tag
                                     :validate #'update-state--optional-tag-p
                                     :required t))
                 :allow-other-keys nil)))

(-> update-state--form->state (list) update-state)
(defun update-state--form->state (form)
  "Return the validated update state represented by FORM."
  (let ((properties (rest form)))
    (make-instance 'update-state
                   :last-attempt-at (getf properties :last-attempt-at)
                   :last-success-at (getf properties :last-success-at)
                   :latest-tag (getf properties :latest-tag)
                   :dismissed-tag (getf properties :dismissed-tag))))

(-> update-state--state->form (update-state) list)
(defun update-state--state->form (state)
  "Return the complete readable form for STATE."
  (list :update-state
        :version *update-state-version*
        :last-attempt-at (update-state-last-attempt-at state)
        :last-success-at (update-state-last-success-at state)
        :latest-tag (update-state-latest-tag state)
        :dismissed-tag (update-state-dismissed-tag state)))

(-> update-state-load (configuration) update-state)
(defun update-state-load (configuration)
  "Return cached update state, using empty state after absence or corruption."
  (handler-case
      (let ((pathname (configuration-update-state-path configuration)))
        (if (probe-file pathname)
            (multiple-value-bind (form sole-form-p)
                (snapshot-read pathname)
              (if (and sole-form-p (update-state--form-p form))
                  (update-state--form->state form)
                  (make-instance 'update-state)))
            (make-instance 'update-state)))
    (error ()
      (make-instance 'update-state))))

(-> update-state--write (configuration update-state) null)
(defun update-state--write (configuration state)
  "Atomically write validated private update STATE."
  (snapshot-write (configuration-update-state-path configuration)
                  (update-state--state->form state))
  nil)

(-> update-state-check-due-p
    (update-state &key (:now integer) (:interval (integer 1)))
    boolean)
(defun update-state-check-due-p
    (state &key (now (get-universal-time))
                (interval *update-check-interval*))
  "Return true when STATE permits a new bounded release request at NOW."
  (let ((last-attempt (update-state-last-attempt-at state)))
    (or (null last-attempt)
        (< now last-attempt)
        (>= (- now last-attempt) interval))))

(-> update-state--mark-attempt (configuration integer) null)
(defun update-state--mark-attempt (configuration now)
  "Record a release request attempt at NOW while preserving cached data."
  (with-lock-held (*update-state-lock*)
    (let ((state (update-state-load configuration)))
      (update-state--write
       configuration
       (make-instance 'update-state
                      :last-attempt-at now
                      :last-success-at (update-state-last-success-at state)
                      :latest-tag (update-state-latest-tag state)
                      :dismissed-tag (update-state-dismissed-tag state)))))
  nil)

(-> update-state--record-success (configuration integer string) null)
(defun update-state--record-success (configuration now latest-tag)
  "Record valid LATEST-TAG fetched successfully at NOW."
  (unless (release-tag-valid-p latest-tag)
    (error 'configuration-error
           :message (format nil "The release service returned malformed tag ~S."
                            latest-tag)))
  (with-lock-held (*update-state-lock*)
    (let ((state (update-state-load configuration)))
      (update-state--write
       configuration
       (make-instance 'update-state
                      :last-attempt-at now
                      :last-success-at now
                      :latest-tag latest-tag
                      :dismissed-tag (update-state-dismissed-tag state)))))
  nil)

(-> update-state-dismiss (configuration string) null)
(defun update-state-dismiss (configuration tag)
  "Atomically suppress the exact release TAG while preserving newer cache data."
  (unless (release-tag-valid-p tag)
    (error 'configuration-error
           :message (format nil "Cannot skip malformed release tag ~S." tag)))
  (with-lock-held (*update-state-lock*)
    (let ((state (update-state-load configuration)))
      (update-state--write
       configuration
       (make-instance 'update-state
                      :last-attempt-at (update-state-last-attempt-at state)
                      :last-success-at (update-state-last-success-at state)
                      :latest-tag (update-state-latest-tag state)
                      :dismissed-tag tag))))
  nil)


;;;; -- Availability Refresh --

(defclass update-availability ()
  ((tag
    :initarg :tag
    :reader update-availability-tag
    :type non-empty-string
    :documentation "The newer cached release tag.")
   (method
    :initarg :method
    :reader update-availability-method
    :type (member :nix :release)
    :documentation "The validated installation method governing update UX."))
  (:documentation "A newer cached release relevant to one installation."))

(-> update-availability-current
    (configuration installation-provenance)
    (option update-availability))
(defun update-availability-current (configuration provenance)
  "Return a newer nondismissed cached release for PROVENANCE, or NIL."
  (let* ((method (installation-provenance-method provenance))
         (current-tag (installation-provenance-current-tag provenance))
         (state (update-state-load configuration))
         (latest-tag (update-state-latest-tag state)))
    (when (and (not (string= (or (uiop:getenv
                                  "AUTOLITH_SUPPRESS_UPDATE_OFFER") "")
                             "1"))
               (member method '(:nix :release))
               current-tag
               latest-tag
               (release-tag< current-tag latest-tag)
               (not (string= latest-tag
                             (or (update-state-dismissed-tag state) ""))))
      (make-instance 'update-availability
                     :tag latest-tag
                     :method method))))

(-> update-check--tag-from-uri (t) (option string))
(defun update-check--tag-from-uri (uri)
  "Return a strict release tag from final response URI, or NIL."
  (handler-case
      (let* ((path (uri-path uri))
             (trimmed (string-right-trim "/" path))
             (separator (position #\/ trimmed :from-end t))
             (tag (subseq trimmed (if separator (1+ separator) 0))))
        (and (release-tag-valid-p tag) tag))
    (error ()
      nil)))

(-> update-check--fetch-latest-tag () string)
(defun update-check--fetch-latest-tag ()
  "Return the newest release tag through one bounded HTTPS redirect request."
  (multiple-value-bind (body status headers final-uri)
      (dexador:get (or (uiop:getenv "AUTOLITH_RELEASE_LATEST_URL")
                       *update-latest-url*)
                   :connect-timeout 3
                   :read-timeout 5
                   :max-redirects 5
                   :force-string t)
    (declare (ignore body headers))
    (let ((tag (and (= status 200)
                    (update-check--tag-from-uri final-uri))))
      (unless tag
        (error 'configuration-error
               :message "The release service did not identify a valid release."))
      tag)))

(-> update-check--fetch () string)
(defun update-check--fetch ()
  "Invoke the configured bounded newest-release request effect."
  (if *update-check-fetch-function*
      (funcall *update-check-fetch-function*)
      (update-check--fetch-latest-tag)))

(-> update-state-refresh
    (configuration &key (:now integer) (:force-p boolean))
    boolean)
(defun update-state-refresh
    (configuration &key (now (get-universal-time)) force-p)
  "Refresh CONFIGURATION's cache once, bypassing freshness when FORCE-P."
  (block nil
    (unless (or force-p
                (update-state-check-due-p
                 (update-state-load configuration) :now now))
      (return nil))
    (handler-case
        (progn
          (update-state--mark-attempt configuration now)
          (update-state--record-success configuration now (update-check--fetch))
          t)
      (serious-condition ()
        nil))))

(-> update-check-start
    (configuration installation-provenance)
    t)
(defun update-check-start (configuration provenance)
  "Start one due background availability refresh, returning its thread or NIL."
  (block nil
    (when (or (eq (installation-provenance-method provenance) ':source)
              (string= (or (uiop:getenv "AUTOLITH_NO_UPDATE_CHECK") "") "1"))
      (return nil))
    (let ((now (get-universal-time)))
      (unless (update-state-check-due-p (update-state-load configuration)
                                        :now now)
        (return nil))
      (handler-case
          (progn
            (update-state--mark-attempt configuration now)
            (make-thread
             (lambda ()
               (handler-bind ((warning #'muffle-warning))
                 (handler-case
                     (update-state--record-success
                      configuration now (update-check--fetch))
                   (serious-condition ()
                     nil))))
             :name "Autolith release availability"))
        (serious-condition ()
          nil)))))
