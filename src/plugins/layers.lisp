(in-package #:autolith)

;;;; -- Plugin Image Layers --

;; The plugin image ladder is data in phase 1. Building, caching, and saving the
;; images belongs to phase 9.

(deftype plugin-image-rung ()
  "One rung of the plugin image ladder."
  '(member 1 2 3))

(define-condition plugin-image-layer-error (autolith-error)
  ((rung
    :initarg :rung
    :reader plugin-image-layer-error-rung
    :type (option plugin-image-rung)
    :documentation "The rung whose data is invalid.")
   (field
    :initarg :field
    :reader plugin-image-layer-error-field
    :type keyword
    :documentation "The layer field that failed validation."))
  (:documentation "One plugin image layer cannot be constructed.")
  (:report
   (lambda (condition stream)
     (format stream "Plugin image layer ~S field ~S is invalid: ~A"
             (plugin-image-layer-error-rung condition)
             (plugin-image-layer-error-field condition)
             (autolith-error-message condition)))))

(defclass plugin-image-layer ()
  ((rung
    :initarg :rung
    :reader plugin-image-layer-rung
    :type plugin-image-rung
    :documentation "1 for the base, 2 for global and site, 3 for one project.")
   (source-commit
    :initarg :source-commit
    :reader plugin-image-layer-source-commit
    :type non-empty-string
    :documentation "The tracked source revision in this layer.")
   (mutation-commit
    :initarg :mutation-commit
    :initform nil
    :reader plugin-image-layer-mutation-commit
    :type (option non-empty-string)
    :documentation "The selected private mutation commit, when one exists.")
   (global-plugins
    :initarg :global-plugins
    :initform nil
    :reader plugin-image-layer-global-plugins
    :type list
    :documentation "The global plugin identities in this layer.")
   (site-plugins
    :initarg :site-plugins
    :initform nil
    :reader plugin-image-layer-site-plugins
    :type list
    :documentation "The site plugin identities in this layer.")
   (project-plugins
    :initarg :project-plugins
    :initform nil
    :reader plugin-image-layer-project-plugins
    :type list
    :documentation "The project plugin identities in this layer.")
   (project
    :initarg :project
    :initform nil
    :reader plugin-image-layer-project
    :type (option non-empty-string)
    :documentation "The project directory for rung 3."))
  (:documentation "One rung of the plugin image ladder as data.

Rung 1 is the tracked source plus the selected mutation commit. Rung 2 adds
global and site plugins. Rung 3 adds the plugins of one project."))

(-> plugin-image-layer-p (t) boolean)
(defun plugin-image-layer-p (value)
  "Return true when VALUE is a plugin image layer."
  (typep value 'plugin-image-layer))

(-> plugin-image-layer--check-plugins
    (plugin-image-rung keyword list)
    null)
(defun plugin-image-layer--check-plugins (rung field plugins)
  "Signal a layer condition unless PLUGINS is a list of plugin identities."
  (unless (and (listp plugins)
               (every #'plugin-identity-p plugins))
    (error 'plugin-image-layer-error
           :message "expected a list of resolved plugin identities"
           :rung rung
           :field field))
  nil)

(-> plugin-image-layer-1
    (string &key (:mutation-commit (option string)))
    plugin-image-layer)
(defun plugin-image-layer-1 (source-commit &key mutation-commit)
  "Return rung 1: tracked source plus the selected mutation commit."
  (make-instance 'plugin-image-layer
                 :rung 1
                 :source-commit source-commit
                 :mutation-commit mutation-commit))

(-> plugin-image-layer-2
    (string &key (:mutation-commit (option string))
                (:global-plugins list)
                (:site-plugins list))
    plugin-image-layer)
(defun plugin-image-layer-2 (source-commit &key mutation-commit
                                           (global-plugins nil)
                                           (site-plugins nil))
  "Return rung 2: rung 1 plus global and site plugins."
  (plugin-image-layer--check-plugins 2 ':global-plugins global-plugins)
  (plugin-image-layer--check-plugins 2 ':site-plugins site-plugins)
  (make-instance 'plugin-image-layer
                 :rung 2
                 :source-commit source-commit
                 :mutation-commit mutation-commit
                 :global-plugins global-plugins
                 :site-plugins site-plugins))

(-> plugin-image-layer-3
    (string string &key (:mutation-commit (option string))
                         (:global-plugins list)
                         (:site-plugins list)
                         (:project-plugins list))
    plugin-image-layer)
(defun plugin-image-layer-3 (source-commit project &key mutation-commit
                                                        (global-plugins nil)
                                                        (site-plugins nil)
                                                        (project-plugins nil))
  "Return rung 3: rung 2 plus the plugins of PROJECT."
  (plugin-image-layer--check-plugins 3 ':global-plugins global-plugins)
  (plugin-image-layer--check-plugins 3 ':site-plugins site-plugins)
  (plugin-image-layer--check-plugins 3 ':project-plugins project-plugins)
  (make-instance 'plugin-image-layer
                 :rung 3
                 :source-commit source-commit
                 :mutation-commit mutation-commit
                 :project project
                 :global-plugins global-plugins
                 :site-plugins site-plugins
                 :project-plugins project-plugins))

(-> plugin-image-layer-plugins (plugin-image-layer) list)
(defun plugin-image-layer-plugins (layer)
  "Return LAYER's plugin identities in load order."
  (append (plugin-image-layer-global-plugins layer)
          (plugin-image-layer-site-plugins layer)
          (plugin-image-layer-project-plugins layer)))

(defmethod print-object ((layer plugin-image-layer) stream)
  "Print LAYER with its rung, source commit, and plugin count."
  (print-unreadable-object (layer stream :type t)
    (format stream "rung ~D ~S ~D plugin~:P~@[ ~S~]"
            (plugin-image-layer-rung layer)
            (plugin-image-layer-source-commit layer)
            (length (plugin-image-layer-plugins layer))
            (plugin-image-layer-project layer))))
