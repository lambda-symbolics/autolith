;;;; -- Restart Publisher --

;;; Publish one exact-heap restart envelope and exit.

(require :asdf)

(let* ((script        (or *load-truename* *load-pathname*))
       (source-root
         (uiop:pathname-parent-directory-pathname
          (uiop:pathname-directory-pathname script)))
       (project-setup (merge-pathnames ".qlot/setup.lisp" source-root))
       (user-setup    (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
       (setup         (if (probe-file project-setup) project-setup user-setup))
       (arguments     (uiop:command-line-arguments))
       (envelope      (pathname
                       (or (first arguments)
                           (error "A restart envelope pathname is required."))))
       (read-form
         (lambda (pathname)
           (with-open-file (stream pathname :direction :input :external-format :utf-8)
             (let* ((*read-eval* nil)
                    (end         (list :end))
                    (form        (read stream nil end)))
               (when (eq form end)
                 (error "The restart envelope is empty."))
               form)))))
  (unless (probe-file setup)
    (error "Autolith needs Quicklisp at ~A" setup))
  (load setup)
  (asdf:load-asd (merge-pathnames "autolith-agent.asd" source-root))
  (asdf:load-system :autolith-agent)
  (let* ((record (funcall read-form envelope))
         (configuration
           (uiop:symbol-call
            "AUTOLITH" "CONFIGURATION-CREATE"
            :source-root                 source-root
            :defer-provider-validation-p t)))
    (unless (and (consp record)
                 (eq (first record) :autolith-restart)
                 (= (or (getf (rest record) :version) 0) 1)
                 (stringp (getf (rest record) :identifier))
                 (integerp (getf (rest record) :created-at))
                 (listp (getf (rest record) :metadata)))
      (error "Invalid restart envelope."))
    (let* ((store
             (uiop:symbol-call "AUTOLITH" "GENERATION-STORE-FOR" configuration))
           (generation
             (uiop:symbol-call
              "SBCL-GENERATIONS" "GENERATION-RECREATE-PENDING"
              store
              :identifier (getf (rest record) :identifier)
              :created-at (getf (rest record) :created-at)
              :metadata   (getf (rest record) :metadata))))
      (uiop:symbol-call
       "AUTOLITH" "GENERATION-PUBLISH"
       configuration generation
       :probe-runner
       (uiop:symbol-call
        "AUTOLITH" "GENERATION-CORE-PROBE-RUNNER-CREATE")))))
