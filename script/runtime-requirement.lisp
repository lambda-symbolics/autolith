;;;; Minimum SBCL runtime enforcement shared by standalone build scripts.

(defun autolith-version-components (version)
  "Return the leading numeric components of the dotted VERSION string."
  (loop for field in (uiop:split-string version :separator ".")
        for end = (or (position-if-not #'digit-char-p field) (length field))
        while (plusp end)
        collect (parse-integer field :end end)))

(defun autolith-version-at-least-p (candidate minimum)
  "Return whether dotted version CANDIDATE is at least dotted version MINIMUM."
  (let ((candidate-components (autolith-version-components candidate))
        (minimum-components (autolith-version-components minimum)))
    (loop for index from 0 below (max (length candidate-components)
                                      (length minimum-components))
          for candidate-component = (or (nth index candidate-components) 0)
          for minimum-component = (or (nth index minimum-components) 0)
          when (> candidate-component minimum-component) return t
          when (< candidate-component minimum-component) return nil
          finally (return t))))

(defun autolith-require-minimum-runtime (version-pathname)
  "Signal an error unless this process satisfies the version at VERSION-PATHNAME."
  (let ((minimum (string-trim '(#\Space #\Tab #\Newline #\Return)
                              (uiop:read-file-string version-pathname))))
    (unless (autolith-version-at-least-p (lisp-implementation-version) minimum)
      (error "Autolith needs SBCL ~A or newer, but this process is SBCL ~A. Set AUTOLITH_SBCL to a suitable executable."
             minimum
             (lisp-implementation-version)))))
