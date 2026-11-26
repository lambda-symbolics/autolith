(in-package #:autolith)

;;;; -- Active-Image Symbol Search --

(defparameter *lisp-apropos-default-limit* 40
  "How many matches lisp.apropos lists when the call sets no limit.")

(defparameter *lisp-apropos-maximum-limit* 200
  "The most matches one lisp.apropos call may list.")

(defparameter *lisp-apropos-documentation-characters* 110
  "The longest documentation excerpt shown beside one match.")

(defparameter *lisp-apropos-suggestion-limit* 3
  "How many near-miss names a failed exact lookup proposes.")

(defparameter *lisp-apropos-token-minimum* 3
  "The shortest hyphen-separated name token that counts toward a near miss.")

(-> lisp-apropos--tracked-path (symbol list configuration) (option string))
(defun lisp-apropos--tracked-path (symbol kinds configuration)
  "Return SYMBOL's defining file relative to the tracked source root, if it lies there."
  (let ((pathname    (symbol-definition-pathname symbol kinds))
        (source-root (config :source-root configuration)))
    (when (and pathname (uiop:subpathp pathname source-root))
      (enough-namestring pathname source-root))))

(-> lisp-apropos-render
    (list &key (:query string) (:package package) (:limit (integer 1))
          (:configuration configuration))
    string)
(defun lisp-apropos-render (matches &key query package limit configuration)
  "Render MATCHES for QUERY over PACKAGE, listing at most LIMIT entries."
  (with-output-to-string (stream)
    (cond
      ((null matches)
       (format stream "No defined name in ~A matches ~S. Try fewer or shorter terms, another package, or search.content over the tracked source."
               (package-name package) query))
      (t
       (format stream "~D defined name~:P in ~A match~:[~;es~] ~S~:[ (showing the first ~D)~;~]:~%"
               (length matches) (package-name package) (= (length matches) 1)
               query (<= (length matches) limit) limit)
       (loop for (symbol . kinds) in matches
             repeat limit
             do (format stream "~%~A  ~{~(~A~)~^, ~}~@[  ~A~]~%"
                        (symbol-label symbol package)
                        kinds
                        (lisp-apropos--tracked-path symbol kinds configuration))
                (let ((lambda-list   (and (intersection kinds '(:function :macro :generic-function))
                                          (symbol-lambda-list symbol)))
                      (documentation (symbol-documentation-line
                                      symbol kinds
                                      :limit *lisp-apropos-documentation-characters*)))
                  (when (or lambda-list documentation)
                    (format stream "  ~@[~(~S~)~]~:[~;  ~]~@[~A~]~%"
                            lambda-list
                            (and lambda-list documentation)
                            documentation))))))))

(-> lisp-apropos--kind-argument (t) (option keyword))
(defun lisp-apropos--kind-argument (value)
  "Return the definition kind keyword named by tool argument VALUE, or NIL for all kinds."
  (cond
    ((null value)
     nil)
    ((and (stringp value)
          (find (string-upcase value) *definition-kinds* :key  #'symbol-name
                                                         :test #'string=)))
    (t
     (error 'tool-error
            :message   (format nil "Unknown definition kind ~S. Choose one of ~{~(~A~)~^, ~}."
                               value *definition-kinds*)
            :tool-name "lisp.apropos"))))

(-> lisp-apropos--limit-argument (hash-table) (integer 1))
(defun lisp-apropos--limit-argument (arguments)
  "Return the requested match limit from ARGUMENTS, clamped to the supported range."
  (min *lisp-apropos-maximum-limit*
       (max 1 (or (workspace-tool-integer-argument arguments "limit")
                  *lisp-apropos-default-limit*))))

(defmethod tool-execute ((tool lisp-apropos-tool)
                         (context tool-context)
                         (arguments hash-table))
  "List defined active-image names matching the required query."
  (declare (ignore tool))
  (let ((query (tool-argument arguments "query" :required t)))
    (unless (non-empty-string-p query)
      (error 'tool-error
             :message   "lisp.apropos requires a non-empty query string."
             :tool-name "lisp.apropos"))
    (let* ((package (self-resolve-package (tool-argument arguments "package")))
           (kind    (lisp-apropos--kind-argument (tool-argument arguments "kind")))
           (limit   (lisp-apropos--limit-argument arguments))
           (matches (package-apropos query :package package :kind kind)))
      (tool-success
       (lisp-apropos-render matches
                            :query         query
                            :package       package
                            :limit         limit
                            :configuration (tool-context-configuration context))))))


;;;; -- Near-Miss Suggestions --

(-> self-symbol-suggestion-text (string &key (:package package)) string)
(defun self-symbol-suggestion-text (name &key (package (find-package '#:autolith)))
  "Return a sentence naming the closest defined names to NAME, or an empty string."
  (if-let ((suggestions (package-symbol-suggestions
                         name :package       package
                              :limit         *lisp-apropos-suggestion-limit*
                              :token-minimum *lisp-apropos-token-minimum*)))
      (format nil " Closest defined names: ~{~A~^, ~}."
              (mapcar (lambda (symbol) (symbol-label symbol package))
                      suggestions))
      ""))
