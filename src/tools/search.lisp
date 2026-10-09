(in-package #:autolith)

;;;; -- Search Tool Configuration --

(defparameter *fff-source-commit*
  "95fd777c2529fc7b4d7572dabff64cc07268f2c5"
  "The reviewed fff v0.11.0 source revision built by Autolith bootstrap.")

(defparameter *search-default-result-limit* 20
  "The default number of fff results returned to the model.")

(defparameter *search-maximum-result-limit* 100
  "The largest fff result page returned to the model.")

(defparameter *search-default-time-budget-milliseconds* 3000
  "The default fff content-search wall-clock budget.")

(defparameter *search-maximum-time-budget-milliseconds* 10000
  "The largest fff content-search wall-clock budget.")

(defclass search-tool (workspace-tool)
  ((engine
    :initarg :engine
    :reader search-tool-engine
    :type worker
    :documentation "The isolated clifff worker shared by one tool registry."))
  (:documentation "A workspace search operation backed by an isolated fff index."))

(defclass search-files-tool (search-tool)
  ()
  (:documentation "Fuzzy-search indexed workspace file paths."))

(defclass search-glob-tool (search-tool)
  ()
  (:documentation "Filter indexed workspace file paths by one literal glob."))

(defclass search-content-tool (search-tool)
  ()
  (:documentation "Search indexed workspace file contents."))


(defmethod tool-child-safe-p ((tool search-tool))
  "Permit isolated indexed workspace searches inside child agents."
  t)

(defmethod tool-storm-guard-exempt-p ((tool search-tool))
  "Exempt indexed workspace discovery from the mutating-call storm guard."
  t)

(-> search--validated-library-path (configuration) pathname)
(defun search--validated-library-path (configuration)
  "Return CONFIGURATION's private fff library once clifff confirms its pinned revision.

AUTOLITH_FFF_LIBRARY names a library to use instead, as the Nix package does."
  (let ((override (uiop:getenv "AUTOLITH_FFF_LIBRARY")))
    (handler-case
        (platform-truename
         *platform*
         (fff-library-locate (merge-pathnames "native/fff/" (config :data-root configuration))
                             *fff-source-commit*
                             :override (and (non-empty-string-p override)
                                            (pathname override))))
      (clifff-error (condition)
        (error 'search-error
               :message (format nil "~A Run ~A."
                                condition
                                (merge-pathnames "script/bootstrap"
                                                 (config :source-root configuration)))
               :operation ':load
               :pathname (clifff-error-pathname condition)
               :cause nil)))))


;;;; -- Tool Arguments --

(-> search-tool--string-argument
    (tool json-object string &key (:required boolean) (:fallback string))
    string)
(defun search-tool--string-argument
    (tool arguments name &key required (fallback ""))
  "Return string argument NAME or signal a typed TOOL failure."
  (let ((value (tool-argument arguments name :required required)))
    (cond
      ((null value)
       fallback)
      ((stringp value)
       value)
      (t
       (error 'tool-error
              :message (format nil "~A requires string argument ~S."
                               (tool-canonical-name tool)
                               name)
              :tool-name (tool-canonical-name tool))))))

(-> search-tool--query-with-constraints (string string) string)
(defun search-tool--query-with-constraints (query constraints)
  "Return QUERY with non-empty CONSTRAINTS prepended as fff path filters."
  (let ((filters (string-trim '(#\Space #\Tab #\Newline #\Return) constraints)))
    (if (string= filters "")
        query
        (format nil "~A ~A" filters query))))

(-> search-tool--blank-p (character) boolean)
(defun search-tool--blank-p (character)
  "Return true when CHARACTER separates fff query tokens."
  (and (member character '(#\Space #\Tab #\Newline #\Return)) t))

(-> search-tool--earmuffed-name-p (string) boolean)
(defun search-tool--earmuffed-name-p (token)
  "Return true when TOKEN reads as a Lisp special variable name such as *limit*.

fff's agent query parser takes a token that starts and ends with an asterisk as
a path glob, so such a name would filter paths instead of being searched."
  (let ((length (length token)))
    (and (>= length 3)
         (char= (char token 0) #\*)
         (char= (char token (1- length)) #\*)
         (notany (lambda (character) (find character "*?[{/."))
                 (subseq token 1 (1- length)))
         t)))

(-> search-tool--escape-earmuffed-names (string) string)
(defun search-tool--escape-earmuffed-names (query)
  "Return QUERY with each earmuffed name escaped so fff searches it as text.

fff reads a token after a backslash as literal text and drops the backslash."
  (with-output-to-string (stream)
    (loop with length = (length query)
          with start = 0
          while (< start length)
          do (let* ((token-start (or (position-if-not #'search-tool--blank-p query
                                                      :start start)
                                     length))
                    (token-end (or (position-if #'search-tool--blank-p query
                                                :start token-start)
                                   length)))
               (write-string query stream :start start :end token-start)
               (when (search-tool--earmuffed-name-p (subseq query token-start token-end))
                 (write-char #\\ stream))
               (write-string query stream :start token-start :end token-end)
               (setf start token-end)))))

(-> search-tool--constraint-tokens (string) list)
(defun search-tool--constraint-tokens (constraints)
  "Return the whitespace-separated filter tokens of CONSTRAINTS."
  (let ((tokens nil)
        (start nil))
    (loop for index from 0 to (length constraints)
          for blank-p = (or (= index (length constraints))
                            (member (char constraints index)
                                    '(#\Space #\Tab #\Newline #\Return)))
          do (cond
               ((and blank-p start)
                (push (subseq constraints start index) tokens)
                (setf start nil))
               ((and (not blank-p) (null start))
                (setf start index))))
    (nreverse tokens)))

(-> search-tool--wildcard-p (string) boolean)
(defun search-tool--wildcard-p (token)
  "Return true when TOKEN contains a glob wildcard."
  (and (find-if (lambda (character) (find character "*?[{")) token) t))

(-> search-tool--directory-constraint-p (string) boolean)
(defun search-tool--directory-constraint-p (token)
  "Return true when TOKEN is a positive directory filter such as src/."
  (and (> (length token) 1)
       (char= (char token (1- (length token))) #\/)
       (char/= (char token 0) #\!)
       (not (search-tool--wildcard-p token))))

(-> search-tool--file-path-constraint-p (string) boolean)
(defun search-tool--file-path-constraint-p (token)
  "Return true when fff reads TOKEN as a file path filter such as src/main.lisp.

This follows fff's rule: no wildcard, no trailing slash or negation, and a final
component whose extension starts with a letter and has at most ten letters or
digits."
  (let* ((name (subseq token (1+ (or (position #\/ token :from-end t) -1))))
         (dot (position #\. name :from-end t))
         (extension (and dot (subseq name (1+ dot)))))
    (and (plusp (length token))
         (char/= (char token 0) #\!)
         (char/= (char token (1- (length token))) #\/)
         (not (search-tool--wildcard-p token))
         extension
         (<= 1 (length extension) 10)
         (alpha-char-p (char extension 0))
         (every #'alphanumericp extension)
         t)))

(-> search-tool--location-glob (string) string)
(defun search-tool--location-glob (token)
  "Return a glob alternative selecting the files of directory or file path TOKEN.

Commas, closing braces and backslashes are escaped for fff's brace expansion."
  (let ((path (with-output-to-string (stream)
                (loop for character across (string-left-trim "/" token)
                      do (when (find character ",}\\")
                           (write-char #\\ stream))
                         (write-char character stream)))))
    (if (search-tool--directory-constraint-p token)
        (format nil "**/~A**" path)
        (format nil "**/~A" path))))

(-> search-tool--fff-constraints (string) string)
(defun search-tool--fff-constraints (constraints)
  "Return CONSTRAINTS with several directories and file paths joined as alternatives.

fff requires every filter to hold, so src/ and docs/ together would select no
file. Several locations become one brace glob that any of them satisfies, while
the other filters still apply to it."
  (let* ((tokens (search-tool--constraint-tokens constraints))
         (locations (remove-if-not (lambda (token)
                                     (or (search-tool--directory-constraint-p token)
                                         (search-tool--file-path-constraint-p token)))
                                   tokens)))
    (if (rest locations)
        (format nil "~{~A ~}{~{~A~^,~}}"
                (remove-if (lambda (token) (member token locations :test #'string=))
                           tokens)
                (mapcar #'search-tool--location-glob locations))
        constraints)))

(-> search-tool--check-constraints (search-tool configuration string) null)
(defun search-tool--check-constraints (tool configuration constraints)
  "Refuse a file path in CONSTRAINTS that names no indexed file.

fff drops a lone file path filter matching no file and searches the whole
workspace instead, and among several locations a mistyped one would silently
contribute nothing. Both read as successful searches, so they fail here."
  (dolist (token (remove-if-not #'search-tool--file-path-constraint-p
                                (search-tool--constraint-tokens constraints)))
    (when (zerop (search-worker-file-count (search-tool-engine tool) configuration
                                           (format nil "**/~A" token)))
      (error 'tool-error
             :message
             (format nil "No indexed file matches the constraint ~A; check the path, or search its directory instead."
                     token)
             :tool-name (tool-canonical-name tool))))
  nil)

(-> search-tool--bounded-integer
    (json-object string
     &key (:fallback integer) (:minimum integer) (:maximum integer))
    integer)
(defun search-tool--bounded-integer
    (arguments name &key (fallback 0) (minimum 0) (maximum most-positive-fixnum))
  "Return integer argument NAME clamped between MINIMUM and MAXIMUM."
  (min maximum
       (max minimum
            (or (workspace-tool-integer-argument arguments name)
                fallback))))

(-> search-tool--common-content-options (json-object) list)
(defun search-tool--common-content-options (arguments)
  "Return validated keyword options shared by content search tools."
  (list :file-offset
        (search-tool--bounded-integer arguments "file-offset"
                                      :maximum #xffffffff)
        :maximum-results
        (search-tool--bounded-integer
         arguments
         "max-results"
         :fallback *search-default-result-limit*
         :minimum 1
         :maximum *search-maximum-result-limit*)
        :maximum-matches-per-file
        (search-tool--bounded-integer arguments "max-matches-per-file"
                                      :fallback 20
                                      :minimum 1
                                      :maximum 100)
        :time-budget-milliseconds
        (search-tool--bounded-integer
         arguments
         "time-budget-ms"
         :fallback *search-default-time-budget-milliseconds*
         :minimum 1
         :maximum *search-maximum-time-budget-milliseconds*)
        :context-lines
        (search-tool--bounded-integer arguments "context" :maximum 10)))
