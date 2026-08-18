(in-package #:autolith)

;;;; -- Search Tool Configuration --

(defparameter *fff-source-commit*
  "e2cad2f09ea617d4c024f396f21d80e557f23a17"
  "The reviewed fff v0.10.3 source revision built by Autolith bootstrap.")

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

(defparameter *fff-library-file-name*
  #+darwin "libfff_c.dylib"
  #-darwin "libfff_c.so"
  "The platform file name of the private fff search library.")

(-> search--library-path (configuration) (values pathname boolean))
(defun search--library-path (configuration)
  "Return the fff library path and whether it is an explicit override."
  (let ((override (uiop:getenv "AUTOLITH_FFF_LIBRARY")))
    (if (non-empty-string-p override)
        (values (pathname override) t)
        (values (merge-pathnames (format nil "native/fff/~A"
                                         *fff-library-file-name*)
                                 (configuration-data-root configuration))
                nil))))

(-> search--installed-manifest-valid-p (pathname) boolean)
(defun search--installed-manifest-valid-p (library)
  "Return true when LIBRARY's private manifest matches the pinned fff source."
  (let ((manifest (merge-pathnames
                   "manifest.sexp"
                   (uiop:pathname-directory-pathname library))))
    (and (probe-file manifest)
         (handler-case
             (with-open-file (stream manifest
                                     :direction ':input
                                     :external-format ':utf-8)
               (let ((*read-eval* nil)
                     (expected (list :fff-library
                                     :version 1
                                     :commit *fff-source-commit*)))
                 (equal (read stream nil nil) expected)))
           (error ()
             nil)))))

(-> search--validated-library-path (configuration) pathname)
(defun search--validated-library-path (configuration)
  "Return CONFIGURATION's existing private fff library after identity checks."
  (multiple-value-bind (library override-p)
      (search--library-path configuration)
    (let ((library (or (probe-file library) library)))
      (unless (probe-file library)
        (error 'search-error
               :message
               (format nil "The private fff library is missing at ~A; run ~A."
                       library
                       (merge-pathnames "script/bootstrap"
                                        (configuration-source-root
                                         configuration)))
               :operation ':load
               :pathname library
               :cause nil))
      (unless (or override-p (search--installed-manifest-valid-p library))
        (error 'search-error
               :message
               (format nil
                       "The private fff library at ~A does not match revision ~A; run ~A."
                       library
                       *fff-source-commit*
                       (merge-pathnames "script/bootstrap"
                                        (configuration-source-root
                                         configuration)))
               :operation ':load
               :pathname library
               :cause nil))
      (truename library))))


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
