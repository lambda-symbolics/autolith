(in-package #:autolith)

;;;; -- Search Tool Configuration --

(defparameter *fff-source-commit*
  "9a7b1c8fc1d20236a255bf3ddb24eeb761bd6bd9"
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
    :initarg       :engine
    :reader        search-tool-engine
    :type          worker
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
  "Return CONFIGURATION's private fff library path after clifff verifies the pinned revision.

AUTOLITH_FFF_LIBRARY overrides the library path; the Nix package uses this override."
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
               :message   (format nil "~A Run ~A."
                                  condition
                                  (merge-pathnames "script/bootstrap"
                                                   (config :source-root configuration)))
               :operation ':load
               :pathname  (clifff-error-pathname condition)
               :cause     nil)))))


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
              :message   (format nil "~A requires string argument ~S."
                                 (tool-canonical-name tool)
                                 name)
              :tool-name (tool-canonical-name tool))))))

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
         :minimum  1
         :maximum  *search-maximum-result-limit*)
        :maximum-matches-per-file
        (search-tool--bounded-integer arguments "max-matches-per-file"
                                      :fallback 20
                                      :minimum  1
                                      :maximum  100)
        :time-budget-milliseconds
        (search-tool--bounded-integer
         arguments
         "time-budget-ms"
         :fallback *search-default-time-budget-milliseconds*
         :minimum  1
         :maximum  *search-maximum-time-budget-milliseconds*)
        :context-lines
        (search-tool--bounded-integer arguments "context" :maximum 10)))
