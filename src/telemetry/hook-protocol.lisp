(in-package #:autolith)

;;;; -- Lifecycle Metadata --

(defvar *telemetry-tool-denied-p* nil
  "Whether this tool attempt received an explicit authorization denial.")

(defvar *telemetry-tool-name* nil
  "The current tool's local name, used only for structured repair metadata.")

(-> telemetry--elapsed-milliseconds (integer) integer)
(defun telemetry--elapsed-milliseconds (start)
  "Return elapsed monotonic milliseconds since START."
  (max 0 (round (* (- (get-internal-real-time) start) 1000)
                internal-time-units-per-second)))

(-> telemetry--call-safely (function) t)
(defun telemetry--call-safely (function)
  "Isolate an optional metadata emission failure without printing its condition."
  (handler-case (funcall function)
    (error () nil)))

(-> telemetry-note-mutation-journal (list) null)
(defun telemetry-note-mutation-journal (record)
  "Project a completed journal transition without reading source, values or output."
  (when *telemetry-run*
    (telemetry--call-safely
     (lambda ()
       (let* ((properties (rest record))
              (kind (getf properties :kind))
              (result (getf properties :result))
              (outcome
                (case result
                  (:pending "proposed")
                  (:installed "applied")
                  (:passed "verified")
                  (:failed "failed")
                  (:discarded "reverted")
                  (:committed "committed"))))
         (when outcome
           (telemetry-note-repair
            :run *telemetry-run*
            :report-id (telemetry-mutation-report-id (or (getf properties :mutation)
                                                        (getf properties :id)))
            :target *telemetry-tool-name*
            :repair-kind (case kind
                           (:definition "redefine_function")
                           (:set "change_setting")
                           (otherwise "other"))
            :outcome outcome
            :verified (eq result ':passed)))))))
  nil)
