(in-package #:autolith)

;;;; -- Local Model Redaction --

(-> redact (string &key (:language string)
                       (:model-directory (or null string pathname))
                       (:runtime-library (or null string pathname)))
    (values (option string) keyword))
(defun redact (text &key (language "en") model-directory runtime-library)
  "Redact TEXT with the local censorship model. Return the complete string and :OK,
or NIL and a failure status. No partial text is returned. LANGUAGE defaults to en.
Use immutable packaged model paths unless MODEL-DIRECTORY and RUNTIME-LIBRARY
are both supplied as absolute paths. Each call closes its native context.
This function runs locally, independently of telemetry consent, and sends no data."
  (handler-case
      (progn
        (when-let ((library (uiop:getenv "AUTOLITH_DEFINGERPRINTER_LIBRARY"))) (defingerprinter:load-library library))
        (let ((redactor (defingerprinter:make-redactor
                         :model-directory model-directory :runtime-library runtime-library)))
          (unwind-protect
               (redaction--text redactor text language)
            (defingerprinter:close-redactor redactor))))
    (defingerprinter:setup-error (condition)
      (values nil (defingerprinter:setup-error-status condition)))))

(-> redaction--text (t string string) (values (option string) keyword))
(defun redaction--text (redactor text language)
  "Redact all of TEXT in bounded sentence or word pieces using REDACTOR.
Return the assembled string and :OK, or NIL and a failure status. Reject native
output at the 1,024-character ABI limit rather than returning truncated text."
  (block complete
    (let ((start  0)
          (length (length text)))
      (values
       (with-output-to-string (stream)
         (loop while (< start length)
               for ceiling = (min length (+ start 512))
               for end = (if (= ceiling length)
                             ceiling
                             (or (loop for index downfrom (1- ceiling) above (+ start 256)
                                       when (and (char= (char text index) #\Space)
                                                 (find (char text (1- index)) ".!?;"))
                                         return (1+ index))
                                 (when-let ((space (position #\Space text :start start :end ceiling :from-end t))) (1+ space))
                                 (return-from complete (values nil ':unsupported-input))))
               do (multiple-value-bind (safe status)
                      (defingerprinter:redact redactor (subseq text start end) :language language)
                    (unless (eq status ':ok)
                      (return-from complete (values nil status)))
                    (unless (and (stringp safe) (< (length safe) 1024))
                      (return-from complete (values nil ':output-limit)))
                    (write-string safe stream))
                  (setf start end)))
       ':ok))))
