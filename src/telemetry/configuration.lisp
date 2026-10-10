(in-package #:autolith)

;;;; -- Telemetry Settings --

(defclass telemetry-endpoint-setting (setting)
  ()
  (:default-initargs :type 'string)
  (:documentation "A credential-free OTLP HTTPS or loopback HTTP endpoint."))

(defun telemetry--endpoint-p (value)
  "Accept only credential-free HTTPS or exact loopback HTTP trace URLs."
  (handler-case
      (let ((uri (and (stringp value) (quri:uri value))))
        (and uri (not (quri:uri-userinfo uri))
             (not (quri:uri-query uri)) (not (quri:uri-fragment uri))
             (string= (or (quri:uri-path uri) "") "/v1/traces")
             (or (string= (or (quri:uri-scheme uri) "") "https")
                 (and (string= (or (quri:uri-scheme uri) "") "http")
                      (member (quri:uri-host uri) '("localhost" "127.0.0.1" "::1" "[::1]")
                              :test #'string=)))
             (not (null (quri:uri-host uri)))
             (not (find-if (lambda (char) (or (<= (char-code char) 32)
                                             (= (char-code char) 127))) value))
             t))
    (error () nil)))

(defmethod setting-validate ((setting telemetry-endpoint-setting) value configuration)
  (declare (ignore configuration))
  (unless (telemetry--endpoint-p value)
    (setting-reject setting "[invalid endpoint]"
                    "Use HTTPS or loopback HTTP /v1/traces without credentials, query or fragment."))
  value)

(defun telemetry--alias-p (value)
  "Recognize an explicitly configured public model identifier."
  (and (stringp value) (<= 1 (length value) 96)
       (alphanumericp (char value 0))
       (every (lambda (char)
                (and (< (char-code char) 128)
                     (or (alphanumericp char) (find char "._:/-")))) value)
       t))

(define-setting :telemetry-enabled-p (boolean-setting)
  :label "Telemetry consent" :group :telemetry :scope :durable :default nil
  :documentation "Explicit consent to export bounded numerical run, model and tool metadata.")

(define-setting :telemetry-diagnostics-p (boolean-setting)
  :label "Diagnostic consent" :group :telemetry :scope :durable :default nil
  :documentation "Separate consent to export locally redacted diagnostic summaries.")

(define-setting :telemetry-endpoint (telemetry-endpoint-setting)
  :label "Telemetry endpoint" :group :telemetry :scope :durable
  :default "http://127.0.0.1:4318/v1/traces"
  :documentation "OTLP/HTTP JSON destination; redirects are refused.")

(define-setting :telemetry-token-file (absolute-file-setting)
  :label "Telemetry token file" :group :telemetry :scope :durable
  :type '(or null pathname) :default nil
  :documentation "Optional private regular file, read only by the upload subprocess.")

(define-setting :telemetry-model-aliases (setting)
  :label "Public model aliases" :group :telemetry :scope :durable
  :type 'list :default nil
  :validator (lambda (value configuration)
               (declare (ignore configuration))
               (unless (and (<= (length value) 128)
                            (every (lambda (entry)
                                     (and (consp entry) (stringp (first entry))
                                          (telemetry--alias-p (rest entry)))) value))
                 "Use an association list of local model names and public aliases."))
  :documentation "Trusted local-name to public-alias pairs; unmapped models export as custom.")

(define-setting :telemetry-model-directory (absolute-file-setting)
  :label "Redaction model directory" :group :telemetry :scope :process
  :type '(or null pathname) :default nil
  :documentation "Optional explicit model directory, paired with telemetry-runtime-library.")

(define-setting :telemetry-runtime-library (absolute-file-setting)
  :label "Redaction runtime library" :group :telemetry :scope :process
  :type '(or null pathname) :default nil
  :documentation "Optional explicit ONNX Runtime library; normally use immutable Nix defaults.")

(define-setting :telemetry-timeout-ms (integer-setting)
  :label "Telemetry deadline" :group :telemetry :scope :durable
  :minimum 10 :maximum 10000 :default 3000
  :documentation "Hard subprocess deadline for redaction or the complete run-end upload.")

(define-setting :telemetry-queue-limit (integer-setting)
  :label "Telemetry queue bound" :group :telemetry :scope :durable
  :minimum 1 :maximum 512 :default 128
  :documentation "Maximum in-memory spans; overflow discards the oldest spans.")
