(in-package #:autolith)

;;;; -- Public Model Catalog --

(defstruct (model-catalog (:constructor model-catalog-create))
  "Last-good public membership, cache and refresh lifecycle under an owner's lock."
  cache-root (lock (bt:make-lock "model catalog"))
  identifiers providers loaded fetched-at attempt-at thread timer
  (wakeup (bt:make-condition-variable :name "model catalog timer"))
  retired-threads)

(defgeneric model-catalog-context-active-p (context)
  (:documentation "Check CONTEXT's authorization epoch while holding the catalog lock."))

(defgeneric model-catalog-context-fetch (context)
  (:documentation "Fetch public JSON through CONTEXT's authorized, bounded transport."))

(-> model-catalog-known-p (model-catalog t) boolean)
(defun model-catalog-known-p (catalog identifier)
  "Return true only for an exact ID in CATALOG's validated public membership."
  (and (model-catalog-identifiers catalog)
       (gethash identifier (model-catalog-identifiers catalog)) t))

(defparameter *model-catalog-source* "https://models.dev/api.json"
  "Fixed public catalog source, never a provider discovery endpoint.")

(defparameter *model-catalog-maximum-bytes* (* 16 1024 1024)
  "Maximum UTF-8 size of the public catalog.")

(defvar *model-catalog-fetch-function* nil
  "Scoped test transport. Receives an authorized context and returns catalog JSON.")

(defparameter *model-catalog-poll-seconds* 60
  "Interval for checking the daily refresh and failure backoff, including idle sessions.")

(defparameter *model-catalog-request-seconds* 10
  "Whole-request deadline across connection, headers and streaming body reads.")

(-> model-catalog--timer
    (model-catalog &key (:context t) (:fetch (option function)) (:interval real))
    null)
(defun model-catalog--timer (catalog &key context fetch interval)
  "Wake periodically while CONTEXT remains active; retirement interrupts the wait."
  (let ((*model-catalog-fetch-function* fetch))
    (loop
      (bt:with-lock-held ((model-catalog-lock catalog))
        (unless (model-catalog-context-active-p context) (return))
        (bt:condition-wait (model-catalog-wakeup catalog)
                           (model-catalog-lock catalog) :timeout interval)
        (unless (model-catalog-context-active-p context) (return)))
      (model-catalog-ensure catalog context)))
  nil)

(define-condition model-catalog-invalid (error)
  ((reason :initarg :reason :reader model-catalog-invalid-reason
           :documentation "A fixed description of the invalid public input."))
  (:documentation "The public catalog or cache does not satisfy its schema.")
  (:report (lambda (condition stream)
             (format stream "Invalid public model catalog: ~A."
                     (model-catalog-invalid-reason condition)))))

(-> model-catalog--id-p (t) boolean)
(defun model-catalog--id-p (value)
  "Accept exact printable ASCII IDs, including internal spaces, without normalization."
  (and (stringp value) (<= 1 (length value) 256)
       (not (char= (char value 0) #\Space))
       (not (char= (char value (1- (length value))) #\Space))
       (every (lambda (character) (<= 32 (char-code character) 126)) value) t))

(-> model-catalog--reject (string) nil)
(defun model-catalog--reject (reason)
  "Reject public data with a fixed diagnostic, without echoing arbitrary input."
  (error 'model-catalog-invalid :reason reason))

(-> model-catalog--limits ((integer 0)) json-limits)
(defun model-catalog--limits (maximum)
  "Bound JSON parsing and encoding, including ignored public metadata."
  (make-json-limits :maximum-characters maximum :maximum-depth 32
                   :maximum-nodes 2000000 :maximum-string-characters maximum
                   :maximum-aggregate-string-characters maximum
                   :maximum-object-key-characters 4096 :maximum-object-members 100000
                   :maximum-array-elements              100000))

(-> model-catalog-parse (string) (values hash-table hash-table))
(defun model-catalog-parse (text)
  "Return the exact native/canonical ID set and validated models.dev provider records.
Every provider has a models object. Every model record has an ID equal to its key.
Optional canonical_model_id must be a valid ID. Other public metadata is ignored."
  (unless (and (<= (length text) *model-catalog-maximum-bytes*)
               (<= (length (babel:string-to-octets text :encoding :utf-8))
                   *model-catalog-maximum-bytes*))
    (model-catalog--reject "size limit"))
  (let ((root        (json-decode text :limits (model-catalog--limits
                                                *model-catalog-maximum-bytes*)))
        (identifiers (make-hash-table :test #'equal))
        (entries     0))
    (unless (and (hash-table-p root) (plusp (hash-table-count root)))
      (model-catalog--reject "provider object required"))
    (maphash
     (lambda (provider record)
       (declare (ignore provider))
       (unless (and (hash-table-p record) (hash-table-p (gethash "models" record)))
         (model-catalog--reject "models object required"))
       (maphash
        (lambda (key model)
          (incf entries)
          (unless (and (<= entries 100000) (model-catalog--id-p key)
                       (hash-table-p model) (equal key (gethash "id" model)))
            (model-catalog--reject "invalid model record"))
          (setf (gethash key identifiers) t)
          (multiple-value-bind (canonical present) (gethash "canonical_model_id" model)
            (when present
              (unless (model-catalog--id-p canonical)
                (model-catalog--reject "invalid canonical ID"))
              (setf (gethash canonical identifiers) t)))
          (when (> (hash-table-count identifiers) 100000)
            (model-catalog--reject "ID count limit")))
        (gethash "models" record)))
     root)
    (unless (plusp (hash-table-count identifiers))
      (model-catalog--reject "empty model catalog"))
    (values identifiers root)))

(-> model-catalog--path (model-catalog) pathname)
(defun model-catalog--path (controller)
  "Locate the shared last-good public catalog cache."
  (merge-pathnames "models/models-dev.json"
                   (model-catalog-cache-root controller)))

(-> model-catalog--read-file (pathname (integer 0)) string)
(defun model-catalog--read-file (path maximum)
  "Read UTF-8 only after checking the file byte bound."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let ((size (file-length stream)))
      (when (> size maximum) (model-catalog--reject "cache size limit"))
      (let ((bytes (make-array size :element-type '(unsigned-byte 8))))
        (unless (= (read-sequence bytes stream) size)
          (model-catalog--reject "incomplete cache"))
        (babel:octets-to-string bytes :encoding :utf-8)))))

(-> model-catalog--cache-limit () (integer 0))
(defun model-catalog--cache-limit ()
  "Allow JSON string escaping plus the fixed cache metadata envelope."
  (+ (* 2 *model-catalog-maximum-bytes*) 1024))

(-> model-catalog--load (model-catalog) null)
(defun model-catalog--load (controller)
  "Load and validate the cache once, only while holding consent and its lock."
  (unless (model-catalog-loaded controller)
    (setf (model-catalog-loaded controller) t)
    ; mallet:suppress no-ignore-errors -- a missing, unreadable or damaged cache leaves the catalog cold
    (ignore-errors
      (let* ((maximum (model-catalog--cache-limit))
             (cache   (json-decode (model-catalog--read-file
                                    (model-catalog--path controller) maximum)
                                  :limits (model-catalog--limits maximum)))
             (time    (and (hash-table-p cache) (gethash "fetched_at" cache)))
             (body    (and (hash-table-p cache) (gethash "body" cache))))
        (unless (and (hash-table-p cache)
                     (equal (gethash "source" cache) *model-catalog-source*)
                     (eql (gethash "version" cache) 1)
                     (typep time '(integer 0)) (<= time (get-universal-time))
                     (stringp body))
          (model-catalog--reject "cache metadata mismatch"))
        (multiple-value-bind (identifiers providers) (model-catalog-parse body)
          (setf (model-catalog-identifiers controller) identifiers
                (model-catalog-providers controller) providers
                (model-catalog-fetched-at controller) time)))))
  nil)

(-> model-catalog--publish (model-catalog string (integer 0)) null)
(defun model-catalog--publish (controller body time)
  "Atomically publish one validated last-good catalog with source and time metadata."
  (let* ((path      (model-catalog--path controller))
         (temporary (make-pathname :name (string-downcase
                                          (format nil "models-dev-~{~2,'0X~}"
                                                  (coerce (ironclad:random-data 8) 'list)))
                                   :type "tmp" :defaults path))
         (maximum   (model-catalog--cache-limit))
         (content   (json-encode (json-object "version" 1 "source" *model-catalog-source*
                                            "fetched_at" time "body" body)
                                 :limits (model-catalog--limits maximum))))
    (ensure-directories-exist path)
    (unwind-protect
         (progn
           (with-open-file (stream temporary :direction ':output :if-exists ':error
                                             :external-format :utf-8)
             (write-string content stream) (finish-output stream))
           (uiop:rename-file-overwriting-target temporary path))
      (when (probe-file temporary) (delete-file temporary))))
  nil)

(-> model-catalog--http-fetch () (option string))
(defun model-catalog--http-fetch ()
  "Read one anonymous, complete, byte-bounded response in the current process.
Native transports without Lisp deadlines must run only in a supervised child."
  (handler-case
      (sb-sys:with-deadline (:seconds *model-catalog-request-seconds*)
        (multiple-value-bind (stream status)
            (platform-public-http-response *platform* *model-catalog-source*)
          (unwind-protect
               (when (eql status 200)
                 (let ((bytes (make-array 0 :element-type '(unsigned-byte 8)
                                           :adjustable t :fill-pointer 0)))
                   (loop for byte = (read-byte stream nil nil)
                         while byte
                         do (when (>= (length bytes) *model-catalog-maximum-bytes*)
                              (model-catalog--reject "size limit"))
                            (vector-push-extend byte bytes))
                   (babel:octets-to-string bytes :encoding :utf-8)))
            (when (streamp stream) (close stream)))))
    (sb-sys:deadline-timeout () nil)))

(-> model-catalog--worker-fetch (&optional (option configuration)) (option string))
(defun model-catalog--worker-fetch (&optional configuration)
  "Fetch within one whole-request deadline, supervising native blocking transports."
  (if (platform-http-supervision-required-p *platform*)
      (let* ((configuration (or configuration (make-configuration :provider-validation-p nil)))
             (record        (supervised-process-create)))
        (supervised-process-request
         record (list *model-catalog-source* *model-catalog-maximum-bytes*)
         :seconds *model-catalog-request-seconds*
         :start   (lambda (install)
                    (let ((core (config :active-image-core configuration)))
                      (when (and core (probe-file core))
                        (funcall install
                                 (uiop:launch-program
                                  (list (namestring sb-ext:*runtime-pathname*) "--noinform"
                                        "--core" (namestring core) "--end-runtime-options"
                                        (namestring (config :source-root configuration))
                                        "--model-catalog-worker")
                                  :input :stream :output :stream :error-output nil
                                  :external-format :utf-8
                                  :environment     (supervised-process-environment))))))))
      (model-catalog--http-fetch)))

(-> model-catalog--worker-main () nil)
(defun model-catalog--worker-main ()
  "Serve one private anonymous catalog request without loading user state."
  (let ((input *standard-input*) (output *standard-output*))
    (let ((*standard-output* (make-broadcast-stream))
          (*error-output*    (make-broadcast-stream))
          (*trace-output*    (make-broadcast-stream))
          (*debug-io*        (make-two-way-stream (make-string-input-stream "") (make-broadcast-stream))))
      (let ((result
              (handler-case
                  (let ((request (supervised-process-read-data input)))
                    (when (and (listp request) (= (length request) 2)
                               (stringp (first request))
                               (typep (second request) `(integer 0 ,*model-catalog-maximum-bytes*)))
                      (let ((*model-catalog-source*        (first request))
                            (*model-catalog-maximum-bytes* (second request)))
                        (model-catalog--http-fetch))))
                (error () nil))))
        (let ((*print-readably* t) (*print-pretty* nil))
          (write result :stream output) (terpri output) (finish-output output)))))
  (uiop:quit 0))

(-> model-catalog--refresh (model-catalog t (option function)) null)
(defun model-catalog--refresh (controller context fetch)
  "Fetch outside the owner's lock; publish only while the original CONTEXT is active."
  (let ((body
          ; mallet:suppress no-ignore-errors -- any transport failure keeps the last-good catalog
          (ignore-errors (if fetch (funcall fetch context)
                             (model-catalog-context-fetch context)))))
    (multiple-value-bind (identifiers providers)
        (and (stringp body)
             (handler-case (model-catalog-parse body)
               ((or model-catalog-invalid json-error) ()
                 nil)))
      (bt:with-lock-held ((model-catalog-lock controller))
        (when (model-catalog-context-active-p context)
          (let ((time (get-universal-time)))
            (setf (model-catalog-attempt-at controller) time)
            (when identifiers
              ;; Keep valid membership even when the last-good disk cache cannot be updated.
              ; mallet:suppress no-ignore-errors -- the disk cache write is best effort
              (ignore-errors (model-catalog--publish controller body time))
              (setf (model-catalog-identifiers controller) identifiers
                    (model-catalog-providers controller) providers
                    (model-catalog-fetched-at controller) time)))))))
  nil)

(-> model-catalog-ensure (model-catalog t) null)
(defun model-catalog-ensure (controller context)
  "Load once and start one refresh; back off five minutes after a failed completion."
  (bt:with-lock-held ((model-catalog-lock controller))
    (when (model-catalog-context-active-p context)
      (model-catalog--load controller)
      (unless (model-catalog-timer controller)
        (let ((fetch    *model-catalog-fetch-function*)
              (interval *model-catalog-poll-seconds*))
          (setf (model-catalog-timer controller)
                (bt:make-thread (lambda () (model-catalog--timer controller :context context
                                                                          :fetch fetch :interval interval))
                                :name "model catalog refresh timer"))))
      (let ((thread (model-catalog-thread controller))
            (now    (get-universal-time)))
        (when (and (or (null thread) (not (bt:thread-alive-p thread)))
                   (or (null (model-catalog-fetched-at controller))
                       (>= (- now (model-catalog-fetched-at controller)) 86400))
                   (or (null (model-catalog-attempt-at controller))
                       (>= (- now (model-catalog-attempt-at controller)) 300)))
          (let ((fetch *model-catalog-fetch-function*))
            (setf (model-catalog-thread controller)
                  (bt:make-thread
                   (lambda () (model-catalog--refresh controller context fetch))
                   :name "public model catalog")))))))
  nil)

(-> model-catalog-retire (model-catalog) null)
(defun model-catalog-retire (catalog)
  "Retire CATALOG's threads under its lock after revoking their context. Join outside it."
  (when (model-catalog-timer catalog)
    (push (model-catalog-timer catalog) (model-catalog-retired-threads catalog))
    (setf (model-catalog-timer catalog) nil)
    (bt:condition-notify (model-catalog-wakeup catalog)))
  (when (model-catalog-thread catalog)
    (push (model-catalog-thread catalog) (model-catalog-retired-threads catalog))
    (setf (model-catalog-thread catalog) nil))
  nil)

(-> model-catalog-join (model-catalog) null)
(defun model-catalog-join (controller)
  "Join revoked catalog threads outside the consent lock before shutdown or saving."
  (let ((threads (bt:with-lock-held ((model-catalog-lock controller))
                   (copy-list (model-catalog-retired-threads controller)))))
    (dolist (thread threads)
      ; mallet:suppress no-ignore-errors -- joining a retired thread is best-effort shutdown cleanup
      (ignore-errors (bt:join-thread thread)))
    (bt:with-lock-held ((model-catalog-lock controller))
      (setf (model-catalog-retired-threads controller)
            (set-difference (model-catalog-retired-threads controller)
                            threads :test #'eq))))
  nil)
