(in-package #:autolith)

;;;; -- Bounded One-Shot Processes --

(defstruct (supervised-process (:constructor supervised-process-create))
  "One child and its I/O thread, with serialized synchronous teardown."
  process thread (lock (bt:make-lock "supervised process lifecycle")) stopped-p)

(-> supervised-process-environment () list)
(defun supervised-process-environment ()
  "Pass only locale, runtime search paths and certificate paths to a one-shot child.
PATH is required when a saved core reopens native libraries by their basename."
  (cons "LANG=en_US.UTF-8"
        (loop for name in '("PATH" "SYSTEMROOT" "WINDIR" "TEMP" "TMP" "TMPDIR" "SBCL_HOME"
                            "SSL_CERT_FILE" "SSL_CERT_DIR" "NIX_SSL_CERT_FILE")
              for value = (uiop:getenv name)
              when value collect (concatenate 'string name "=" value))))

(-> git-process-environment (&key (:overrides list) (:absolute-xdg-p boolean)) list)
(defun git-process-environment (&key overrides absolute-xdg-p)
  "Assemble process-local Git bindings, excluding inherited repository selectors.
OVERRIDES take precedence. ABSOLUTE-XDG-P restricts forwarded XDG_CONFIG_HOME
when the caller also uses it to grant sandbox read access."
  (append overrides
          (loop for name in '("PATH" "HOME" "USERPROFILE" "SYSTEMROOT" "WINDIR"
                              "TMPDIR" "TMP" "TEMP" "LANG" "LC_ALL" "XDG_CONFIG_HOME")
                for value = (uiop:getenv name)
                for prefix = (concatenate 'string name "=")
                when (and value
                          (or (not absolute-xdg-p) (not (string= name "XDG_CONFIG_HOME"))
                              (and (plusp (length value))
                                   (uiop:absolute-pathname-p (uiop:parse-native-namestring value))))
                          (notany (lambda (binding) (uiop:string-prefix-p prefix binding)) overrides))
                  collect (concatenate 'string prefix value))))

(-> supervised-process-read-data (stream) t)
(defun supervised-process-read-data (stream)
  "Read one bounded printer-compatible data response without owning STREAM.

The process owner retains deadline, EOF handling and teardown authority."
  (sexp-config:read-next-source
   stream
   (make-source-grammar
    :label                     "One-shot process data"
    :maximum-depth 128 :maximum-nodes 65536 :maximum-string-characters (* 16 1024 1024)
    :improper-lists-permitted-p t :readable-strings-permitted-p t
    :octet-vectors-permitted-p t :qualified-common-lisp-symbols-permitted-p t
    :read-default-float-format 'single-float
    :allowed-atom-predicate
    (lambda (value)
      (or (null value) (eq value t) (keywordp value) (stringp value)
          (numberp value) (typep value '(vector (unsigned-byte 8))))))
   :maximum-octets (* 32 1024 1024)))

(-> supervised-process-exchange (t t) t)
(defun supervised-process-exchange (process request)
  "Write REQUEST and read one response without reader evaluation."
  (handler-case
      (progn
        (let ((*print-readably* t) (*print-pretty* nil)
              (input (uiop:process-info-input process)))
          (write request :stream input)
          (terpri input)
          (finish-output input)
          (close input))
        (supervised-process-read-data (uiop:process-info-output process)))
    (error () nil)))

(-> supervised-process-terminate (t) null)
(defun supervised-process-terminate (process)
  "Kill and reap a UIOP child; its request owner closes streams after joining I/O."
  (when process
    (uiop:terminate-process process :urgent t)
    (uiop:wait-process process))
  nil)

(-> supervised-process--close-streams (t) null)
(defun supervised-process--close-streams (process)
  "Abort protocol streams after their I/O thread has finished."
  (when process
    (dolist (stream (list (uiop:process-info-error-output process)
                         (uiop:process-info-input process)
                         (uiop:process-info-output process)))
      (when (streamp stream)
        (handler-case (close stream :abort t)
          (stream-error () nil)))))
  nil)

(-> supervised-process-stop (supervised-process &key (:terminate function)) null)
(defun supervised-process-stop (record &key (terminate #'supervised-process-terminate))
  "Serialize child termination for RECORD using TERMINATE."
  (bt:with-lock-held ((supervised-process-lock record))
    (unless (supervised-process-stopped-p record)
      (funcall terminate (supervised-process-process record))
      (setf (supervised-process-stopped-p record) t)))
  nil)

(-> supervised-process-request
    (supervised-process t &key (:seconds real) (:start function)
                              (:active-p (option function)) (:accept (option function))
                              (:stop function) (:complete (option function))) t)
(defun supervised-process-request (record request &key seconds start active-p accept
                                                      (stop #'supervised-process-stop) complete)
  "Exchange REQUEST within one deadline, including startup and body I/O.
START receives an installer function accepting the newly launched process. Owners
must install its process and I/O thread and register RECORD atomically with their
admission check. ACTIVE-P checks continuing authority. ACCEPT, when supplied,
receives a response thunk to call under the owner's authorization lock. STOP kills
and reaps the child. The owner joins I/O before closing streams; COMPLETE then
releases owner records."
  (let ((deadline    (+ (get-internal-real-time)
                        (ceiling (* seconds internal-time-units-per-second))))
        (result nil) (done nil)
        (result-lock (bt:make-lock "supervised process response")))
    (unwind-protect
         (progn
           (funcall start
                    (lambda (process)
                      (setf (supervised-process-process record) process
                            (supervised-process-thread record)
                            (bt:make-thread
                             (lambda ()
                               (let ((response
                                       (handler-case
                                           (sb-sys:with-deadline
                                               (:seconds
                                                (max 0
                                                     (/ (- deadline (get-internal-real-time))
                                                        internal-time-units-per-second)))
                                             (supervised-process-exchange process request))
                                         ((or sb-sys:deadline-timeout error) () nil))))
                                 (bt:with-lock-held (result-lock)
                                   (setf result response done t))))
                             :name "supervised subprocess I/O"))))
           (when (supervised-process-process record)
             (loop
               (when (bt:with-lock-held (result-lock) done) (return))
               (unless (or (null active-p) (funcall active-p)) (return))
               (when (>= (get-internal-real-time) deadline) (return))
               (sleep 0.005))
             (when (< (get-internal-real-time) deadline)
               (let ((response (lambda ()
                                 (bt:with-lock-held (result-lock)
                                  (when (and done (< (get-internal-real-time) deadline)) result)))))
                 (if accept
                     (funcall accept response)
                     (when (or (null active-p) (funcall active-p))
                       (funcall response)))))))
      (unwind-protect
           (funcall stop record)
        (unwind-protect
             (when (supervised-process-thread record)
               (bt:join-thread (supervised-process-thread record)))
          (unwind-protect
               (supervised-process--close-streams (supervised-process-process record))
            (when complete (funcall complete record))))))))
