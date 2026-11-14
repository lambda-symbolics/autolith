(in-package #:autolith)

;;;; -- World Server Restart Tests --

(-> world-restart-tests--ended-process-id () (integer 1))
(defun world-restart-tests--ended-process-id ()
  "Return the process ID of a child that has already exited."
  (let ((process (uiop:launch-program (list (namestring sb-ext:*runtime-pathname*) "--version")
                                      :output nil :error-output nil)))
    (uiop:wait-process process)
    (uiop:process-info-pid process)))

(-> test-world-server-restart-marker () null)
(defun test-world-server-restart-marker ()
  "Test a restart marker keeps the world server running while its launcher lives, and is dropped once stale."
  (with-test-configuration (configuration)
    (configuration-ensure-directories configuration)
    (let ((pathname (world-server-restart-pathname configuration))
          (now      (get-universal-time))
          (ended    (world-restart-tests--ended-process-id)))
      (ensure-directories-exist pathname)
      (test-assert (not (world-server-running-p configuration))
                   "no world server runs without a lease or a restart marker")
      (loop for (form running-p description)
              in `(((:autolith-world-restart :version 1 :launcher-pid ,(current-process-id)
                     :created-at ,now :generation-id "g-test")
                    t "a marker whose launcher lives says the server is restarting")
                   ((:autolith-world-restart :version 1 :launcher-pid ,ended
                     :created-at ,now :generation-id "g-test")
                    nil "a marker whose launcher ended is stale")
                   ((:autolith-world-restart :version 1 :launcher-pid ,(current-process-id)
                     :created-at ,(- now 3600) :generation-id "g-test")
                    nil "a marker older than its bound is stale")
                   ((:something-else)
                    nil "an unreadable marker is stale"))
            do (snapshot-write pathname form)
               (test-assert (and (eq (world-server-running-p configuration) running-p)
                                 (eq (not (null (probe-file pathname))) running-p))
                            (format nil "~A, and ~:[is removed~;stays~]" description running-p))
               (world-server--restart-unmark configuration))))
  nil)

(-> world-restart-tests--submit (cons string &key (:until function)) list)
(defun world-restart-tests--submit (entry text &key until)
  "Submit TEXT to ENTRY's channel as a structured client and return its events, oldest first, once UNTIL accepts them or a minute passed."
  (let* ((lock       (make-lock "Autolith restart test events"))
         (events     nil)
         (connection (localgroup-client-connect
                      entry :name           "restart test"
                            :approves-p     nil
                            :event-function (lambda (event)
                                              (with-lock-held (lock)
                                                (push event events))))))
    (flet ((events ()
             "Return the events so far, oldest first."
             (with-lock-held (lock)
               (reverse events))))
      (unwind-protect
           (progn
             (localgroup-client-call connection ':submit :text text)
             (task-tests--wait-until (lambda () (funcall until (events))) 60)
             (events))
        (localgroup-client-close connection)))))

(-> world-restart-tests--record-state (pathname list) list)
(defun world-restart-tests--record-state (pathname keys)
  "Read only small stage records and return their whitelisted scalar fields."
  (if (probe-file pathname)
      (with-open-file (stream pathname :element-type '(unsigned-byte 8))
        (if (> (file-length stream) 16384)
            (list :file (file-namestring pathname) :status ':oversized)
            (let ((record (snapshot-read pathname)))
              (list :file (file-namestring pathname)
                    :fields
                    (loop for key in keys
                          for value = (getf (rest record) key)
                          when (or (integerp value)
                                   (and (stringp value) (<= (length value) 128)))
                            append (list key value))))))
      (list :file (file-namestring pathname) :status ':absent)))

(-> world-restart-tests--file-state (pathname) list)
(defun world-restart-tests--file-state (pathname)
  "Return a stage file's name and byte length without reading its contents."
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (list :file (file-namestring pathname) :bytes (file-length stream))))

(-> world-restart-tests--phase-pathname (configuration) pathname)
(defun world-restart-tests--phase-pathname (configuration)
  "Return this fixture's independently written checkpoint phase log."
  (merge-pathnames "checkpoint-phases.sexp" (config :state-root configuration)))

(-> world-restart-tests--write-phase-probe (stream pathname) null)
(defun world-restart-tests--write-phase-probe (stream pathname)
  "Write bounded synchronous checkpoint and SBCL teardown instrumentation."
  (write
   `(let* ((prepare (symbol-function 'checkpoint--prepare-saver))
           (gc (symbol-function 'sb-ext:gc))
           (save (symbol-function 'sb-ext:save-lisp-and-die))
           (checkpoint (symbol-function 'sbcl-generations:checkpoint-restart-save-and-exit))
           (deinit (symbol-function 'sb-impl::deinit))
           (save-hooks sb-ext:*save-hooks*)
           (definitions (list (cons 'checkpoint--prepare-saver prepare)
                              (cons 'sb-ext:gc gc) (cons 'sb-ext:save-lisp-and-die save)
                              (cons 'sbcl-generations:checkpoint-restart-save-and-exit checkpoint)
                              (cons 'sb-impl::deinit deinit)))
           (records 0) (bytes 0) (armed-p nil) (deinit-p nil) (ordinal 0)
           (busy-p nil) (failed-p nil) (truncated-p nil)
           (requester nil) (worker-kind "") (active-p nil))
      (declare (notinline write close))
      (labels ((thread-state (thread)
                 "Return a bounded name and liveness pair, never a raw thread."
                 (when (typep thread 'sb-thread:thread)
                   (let ((name (or (sb-thread:thread-name thread) "")))
                     (list (subseq name 0 (min 64 (length name)))
                           (not (null (sb-thread:thread-alive-p thread)))))))

               (system-thread (name)
                 "Read an optional implementation thread without interning a symbol."
                 (let ((symbol (find-symbol name "SB-IMPL")))
                   (when (and symbol (boundp symbol)
                              (typep (symbol-value symbol) 'sb-thread:thread))
                     (symbol-value symbol))))

               (waiting-lock (thread)
                 "Normalize plain and deadline-paired thread wait identities."
                 (when (typep thread 'sb-thread:thread)
                   (let ((waiting (sb-thread::thread-waiting-for thread)))
                     (if (consp waiting) (rest waiting) waiting))))

               (record-phase (phase &optional target)
                 "Append at most 64 records and 32 KiB, explicitly marking budget exhaustion."
                 (when (and (not busy-p) (not failed-p) (not truncated-p))
                   (setf busy-p t)
                   (unwind-protect
                        (handler-case
                            (let* ((*print-pretty* nil)
                                   (*print-level*  nil)
                                   (*print-length* nil)
                                   (waiting        (waiting-lock target))
                                   (scheduler      sb-impl::*scheduler-lock*)
                                   (make-thread    sb-thread::*make-thread-lock*)
                                   (record
                                     (list :phase phase :pid (current-process-id) :gc ordinal
                                           :thread                  (first (thread-state sb-thread:*current-thread*))
                                           :worker-kind worker-kind :active-p active-p
                                           :target                  (thread-state target)
                                           :scheduler-owned-p       (eq (sb-thread:mutex-owner scheduler) requester)
                                           :target-scheduler-wait-p (eq waiting scheduler)
                                           :make-owned-p            (eq (sb-thread:mutex-owner make-thread) requester)
                                           :target-make-wait-p      (eq waiting make-thread)
                                           :timer                   (thread-state (system-thread "*TIMER-THREAD*"))
                                           :finalizer               (thread-state (system-thread "*FINALIZER-THREAD*"))))
                                   (size           (+ 2 (length (sb-ext:string-to-octets
                                                                 (write-to-string record :readably t :pretty nil)
                                                                 :external-format ':utf-8)))))
                              ;; Reserve one record and 512 bytes for a visible terminal marker.
                              (when (or (>= records 63) (> (+ bytes size) (- 32768 512)))
                                (setf record (list :phase  ':probe-truncated
                                                   :reason (if (>= records 63) ':record-limit ':byte-limit)
                                                   :records records :bytes bytes)
                                      size (+ 2 (length (sb-ext:string-to-octets
                                                         (write-to-string record :readably t :pretty nil)
                                                         :external-format ':utf-8)))
                                      truncated-p t))
                              (incf records)
                              (incf bytes size)
                              (with-open-file (output ,pathname :direction        ':output
                                                               :if-exists         (if (= records 1) ':supersede ':append)
                                                               :if-does-not-exist ':create
                                                               :external-format   ':utf-8)
                                (write record :stream output :readably t :pretty nil)
                                (terpri output)))
                          (error (condition)
                            (setf failed-p t)
                            (handler-case
                                (format *error-output* "~&CHECKPOINT-PROBE-WRITER-FAILURE ~S ~S~%"
                                        phase (type-of condition))
                              (error () nil))))
                     (setf busy-p nil))))

               (restore-probe ()
                 "Restore all wrapped definitions and hooks before streams close or on unwind."
                 (setf armed-p nil deinit-p nil)
                 (sb-ext:without-package-locks
                   (dolist (definition definitions)
                     (setf (symbol-function (first definition)) (rest definition)))
                   (setf sb-ext:*save-hooks* save-hooks)))

               (requester-p ()
                 "Return whether this call belongs to the prepared checkpoint thread."
                 (and armed-p (eq requester sb-thread:*current-thread*)))

               (make-itimer-candidate (thread-symbol handle-symbol close-timer)
                 "Build the test-only candidate: terminate locked, join unlocked, then close locked."
                 (lambda ()
                   (let ((thread (sb-int:with-system-mutex (sb-impl::*scheduler-lock*)
                                   (let ((thread (symbol-value thread-symbol)))
                                     (when thread (sb-thread:terminate-thread thread))
                                     thread))))
                     (when thread (sb-thread:join-thread thread :default nil))
                     (sb-int:with-system-mutex (sb-impl::*scheduler-lock*)
                       (when (symbol-value handle-symbol)
                         (funcall close-timer (symbol-value handle-symbol))
                         (setf (symbol-value handle-symbol) nil))))))

               (install-step (symbol phase)
                 "Bracket one implementation call after the original save hooks finish."
                 (when (and symbol (fboundp symbol))
                    (let* ((original (symbol-function symbol))
                           (delegate
                             (if (eq phase ':itimer)
                                 (let* ((thread-symbol (find-symbol "*TIMER-THREAD*" "SB-IMPL"))
                                        (handle-symbol (find-symbol "*WAITABLE-TIMER-HANDLE*" "SB-IMPL"))
                                        (package       (find-package "SB-WIN32"))
                                        (close-symbol  (and package (find-symbol "OS-CLOSE-WTIMER" package))))
                                   (unless (and thread-symbol (boundp thread-symbol)
                                                handle-symbol (boundp handle-symbol)
                                                close-symbol (fboundp close-symbol))
                                     (error 'platform-error :operation ':phase-probe
                                            :message                   "Windows timer candidate bindings are unavailable."))
                                   (make-itimer-candidate thread-symbol handle-symbol (symbol-function close-symbol)))
                                 original))
                           (enter    (intern (concatenate 'string (symbol-name phase) "-ENTER") "KEYWORD"))
                           (exit     (intern (concatenate 'string (symbol-name phase) "-EXIT") "KEYWORD"))
                           (unwind   (intern (concatenate 'string (symbol-name phase) "-UNWIND") "KEYWORD")))
                     (push (cons symbol original) definitions)
                     (setf (symbol-function symbol)
                           (lambda (&rest arguments)
                             (if (and deinit-p (requester-p))
                                 (let ((target     (when (member phase '(:join-thread :primitive-join))
                                                     (first arguments)))
                                       (returned-p nil))
                                   (record-phase enter target)
                                   (if (eq phase ':stream-deinit)
                                       (progn (restore-probe) (apply original arguments))
                                       (unwind-protect
                                             (multiple-value-prog1 (apply delegate arguments)
                                              (setf returned-p t)
                                              (record-phase exit target))
                                         (unless returned-p (record-phase unwind target)))))
                                 (apply original arguments))))))))
        (sb-ext:without-package-locks
          (dolist (step '(("ITIMER-EMULATION-DEINIT" "SB-IMPL" :itimer)
                          ("JOIN-THREAD" "SB-THREAD" :join-thread)
                          ("PRIMITIVE-JOIN" "SB-THREAD" :primitive-join)
                          ("%DISPOSE-THREAD-STRUCTS" "SB-THREAD" :dispose)
                          ("FINALIZER-THREAD-STOP" "SB-IMPL" :finalizer-stop)
                          ("TUNE-IMAGE-FOR-DUMP" "SB-IMPL" :tune-image)
                          ("FLOAT-DEINIT" "SB-IMPL" :float-deinit)
                          ("PROFILE-DEINIT" "SB-IMPL" :profile-deinit)
                          ("FOREIGN-DEINIT" "SB-IMPL" :foreign-deinit)
                          ("REBUILD-CTYPE-HASHSETS" "SB-KERNEL" :ctype-rebuild)
                          ("DROP-ALL-HASH-CACHES" "SB-IMPL" :cache-drop)
                          ("OS-DEINIT" "SB-IMPL" :os-deinit)
                          ("FINALIZERS-DEINIT" "SB-IMPL" :finalizers-deinit)
                          ("REBUILD-PATHNAME-CACHE" "SB-IMPL" :pathname-rebuild)
                          ("RESTORE-CPU-SPECIFIC-ROUTINES" "SB-VM" :cpu-restore)
                          ("STREAM-DEINIT" "SB-IMPL" :stream-deinit)))
            (install-step (find-symbol (first step) (second step)) (third step)))
          (setf (symbol-function 'checkpoint--prepare-saver)
                (lambda (worker generation)
                  (setf armed-p t deinit-p nil ordinal 0 requester sb-thread:*current-thread*
                        worker-kind (symbol-name (type-of worker))
                        active-p (not (null *active-application*)))
                  (record-phase ':prepare-enter)
                  (multiple-value-prog1 (funcall prepare worker generation)
                    (record-phase ':prepare-exit)))
                (symbol-function 'sb-ext:gc)
                (lambda (&rest arguments)
                  (if (requester-p)
                      (let ((returned-p nil))
                        (incf ordinal)
                        (record-phase ':gc-enter)
                        (unwind-protect
                             (multiple-value-prog1 (apply gc arguments)
                               (setf returned-p t)
                               (record-phase ':gc-exit))
                          (unless returned-p (record-phase ':gc-unwind))))
                      (apply gc arguments)))
                (symbol-function 'sb-ext:save-lisp-and-die)
                (lambda (&rest arguments)
                  (when (requester-p) (record-phase ':save-call-enter))
                  (unwind-protect (apply save arguments) (restore-probe)))
                (symbol-function 'sb-impl::deinit)
                (lambda (&rest arguments)
                  (unwind-protect (apply deinit arguments) (restore-probe)))
                (symbol-function 'sbcl-generations:checkpoint-restart-save-and-exit)
                (lambda (&rest arguments)
                  (unwind-protect (apply checkpoint arguments) (restore-probe)))))
        (setf sb-ext:*save-hooks*
              (append (list (lambda ()
                              (when (requester-p) (record-phase ':save-hooks-enter))))
                      save-hooks
                      (list (lambda ()
                              (when (requester-p)
                                (record-phase ':save-hooks-exit)
                                (setf deinit-p t))))))
        (values #'restore-probe (copy-list definitions) #'make-itimer-candidate)))
   :stream stream :readably t)
  (terpri stream)
  nil)

(-> world-restart-tests--phases (configuration) list)
(defun world-restart-tests--phases (configuration)
  "Read at most 64 phase records from a small log, tolerating an incomplete final record."
  (let ((pathname (world-restart-tests--phase-pathname configuration)))
    (if (probe-file pathname)
        (with-open-file (stream pathname)
          (if (> (file-length stream) 32768)
              (list :status ':oversized)
              (let ((*read-eval* nil)
                    (end         (list ':end)))
                (loop repeat 64
                      collect (let ((record (handler-case (read stream nil end)
                                              (end-of-file () end))))
                                (when (eq record end)
                                  (loop-finish))
                                record)))))
        (list :status ':absent))))

(-> world-restart-tests--diagnostics (configuration (integer 1) t) list)
(defun world-restart-tests--diagnostics (configuration old-pid process)
  "Collect bounded stage metadata without changing restart or lease state."
  (labels ((probe (function)
             "Keep an unavailable probe from hiding the checkpoint failure."
             (handler-case (funcall function)
               (error (condition)
                 (list :unavailable (type-of condition)))))

           (limited (pathnames)
             "Keep at most eight entries from a fixture-owned directory."
             (subseq pathnames 0 (min 8 (length pathnames)))))
    (list :original-pid       old-pid
          :original-alive-p   (probe (lambda () (platform-process-alive-p *platform* old-pid)))
          :owned-launch-pid   (probe (lambda () (when process
                                                 (platform-process-object-pid *platform* process))))
          :owned-tree-alive-p (probe (lambda () (when process
                                                 (platform-process-object-alive-p *platform* process))))
          :checkpoint-phases  (probe (lambda () (world-restart-tests--phases configuration)))
          :world-lease        (probe (lambda ()
                                       (multiple-value-list
                                        (platform-file-lock-owner *platform*
                                                                  (world-server-lock-pathname configuration)))))
          :restart-marker
          (probe (lambda ()
                   (let* ((marker (world-restart-tests--record-state
                                   (world-server-restart-pathname configuration)
                                   '(:version :launcher-pid :created-at :generation-id)))
                          (pid    (getf (getf marker :fields) :launcher-pid)))
                     (append marker (list :launcher-alive-p
                                          (and (typep pid '(integer 1))
                                               (platform-process-alive-p *platform* pid)))))))
          :restart-envelopes
          (probe (lambda ()
                   (mapcar (lambda (pathname)
                             (world-restart-tests--record-state pathname '(:version :identifier :created-at)))
                           (limited (directory (merge-pathnames "restart-pointers/*.sexp"
                                                                (config :state-root configuration)))))))
          :generation-files
          (probe (lambda ()
                   (mapcar (lambda (directory)
                             (list :generation (first (last (pathname-directory directory)))
                                   :files      (mapcar #'world-restart-tests--file-state
                                                       (limited (uiop:directory-files directory)))))
                           (limited (uiop:subdirectories
                                     (merge-pathnames "generations/" (config :data-root configuration))))))))))

(-> world-restart-tests--check-phase-emission-failures () null)
(defun world-restart-tests--check-phase-emission-failures ()
  "Inject reentry and I/O failures without leaving wrapped definitions installed."
  (with-test-configuration (configuration)
    (configuration-ensure-directories configuration)
    (dolist (scenario '(:gc :write :close))
      (let* ((pathname  (world-restart-tests--phase-pathname configuration))
             (operation (if (eq scenario ':close) 'close 'write))
             (original  (symbol-function operation))
             (attempts  0)
             (restore   nil))
        (when (probe-file pathname) (delete-file pathname))
        (test-call-with-function-replacements
         (list (list 'checkpoint--prepare-saver
                     (lambda (worker generation)
                       (declare (ignore worker generation))
                       nil)))
         (lambda ()
           (unwind-protect
                (progn
                  (let* ((source      (with-output-to-string (stream)
                                        (world-restart-tests--write-phase-probe stream pathname)))
                         (*read-eval* nil))
                    (setf restore (eval (read-from-string source))))
                  (sb-ext:without-package-locks
                    (test-call-with-function-replacements
                     (list (list operation
                                 (lambda (&rest arguments)
                                   (incf attempts)
                                   (case scenario
                                     (:gc
                                      (checkpoint--prepare-saver nil nil)
                                      (apply original arguments))
                                     (:write
                                      (write-string "(" (getf (rest arguments) :stream))
                                      (error 'platform-error :operation ':phase-probe
                                             :message                   "Injected partial-record write failure."))
                                     (:close
                                      (apply original arguments)
                                      (error 'platform-error :operation ':phase-probe
                                             :message                   "Injected record-close failure."))))))
                     (lambda ()
                       (checkpoint--prepare-saver nil nil)
                       (checkpoint--prepare-saver nil nil)))))
             (when restore (funcall restore)))))
        (test-assert (if (eq scenario ':gc)
                         (and (= attempts 4)
                              (equal (mapcar (lambda (record) (getf record :phase))
                                             (world-restart-tests--phases configuration))
                                     '(:prepare-enter :prepare-exit :prepare-enter :prepare-exit)))
                         (= attempts 1))
                     (format nil "Phase emission scenario ~S made ~D attempts; reentry is refused and I/O errors stop emission."
                             scenario attempts)))))
  nil)

(-> world-restart-tests--check-phase-probe () null)
(defun world-restart-tests--check-phase-probe ()
  "Test synchronous boundaries, restoration, hook order and bounded crash-readable records."
  (with-test-configuration (configuration)
    (configuration-ensure-directories configuration)
    (let* ((pathname             (world-restart-tests--phase-pathname configuration))
           (order                nil)
           (old-save-hooks       sb-ext:*save-hooks*)
           (old-after-gc-hooks   sb-ext:*after-gc-hooks*)
           (fail-gc-p            nil)
           (*active-application* nil)
           (symbols              '(checkpoint--prepare-saver sb-ext:gc sb-ext:save-lisp-and-die
                                   sbcl-generations:checkpoint-restart-save-and-exit)))
      (unwind-protect
           (sb-ext:without-package-locks
             (test-call-with-function-replacements
              (list (list 'checkpoint--prepare-saver
                          (lambda (worker generation)
                            (declare (ignore worker generation))
                            nil))
                    (list 'sb-ext:gc
                          (lambda (&rest arguments)
                            (declare (ignore arguments))
                            (when fail-gc-p
                              (error 'platform-error :operation ':phase-probe
                                     :message                   "Injected synchronous GC failure."))
                            (values ':collected ':second)))
                    (list 'sb-ext:save-lisp-and-die
                          (lambda (&rest arguments)
                            (declare (ignore arguments))
                            (mapc #'funcall sb-ext:*save-hooks*)
                            (values ':saved ':second)))
                    (list 'sbcl-generations:checkpoint-restart-save-and-exit
                          (lambda (&rest arguments)
                            (declare (ignore arguments))
                            (checkpoint--prepare-saver nil nil)
                            (sb-ext:gc :full t)
                            (sb-ext:save-lisp-and-die "unused"))))
              (lambda ()
                (let ((originals (mapcar #'symbol-function symbols)))
                  (dolist (scenario '(:normal :unwind :bounded))
                    (setf order nil fail-gc-p (eq scenario ':unwind)
                          sb-ext:*save-hooks* (list (lambda () (push ':first order))
                                                   (lambda () (push ':second order))))
                    (let* ((source      (with-output-to-string (stream)
                                          (world-restart-tests--write-phase-probe stream pathname)))
                           (*read-eval* nil)
                           (restore     (eval (read-from-string source))))
                      (unwind-protect
                           (case scenario
                             (:normal
                              (test-assert (equal (multiple-value-list (sb-ext:gc)) '(:collected :second))
                                           "dormant wrappers preserve multiple values")
                              (test-assert (not (probe-file pathname)) "GC probes are dormant before preparation")
                              (test-assert
                               (equal (multiple-value-list
                                       (sbcl-generations:checkpoint-restart-save-and-exit nil nil))
                                      '(:saved :second))
                               "the checkpoint wrapper preserves returning values")
                              (let ((records (world-restart-tests--phases configuration)))
                                (test-assert
                                 (and (equal (reverse order) '(:first :second))
                                      (equal (mapcar (lambda (record) (getf record :phase)) records)
                                             '(:prepare-enter :prepare-exit :gc-enter :gc-exit
                                               :save-call-enter :save-hooks-enter :save-hooks-exit))
                                      (every (lambda (record) (<= (length (getf record :thread)) 128)) records)
                                      (= (getf (third records) :gc) 1))
                                 "synchronous probes preserve hook order and bounded thread identity")
                                (with-open-file (stream pathname :direction ':output :if-exists ':append)
                                  (write-string "(" stream))
                                (test-assert (equal records (world-restart-tests--phases configuration))
                                             "collection tolerates an interrupted final record")))
                             (:unwind
                              (test-assert
                               (handler-case
                                   (progn (sbcl-generations:checkpoint-restart-save-and-exit nil nil) nil)
                                 (platform-error () t))
                               "GC failure propagates through both wrappers")
                              (test-assert
                               (equal (mapcar (lambda (record) (getf record :phase))
                                              (world-restart-tests--phases configuration))
                                      '(:prepare-enter :prepare-exit :gc-enter :gc-unwind))
                               "a synchronous GC nonlocal exit is distinct from return"))
                             (:bounded
                              (loop repeat 70 do (checkpoint--prepare-saver nil nil))
                              (let ((records (world-restart-tests--phases configuration)))
                                (test-assert (and (= (length records) 64)
                                                  (eq (getf (first (last records)) :phase) ':probe-truncated)
                                                  (eq (getf (first (last records)) :reason) ':record-limit))
                                             "record exhaustion ends with an explicit truncation marker"))))
                        (funcall restore)))
                    (test-assert (and (every #'eq originals (mapcar #'symbol-function symbols))
                                      (eq sb-ext:*after-gc-hooks* old-after-gc-hooks))
                                 "returning paths restore definitions without changing after-GC hooks")))))
             (dolist (tail '("(:phase :overflow)" ")"))
               (with-open-file (stream pathname :direction ':output :if-exists ':supersede)
                 (dotimes (index 64)
                   (write (list :phase ':prepare-enter :gc index) :stream stream)
                   (terpri stream))
                 (write-string tail stream))
               (test-assert
                (equal (world-restart-tests--phases configuration)
                       (loop for index below 64 collect (list :phase ':prepare-enter :gc index)))
                "collection never reads beyond its record budget"))
             (with-open-file (stream pathname :direction ':output :if-exists ':supersede)
               (write-string (make-string 32769 :initial-element #\Space) stream))
             (test-assert (eq (getf (world-restart-tests--phases configuration) :status) ':oversized)
                          "oversized logs are not parsed"))
        (setf sb-ext:*save-hooks* old-save-hooks))))
  nil)

(-> world-restart-tests--check-deinit-probe () null)
(defun world-restart-tests--check-deinit-probe ()
  "Test teardown identities, restoration before streams close, unwind and byte exhaustion."
  (with-test-configuration (configuration)
    (configuration-ensure-directories configuration)
    (let* ((pathname          (world-restart-tests--phase-pathname configuration))
           (old-hooks         sb-ext:*save-hooks*)
           (old-name          (sb-thread:thread-name sb-thread:*current-thread*))
           (definitions       nil)
           (stream-restored-p nil)
           (failure           (make-condition 'platform-error :operation ':phase-probe :message "Injected teardown failure."))
           (fail-p            nil))
      (unwind-protect
           (sb-ext:without-package-locks
             (test-call-with-function-replacements
              (list (list 'checkpoint--prepare-saver
                          (lambda (&rest arguments) (declare (ignore arguments)) nil))
                    (list 'sb-ext:save-lisp-and-die
                          (lambda (&rest arguments) (declare (ignore arguments)) (sb-impl::deinit)))
                    (list 'sb-impl::deinit
                          (lambda ()
                            (mapc #'funcall sb-ext:*save-hooks*)
                            (sb-thread:join-thread sb-thread:*current-thread*)
                            (sb-thread::primitive-join sb-thread:*current-thread* nil)
                            (sb-impl::finalizer-thread-stop)
                            (sb-impl::tune-image-for-dump)
                            (sb-impl::stream-deinit)))
                    (list 'sb-thread:join-thread
                          (lambda (&rest arguments) (declare (ignore arguments)) ':joined))
                    (list 'sb-thread::primitive-join
                          (lambda (&rest arguments) (declare (ignore arguments)) nil))
                    (list 'sb-impl::finalizer-thread-stop (lambda () nil))
                    (list 'sb-impl::tune-image-for-dump (lambda () (when fail-p (error failure))))
                    (list 'sb-impl::stream-deinit
                          (lambda ()
                            (setf stream-restored-p
                                  (every (lambda (definition)
                                           (eq (symbol-function (first definition)) (rest definition)))
                                         definitions)))))
              (lambda ()
                (dolist (scenario '(:normal :unwind :bytes))
                  (when (probe-file pathname) (delete-file pathname))
                  (setf sb-ext:*save-hooks* nil stream-restored-p nil fail-p (eq scenario ':unwind))
                  (let* ((source      (with-output-to-string (stream)
                                        (world-restart-tests--write-phase-probe stream pathname)))
                         (*read-eval* nil))
                    (multiple-value-bind (restore captured) (eval (read-from-string source))
                      (setf definitions captured)
                      (unwind-protect
                           (progn
                             (when (eq scenario ':bytes)
                               (setf (sb-thread:thread-name sb-thread:*current-thread*)
                                     (make-string 128 :initial-element (code-char #x1f600))))
                             (checkpoint--prepare-saver nil nil)
                             (case scenario
                               (:normal
                                (sb-ext:save-lisp-and-die "unused")
                                (test-assert stream-restored-p "all definitions are restored before stream teardown")
                                (let* ((records (world-restart-tests--phases configuration))
                                       (phases  (mapcar (lambda (record) (getf record :phase)) records))
                                       (join    (find ':join-thread-enter records
                                                      :key (lambda (record) (getf record :phase)))))
                                  (test-assert
                                   (and (equal phases
                                               '(:prepare-enter :prepare-exit :save-call-enter
                                                 :save-hooks-enter :save-hooks-exit
                                                 :join-thread-enter :join-thread-exit
                                                 :primitive-join-enter :primitive-join-exit
                                                 :finalizer-stop-enter :finalizer-stop-exit
                                                 :tune-image-enter :tune-image-exit :stream-deinit-enter))
                                        (stringp (first (getf join :target)))
                                        (second (getf join :target))
                                        (member (getf join :target-scheduler-wait-p) '(t nil))
                                        (member (getf join :make-owned-p) '(t nil)))
                                   "teardown phases stop at stream entry and expose bounded target identity")))
                               (:unwind
                                (test-assert
                                 (eq failure (handler-case (sb-ext:save-lisp-and-die "unused")
                                               (platform-error (condition) condition)))
                                 "teardown wrappers propagate the original condition")
                                (test-assert
                                 (find ':tune-image-unwind (world-restart-tests--phases configuration)
                                       :key (lambda (record) (getf record :phase)))
                                 "an earlier teardown unwind is recorded before restoration"))
                               (:bytes
                                (mapc #'funcall sb-ext:*save-hooks*)
                                (loop repeat 100 do (sb-thread:join-thread sb-thread:*current-thread*))
                                (let* ((records (world-restart-tests--phases configuration))
                                       (last    (first (last records))))
                                  (test-assert
                                   (and (< (length records) 64)
                                        (eq (getf last :phase) ':probe-truncated)
                                        (eq (getf last :reason) ':byte-limit)
                                        (with-open-file (stream pathname) (<= (file-length stream) 32768)))
                                   "UTF-8 byte exhaustion is marked within the file and record limits"))))
                             (test-assert
                              (or (eq scenario ':bytes)
                                  (every (lambda (definition)
                                           (eq (symbol-function (first definition)) (rest definition)))
                                         captured))
                              "returning and unwinding teardown paths restore every wrapped definition"))
                        (funcall restore)
                        (setf (sb-thread:thread-name sb-thread:*current-thread*) old-name))))))))
        (setf sb-ext:*save-hooks* old-hooks))))
  nil)

(-> world-restart-tests--check-itimer-candidate () null)
(defun world-restart-tests--check-itimer-candidate ()
  "Check the generated timer candidate's lock scope, close ordering and failure propagation."
  (with-test-configuration (configuration)
    (configuration-ensure-directories configuration)
    (let* ((source      (with-output-to-string (stream)
                          (world-restart-tests--write-phase-probe
                           stream (world-restart-tests--phase-pathname configuration))))
           (*read-eval* nil))
      (multiple-value-bind (restore definitions make-candidate) (eval (read-from-string source))
        (declare (ignore definitions))
        (unwind-protect
             (dolist (scenario '(:normal :join-failure :no-thread :no-handle))
               (let* ((thread-symbol (gensym "TIMER-THREAD-"))
                      (handle-symbol (gensym "TIMER-HANDLE-"))
                      (thread        (unless (eq scenario ':no-thread) sb-thread:*current-thread*))
                      (handle        (unless (eq scenario ':no-handle) (list ':handle)))
                      (events        nil)
                      (failure       (make-condition 'platform-error :operation ':phase-probe
                                                     :message                   "Injected timer join failure.")))
                 (labels ((scheduler-owned-p ()
                            "Return whether this requester owns the timer scheduler lock."
                            (eq (sb-thread:mutex-owner sb-impl::*scheduler-lock*) sb-thread:*current-thread*)))
                   (progv (list thread-symbol handle-symbol) (list thread handle)
                     (sb-ext:without-package-locks
                       (test-call-with-function-replacements
                        (list (list 'sb-thread:terminate-thread
                                    (lambda (target)
                                      (push (list ':terminate (eq target thread) (scheduler-owned-p)) events)))
                              (list 'sb-thread:join-thread
                                    (lambda (target &key (default nil default-p))
                                      (push (list ':join (eq target thread) (scheduler-owned-p)
                                                  default-p default (eq (symbol-value handle-symbol) handle))
                                            events)
                                      (when (eq scenario ':join-failure) (error failure))
                                      (values nil ':abort))))
                        (lambda ()
                          (let* ((candidate
                                   (funcall make-candidate thread-symbol handle-symbol
                                            (lambda (target)
                                              (push (list ':close (eq target handle) (scheduler-owned-p)) events))))
                                 (result (handler-case (multiple-value-list (funcall candidate))
                                           (platform-error (condition) condition))))
                            (test-assert
                             (and (if (eq scenario ':join-failure) (eq result failure) (equal result '(nil)))
                                  (eq (symbol-value thread-symbol) thread)
                                  (eq (symbol-value handle-symbol) (when (eq scenario ':join-failure) handle)))
                             (format nil "Timer candidate ~S preserves result/error and timer/handle state." scenario))))))
                     (test-assert
                      (equal (reverse events)
                             (case scenario
                               (:normal
                                '((:terminate t t) (:join t nil t nil t) (:close t t)))
                               (:join-failure
                                '((:terminate t t) (:join t nil t nil t)))
                               (:no-thread
                                '((:close t t)))
                               (:no-handle
                                '((:terminate t t) (:join t nil t nil t)))))
                      (format nil "Timer candidate ~S terminates locked, joins unlocked with DEFAULT NIL, and closes locked after join." scenario))))))
          (funcall restore)))))
  nil)

(-> test-world-server-restart-diagnostics () null)
(defun test-world-server-restart-diagnostics ()
  "Test timeout evidence for missing, saved and unreadable checkpoint stages."
  (world-restart-tests--check-phase-probe)
  (world-restart-tests--check-phase-emission-failures)
  (world-restart-tests--check-deinit-probe)
  (world-restart-tests--check-itimer-candidate)
  (with-test-configuration (configuration)
    (configuration-ensure-directories configuration)
    (let* ((pid     (current-process-id))
           (pointer (merge-pathnames "restart-pointers/launcher-test.sexp"
                                     (config :state-root configuration)))
           (core    (merge-pathnames "generations/test-generation/autolith.core"
                                     (config :data-root configuration)))
           (missing (world-restart-tests--diagnostics configuration pid nil)))
      (test-assert (and (eql (getf missing :original-pid) pid)
                        (getf missing :original-alive-p)
                        (eq (getf (getf missing :restart-marker) :status) ':absent))
                   "missing stages report absence alongside original process liveness")
      (world-server--restart-mark configuration :launcher-pid pid :generation-id "test-generation")
      (ensure-directories-exist pointer)
      (snapshot-write pointer '(:autolith-restart :version 1 :identifier "test-generation"
                                :created-at 123 :metadata (:private "omitted")))
      (ensure-directories-exist core)
      (with-open-file (stream core :direction ':output :element-type '(unsigned-byte 8))
        (write-sequence #(1 2 3) stream))
      (let* ((saved      (world-restart-tests--diagnostics configuration pid nil))
             (envelope   (first (getf saved :restart-envelopes)))
             (generation (first (getf saved :generation-files))))
        (test-assert (and (equal (getf (getf envelope :fields) :identifier) "test-generation")
                          (not (member :metadata (getf envelope :fields)))
                          (equal (getf generation :generation) "test-generation")
                          (= (getf (first (getf generation :files)) :bytes) 3))
                     "saved-stage evidence contains identity and sizes, not envelope metadata"))
      (with-open-file (stream pointer :direction ':output :if-exists ':supersede)
        (write-string "(" stream))
      (test-assert (getf (getf (world-restart-tests--diagnostics configuration pid nil)
                               :restart-envelopes)
                         :unavailable)
                   "an unreadable envelope does not abort timeout evidence collection")
      (with-open-file (stream pointer :direction ':output :if-exists ':supersede)
        (write-string (make-string 16385 :initial-element #\Space) stream))
      (test-assert (eq (getf (world-restart-tests--record-state pointer '(:identifier)) :status)
                       ':oversized)
                   "oversized stage records are not parsed")))
  nil)

(-> world-restart-tests--checkpoint (configuration cons &key (:process t)) list)
(defun world-restart-tests--checkpoint (configuration entry &key process)
  "Return the resumed world and channel entries, events, and timeout diagnostics when unavailable."
  (let* ((channel (getf (rest (rest entry)) :session-id))
         (old-pid (getf (rest (rest (world-client-entry configuration))) :pid))
         (events  (world-restart-tests--submit
                   entry "(checkpoint)"
                   :until (lambda (events) (find ':closed events :key #'first))))
         (world   nil)
         (resumed nil))
    (if (task-tests--wait-until
         (lambda ()
           (setf world   (world-client-entry configuration)
                 resumed (localgroup--find-record-or-nil configuration channel))
           (and world resumed
                (/= (getf (rest (rest world)) :pid) old-pid)
                (eql (getf (rest (rest resumed)) :pid) (getf (rest (rest world)) :pid))))
         180)
        (list world resumed events)
        (list nil nil events
              (world-restart-tests--diagnostics configuration old-pid process)))))

(-> world-restart-tests--logs (configuration) string)
(defun world-restart-tests--logs (configuration)
  "Return the tails of CONFIGURATION's detached process logs, to explain a failed restart."
  (with-output-to-string (output)
    (dolist (pathname (directory (merge-pathnames "localgroup/logs/*.log"
                                                  (config :state-root configuration))))
      (let ((text (uiop:read-file-string pathname)))
        (format output "~&--- ~A~%~A~%" (file-namestring pathname)
                (subseq text (max 0 (- (length text) 4000))))))))

(-> world-restart-tests--conversation-held-p (configuration string) boolean)
(defun world-restart-tests--conversation-held-p (configuration identifier)
  "Return true when another process holds the lease on conversation IDENTIFIER."
  (handler-case
      (progn
        (conversation-lease-release (conversation-lease-acquire configuration identifier))
        nil)
    (conversation-in-use ()
      t)))

(-> world-restart-tests--start (configuration pathname) null)
(defun world-restart-tests--start (configuration root)
  "Start CONFIGURATION's world server under the Lisp launcher, which supervises restarts as on Windows, with a channel for ROOT.

The executable user init gives the server the Windows checkpoint behavior and
skips the repository check."
  (configuration-ensure-directories configuration)
  (with-open-file (stream (configuration-user-init-path configuration)
                          :direction ':output :if-exists ':supersede
                          :external-format ':utf-8)
    (write-string "(setf (config :onboarding-p) nil
      (config :telemetry-enabled-p) nil
      *localgroup-first-attach-timeout-seconds* 0.2)
(defmethod platform-supports-p :around (platform (capability (eql :restartable-image-saver)))
  (declare (ignore platform))
  t)
(defun checkpoint--source-snapshot (configuration &key allow-dirty-p)
  (declare (ignore allow-dirty-p))
  (source-revision (make-source-provenance (config :source-root configuration))))
(defun checkpoint--revalidate-source (configuration commit &key allow-dirty-p)
  (declare (ignore configuration commit allow-dirty-p))
  nil)" stream)
    (terpri stream)
    (world-restart-tests--write-phase-probe stream (world-restart-tests--phase-pathname configuration)))
  (test-call-with-function-replacements
   (list (list 'platform-session-launch-command
               (lambda (platform source-root)
                 (declare (ignore platform))
                 (list (lisp-worker-sbcl-command) "--noinform"
                       "--no-userinit" "--no-sysinit" "--script"
                       (namestring (merge-pathnames "script/launcher.lisp" source-root))))))
   (lambda ()
     (world-client-open configuration :directory (namestring root))))
  nil)

(define-condition world-restart-test-cleanup-error (error)
  ((failures :initarg       :failures
             :reader        world-restart-test-cleanup-failures
             :documentation "Cleanup conditions, in operation order."))
  (:documentation "One or more fixture cleanup operations failed after every launch was attempted.")
  (:report (lambda (condition stream)
             (format stream "Restart fixture cleanup encountered ~D failures:~{~%~A~}"
                     (length (world-restart-test-cleanup-failures condition))
                     (world-restart-test-cleanup-failures condition)))))

(-> world-restart-tests--stop (configuration list) null)
(defun world-restart-tests--stop (configuration processes)
  "Attempt cleanup of every retained launch before reporting accumulated failures."
  (let ((failures nil))
    (labels ((attempt (function)
               "Return FUNCTION's result and success flag, recording any error."
               (handler-case
                   (values (funcall function) t)
                 (error (condition)
                   (push condition failures)
                   (values nil nil)))))
      (attempt (lambda ()
                 (let ((world (world-client-entry configuration)))
                   (when world
                     ; mallet:suppress no-ignore-errors -- stopping an endpoint that may already be closed is advisory
                     (ignore-errors (localgroup-query-record world ':stop))))))
      (dolist (process processes)
        (multiple-value-bind (alive-p known-p)
            (attempt (lambda () (platform-process-object-alive-p *platform* process)))
          (when (or alive-p (not known-p))
            (attempt (lambda () (platform-terminate-process-object *platform* process :force t)))))
        (multiple-value-bind (ended-p checked-p)
            (attempt (lambda ()
                       (task-tests--wait-until
                        (lambda () (not (platform-process-object-alive-p *platform* process))) 10)))
          (when checked-p
            (if ended-p
                (attempt (lambda () (platform-wait-process *platform* process)))
                (push (make-condition 'platform-error :operation ':terminate
                                      :message                   "The retained launcher tree did not end during cleanup.")
                      failures))))
        (attempt (lambda () (platform-release-process *platform* process)))))
    (when failures
      (error 'world-restart-test-cleanup-error :failures (nreverse failures))))
  nil)

(-> world-restart-tests--call-with-owned-launch (configuration function) t)
(defun world-restart-tests--call-with-owned-launch (configuration function)
  "Call FUNCTION with a launch lookup function, retaining its fresh launches until cleanup."
  (let ((launch     *localgroup-fresh-launch-function*)
        (release    (symbol-function 'platform-release-process))
        (processes  nil)
        (finished-p nil))
    (unwind-protect
         (multiple-value-prog1
             (let ((*localgroup-fresh-launch-function*
                     (lambda (&rest arguments)
                       (let ((process (apply launch arguments)))
                         (push process processes)
                         process))))
               (test-call-with-function-replacements
                (list (list 'platform-release-process
                            (lambda (platform process)
                              (unless (member process processes :test #'eq)
                                (funcall release platform process)))))
                (lambda () (funcall function (lambda () (first processes))))))
           (setf finished-p t))
      (handler-case
          (world-restart-tests--stop configuration processes)
        (error (condition)
          (if finished-p
              (error condition)
              (format *error-output* "~&Restart fixture cleanup failed: ~A~%" condition)))))))

(-> test-world-server-restart-owned-cleanup () null)
(defun test-world-server-restart-owned-cleanup ()
  "Test owned-launch cleanup without an endpoint, on normal return and nonlocal exit."
  (with-test-configuration (configuration)
    (dolist (exit-p '(nil t))
      (let* ((owned                              (list ':owned))
             (other                              (list ':other))
             (alive-p                            t)
             (events                             nil)
             (*localgroup-fresh-launch-function* (lambda (&rest arguments)
                                                  (declare (ignore arguments))
                                                  owned)))
        (test-call-with-function-replacements
         (list (list 'world-client-entry (lambda (configuration)
                                          (declare (ignore configuration))
                                          nil))
               (list 'platform-terminate-process-object
                     (lambda (platform process &key force)
                       (declare (ignore platform force))
                       (push (list ':terminate process) events)
                       (setf alive-p nil)))
               (list 'platform-process-object-alive-p
                     (lambda (platform process)
                       (declare (ignore platform process))
                       alive-p))
               (list 'platform-wait-process
                     (lambda (platform process)
                       (declare (ignore platform))
                       (push (list ':wait process) events)))
               (list 'platform-release-process
                     (lambda (platform process)
                       (declare (ignore platform))
                       (push (list ':release process) events))))
         (lambda ()
           (catch 'fixture-exit
             (world-restart-tests--call-with-owned-launch
              configuration
              (lambda (launch-process)
                (declare (ignore launch-process))
                (let ((process (funcall *localgroup-fresh-launch-function*)))
                  (platform-release-process *platform* process)
                  (test-assert (null events) "handoff does not release the retained launch")
                  (platform-release-process *platform* other)
                  (when exit-p
                    (throw 'fixture-exit nil))))))))
        (test-assert (equal (reverse events)
                            (list (list ':release other) (list ':terminate owned)
                                  (list ':wait owned) (list ':release owned)))
                     "cleanup terminates, waits and releases the owned launch without an endpoint"))))
  nil)

(-> test-world-server-restart-cleanup-failures () null)
(defun test-world-server-restart-cleanup-failures ()
  "Test all retained launches are attempted despite cleanup errors, preserving body failures."
  (with-test-configuration (configuration)
    (dolist (stage '(:lookup :probe :terminate :wait :release))
      (dolist (body-fails-p '(nil t))
        (let* ((first-launch   (list ':first))
               (faulty-launch  (list ':faulty))
               (pending        (list first-launch faulty-launch))
               (alive          (copy-list pending))
               (events         nil)
               (cleanup-error  (make-condition 'platform-error :operation stage
                                                :message                  "Injected cleanup failure."))
               (body-error     (make-condition 'platform-error :operation ':fixture-body
                                                :message                  "Injected body failure."))
               (*error-output* (make-string-output-stream))
               (*localgroup-fresh-launch-function*
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (pop pending))))
          (labels ((record (operation process)
                     "Record an attempted cleanup operation and inject its selected failure."
                     (push (list operation process) events)
                     (when (and (eq process faulty-launch) (eq operation stage))
                       (error cleanup-error))))
            (test-call-with-function-replacements
             (list (list 'world-client-entry
                         (lambda (configuration)
                           (declare (ignore configuration))
                           (when (eq stage ':lookup)
                             (error cleanup-error))
                           nil))
                   (list 'platform-process-object-alive-p
                         (lambda (platform process)
                           (declare (ignore platform))
                           (record ':probe process)
                           (not (null (member process alive :test #'eq)))))
                   (list 'platform-terminate-process-object
                         (lambda (platform process &key force)
                           (declare (ignore platform force))
                           (setf alive (remove process alive :test #'eq))
                           (record ':terminate process)))
                   (list 'platform-wait-process
                         (lambda (platform process)
                           (declare (ignore platform))
                           (record ':wait process)))
                   (list 'platform-release-process
                         (lambda (platform process)
                           (declare (ignore platform))
                           (record ':release process))))
             (lambda ()
               (let ((observed
                       (handler-case
                           (world-restart-tests--call-with-owned-launch
                            configuration
                            (lambda (launch-process)
                              (declare (ignore launch-process))
                              (funcall *localgroup-fresh-launch-function*)
                              (funcall *localgroup-fresh-launch-function*)
                              (when body-fails-p
                                (error body-error))))
                         (error (condition) condition))))
                 (test-assert (if body-fails-p
                                  (eq observed body-error)
                                  (and (typep observed 'world-restart-test-cleanup-error)
                                       (member cleanup-error
                                               (world-restart-test-cleanup-failures observed)
                                               :test #'eq)))
                              "cleanup reports its failures only after preserving any original body error")
                 (test-assert (and (find (list ':release faulty-launch) events :test #'equal)
                                   (find (list ':terminate first-launch) events :test #'equal)
                                   (find (list ':wait first-launch) events :test #'equal)
                                   (find (list ':release first-launch) events :test #'equal))
                              "a cleanup error cannot abandon the remaining retained launch")))))))))
  nil)

(-> test-world-server-restart-checkpoint () null)
(defun test-world-server-restart-checkpoint ()
  "Test a world server that cannot fork restarts from its saved heap under its launcher, twice, and serves its channel again."
  (with-platform-capability (':detached-sessions "a launcher-supervised world server restart")
    (with-test-configuration (unused root)
      (declare (ignore unused))
      (session-cli-tests--call-with-roots
       root
       (lambda ()
         (let ((configuration (configuration-create :defer-provider-validation-p t)))
           (world-restart-tests--call-with-owned-launch
            configuration
            (lambda (launch-process)
              (world-restart-tests--start configuration root)
              (let* ((entry   (first (localgroup-endpoint-records configuration)))
                     (channel (getf (rest (rest entry)) :session-id))
                     (token   (getf (rest (rest entry)) :token)))
                (destructuring-bind (world resumed events &optional diagnostics)
                    (world-restart-tests--checkpoint configuration entry
                                                     :process (funcall launch-process))
                  (test-assert (and world resumed)
                               (format nil "a new process serves the world and the channel after the checkpoint~%~S~%~S~%~A"
                                       events diagnostics (world-restart-tests--logs configuration)))
                  (when (and world resumed)
                    (test-assert (equal (getf (rest (rest resumed)) :token) token)
                                 "the channel keeps the token its clients hold")
                    (test-assert (directory (merge-pathnames "generations/*/autolith.core"
                                                             (config :data-root configuration)))
                                 "the launcher published the saved heap as a generation")
                    (test-assert (and (not (probe-file (world-server-restart-pathname configuration)))
                                      (world-server-running-p configuration)
                                      (world-restart-tests--conversation-held-p configuration channel))
                                 "the restarted server holds the world and the conversation, and removed its marker")
                    ;; The heap was saved holding locks of a thread that
                    ;; is gone; a second checkpoint needs them again.
                    (destructuring-bind (again again-resumed again-events &optional diagnostics)
                        (world-restart-tests--checkpoint configuration resumed
                                                         :process (funcall launch-process))
                      (test-assert (and again again-resumed)
                                   (format nil "the restarted server checkpoints and restarts again~%~S~%~S~%~A"
                                           again-events diagnostics (world-restart-tests--logs configuration)))
                      (when again-resumed
                        (let ((evaluated (world-restart-tests--submit
                                          again-resumed "(+ 40 2)"
                                          :until (lambda (events)
                                                   (find ':work-finished events :key #'first)))))
                          (test-assert (find-if (lambda (event)
                                                  (and (eq (first event) ':present)
                                                       (search "42" (getf (rest event) :text))))
                                                evaluated)
                                       (format nil "the channel evaluates input after two restarts: ~S"
                                               evaluated)))))))))))))))
  nil)
