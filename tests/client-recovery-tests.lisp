(in-package #:autolith)

(-> test-localgroup-client-termination () null)
(defun test-localgroup-client-termination ()
  "Keep transport and rendering failures distinct from intentional terminal departure."
  (let* ((terminal (make-instance 'scripted-terminal :columns 80))
         (screen   (localgroup-screen-create :terminal terminal)))
    (test-call-with-function-replacements
     (list (list 'image-daemon:daemon-attach-client-run
                 (lambda (stream &rest options)
                   (declare (ignore stream))
                   (let ((receive (getf options :packet-function)))
                     (test-assert (null (funcall receive '(:evaluation :id "pump" :forms ("(values)"))))
                                  "evaluation leaves the terminal packet loop running")
                     (test-assert (funcall receive '(:switch :session-id "next"))
                                  "the following switch ends the terminal packet loop"))
                   (values nil '(:kind :detached)))))
     (lambda ()
       (test-assert (equal '(:switch :session-id "next")
                           (localgroup--attach-terminal-loop
                            (make-string-input-stream "") terminal ':control :screen screen))
                    "the terminal returns the switch received after evaluation")))
    (dolist (kind '(:detached :revoked :application-exit :unexpected-eof
                    :decoding-failure :callback-failure :transport-failure
                    :remote-failure :queue-overflow))
      (test-call-with-function-replacements
       (list (list 'image-daemon:daemon-attach-client-run
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (values nil (list :kind kind :message "attachment ended")))))
       (lambda ()
         (let ((result (localgroup--attach-terminal-loop
                        (make-string-input-stream "") terminal ':control :screen screen)))
           (test-assert
            (if (member kind '(:detached :revoked :application-exit))
                (null result)
                (and (eq (first result) ':attachment-lost)
                     (eq (getf (rest result) :kind) kind)))
            (format nil "~A has an explicit local client outcome" kind)))))))
  nil)

(-> test-localgroup-client-checkpoint-recovery () null)
(defun test-localgroup-client-checkpoint-recovery ()
  "Recover from checkpoint wait and replacement failures without converting initial refusal."
  (dolist (phase '(:wait :reattach :initial))
    (let* ((entry (cons #p"checkpoint-registry" '(:endpoint :session-id "channel")))
           (cause (make-condition 'localgroup-error :operation ':attach
                                  :message                     "private checkpoint failure"))
           (prior '((:stage "stream" :condition-type "STREAM-ERROR")))
           (calls 0)
           (waits 0))
      (test-call-with-function-replacements
       (list
        (list 'localgroup--attach-record-once
              (lambda (configuration candidate mode &key history-position deadline
                                                           &allow-other-keys)
                (declare (ignore configuration mode))
                (incf calls)
                (test-assert (eq candidate entry) "checkpoint recovery retains its endpoint identity")
                (when (eq phase ':initial) (error cause))
                (when (> calls 1)
                  (test-assert (and (= history-position 5) (null deadline))
                               "replacement attachment retains history and gets its own deadline")
                  (error cause))
                (values '(:checkpoint-reconnect :id "transition" :history-position 5)
                        (list :cleanup-failures prior))))
        (list 'localgroup--wait-for-checkpoint-entry
              (lambda (candidate transition &key configuration)
                (declare (ignore configuration))
                (incf waits)
                (test-assert (and (eq candidate entry) (string= transition "transition"))
                             "only the explicit checkpoint transition is rediscovered")
                (when (eq phase ':wait) (error cause))
                entry)))
       (lambda ()
         (if (eq phase ':initial)
             (test-assert
              (and (eq (handler-case (localgroup-attach-record nil entry ':control)
                         (error (condition) condition)) cause)
                   (= calls 1) (zerop waits))
              "initial refusal preserves its exact condition without checkpoint recovery")
             (multiple-value-bind (result metadata)
                 (localgroup-attach-record nil entry ':control)
               (test-assert
                (and (eq (first result) ':attachment-lost)
                     (eq (getf (rest result) :kind) ':transport-failure)
                     (equal (getf (rest result) :stage) "checkpoint-reconnect")
                     (equal (getf (rest result) :cleanup-failures) prior)
                     (equal (getf metadata :cleanup-failures) prior)
                     (= waits 1) (= calls (if (eq phase ':wait) 1 2))
                     (not (search "private" (write-to-string (list result metadata)))))
                "post-ACK checkpoint failures retain recovery and prior cleanup metadata")))))))
  nil)

(-> test-main-world-client-cleanup-cause nil null)
(defun test-main-world-client-cleanup-cause ()
  "Preserve shared-screen owner exits and original errors through independent cleanup failures."
  (with-test-configuration (configuration)
   (dolist (phase '(:exit :detach :recovery-quit :primary-error))
     (let* ((terminal  (make-instance 'scripted-terminal :columns 80))
            (panel     (world-sidebar-create configuration))
            (screen    (localgroup-screen-create :terminal terminal :panel panel))
            (exit      (when (eq phase ':exit) '(:status 7 :message "finished")))
            (cause
             (make-condition 'localgroup-error :operation ':attach :message
                             "original attachment failure"))
            (attempts  nil)
            (relayed-p nil)
            (relayed   nil)
            (choice-p  nil))
       (test-call-with-function-replacements
        (list
         (list 'world-sidebar-create
               (lambda (&rest arguments) (declare (ignore arguments)) panel))
         (list 'localgroup-client-screen-create
               (lambda (&rest arguments) (declare (ignore arguments)) screen))
         (list 'world-sidebar-start
               (lambda (candidate) (declare (ignore candidate)) nil))
         (list 'terminal--terminal-mode-or-nil
               (lambda (candidate) (declare (ignore candidate)) t))
         (list 'main--call-with-channel-loading
               (lambda (candidate title function &key &allow-other-keys)
                 (declare (ignore candidate title))
                 (funcall function)))
         (list 'main--open-world-channel
               (lambda (&rest arguments)
                 (declare (ignore arguments))
                 (values "bfirst1" nil)))
         (list 'main--attach-world-channel
               (lambda
                   (candidate identifier mode
                    &key attached-function &allow-other-keys)
                 (declare (ignore candidate identifier mode))
                 (funcall attached-function)
                 (when (eq phase ':primary-error) (error cause))
                 (if (eq phase ':recovery-quit)
                     '(:attachment-lost :kind :callback-failure :message
                       "rendering failed")
                     exit)))
         (list 'main--attachment-choice
               (lambda (&rest arguments)
                 (declare (ignore arguments))
                 (setf choice-p t)
                 nil))
         (list 'localgroup-client-screen-release
               (lambda (candidate)
                 (declare (ignore candidate))
                 (push "client-screen" attempts)
                 (error "private screen cleanup payload")))
         (list 'world-sidebar-stop
               (lambda (candidate)
                 (declare (ignore candidate))
                 (push "sidebar" attempts)
                 (error "private sidebar cleanup payload")))
         (list 'localgroup-relay-exit
               (lambda (candidate result mode &key session-id)
                 (declare (ignore candidate mode))
                 (test-assert (equal session-id "bfirst1")
                  "cleanup retains the last attached channel")
                 (setf relayed-p t
                       relayed result))))
        (lambda ()
          (if (eq phase ':primary-error)
              (test-assert
               (and
                (eq
                 (handler-case (main--run-world-client configuration)
                               (error (condition) condition))
                 cause)
                (not relayed-p))
               "independent cleanup cannot replace the original primary condition")
              (multiple-value-bind (success metadata)
                  (main--run-world-client configuration)
                (test-assert
                 (and success relayed-p (eq relayed exit)
                      (eq choice-p (eq phase ':recovery-quit))
                      (= (length (getf metadata :cleanup-failures)) 2)
                      (not (search "private" (write-to-string metadata))))
                 "exit, detach and recovery quit reach relay exit unchanged with class-only diagnostics")))
          (test-assert (equal (reverse attempts) '("client-screen" "sidebar"))
           "each shared-screen owner cleanup stage is attempted independently"))))))
  nil)

(-> test-main-attachment-choice () null)
(defun test-main-attachment-choice ()
  "Offer manual retry, a different channel and detach without invoking a renderer again."
  (dolist (entry '(((#\r) (:reattach))
                   (((:insert "bsecnd2") :enter) (:switch :session-id "bsecnd2"))
                   ((#\q) nil)))
    (let* ((events   (copy-list (first entry)))
           (expected (second entry))
           (terminal (make-instance 'scripted-terminal :columns 80))
           (screen   (localgroup-screen-create :terminal terminal)))
      (test-call-with-function-replacements
       (list (list 'terminal-read-event
                   (lambda (candidate)
                     (declare (ignore candidate))
                     (or (pop events) ':end-of-input)))
             (list 'localgroup-screen-close
                   (lambda (candidate)
                     (declare (ignore candidate))
                     (error "renderer cleanup failed"))))
       (lambda ()
         (let ((*error-output* (make-broadcast-stream)))
           (test-assert
            (equal (main--attachment-choice
                    screen "bfirst1" '(:kind :callback-failure :message "Rendering failed."))
                   expected)
            "a rendering failure leaves explicit attachment recovery choices"))))))
  nil)

(-> test-localgroup-client-cleanup-cause () null)
(defun test-localgroup-client-cleanup-cause ()
  "Retain intentional exits and original loss despite independent screen/mode cleanup errors."
  (dolist (kind '(:detached :application-exit :unexpected-eof))
    (let* ((terminal (make-instance 'scripted-terminal :columns 80))
           (screen   (localgroup-screen-create :terminal terminal))
           (exit     (when (eq kind ':application-exit) '(:status 7 :message "finished")))
           (attempts nil))
      (test-call-with-function-replacements
       (list (list 'localgroup-screen-create
                   (lambda (&rest arguments) (declare (ignore arguments)) screen))
             (list 'image-daemon:daemon-attach-client-run
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (values exit (list :kind kind :message "original loss"))))
             (list 'localgroup-screen-close
                   (lambda (candidate) (declare (ignore candidate))
                     (push "screen" attempts) (error "private screen payload")))
             (list 'mode-tracking-output-stream-restore
                   (lambda (output) (declare (ignore output))
                     (push "terminal-mode" attempts) (error "private mode payload"))))
       (lambda ()
         (multiple-value-bind (result metadata)
             (localgroup--attach-terminal-loop (make-string-input-stream "") terminal ':control)
           (test-assert
            (and (equal (reverse attempts) '("screen" "terminal-mode"))
                 (= (length (getf metadata :cleanup-failures)) 2)
                 (if (eq kind ':unexpected-eof)
                     (and (eq (first result) ':attachment-lost)
                          (eq (getf (rest result) :kind) kind)
                          (equal (getf (rest result) :message) "original loss"))
                     (equal result exit))
                 (not (search "private" (write-to-string (list result metadata)))))
            "cleanup attempts every stage, retains the original outcome, and reports class-only metadata"))))))
  nil)

(defclass client-cleanup-failing-stream (trivial-gray-streams:fundamental-character-input-stream)
  ((condition :initarg :condition :reader client-cleanup-failing-stream-condition
              :documentation "The injected stream cleanup condition."))
  (:documentation "A transport fixture whose close fails without owning a native descriptor."))

(defmethod close ((stream client-cleanup-failing-stream) &key abort)
  "Inject the independently reported stream cleanup failure."
  (declare (ignore abort))
  (error (client-cleanup-failing-stream-condition stream)))

(-> test-localgroup-client-accepted-setup-failure () null)
(defun test-localgroup-client-accepted-setup-failure ()
  "Recover after acknowledgment while preserving refusal causes and independent cleanup diagnostics."
  (with-test-configuration (configuration)
    (dolist (stage '(:refusal :attached-callback :terminal-start :resize-watch :receive))
      (let* ((terminal      (make-instance 'scripted-terminal :columns 80))
             (screen        (localgroup-screen-create :terminal terminal))
             (cause         (make-condition 'localgroup-error :operation ':attach :message "private original payload"))
             (cleanup-cause (make-condition 'simple-error :format-control "private cleanup payload"))
             (stream        (make-instance 'client-cleanup-failing-stream :condition cleanup-cause))
             (socket        (make-instance 'sb-bsd-sockets:inet-socket :type ':stream :protocol ':tcp))
             (close-socket  (symbol-function 'sb-bsd-sockets:socket-close))
             (entry         (cons #P"/unused/attachment.sexp"
                                  '(:localgroup-session :session-id "bfirst1" :port 1 :token "fixture"))))
        (unwind-protect
             (test-call-with-function-replacements
              (list
               (list 'localgroup-client-screen-create
                     (lambda (&rest arguments) (declare (ignore arguments)) screen))
               (list 'terminal--terminal-mode-or-nil
                     (lambda (candidate) (declare (ignore candidate)) t))
               (list 'localgroup-screen-pane-size
                     (lambda (candidate) (declare (ignore candidate)) (values 24 80)))
               (list 'localgroup-screen-active-p
                     (lambda (candidate) (declare (ignore candidate)) t))
               (list 'daemon-connect
                     (lambda (port) (declare (ignore port)) (values socket stream)))
               (list 'daemon-write-packet
                     (lambda (&rest arguments) (declare (ignore arguments)) nil))
               (list 'daemon-read-response
                     (lambda (&rest arguments)
                       (declare (ignore arguments))
                       (if (eq stage ':refusal) (error cause) '(:attached :history ""))))
               (list 'terminal-start
                     (lambda (candidate) (declare (ignore candidate))
                       (when (eq stage ':terminal-start) (error cause))))
               (list 'platform-watch-terminal-resize
                     (lambda (&rest arguments)
                       (declare (ignore arguments))
                       (if (eq stage ':resize-watch) (error cause) ':watch)))
               (list 'platform-unwatch-terminal-resize
                     (lambda (&rest arguments) (declare (ignore arguments)) (error cleanup-cause)))
               (list 'localgroup-client-screen-release
                     (lambda (candidate) (declare (ignore candidate)) (error cleanup-cause)))
               (list 'sb-bsd-sockets:socket-close
                     (lambda (&rest arguments) (declare (ignore arguments)) (error cleanup-cause)))
               (list 'localgroup--attach-terminal-loop
                     (lambda (&rest arguments)
                       (declare (ignore arguments))
                       (values '(:attachment-lost :kind :unexpected-eof :message "original loss")
                               '(:kind :unexpected-eof)))))
              (lambda ()
                (multiple-value-bind (result metadata)
                    (handler-case
                        (localgroup--attach-record-once
                         configuration entry ':control
                         :attached-function (lambda ()
                                              (when (eq stage ':attached-callback) (error cause))))
                      (error (condition) condition))
                  (if (eq stage ':refusal)
                      (test-assert (eq result cause)
                                   "independent teardown failures never replace the original refusal")
                      (test-assert
                       (and (eq (first result) ':attachment-lost)
                            (eq (getf (rest result) :kind)
                                (if (eq stage ':receive) ':unexpected-eof ':callback-failure))
                            (= (length (getf metadata :cleanup-failures))
                               (if (eq stage ':receive) 4 3))
                            (not (search "private" (write-to-string (list result metadata)))))
                       "post-ack setup failures become loss while independent cleanup remains secondary")))))
          (funcall close-socket socket :abort t)))))
  nil)

(-> test-world-client-attachment-recovery () null)
(defun test-world-client-attachment-recovery ()
  "Retry a lost attachment with bounded discovery and no channel open or work resubmission."
  (with-test-configuration (configuration)
    (let ((opens        0)
          (attaches     nil)
          (choices      0)
          (reported     nil)
          (attach-count 0))
      (test-call-with-function-replacements
       (list
        (list 'main--open-world-channel
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (incf opens)
                (values "bfirst1" nil)))
        (list 'world-client-open
              (lambda (&rest arguments)
                (declare (ignore arguments))
                (error "Attachment recovery must not open or rerun a channel.")))
        (list 'world-sidebar-start (lambda (panel) (declare (ignore panel))))
        (list 'world-sidebar-stop (lambda (panel) (declare (ignore panel))))
        (list 'localgroup--find-record
              (lambda (candidate identifier)
                (declare (ignore candidate))
                (list identifier)))
        (list 'localgroup-attach-record
              (lambda (candidate record mode
                       &key panel screen notice client-version attached-function deadline)
                (declare (ignore candidate mode panel screen notice client-version))
                (test-assert (and deadline (> deadline (get-internal-real-time)))
                             "each manual reattachment has its own bounded handshake")
                (push (first record) attaches)
                (incf attach-count)
                (case attach-count
                  (1
                   (funcall attached-function)
                   '(:attachment-lost :kind :unexpected-eof :message "Connection lost."))
                  (2
                   (error 'localgroup-error :operation ':attach :message "Endpoint not ready."))
                  (otherwise
                   (funcall attached-function)
                   nil))))
        (list 'main--attachment-choice
              (lambda (screen identifier termination)
                (declare (ignore screen))
                (incf choices)
                (test-assert (and (string= identifier "bfirst1") (getf termination :message))
                             "the recovery prompt retains identity and reports each failure")
                '(:reattach)))
        (list 'localgroup-relay-exit
              (lambda (candidate exit mode &key session-id)
                (declare (ignore candidate mode))
                (setf reported (list session-id exit)))))
       (lambda ()
         (let ((*error-output* (make-broadcast-stream)))
           (main--run-world-client configuration))))
      (test-assert (and (= opens 1) (= choices 2)
                        (equal attaches '("bfirst1" "bfirst1" "bfirst1"))
                        (equal reported '("bfirst1" nil)))
                   "loss and a failed retry retain the terminal until explicit retry or departure")))
  nil)
