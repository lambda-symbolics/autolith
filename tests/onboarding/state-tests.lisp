(in-package #:autolith)

;;;; -- Versioned Onboarding State --

(-> onboarding-state-test--write (configuration string) null)
(defun onboarding-state-test--write (configuration text)
  "Replace a fixture snapshot with literal TEXT for malformed-reader probes."
  (test-write-text (onboarding-state-path configuration) text)
  nil)

(-> test-onboarding-state-selection () null)
(defun test-onboarding-state-selection ()
  "Exercise missing, partial, completed, refused and revised lesson selection."
  (with-test-configuration (configuration)
    (let* ((first  (list :id "intro" :version 1 :opaque (make-hash-table)))
           (second (list :id "prompt" :version 2))
           (third  (list :id "vault" :version 1))
           (pages  (list first second third)))
      (test-assert (equal (onboarding-state-pending configuration pages) pages)
                   "A missing mark leaves every lesson pending")
      (test-assert (not (probe-file (onboarding-state-path configuration)))
                   "Reading missing state creates no acknowledgement snapshot")
      (onboarding-state-record configuration (list first) ':completed)
      (test-assert (equal (onboarding-state-pending configuration pages)
                          (list second third))
                   "An interrupted walkthrough acknowledges only completed slides")
      (onboarding-state-record configuration (list second) ':refused)
      (test-assert (eq (first (onboarding-state-pending configuration pages)) third)
                   "Completion and refusal both suppress their recorded revision")
      (let* ((new-first  (list :id "intro" :version 2))
             (old-second (list :id "prompt" :version 1))
             (new-page   (list :id "agents" :version 1))
             (updated    (list third new-first old-second new-page)))
        (test-assert (equal (onboarding-state-pending configuration updated)
                            (list third new-first new-page))
                     "New IDs and newer revisions are pending in caller order"))
      (onboarding-state-record configuration (list third) ':completed)
      (test-assert (null (onboarding-state-pending configuration pages))
                   "All acknowledged lessons survive repeated fresh reads")
      (let ((saved (sexp-store:store-read (onboarding-state--store configuration))))
        (test-assert (and (= (length saved) 3)
                          (every (lambda (page) (= (length page) 6)) saved))
                     "Opaque lesson content is omitted from portable roundtrip state")
        (test-assert (eq (getf (find "prompt" saved :test #'string=
                                   :key                   (lambda (page) (getf page :id))) :status)
                         ':refused)
                     "Refusal status survives the persisted roundtrip"))))
  nil)

(-> test-onboarding-state-monotonic-merge () null)
(defun test-onboarding-state-monotonic-merge ()
  "Concurrent independent writers preserve unrelated pages and maximum versions."
  (with-test-configuration (configuration)
    (let ((threads nil))
      (unwind-protect
           (progn
             (dotimes (index 8)
               (let ((index index))
                 (push
                  (make-thread
                   (lambda ()
                     (let ((pages (list (list :id      (format nil "slide-~D" index)
                                              :version 1))))
                       (onboarding-state-record configuration pages ':completed)
                       (loop for version downfrom 8 to 1
                             do (onboarding-state-record
                                 configuration (list (list :id "shared" :version version))
                                 (if (evenp index) ':completed ':refused)))))
                   :name "onboarding state writer")
                  threads)))
             (dolist (thread threads)
               (join-thread thread))
             (setf threads nil)
             (let* ((saved  (sexp-store:store-read (onboarding-state--store configuration)))
                    (shared (find "shared" saved :test #'string=
                                  :key                 (lambda (page) (getf page :id)))))
               (test-assert (= (length saved) 9)
                            "Concurrent fresh store instances preserve all distinct slides")
               (test-assert (= (getf shared :version) 8)
                            "Out-of-order writers cannot decrease acknowledged versions")
               (test-assert (eq (getf shared :status) ':completed)
                            "Completion wins over refusal for equal revisions")))
        (dolist (thread threads)
          (join-thread thread)))))
  nil)

(-> test-onboarding-state-invalid-records () null)
(defun test-onboarding-state-invalid-records ()
  "Reject malformed, unsupported, duplicate, circular and reader-eval records."
  (with-test-configuration (configuration)
    (let ((pages '((:id "intro" :version 1))))
      (dolist (text '(""
                      "(:onboarding :version 1 :pages ("
                      "(:onboarding :version 2 :pages ((:id \"intro\" :version 99 :status :completed)))"
                      "(:onboarding :version 1 :pages ((:id \"intro\" :version 1 :status :unknown)))"
                      "(:onboarding :version 1 :pages ((:id \"intro\" :id \"alias\" :version 1 :status :completed)))"
                      "(:onboarding :version 1 :pages ((:id \"intro\" :version 1 :status :completed) (:id \"intro\" :version 2 :status :refused)))"
                      "(:onboarding :version 1 :pages #1=((:id \"intro\" :version 1 :status :completed) . #1#))"
                      "(:onboarding :version 1 :pages nil) (:extra)"
                      "#.(error \"Reader evaluation ran\")"))
        (onboarding-state-test--write configuration text)
        (test-assert
         (test-signals-p 'onboarding-state-error
          (lambda () (onboarding-state-pending configuration pages)))
         "Unsupported or malformed snapshots do not acknowledge lessons")
        (test-assert
         (test-signals-p 'onboarding-state-error
          (lambda () (onboarding-state-record configuration pages ':completed)))
         "Recording does not silently overwrite an unreadable snapshot")
        (test-assert (string= text (uiop:read-file-string
                                    (onboarding-state-path configuration)))
                     "A failed read or record preserves the original bytes"))
      (let ((evaluated-p nil))
        (declare (special evaluated-p))
        (onboarding-state-test--write
         configuration "#.(progn (setf autolith::evaluated-p t) '(:onboarding :version 1 :pages nil))")
        (test-assert
         (test-signals-p 'onboarding-state-error
          (lambda () (onboarding-state-pending configuration pages)))
         "Reader evaluation syntax is rejected through the domain condition")
        (test-assert (not evaluated-p) "Snapshot reading never evaluates forms"))))
  nil)

(-> test-onboarding-state-recovery-and-validation () null)
(defun test-onboarding-state-recovery-and-validation ()
  "Recover bad state via reset or retry and reject updates over the page limit."
  (with-test-configuration (configuration)
    (let ((pages '((:id "intro" :version 1))))
      (onboarding-state-test--write configuration "(:unsupported)")
      (let ((pending
              (handler-bind
                  ((onboarding-state-error
                     (lambda (condition)
                       (let ((restart (find-restart 'reset-onboarding-state condition)))
                         (when restart (invoke-restart restart))))))
                (onboarding-state-pending configuration pages))))
        (test-assert (equal pending pages) "Explicit reset acknowledges nothing")
        (test-assert (string= (uiop:read-file-string (onboarding-state-path configuration))
                              "(:unsupported)")
                     "Resetting a read does not publish state"))
      (handler-bind
          ((onboarding-state-error
             (lambda (condition)
               (let ((restart (find-restart 'reset-onboarding-state condition)))
                 (when restart (invoke-restart restart))))))
        (onboarding-state-record configuration pages ':completed))
      (test-assert (null (onboarding-state-pending configuration pages))
                   "An explicit reset followed by successful recording publishes valid state")
      (onboarding-state-test--write configuration "(:unsupported)")
      (let ((retried-p nil))
        (handler-bind
            ((onboarding-state-error
               (lambda (condition)
                 (unless retried-p
                   (setf retried-p t)
                   (onboarding-state-test--write
                    configuration "(:onboarding :version 1 :pages nil)")
                   (invoke-restart (find-restart 'retry-onboarding-state condition))))))
          (test-assert (equal (onboarding-state-pending configuration pages) pages)
                       "Retry rereads corrected state")))
      (let ((*onboarding-state-page-limit* 1))
        (onboarding-state-record configuration pages ':completed)
        (test-assert
         (test-signals-p 'onboarding-state-error
          (lambda ()
            (onboarding-state-record configuration '((:id "extra" :version 1)) ':completed)))
         "Merging beyond the bounded page count is rejected before publication"))
      (test-assert (null (onboarding-state-pending configuration pages))
                   "Failed validation leaves prior acknowledgements available")))
  nil)

(-> test-onboarding-state-publication-failure () null)
(defun test-onboarding-state-publication-failure ()
  "A storage publication failure preserves the complete old durable state."
  (with-test-configuration (configuration)
    (let ((pages '((:id "intro" :version 1))))
      (onboarding-state-record configuration pages ':completed)
      (let ((before (uiop:read-file-string (onboarding-state-path configuration))))
        (test-call-with-function-replacements
         (list
          (list 'sexp-store:log-write
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  (error 'sexp-store:store-error
                         :message   "Injected publication failure."
                         :pathname  (onboarding-state-path configuration)
                         :operation ':publish))))
         (lambda ()
           (test-assert
            (test-signals-p 'onboarding-state-error
             (lambda ()
               (onboarding-state-record configuration '((:id "intro" :version 2)) ':completed)))
            "Publication failures propagate without falsely acknowledging a page")))
        (test-assert (string= before (uiop:read-file-string
                                      (onboarding-state-path configuration)))
                     "Publication failure preserves old snapshot bytes")
        (test-assert (equal (onboarding-state-pending configuration
                                                    '((:id "intro" :version 2)))
                            '((:id "intro" :version 2)))
                     "A failed newer acknowledgement is still pending"))))
  nil)

(-> test-onboarding-state-process-merge () null)
(defun test-onboarding-state-process-merge ()
  "Acknowledge slides in separate processes without losing either session's work."
  (with-test-fixture (':fork "process-shared onboarding state")
    (with-test-configuration (configuration)
      (let ((original-store (onboarding-state--store configuration)))
        (test-assert (null (sexp-store:store-read original-store))
                     "The parent initially observes an unacknowledged first launch")
        (multiple-value-bind (reaped-p clean-exit-p)
            (test-fixture-call-with-forked-holder
             *platform*
             (lambda ()
               (onboarding-state-record
                configuration '((:id "child" :version 1) (:id "shared" :version 4))
                ':refused))
             (lambda (ready-p)
               (test-assert ready-p "The other process publishes its acknowledgements")
               (onboarding-state-record
                configuration '((:id "parent" :version 1) (:id "shared" :version 1))
                ':completed)))
          (test-assert (and reaped-p clean-exit-p)
                       "The independent state writer exits cleanly"))
        (let* ((saved  (sexp-store:store-read original-store))
               (shared (find "shared" saved :test #'string=
                             :key                 (lambda (page) (getf page :id)))))
          (test-assert (= (length saved) 3)
                       "A previously opened store reads both sessions' merged slides")
          (test-assert (and (= (getf shared :version) 4)
                            (eq (getf shared :status) ':refused))
                       "An older acknowledgement cannot replace another process's newer refusal")))))
  nil)
