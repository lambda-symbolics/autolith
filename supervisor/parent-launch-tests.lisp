;;;; -- Optional Parent Launch Checks --

(in-package #:autolith-supervisor)

(declaim (ftype (function () parent-terminal) parent-launch-test--terminal))

(defun parent-launch-test--terminal ()
  "Create explicit trusted callbacks with no private state or real terminal I/O."
  (make-parent-terminal :input-source (lambda (timeout)
                                       (declare (ignore timeout))
                                       (values nil ':eof))
                        :renderer     (lambda (region kind payload)
                                        (declare (ignore region kind payload)))))

(declaim (ftype (function () parent-launch) parent-launch-test--object))

(defun parent-launch-test--object ()
  "Create only the short control state used by queue and wait checks."
  (make-instance 'parent-launch :store nil :replay-store nil
                               :terminal (parent-launch-test--terminal)))

(declaim (ftype (function (function keyword) boolean) parent-launch-test--fails-p))

(defun parent-launch-test--fails-p (function reason)
  "Return true only for the specified redacted launch condition."
  (handler-case
      (progn (funcall function) nil)
    (parent-launch-error (condition)
      (eq reason (parent-launch-error-reason condition)))))

(declaim (ftype (function () null) parent-launch-test--bounds))

(defun parent-launch-test--bounds ()
  "Bound both output event count and UTF-8 byte retention."
  (let ((launch (parent-launch-test--object)))
    (loop repeat 128 do (parent-launch--output launch ':progress "x"))
    (assert (parent-launch-test--fails-p
             (lambda () (parent-launch--output launch ':progress "x")) ':output-overflow))
    (assert (= 128 (length (parent-launch-output launch))))
    (assert (= 128 (parent-launch-output-bytes launch))))
  (let ((launch (parent-launch-test--object)))
    (parent-launch--output launch ':progress (make-string 65536 :initial-element #\a))
    (assert (parent-launch-test--fails-p
             (lambda () (parent-launch--output launch ':diagnostic "x")) ':output-overflow))
    (assert (= 65536 (parent-launch-output-bytes launch))))
  nil)

(declaim (ftype (function () null) parent-launch-test--lifecycle))

(defun parent-launch-test--lifecycle ()
  "Ignore another turn's completion and notify only the matching durable result."
  (let ((launch (parent-launch-test--object)))
    (setf (parent-launch-turn launch) "owned")
    (parent-launch--lifecycle launch :kind ':turn-finished :turn-id "other" :outcome ':completed)
    (assert (null (parent-launch-outcome launch)))
    (parent-launch--lifecycle launch :kind ':turn-finished :turn-id "owned" :outcome ':failed)
    (assert (eq ':failed (parent-launch-outcome launch)))
    (parent-launch--lifecycle launch :kind ':stopped :turn-id nil :outcome nil)
    (assert (parent-launch-stopped launch)))
  nil)

(declaim (ftype (function () null) parent-launch-test--wait))

(defun parent-launch-test--wait ()
  "Bound waits and preserve the first failure without accepting new work."
  (let ((launch (parent-launch-test--object)))
    (assert (not (parent-launch--wait launch (constantly nil) 0.01)))
    (parent-launch--fail launch ':connection-failed)
    (parent-launch--fail launch ':presentation-failed)
    (assert (eq ':connection-failed (parent-launch-failure launch)))
    (assert (not (parent-launch--wait launch (constantly nil) 10)))
    (assert (parent-launch-test--fails-p
             (lambda () (parent-launch-turn-run launch "next" "not sent")) ':not-accepting-input)))
  nil)

(declaim (ftype (function () null) parent-launch-test--explicit-environment))

(defun parent-launch-test--explicit-environment ()
  "Reject inherited-environment launch before opening storage or starting any child."
  (assert (parent-launch-test--fails-p
           (lambda ()
             (parent-launch-open :terminal (parent-launch-test--terminal)
                                 :profiles (make-hash-table) :accounts (make-hash-table)))
           ':invalid-configuration))
  nil)

(declaim (ftype (function () null) parent-launch-test--drain))

(defun parent-launch-test--drain ()
  "Drain accepted output before terminal close and serialize repeated cleanup."
  (let* ((rendered nil)
         (terminal (make-parent-terminal
                    :input-source (lambda (timeout) (declare (ignore timeout)) (values nil ':eof))
                    :renderer     (lambda (region kind payload)
                                    (declare (ignore region kind))
                                    (push (getf payload :text) rendered))))
         (launch   (make-instance 'parent-launch :store nil :replay-store nil :terminal terminal)))
    (parent-launch--output launch ':progress "first")
    (parent-launch--output launch ':progress "second")
    (setf (parent-launch-presenter launch)
          (sb-thread:make-thread (lambda () (parent-launch--present launch))))
    (parent-launch-close launch)
    (assert (equal '("second" "first") rendered))
    (assert (null (parent-launch-failure launch)))
    (assert (eq (parent-launch-evidence launch) (parent-launch-close launch))))
  nil)

(declaim (ftype (function () integer) run-parent-launch-unit-tests))

(defun run-parent-launch-unit-tests ()
  "Run parent-only control checks without loading the agent system."
  (assert (null (find-package '#:autolith)))
  (mapc #'funcall (list #'parent-launch-test--bounds #'parent-launch-test--lifecycle
                        #'parent-launch-test--wait #'parent-launch-test--explicit-environment
                        #'parent-launch-test--drain))
  5)
