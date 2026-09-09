(in-package #:autolith)

;;;; -- Windows Host Fixtures --

;;; The Windows implementation of the host fixture protocol declared in
;;; tests/test-support.lisp. Symbolic links are available once the account
;;; may create them, which Developer Mode or administrative rights grant.
;;; FIFOs, device nodes, POSIX file modes, forked children, pseudo-terminals,
;;; and the POSIX shell do not exist here, so the checks that need them are
;;; recorded as skipped.

(win32--define win32-fixture--create-symbolic-link "CreateSymbolicLinkW"
  (sb-alien:unsigned 8)
  (link win32-wide-string) (target win32-wide-string) (flags win32-dword))
(win32--define win32-fixture--remove-directory "RemoveDirectoryW" win32-bool
  (path win32-wide-string))
(win32--define win32-fixture--delete-file "DeleteFileW" win32-bool
  (path win32-wide-string))
(win32--define win32-fixture--create-pipe "CreatePipe" win32-bool
  (read-handle (* win32-handle)) (write-handle (* win32-handle))
  (attributes (* t)) (size win32-dword))

(defparameter *win32-symbolic-link-flag-directory* #x1
  "SYMBOLIC_LINK_FLAG_DIRECTORY.")

(defparameter *win32-symbolic-link-flag-allow-unprivileged-create* #x2
  "SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE, honoured in Developer Mode.")

(defvar *win32-fixture-symbolic-links-p* ':unknown
  "Whether this account may create symbolic links, probed once per process.")

(-> win32-fixture--native (string) string)
(defun win32-fixture--native (namestring)
  "Return NAMESTRING with Windows separators and no trailing separator."
  (string-right-trim "\\" (substitute #\\ #\/ namestring)))

(-> win32-fixture--unavailable (test-fixture-kind) nil)
(defun win32-fixture--unavailable (fixture)
  "Signal that FIXTURE is a POSIX facility Windows does not provide."
  (win32--unavailable fixture
                      (format nil "Windows hosts have no ~(~A~) for the tests to use."
                              fixture)))

(-> win32-fixture--symbolic-links-p () boolean)
(defun win32-fixture--symbolic-links-p ()
  "Return whether this account may create symbolic links, probing once."
  (when (eq *win32-fixture-symbolic-links-p* ':unknown)
    (let* ((root (test-make-temporary-root))
           (link (uiop:native-namestring (merge-pathnames "probe-link" root))))
      (unwind-protect
           (setf *win32-fixture-symbolic-links-p*
                 (handler-case
                     (progn
                       (test-fixture-make-symbolic-link *platform* "probe-target" link)
                       (test-fixture-remove-link *platform* link)
                       t)
                   (platform-error ()
                     nil)))
        (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore))))
  *win32-fixture-symbolic-links-p*)

(defmethod test-fixture-available-p ((platform win32-platform) fixture)
  "Symbolic links depend on the account; the other fixtures are POSIX facilities."
  (declare (ignore platform))
  (check-type fixture test-fixture-kind)
  (and (eq fixture ':symbolic-links)
       (win32-fixture--symbolic-links-p)))


;;;; -- Files --

(defmethod test-fixture-make-symbolic-link ((platform win32-platform)
                                            target link)
  "Create LINK with CreateSymbolicLinkW, as a directory link when TARGET is one."
  (declare (ignore platform))
  (let* ((link-pathname (uiop:parse-native-namestring (win32-fixture--native link)))
         (target-pathname
           (merge-pathnames (uiop:parse-native-namestring target)
                            (uiop:pathname-directory-pathname link-pathname)))
         (flags (logior *win32-symbolic-link-flag-allow-unprivileged-create*
                        (if (uiop:directory-exists-p target-pathname)
                            *win32-symbolic-link-flag-directory*
                            0))))
    (when (zerop (win32-fixture--create-symbolic-link
                  (win32-fixture--native link)
                  (substitute #\\ #\/ target)
                  flags))
      (win32--fail ':link link-pathname))
    nil))

(defmethod test-fixture-remove-link ((platform win32-platform) link)
  "Delete LINK through the entry point for its kind, leaving its target alone."
  (declare (ignore platform))
  (let* ((native (win32-fixture--native link))
         (pathname (uiop:parse-native-namestring native))
         (attributes (win32--get-file-attributes native)))
    (when (= attributes *win32-invalid-file-attributes*)
      (win32--fail ':unlink pathname))
    (when (zerop (if (logtest attributes *win32-file-attribute-directory*)
                     (win32-fixture--remove-directory native)
                     (win32-fixture--delete-file native)))
      (win32--fail ':unlink pathname))
    nil))

(defmethod test-fixture-make-fifo ((platform win32-platform) pathname)
  "Windows has no FIFOs."
  (declare (ignore platform pathname))
  (win32-fixture--unavailable ':fifos))

(defmethod test-fixture-device-node ((platform win32-platform))
  "Windows device names are not filesystem entries the tests can classify."
  (declare (ignore platform))
  (win32-fixture--unavailable ':device-nodes))

(defmethod test-fixture-file-mode ((platform win32-platform) pathname)
  "Windows files carry no POSIX permission bits."
  (declare (ignore platform pathname))
  (win32-fixture--unavailable ':file-modes))

(defmethod test-fixture-set-file-mode ((platform win32-platform) pathname mode)
  "Windows files carry no POSIX permission bits."
  (declare (ignore platform pathname mode))
  (win32-fixture--unavailable ':file-modes))

(defmethod test-fixture-permissions-p ((platform win32-platform)
                                       pathname permissions)
  "Check the owner-only access control list and its withheld write access."
  (let ((status (platform-path-status platform pathname)))
    (ecase permissions
      ((:private-file :private-directory)
       (platform-file-status-private-p status))
      (:read-only
       (and (platform-file-status-private-p status)
            (platform-file-status-read-only-p status))))))


;;;; -- Descriptors and Processes --

(-> win32-fixture--create-pipe-handles () (values integer integer))
(defun win32-fixture--create-pipe-handles ()
  "Create an anonymous pipe and return its read and write handles."
  (sb-alien:with-alien ((read-handle win32-handle)
                        (write-handle win32-handle))
    (when (zerop (win32-fixture--create-pipe (sb-alien:addr read-handle)
                                             (sb-alien:addr write-handle)
                                             nil
                                             0))
      (win32--fail ':pipe nil))
    (values read-handle write-handle)))

(-> win32-fixture--handle-stream (integer (member :input :output)) stream)
(defun win32-fixture--handle-stream (handle direction)
  "Return an unbuffered character stream over pipe HANDLE in DIRECTION."
  (sb-sys:make-fd-stream handle
                         :input (eq direction ':input)
                         :output (eq direction ':output)
                         :element-type 'character
                         :external-format ':utf-8
                         :buffering ':none
                         :auto-close nil))

(defmethod test-fixture-call-with-descriptor-input ((platform win32-platform)
                                                    content function)
  "Feed CONTENT through an anonymous pipe whose handles back the streams."
  (declare (ignore platform))
  (multiple-value-bind (read-handle write-handle)
      (win32-fixture--create-pipe-handles)
    (let ((input nil)
          (output nil))
      (unwind-protect
           (progn
             (setf output (win32-fixture--handle-stream write-handle ':output))
             (write-string content output)
             (finish-output output)
             (close output)
             (setf input (win32-fixture--handle-stream read-handle ':input))
             (funcall function input))
        (if input
            (close input)
            (win32--close-handle read-handle))
        (if output
            (close output)
            (win32--close-handle write-handle))))))

(defmethod test-fixture-run-forked ((platform win32-platform) function)
  "Windows processes cannot fork."
  (declare (ignore platform function))
  (win32-fixture--unavailable ':fork))

(defmethod test-fixture-call-with-forked-holder ((platform win32-platform)
                                                 holder-function function)
  "Windows processes cannot fork."
  (declare (ignore platform holder-function function))
  (win32-fixture--unavailable ':fork))


;;;; -- Terminals --

(defmethod test-fixture-call-with-pseudo-terminal ((platform win32-platform)
                                                   function)
  "Windows offers no pseudo-terminal the tests can drive through a descriptor."
  (declare (ignore platform function))
  (win32-fixture--unavailable ':pseudo-terminals))

(defmethod test-fixture-terminal-input-mode ((platform win32-platform)
                                             descriptor)
  "Windows offers no pseudo-terminal the tests can drive through a descriptor."
  (declare (ignore platform descriptor))
  (win32-fixture--unavailable ':pseudo-terminals))

(defmethod test-fixture-terminal-echo-p ((platform win32-platform) descriptor)
  "Windows offers no pseudo-terminal the tests can drive through a descriptor."
  (declare (ignore platform descriptor))
  (win32-fixture--unavailable ':pseudo-terminals))
