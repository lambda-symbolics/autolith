(in-package #:autolith-supervisor)

(defun memory--make-identifier ()
  "Return a random version 4 UUID, unguessable and unique across processes.

Its octets come from ironclad's operating-system random source on every host."
  (let ((octets (ironclad:random-data 16)))
    (setf (aref octets 6) (logior 64 (logand (aref octets 6) 15))
          (aref octets 8) (logior 128 (logand (aref octets 8) 63)))
    (format nil "~(~{~2,'0X~}-~{~2,'0X~}-~{~2,'0X~}-~{~2,'0X~}-~{~2,'0X~}~)"
            (coerce (subseq octets 0 4) 'list) (coerce (subseq octets 4 6) 'list)
            (coerce (subseq octets 6 8) 'list) (coerce (subseq octets 8 10) 'list)
            (coerce (subseq octets 10 16) 'list))))

(defun memory--state-lock-pathname (path name)
  "Return the private sibling lock path."
  (merge-pathnames name (uiop/pathname:pathname-directory-pathname path)))

(defun memory--path-status (path &key follow-links-p)
  "Return file status, or NIL for a missing file."
  (handler-case (ls-compat.posix:file-information path :follow-links-p follow-links-p)
                (ls-compat.posix:file-operation-failed (condition)
                 (if (eq (ls-compat.posix:file-operation-failed-reason condition) :missing)
                     nil
                     (error condition)))))
