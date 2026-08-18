(in-package #:autolith)

;;;; -- Recursive Inference Context Objects --

(defclass rlm-context-object ()
  ((digest
    :initarg :digest
    :reader rlm-context-object-digest
    :type non-empty-string
    :documentation "The SHA-256 content digest naming this immutable object.")
   (label
    :initarg :label
    :reader rlm-context-object-label
    :type non-empty-string
    :documentation "The short human-meaningful name shown to the root model.")
   (characters
    :initarg :characters
    :reader rlm-context-object-characters
    :type (integer 0)
    :documentation "The exact character count of the stored content.")
   (pathname
    :initarg :pathname
    :reader rlm-context-object-pathname
    :type pathname
    :documentation "The content-addressed file holding the exact content."))
  (:documentation
   "One immutable content-addressed context object handled by reference."))

(-> rlm-object-root (configuration) pathname)
(defun rlm-object-root (configuration)
  "Return the content-addressed immutable context object directory."
  (merge-pathnames "inferences/objects/"
                   (configuration-data-root configuration)))

(-> rlm-object--pathname (configuration string) pathname)
(defun rlm-object--pathname (configuration digest)
  "Return DIGEST's canonical object pathname under CONFIGURATION."
  (merge-pathnames (make-pathname :name digest :type "txt")
                   (rlm-object-root configuration)))

(-> rlm-context-intern
    (configuration string &key (:label (option string)))
    rlm-context-object)
(defun rlm-context-intern (configuration content &key label)
  "Store CONTENT once under its digest and return its object handle.

Interning identical content is idempotent: the object file is written
atomically the first time and reused afterwards, so repeated or shared
context never duplicates storage."
  (let* ((digest (rlm-view--digest content))
         (pathname (rlm-object--pathname configuration digest)))
    (unless (probe-file pathname)
      (ensure-directories-exist pathname)
      (uiop:with-temporary-file (:pathname temporary
                                 :stream stream
                                 :keep t
                                 :directory (rlm-object-root configuration)
                                 :prefix "intern"
                                 :external-format ':utf-8)
        (write-string content stream)
        (finish-output stream)
        :close-stream
        ;; Concurrent interns of the same digest write identical bytes, so
        ;; the last rename winning is harmless.
        (rename-file temporary pathname)))
    (make-instance 'rlm-context-object
                   :digest digest
                   :label (or label "context")
                   :characters (length content)
                   :pathname pathname)))

(-> rlm-context-intern-pathname
    (configuration pathname &key (:label (option string)))
    rlm-context-object)
(defun rlm-context-intern-pathname (configuration pathname &key label)
  "Intern the file at PATHNAME, defaulting the label to its name."
  (handler-case
      (rlm-context-intern configuration
                          (uiop:read-file-string pathname)
                          :label (or label (file-namestring pathname)))
    (error (condition)
      (error 'rlm-view-error
             :designator pathname
             :message (format nil "~A" condition)))))

(-> rlm-context-object-find
    (configuration string)
    (option rlm-context-object))
(defun rlm-context-object-find (configuration digest)
  "Return the stored object handle for DIGEST, or NIL when absent."
  (let ((pathname (rlm-object--pathname configuration digest)))
    (when (probe-file pathname)
      (make-instance 'rlm-context-object
                     :digest digest
                     :label "context"
                     :characters (length (uiop:read-file-string pathname))
                     :pathname pathname))))

(-> rlm-context-designator-object (configuration t) rlm-context-object)
(defun rlm-context-designator-object (configuration designator)
  "Intern one root context DESIGNATOR: an object, string, or pathname."
  (typecase designator
    (rlm-context-object designator)
    (string (rlm-context-intern configuration designator))
    (pathname (rlm-context-intern-pathname configuration designator))
    (cons
     (let ((label (getf designator ':label))
           (content (getf designator ':content))
           (path (getf designator ':path)))
       (cond
         ((stringp content)
          (rlm-context-intern configuration content :label label))
         ((pathnamep path)
          (rlm-context-intern-pathname configuration path :label label))
         (t
          (error 'rlm-view-error
                 :designator designator
                 :message "expected :label with exactly one of :content or :path")))))
    (t
     (error 'rlm-view-error
            :designator designator
            :message "expected a context object, string, pathname, or plist"))))
