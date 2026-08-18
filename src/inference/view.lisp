(in-package #:autolith)

;;;; -- Recursive Inference Context Views --

(define-condition rlm-view-error
    (error)
  ((designator
    :initarg :designator
    :reader rlm-view-error-designator
    :type t
    :documentation "The view designator that could not be materialized.")
   (message
    :initarg :message
    :reader rlm-view-error-message
    :type string
    :documentation "The concise materialization failure."))
  (:documentation "A context view designator could not be materialized.")
  (:report
   (lambda (condition stream)
     (format stream "Invalid context view ~S: ~A"
             (rlm-view-error-designator condition)
             (rlm-view-error-message condition)))))

(defclass rlm-view ()
  ((label
    :initarg :label
    :reader rlm-view-label
    :type string
    :documentation "The short name identifying this view inside the frame.")
   (origin
    :initarg :origin
    :reader rlm-view-origin
    :type string
    :documentation "The literal marker or source pathname the content came from.")
   (content
    :initarg :content
    :reader rlm-view-content
    :type string
    :documentation "The exact read-only text the frame sees.")
   (digest
    :initarg :digest
    :reader rlm-view-digest
    :type string
    :documentation "The SHA-256 content digest recorded for provenance."))
  (:documentation
   "One materialized read-only context block passed to an inference frame."))

(-> rlm-view--digest (string) string)
(defun rlm-view--digest (content)
  "Return the lowercase SHA-256 hex digest of CONTENT's exact UTF-8 bytes."
  (string-downcase
   (with-output-to-string (stream)
     (loop for octet across (digest-sequence
                             ':sha256
                             (sb-ext:string-to-octets content
                                                      :external-format ':utf-8))
           do (format stream "~2,'0X" octet)))))

(-> rlm-view--create (string string string) rlm-view)
(defun rlm-view--create (label origin content)
  "Create a materialized view of CONTENT with provenance LABEL and ORIGIN."
  (make-instance 'rlm-view
                 :label label
                 :origin origin
                 :content content
                 :digest (rlm-view--digest content)))

(-> rlm-view--from-pathname (pathname (option string)) rlm-view)
(defun rlm-view--from-pathname (pathname label)
  "Materialize the file at PATHNAME, defaulting LABEL to its name."
  (let ((content
          (handler-case
              (uiop:read-file-string pathname)
            (error (condition)
              (error 'rlm-view-error
                     :designator pathname
                     :message (format nil "~A" condition))))))
    (rlm-view--create (or label (file-namestring pathname))
                      (namestring pathname)
                      content)))

(-> rlm-view--from-plist (list) rlm-view)
(defun rlm-view--from-plist (designator)
  "Materialize a (:label ... :content ...) or (:label ... :path ...) plist."
  (let ((label (getf designator ':label))
        (content (getf designator ':content))
        (path (getf designator ':path)))
    (unless (and (or (null label) (stringp label))
                 (or (stringp content) (pathnamep path))
                 (not (and content path)))
      (error 'rlm-view-error
             :designator designator
             :message "expected :label with exactly one of :content or :path"))
    (if content
        (rlm-view--create (or label "literal") "literal" content)
        (rlm-view--from-pathname path label))))

(-> rlm-view-materialize (t) rlm-view)
(defun rlm-view-materialize (designator)
  "Materialize one view DESIGNATOR: a string, a pathname, or a plist."
  (typecase designator
    (rlm-view designator)
    (string (rlm-view--create "literal" "literal" designator))
    (pathname (rlm-view--from-pathname designator nil))
    (cons (rlm-view--from-plist designator))
    (t (error 'rlm-view-error
              :designator designator
              :message "expected a string, a pathname, or a view plist"))))

(-> rlm-views-materialize (list) list)
(defun rlm-views-materialize (designators)
  "Materialize DESIGNATORS in order, numbering duplicate labels."
  (let ((views (mapcar #'rlm-view-materialize designators))
        (counts (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal)))
    (dolist (view views)
      (incf (gethash (rlm-view-label view) counts 0)))
    (loop for view in views
          for label = (rlm-view-label view)
          collect (if (> (gethash label counts) 1)
                      (rlm-view--create
                       (format nil "~A#~D" label (incf (gethash label seen 0)))
                       (rlm-view-origin view)
                       (rlm-view-content view))
                      view))))

(-> rlm-views-render (list) (option string))
(defun rlm-views-render (views)
  "Render materialized VIEWS as delimited read-only context blocks.

The closing delimiter repeats the content digest, so view content
cannot forge its own end or another view: embedding a closing tag
changes the digest away from the embedded value, and finding a
fixpoint is a preimage problem."
  (when views
    (with-output-to-string (stream)
      (loop for view in views
            for first-p = t then nil
            for digest = (subseq (rlm-view-digest view) 0 12)
            do (unless first-p
                 (terpri stream))
               (format stream "<view label=~S origin=~S sha256=~S>~%~A~%</view sha256=~S>~%"
                       (rlm-view-label view)
                       (rlm-view-origin view)
                       digest
                       (rlm-view-content view)
                       digest)))))
