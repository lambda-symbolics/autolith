(in-package #:autolith-supervisor)

;;;; -- User Input --

(defgeneric user-message-input-text (input)
  (:documentation "Return the editable text carried by user INPUT."))

(defgeneric user-message-input-image-pathnames (input)
  (:documentation "Return the absolute local image pathnames attached to user INPUT."))

(defclass user-message-input ()
  ((text
    :initarg       :text
    :reader        user-message-input-text
    :type          string
    :documentation "The editable user text, including visible image labels.")
   (image-pathnames
    :initarg       :image-pathnames
    :initform      nil
    :reader        user-message-input-image-pathnames
    :type          list
    :documentation "Absolute local image pathnames attached to this submission."))
  (:documentation "One user submission containing text and local image attachments."))

(defmethod user-message-input-text ((input string))
  "Plain string INPUT is its own text."
  input)

(defmethod user-message-input-image-pathnames ((input string))
  "Plain string INPUT carries no image attachments."
  (declare (ignore input)))

(serapeum/types:-> user-message-input-create
    (&key (:text string) (:image-pathnames list))
    user-message-input)
(defun user-message-input-create (&key (text "") image-pathnames)
  "Create a validated inert submission without reading image contents."
  (unless
      (and (stringp text) (or (plusp (length text)) image-pathnames)
           (listp image-pathnames) (every #'pathnamep image-pathnames)
           (every #'uiop/pathname:absolute-pathname-p image-pathnames))
    (error 'type-error :datum (list text image-pathnames) :expected-type
           '(cons string list)))
  (make-instance 'user-message-input :text text :image-pathnames
                 (copy-list image-pathnames)))

(defgeneric user-message-input-copy (input)
  (:documentation "Return a detached copy of user INPUT."))

(defmethod user-message-input-copy ((input string))
  "Return a fresh copy of plain string INPUT."
  (copy-seq input))

(defmethod user-message-input-copy ((input user-message-input))
  "Return a submission with copies of INPUT's text and image pathname list."
  (make-instance 'user-message-input
                 :text (copy-seq (user-message-input-text input))
                 :image-pathnames
                 (copy-list (user-message-input-image-pathnames input))))
