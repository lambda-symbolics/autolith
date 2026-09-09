(in-package #:autolith)

;;;; -- User Input --

(defgeneric user-message-input-text (input)
  (:documentation "Return the editable text carried by user INPUT."))

(defgeneric user-message-input-image-pathnames (input)
  (:documentation "Return the absolute local image pathnames attached to user INPUT."))

(defclass user-message-input ()
  ((text
    :initarg :text
    :reader user-message-input-text
    :type string
    :documentation "The editable user text, including visible image labels.")
   (image-pathnames
    :initarg :image-pathnames
    :initform nil
    :reader user-message-input-image-pathnames
    :type list
    :documentation "Absolute local image pathnames attached to this submission."))
  (:documentation "One user submission containing text and local image attachments."))

(-> user-message-input-text ((or string user-message-input)) string)
(-> user-message-input-image-pathnames ((or string user-message-input)) list)

(defmethod user-message-input-text ((input string))
  input)

(defmethod user-message-input-image-pathnames ((input string))
  (declare (ignore input)))

(-> user-message-input-create
    (&key (:text string) (:image-pathnames list))
    user-message-input)
(defun user-message-input-create (&key (text "") image-pathnames)
  "Create one validated user submission from TEXT and IMAGE-PATHNAMES."
  (unless (or (non-empty-string-p text) image-pathnames)
    (error 'configuration-error
           :message "A user submission requires text or an image."))
  (unless (every #'pathnamep image-pathnames)
    (error 'configuration-error
           :message "Every attached image must have an absolute pathname."))
  (unless (every #'uiop:absolute-pathname-p image-pathnames)
    (error 'configuration-error
           :message "Every attached image pathname must be absolute."))
  (make-instance 'user-message-input
                 :text text
                 :image-pathnames (copy-list image-pathnames)))

(-> user-message-input-copy ((or string user-message-input))
    (or string user-message-input))
(defgeneric user-message-input-copy (input)
  (:documentation "Return a detached copy of user INPUT."))

(defmethod user-message-input-copy ((input string))
  (copy-seq input))

(defmethod user-message-input-copy ((input user-message-input))
  (make-instance 'user-message-input
                 :text (copy-seq (user-message-input-text input))
                 :image-pathnames
                 (copy-list (user-message-input-image-pathnames input))))


;;;; -- Prepared Attachments --

(defparameter *image-input-maximum-source-bytes* (* 1024 1024 1024)
  "The Codex-compatible sanity limit for one source image.")

(defparameter *image-input-maximum-dimension* 2048
  "The maximum high-detail prompt-image width or height.")

(defparameter *image-input-patch-size* 32
  "The provider image-token patch width and height.")

(defparameter *image-input-maximum-patches* 2500
  "The maximum high-detail prompt-image patch count.")

(defclass image-attachment ()
  ((identifier
    :initarg :identifier
    :reader image-attachment-identifier
    :type non-empty-string
    :documentation "The stable identifier of this conversation artifact.")
   (pathname
    :initarg :pathname
    :reader image-attachment-pathname
    :type pathname
    :documentation "The private prepared image artifact pathname.")
   (source-name
    :initarg :source-name
    :reader image-attachment-source-name
    :type non-empty-string
    :documentation "The original absolute pathname shown to the model.")
   (mime-type
    :initarg :mime-type
    :reader image-attachment-mime-type
    :type non-empty-string
    :documentation "The media type of the prepared artifact.")
   (width
    :initarg :width
    :reader image-attachment-width
    :type (integer 1)
    :documentation "The prepared image width in pixels.")
   (height
    :initarg :height
    :reader image-attachment-height
    :type (integer 1)
    :documentation "The prepared image height in pixels."))
  (:documentation "A validated provider-ready image stored outside conversation text."))

(-> image-input--error (pathname keyword string &optional t) null)
(defun image-input--error (pathname stage message &optional cause)
  "Signal a structured image failure for PATHNAME at STAGE."
  (error 'image-input-error
         :message message
         :pathname pathname
         :stage stage
         :cause cause))

(-> image-input--octets-match-p
    ((simple-array (unsigned-byte 8) (*)) integer list)
    boolean)
(defun image-input--octets-match-p (bytes start expected)
  "Return true when BYTES contains EXPECTED octets beginning at START."
  (and (<= (+ start (length expected)) (length bytes))
       (loop for expected-octet in expected
             for index from start
             always (= (aref bytes index) expected-octet))))

(-> image-input--ascii-match-p
    ((simple-array (unsigned-byte 8) (*)) integer string)
    boolean)
(defun image-input--ascii-match-p (bytes start expected)
  "Return true when BYTES contains ASCII EXPECTED beginning at START."
  (and (<= (+ start (length expected)) (length bytes))
       (loop for character across expected
             for index from start
             always (= (aref bytes index) (char-code character)))))

(-> image-input--unsigned-little-endian
    ((simple-array (unsigned-byte 8) (*)) integer integer)
    integer)
(defun image-input--unsigned-little-endian (bytes start count)
  "Decode COUNT little-endian octets from BYTES beginning at START."
  (loop for index from start below (+ start count)
        for shift from 0 by 8
        sum (ash (aref bytes index) shift)))

(-> image-input--unsigned-big-endian
    ((simple-array (unsigned-byte 8) (*)) integer integer)
    integer)
(defun image-input--unsigned-big-endian (bytes start count)
  "Decode COUNT big-endian octets from BYTES beginning at START."
  (loop with value = 0
        for index from start below (+ start count)
        do (setf value (+ (ash value 8) (aref bytes index)))
        finally (return value)))

(-> image-input--format
    ((simple-array (unsigned-byte 8) (*)))
    (option keyword))
(defun image-input--format (bytes)
  "Recognize a supported prompt-image format from leading BYTES."
  (cond
    ((image-input--octets-match-p bytes 0 '(137 80 78 71 13 10 26 10))
     ':png)
    ((image-input--octets-match-p bytes 0 '(255 216 255))
     ':jpeg)
    ((or (image-input--ascii-match-p bytes 0 "GIF87a")
         (image-input--ascii-match-p bytes 0 "GIF89a"))
     ':gif)
    ((and (image-input--ascii-match-p bytes 0 "RIFF")
          (image-input--ascii-match-p bytes 8 "WEBP"))
     ':webp)
    (t
     nil)))

(-> image-input--jpeg-dimensions
    ((simple-array (unsigned-byte 8) (*)))
    (values integer integer))
(defun image-input--jpeg-dimensions (bytes)
  "Return JPEG width and height from complete BYTES."
  (block image-input--jpeg-dimensions
    (let ((position 2))
      (loop while (< (+ position 3) (length bytes))
            do (if (/= (aref bytes position) #xff)
                   (incf position)
                   (progn
                     (loop while (and (< position (length bytes))
                                      (= (aref bytes position) #xff))
                           do (incf position))
                     (when (>= position (length bytes))
                       (return))
                     (let ((marker (aref bytes position)))
                       (incf position)
                       (cond
                         ((member marker '(#xd8 #xd9 #x01))
                          nil)
                         ((<= #xd0 marker #xd7)
                          nil)
                         ((>= (+ position 1) (length bytes))
                          (return))
                         (t
                          (let ((segment-length
                                  (image-input--unsigned-big-endian
                                   bytes position 2)))
                            (when (or (< segment-length 2)
                                      (> (+ position segment-length)
                                         (length bytes)))
                              (return))
                            (when (and
                                   (member marker
                                           '(#xc0 #xc1 #xc2 #xc3 #xc5 #xc6 #xc7
                                             #xc9 #xca #xcb #xcd #xce #xcf))
                                   (<= (+ position 6) (length bytes)))
                              (return-from image-input--jpeg-dimensions
                                (values
                                 (image-input--unsigned-big-endian
                                  bytes (+ position 5) 2)
                                 (image-input--unsigned-big-endian
                                  bytes (+ position 3) 2))))
                            (incf position segment-length)))))))))
    (values 0 0)))

(-> image-input--webp-dimensions
    ((simple-array (unsigned-byte 8) (*)))
    (values integer integer))
(defun image-input--webp-dimensions (bytes)
  "Return WebP width and height from leading BYTES."
  (cond
    ((image-input--ascii-match-p bytes 12 "VP8X")
     (if (>= (length bytes) 30)
         (values (1+ (image-input--unsigned-little-endian bytes 24 3))
                 (1+ (image-input--unsigned-little-endian bytes 27 3)))
         (values 0 0)))
    ((image-input--ascii-match-p bytes 12 "VP8L")
     (if (and (>= (length bytes) 25) (= (aref bytes 20) #x2f))
         (let ((b0 (aref bytes 21))
               (b1 (aref bytes 22))
               (b2 (aref bytes 23))
               (b3 (aref bytes 24)))
           (values (1+ (logior b0 (ash (logand b1 #x3f) 8)))
                   (1+ (logior (ash b1 -6)
                               (ash b2 2)
                               (ash (logand b3 #x0f) 10)))))
         (values 0 0)))
    ((and (image-input--ascii-match-p bytes 12 "VP8 ")
          (image-input--octets-match-p bytes 23 '(157 1 42))
          (>= (length bytes) 30))
     (values (logand (image-input--unsigned-little-endian bytes 26 2) #x3fff)
             (logand (image-input--unsigned-little-endian bytes 28 2) #x3fff)))
    (t
     (values 0 0))))

(-> image-input--dimensions
    (keyword (simple-array (unsigned-byte 8) (*)))
    (values integer integer))
(defun image-input--dimensions (format bytes)
  "Return FORMAT's width and height from complete or leading BYTES."
  (case format
    (:png
     (if (>= (length bytes) 24)
         (values (image-input--unsigned-big-endian bytes 16 4)
                 (image-input--unsigned-big-endian bytes 20 4))
         (values 0 0)))
    (:gif
     (if (>= (length bytes) 10)
         (values (image-input--unsigned-little-endian bytes 6 2)
                 (image-input--unsigned-little-endian bytes 8 2))
         (values 0 0)))
    (:jpeg
     (image-input--jpeg-dimensions bytes))
    (:webp
     (image-input--webp-dimensions bytes))
    (t
     (values 0 0))))

(-> image-input--read-file
    (pathname)
    (simple-array (unsigned-byte 8) (*)))
(defun image-input--read-file (pathname)
  "Read PATHNAME after enforcing the source-byte sanity limit."
  (with-open-file (stream pathname
                          :direction ':input
                          :element-type '(unsigned-byte 8))
    (let ((length (file-length stream)))
      (when (> length *image-input-maximum-source-bytes*)
        (image-input--error
         pathname
         ':recognition
         (format nil "Image ~A is larger than the ~:D-byte input limit."
                 pathname
                 *image-input-maximum-source-bytes*)))
      (let ((bytes (make-array length :element-type '(unsigned-byte 8))))
        (unless (= (read-sequence bytes stream) length)
          (image-input--error pathname ':recognition
                              (format nil "Image ~A could not be read completely."
                                      pathname)))
        bytes))))

(defparameter *image-input-inspection-prefix-bytes* 30
  "The leading bytes identifying every format and every non-JPEG dimension.")

(-> image-input--inspect
    (pathname)
    (values keyword integer integer))
(defun image-input--inspect (pathname)
  "Identify PATHNAME, returning its image format and pixel dimensions.

Only JPEG reads the complete file, because its dimension marker can
follow arbitrarily large metadata segments; every other supported
format is identified from the leading bytes alone."
  (let ((absolute
          (handler-case
              (platform-truename *platform* pathname)
            (error (condition)
              (image-input--error
               pathname ':recognition
               (format nil "Image ~A does not exist or cannot be read." pathname)
               condition)))))
    (with-open-file (stream absolute
                            :direction ':input
                            :element-type '(unsigned-byte 8))
      (let ((length (file-length stream)))
        (when (> length *image-input-maximum-source-bytes*)
          (image-input--error
           absolute
           ':recognition
           (format nil "Image ~A is larger than the ~:D-byte input limit."
                   absolute
                   *image-input-maximum-source-bytes*)))
        (let* ((prefix-length
                 (min length *image-input-inspection-prefix-bytes*))
               (bytes (make-array prefix-length
                                  :element-type '(unsigned-byte 8))))
          (unless (= (read-sequence bytes stream) prefix-length)
            (image-input--error
             absolute ':recognition
             (format nil "Image ~A could not be read completely." absolute)))
          (let ((format (image-input--format bytes)))
            (unless format
              (image-input--error
               absolute ':recognition
               (format nil
                       "Autolith cannot attach ~A: use PNG, JPEG, GIF, or WebP."
                       absolute)))
            (when (eq format ':jpeg)
              (let ((complete (make-array length
                                          :element-type '(unsigned-byte 8))))
                (replace complete bytes)
                (unless (= (read-sequence complete stream
                                          :start prefix-length)
                           length)
                  (image-input--error
                   absolute ':recognition
                   (format nil "Image ~A could not be read completely."
                           absolute)))
                (setf bytes complete)))
            (multiple-value-bind (width height)
                (image-input--dimensions format bytes)
              (unless (and (plusp width) (plusp height))
                (image-input--error
                 absolute ':recognition
                 (format nil "Image ~A has no valid pixel dimensions."
                         absolute)))
              (values format width height))))))))

(-> image-input--dimensions-fit-p (integer integer) boolean)
(defun image-input--dimensions-fit-p (width height)
  "Return true when WIDTH and HEIGHT satisfy Codex high-detail limits."
  (and (<= width *image-input-maximum-dimension*)
       (<= height *image-input-maximum-dimension*)
       (<= (* (ceiling width *image-input-patch-size*)
              (ceiling height *image-input-patch-size*))
           *image-input-maximum-patches*)))

(-> image-input--target-dimensions (integer integer) (values integer integer))
(defun image-input--target-dimensions (width height)
  "Return WIDTH and HEIGHT reduced to Codex high-detail prompt limits."
  (if (image-input--dimensions-fit-p width height)
      (values width height)
      (let* ((maximum (max width height))
             (dimension-scale
               (min 1.0d0 (/ *image-input-maximum-dimension*
                             (coerce maximum 'double-float))))
             (scaled-width (max 1 (round (* width dimension-scale))))
             (scaled-height (max 1 (round (* height dimension-scale)))))
        (if (image-input--dimensions-fit-p scaled-width scaled-height)
            (values scaled-width scaled-height)
            (let* ((patch-size (coerce *image-input-patch-size* 'double-float))
                   (scale
                     (sqrt (/ (* patch-size patch-size
                                 *image-input-maximum-patches*)
                              (* (coerce scaled-width 'double-float)
                                 scaled-height))))
                   (patches-wide (/ (* scaled-width scale) patch-size))
                   (patches-high (/ (* scaled-height scale) patch-size))
                   (adjusted-scale
                     (* scale
                        (min (/ (floor patches-wide) patches-wide)
                             (/ (floor patches-high) patches-high)))))
              (values (max 1 (floor (* scaled-width adjusted-scale)))
                      (max 1 (floor (* scaled-height adjusted-scale)))))))))

(-> image-input--decoded-image (pathname keyword) array)
(defun image-input--decoded-image (pathname format)
  "Decode PATHNAME as FORMAT through the pinned Lisp image codec."
  (handler-case
      (with-open-file (stream pathname
                              :direction ':input
                              :element-type '(unsigned-byte 8))
        (read-image-stream stream
                           (ecase format
                             (:png "png")
                             (:jpeg "jpeg")
                             (:gif "gif"))))
    (error (condition)
      (image-input--error
       pathname ':decoding
       (format nil "Image ~A could not be decoded: ~A" pathname condition)
       condition))))

(-> image-input--8-bit-image (array) array)
(defun image-input--8-bit-image (image)
  "Coerce IMAGE to a PNG-writable eight-bit representation."
  (cond
    ((typep image 'rgba-image)
     (coerce-image image '8-bit-rgba-image))
    ((typep image 'rgb-image)
     (coerce-image image '8-bit-rgb-image))
    ((typep image 'gray-alpha-image)
     (coerce-image image '8-bit-gray-alpha-image))
    ((typep image 'gray-image)
     (coerce-image image '8-bit-gray-image))
    (t
     (error "Unsupported decoded image representation ~S." (type-of image)))))

(-> image-input--temporary-pathname (pathname string) pathname)
(defun image-input--temporary-pathname (target identifier)
  "Return a private temporary pathname beside TARGET for IDENTIFIER."
  (make-pathname :name (format nil ".~A.~A"
                               (pathname-name target)
                               identifier)
                 :type "tmp"
                 :defaults target))

(-> image-input--publish
    (pathname pathname keyword integer integer string)
    image-attachment)
(defun image-input--publish
    (source artifact-root format width height source-name)
  "Prepare SOURCE under ARTIFACT-ROOT and return its immutable attachment."
  (multiple-value-bind (target-width target-height)
      (image-input--target-dimensions width height)
    (let* ((identifier (make-identifier))
           (preserve-p (and (member format '(:png :jpeg))
                            (= width target-width)
                            (= height target-height)))
           (output-format (if preserve-p format ':png))
           (extension (ecase output-format
                        (:png "png")
                        (:jpeg "jpg")))
           (mime-type (ecase output-format
                        (:png "image/png")
                        (:jpeg "image/jpeg")))
           (target (merge-pathnames
                    (make-pathname :name identifier :type extension)
                    artifact-root))
           (temporary (image-input--temporary-pathname target identifier)))
      (ensure-directories-exist target)
      (platform-make-private *platform* artifact-root)
      (unwind-protect
           (handler-case
               (progn
                 (if preserve-p
                     (progn
                       (image-input--decoded-image source format)
                       (uiop:copy-file source temporary))
                     (let* ((decoded (image-input--decoded-image source format))
                            (resized
                              (if (and (= width target-width)
                                       (= height target-height))
                                  decoded
                                  (resize-image decoded
                                                target-height target-width
                                                :interpolate ':bilinear))))
                       (write-png-file temporary
                                       (image-input--8-bit-image resized))))
                 (uiop:rename-file-overwriting-target temporary target)
                 (platform-make-private *platform* target :read-only-p t))
             (image-input-error (condition)
               (error condition))
             (error (condition)
               (image-input--error
                source ':persistence
                (format nil "Image ~A could not be stored: ~A" source condition)
                condition)))
        (when (probe-file temporary)
          (delete-file temporary)))
      (make-instance 'image-attachment
                     :identifier identifier
                     :pathname target
                     :source-name source-name
                     :mime-type mime-type
                     :width target-width
                     :height target-height))))

(-> image-input-prepare (pathname pathname) image-attachment)
(defun image-input-prepare (source artifact-root)
  "Validate and persist SOURCE beneath private ARTIFACT-ROOT."
  (let ((absolute
          (handler-case
              (platform-truename *platform* source)
            (error (condition)
              (image-input--error
               source ':recognition
               (format nil "Image ~A does not exist or cannot be read." source)
               condition)))))
    (multiple-value-bind (format width height)
        (image-input--inspect absolute)
      (when (and (eq format ':webp)
                 (not (image-input--dimensions-fit-p width height)))
        (image-input--error
         absolute ':resizing
         (format nil
                 "WebP image ~A is ~Dx~D; use one within the high-detail image limits."
                 absolute width height)))
      (if (eq format ':webp)
          (let* ((identifier (make-identifier))
                 (target (merge-pathnames
                          (make-pathname :name identifier :type "webp")
                          artifact-root))
                 (temporary (image-input--temporary-pathname target identifier)))
            (ensure-directories-exist target)
            (platform-make-private *platform* artifact-root)
            (unwind-protect
                 (progn
                   (uiop:copy-file absolute temporary)
                   (uiop:rename-file-overwriting-target temporary target)
                   (platform-make-private *platform* target :read-only-p t))
              (when (probe-file temporary)
                (delete-file temporary)))
            (make-instance 'image-attachment
                           :identifier identifier
                           :pathname target
                           :source-name (namestring absolute)
                           :mime-type "image/webp"
                           :width width
                           :height height))
          (image-input--publish absolute artifact-root format width height
                                (namestring absolute))))))


;;;; -- Paste Recognition --

(-> image-input--unquote-pasted-path (string) (option string))
(defun image-input--unquote-pasted-path (text)
  "Return one POSIX shell-like path token from pasted TEXT, or NIL."
  (let ((result (make-string-output-stream))
        (quote nil)
        (escaped-p nil))
    (loop for character across (string-trim '(#\Space #\Tab #\Newline #\Return)
                                             text)
          do (cond
               (escaped-p
                (write-char character result)
                (setf escaped-p nil))
               ((and (null quote) (char= character #\\))
                (setf escaped-p t))
               ((and (null quote) (member character '(#\' #\")))
                (setf quote character))
               ((and quote (char= character quote))
                (setf quote nil))
               ((and (null quote) (find character '(#\Space #\Tab #\Newline)))
                (return-from image-input--unquote-pasted-path nil))
               (t
                (write-char character result))))
    (if (or quote escaped-p)
        nil
        (get-output-stream-string result))))

(-> image-input-normalize-pasted-path (string) (option pathname))
(defun image-input-normalize-pasted-path (text)
  "Normalize one pasted local path or file URL into an absolute pathname."
  (let ((token (image-input--unquote-pasted-path text)))
    (when (non-empty-string-p token)
      (let* ((file-url-p (uiop:string-prefix-p "file://" token))
             (decoded
               (if file-url-p
                   (url-decode (subseq token (length "file://")))
                   token))
             (pathname
               (uiop:ensure-pathname (platform-pathname decoded)
                                     :defaults (uiop:getcwd)
                                     :ensure-absolute t
                                     :want-non-wild t)))
        (and (uiop:file-exists-p pathname) (platform-truename *platform* pathname))))))

(-> image-input-recognize-pasted-path (string) (option pathname))
(defun image-input-recognize-pasted-path (text)
  "Return the supported image named by pasted TEXT, or NIL."
  (handler-case
      (let ((pathname (image-input-normalize-pasted-path text)))
        (and pathname (image-input-validate-pathname pathname)))
    (image-input-error ()
      nil)
    (error ()
      nil)))

(-> image-input-validate-pathname ((or string pathname)) pathname)
(defun image-input-validate-pathname (location)
  "Return LOCATION as an absolute supported image pathname, or signal."
  (let ((pathname
          (handler-case
              (uiop:ensure-pathname (platform-pathname location)
                                    :defaults (uiop:getcwd)
                                    :ensure-absolute t
                                    :want-non-wild t)
            (error (condition)
              (image-input--error
               (pathname location)
               ':recognition
               (format nil "Image location ~A is not a valid local pathname."
                       location)
               condition)))))
    (multiple-value-bind (format width height)
        (image-input--inspect pathname)
      (declare (ignore format))
      (if (and (plusp width) (plusp height))
          (platform-truename *platform* pathname)
          (image-input--error
           pathname ':recognition
           (format nil "Image ~A has no valid pixel dimensions." pathname))))))


;;;; -- Durable Projection --

(-> image-attachment-record (image-attachment) list)
(defun image-attachment-record (attachment)
  "Return ATTACHMENT's portable durable descriptor."
  (list :id (image-attachment-identifier attachment)
        :artifact (file-namestring (image-attachment-pathname attachment))
        :source (image-attachment-source-name attachment)
        :mime (image-attachment-mime-type attachment)
        :detail "high"
        :width (image-attachment-width attachment)
        :height (image-attachment-height attachment)))

(-> image-input--artifact-name-p (string string) boolean)
(defun image-input--artifact-name-p (name identifier)
  "Return true when artifact NAME is a safe basename for IDENTIFIER."
  (let ((pathname (pathname name)))
    (and (string= (file-namestring pathname) name)
         (string= (or (pathname-name pathname) "") identifier)
         (member (string-downcase (or (pathname-type pathname) ""))
                 '("png" "jpg" "webp")
                 :test #'string=)
         (member (pathname-directory pathname) '(nil (:relative)) :test #'equal)
         (null (pathname-device pathname)))))

(-> image-attachment-from-record (list pathname) image-attachment)
(defun image-attachment-from-record (record artifact-root)
  "Validate durable attachment RECORD beneath ARTIFACT-ROOT."
  (let* ((identifier (getf record :id))
         (artifact (getf record :artifact))
         (source (getf record :source))
         (mime (getf record :mime))
         (detail (getf record :detail))
         (width (getf record :width))
         (height (getf record :height)))
    (unless (and (non-empty-string-p identifier)
                 (non-empty-string-p artifact)
                 (image-input--artifact-name-p artifact identifier)
                 (non-empty-string-p source)
                 (member mime '("image/png" "image/jpeg" "image/webp")
                         :test #'string=)
                 (string= (or detail "") "high")
                 (typep width '(integer 1))
                 (typep height '(integer 1)))
      (image-input--error
       artifact-root ':loading
       "A persisted conversation image descriptor is malformed."))
    (let ((pathname (merge-pathnames artifact artifact-root)))
      (unless (probe-file pathname)
        (image-input--error
         pathname ':loading
         (format nil "Conversation image artifact ~A is missing." pathname)))
      (make-instance 'image-attachment
                     :identifier identifier
                     :pathname pathname
                     :source-name source
                     :mime-type mime
                     :width width
                     :height height))))

(-> image-input--data-url (image-attachment) string)
(defun image-input--data-url (attachment)
  "Return ATTACHMENT as a base64 data URL for one provider request.

The URL is pure ASCII, so it is built as a compact base string without
materializing a separate intermediate base64 string."
  (let ((bytes (image-input--read-file (image-attachment-pathname attachment))))
    (with-output-to-string (stream nil :element-type 'base-char)
      (write-string "data:" stream)
      (write-string (image-attachment-mime-type attachment) stream)
      (write-string ";base64," stream)
      (usb8-array-to-base64-stream bytes stream))))

(-> image-input-content-item (image-attachment) json-object)
(defun image-input-content-item (attachment)
  "Return ATTACHMENT as one Codex-compatible provider image item."
  (json-object
   "type" "input_image"
   "image_url" (image-input--data-url attachment)
   "detail" "high"))

(-> image-input-content-items (image-attachment integer) list)
(defun image-input-content-items (attachment label-number)
  "Return Codex-compatible provider content for ATTACHMENT labelled LABEL-NUMBER."
  (list
   (json-object
    "type" "input_text"
    "text" (format nil "<image name=[Image #~D] path=\"~A\">"
                   label-number
                   (image-attachment-source-name attachment)))
   (image-input-content-item attachment)
   (json-object "type" "input_text" "text" "</image>")))
