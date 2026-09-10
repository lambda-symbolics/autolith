(in-package #:autolith)

;;;; -- Release Server Configuration --

(defparameter *release-server-default-address* "127.0.0.1"
  "The default loopback address for the release HTTP service.")

(defparameter *release-server-default-port* 8098
  "The default loopback port for the release HTTP service.")

(defparameter *release-server-maximum-request-head-size* 16384
  "The maximum accepted HTTP request head size in bytes.")

(defclass release-server-configuration ()
  ((source-root
    :initarg :source-root
    :reader release-server-configuration-source-root
    :type pathname
    :documentation "The tracked Autolith checkout containing script/install.")
   (public-root
    :initarg :public-root
    :reader release-server-configuration-public-root
    :type pathname
    :documentation "The root containing atomically published release directories.")
   (address
    :initarg :address
    :reader release-server-configuration-address
    :type string
    :documentation "The IPv4 address accepted by the HTTP listener.")
   (port
    :initarg :port
    :reader release-server-configuration-port
    :type (integer 1 65535)
    :documentation "The TCP port accepted by the HTTP listener.")
   (source-tag
    :initarg :source-tag
    :initform nil
    :reader release-server-configuration-source-tag
    :type (option string)
    :documentation "The exact source tag captured when this server started.")
   (source-commit
    :initarg :source-commit
    :initform nil
    :reader release-server-configuration-source-commit
    :type (option string)
    :documentation "The exact source commit captured when this server started."))
  (:documentation "Filesystem and listener settings for the release service."))

(defclass release-server-response ()
  ((status
    :initarg :status
    :reader release-server-response-status
    :type integer
    :documentation "The numeric HTTP response status.")
   (content-type
    :initarg :content-type
    :reader release-server-response-content-type
    :type string
    :documentation "The response media type.")
   (headers
    :initarg :headers
    :initform nil
    :reader release-server-response-headers
    :type list
    :documentation "Additional trusted response headers as name/value conses.")
   (body
    :initarg :body
    :reader release-server-response-body
    :type (or null string pathname)
    :documentation "The in-memory text or file backing the response body."))
  (:documentation "One trusted HTTP response produced by release routing."))

(define-condition release-server-request-error (error)
  ((message
    :initarg :message
    :reader release-server-request-error-message
    :type string
    :documentation "A concise description of the malformed request."))
  (:report (lambda (condition stream)
             (write-string (release-server-request-error-message condition)
                           stream)))
  (:documentation "A malformed or oversized request received by the server."))

(-> release-server--environment-port ((option string)) (integer 1 65535))
(defun release-server--environment-port (value)
  "Parse an optional listener port VALUE, defaulting when it is absent."
  (if (null value)
      *release-server-default-port*
      (handler-case
          (let ((port (parse-integer value :junk-allowed nil)))
            (unless (typep port '(integer 1 65535))
              (error 'parse-error))
            port)
        (error ()
          (error 'configuration-error
                 :message (format nil "Invalid release server port ~S." value))))))

(-> release-server--git-output (pathname list) string)
(defun release-server--git-output (directory arguments)
  "Return trimmed output from Git ARGUMENTS in DIRECTORY."
  (let ((canonical
          (namestring
           (truename (uiop:ensure-directory-pathname directory)))))
    (string-trim
     '(#\Space #\Tab #\Newline #\Return)
     (uiop:run-program
      (append
       (list "git" "-c" (format nil "safe.directory=~A" canonical)
             "-C" canonical)
       arguments)
      :output ':string
      :error-output ':output))))

(-> release-server--source-identity
    (pathname)
    (values (option string) (option string)))
(defun release-server--source-identity (source-root)
  "Return SOURCE-ROOT's exact lightweight semantic tag and commit, if any."
  (handler-case
      (let* ((commit
               (release-server--git-output
                source-root '("rev-parse" "HEAD")))
             (tags
               (remove-if-not
                #'release-tag-valid-p
                (uiop:split-string
                 (release-server--git-output
                  source-root '("tag" "--points-at" "HEAD"))
                 :separator '(#\Newline #\Return))))
             (tag (and (= (length tags) 1) (first tags))))
        (if (and tag
                 (= (length commit) 40)
                 (string=
                  (release-server--git-output
                   source-root
                   (list "cat-file" "-t" (format nil "refs/tags/~A" tag)))
                  "commit"))
            (values tag commit)
            (values nil nil)))
    (error ()
      (values nil nil))))

(-> release-server-configuration-create
    (&key (:source-root (option pathname))
          (:public-root (option pathname))
          (:address (option string))
          (:port (option integer))
          (:source-tag (option string))
          (:source-commit (option string)))
    release-server-configuration)
(defun release-server-configuration-create
    (&key source-root public-root address port source-tag source-commit)
  "Create release server settings from arguments and environment defaults."
  (let* ((configured-source-root
           (uiop:ensure-directory-pathname
            (or source-root
                (let ((configured
                        (uiop:getenv "AUTOLITH_RELEASE_SOURCE_ROOT")))
                  (and configured (pathname configured)))
                (asdf:system-source-directory :autolith))))
         (resolved-source-root
           (or (ignore-errors (truename configured-source-root))
               configured-source-root)))
    (multiple-value-bind (detected-tag detected-commit)
        (release-server--source-identity resolved-source-root)
      (make-instance
       'release-server-configuration
       :source-root resolved-source-root
       :public-root
       (uiop:ensure-directory-pathname
        (or public-root
            (let ((configured
                    (uiop:getenv "AUTOLITH_RELEASE_PUBLIC_ROOT")))
              (and configured (pathname configured)))
            #p"/srv/autolith-release-server/"))
       :address (or address
                    (uiop:getenv "AUTOLITH_RELEASE_LISTEN_ADDRESS")
                    *release-server-default-address*)
       :port (or port
                 (release-server--environment-port
                  (uiop:getenv "AUTOLITH_RELEASE_LISTEN_PORT")))
       :source-tag (or source-tag detected-tag)
       :source-commit (or source-commit detected-commit)))))


;;;; -- Published Releases --

(defparameter *release-server-platform-ids*
  '("x86_64-linux" "aarch64-linux"
    "x86_64-linux-musl" "aarch64-linux-musl"
    "x86_64-darwin" "arm64-darwin" "x86_64-freebsd" "x86_64-netbsd"
    "x86_64-openbsd" "x86_64-windows")
  "Canonical platform identifiers published as binary release archives.")

(-> release-server--archive-name (string &optional string) string)
(defun release-server--archive-name (tag &optional (platform "x86_64-linux"))
  "Return the archive name belonging to TAG and PLATFORM."
  (format nil "autolith-~A-~A.~A"
          tag platform
          (if (string= platform "x86_64-windows") "zip" "tar.gz")))

(-> release-server--artifact-name-p (string string) boolean)
(defun release-server--artifact-name-p (tag name)
  "Return true when NAME is a known archive or checksum for TAG."
  (loop for platform in *release-server-platform-ids*
        for archive = (release-server--archive-name tag platform)
        thereis (or (string= name archive)
                    (string= name (format nil "~A.sha256" archive)))))

(-> release-server--release-directory
    (release-server-configuration string)
    pathname)
(defun release-server--release-directory (configuration tag)
  "Return TAG's publication directory below CONFIGURATION."
  (merge-pathnames
   (format nil "releases/~A/" tag)
   (release-server-configuration-public-root configuration)))

(-> release-server--release-complete-p
    (release-server-configuration string)
    boolean)
(defun release-server--release-complete-p (configuration tag)
  "Return true when TAG has both its archive and checksum published."
  (and (release-tag-valid-p tag)
       (let* ((directory (release-server--release-directory configuration tag))
              (archive (release-server--archive-name tag)))
         (not
          (null
           (and (uiop:file-exists-p (merge-pathnames archive directory))
                (uiop:file-exists-p
                 (merge-pathnames (format nil "~A.sha256" archive)
                                  directory))))))))

(-> release-server-published-tags (release-server-configuration) list)
(defun release-server-published-tags (configuration)
  "Return complete published release tags in semantic-version order."
  (let ((releases-root
          (merge-pathnames "releases/"
                           (release-server-configuration-public-root
                            configuration))))
    (sort
     (loop for directory in (uiop:subdirectories releases-root)
           for components = (pathname-directory directory)
           for tag = (and components (first (last components)))
           when (and (stringp tag)
                     (release-server--release-complete-p configuration tag))
             collect tag)
     #'release-tag<)))

(-> release-server-latest-tag
    (release-server-configuration)
    (option string))
(defun release-server-latest-tag (configuration)
  "Return the newest complete published release tag, or NIL."
  (first (last (release-server-published-tags configuration))))


;;;; -- HTTP Routing --

(-> release-server--response
    (integer string (or null string pathname) &key (:headers list))
    release-server-response)
(defun release-server--response (status content-type body &key headers)
  "Create one trusted release server response."
  (make-instance 'release-server-response
                 :status status
                 :content-type content-type
                 :headers headers
                 :body body))

(-> release-server--not-found () release-server-response)
(defun release-server--not-found ()
  "Return the plain-text missing-resource response."
  (release-server--response 404 "text/plain; charset=utf-8"
                            (format nil "not found~%")))

(-> release-server--release-route
    (release-server-configuration string)
    release-server-response)
(defun release-server--release-route (configuration path)
  "Route one normalized PATH below /releases/."
  (let ((remainder (subseq path (length "/releases/"))))
    (cond
      ((string= remainder "latest")
       (let ((tag (release-server-latest-tag configuration)))
         (if tag
             (release-server--response
              302
              "text/plain; charset=utf-8"
              (format nil "~A~%" tag)
              :headers (list (cons "Location" (format nil "/releases/~A" tag))
                             (cons "Cache-Control" "no-store")))
             (release-server--response
              503
              "text/plain; charset=utf-8"
              (format nil "no release is available~%")))))
      ((or (zerop (length remainder))
           (find #\/ remainder)
           (not (release-tag-valid-p remainder)))
       (let* ((separator (position #\/ remainder))
              (tag (and separator (subseq remainder 0 separator)))
              (name (and separator (subseq remainder (1+ separator)))))
           (if (and tag
                    name
                    (release-server--release-complete-p configuration tag)
                    (release-server--artifact-name-p tag name)
                    (uiop:file-exists-p
                     (merge-pathnames
                      name
                      (release-server--release-directory configuration tag))))
              (release-server--response
               200
               (cond
                 ((uiop:string-suffix-p name ".sha256")
                  "text/plain; charset=utf-8")
                 ((uiop:string-suffix-p name ".zip")
                  "application/zip")
                 (t
                  "application/gzip"))
               (merge-pathnames
                name
                (release-server--release-directory configuration tag))
              :headers
              (list (cons "Cache-Control"
                          "public, max-age=31536000, immutable")))
             (release-server--not-found))))
      ((release-server--release-complete-p configuration remainder)
       (release-server--response
        200
        "text/plain; charset=utf-8"
        (format nil "~A~%" remainder)
        :headers (list (cons "Cache-Control" "public, max-age=60"))))
      (t
       (release-server--not-found)))))

(-> release-server-route
    (release-server-configuration string string)
    release-server-response)
(defun release-server-route (configuration method target)
  "Route HTTP METHOD and origin-form TARGET without filesystem traversal."
  (let ((path (subseq target 0 (or (position #\? target) (length target)))))
    (cond
      ((not (member method '("GET" "HEAD") :test #'string=))
       (release-server--response
        405
        "text/plain; charset=utf-8"
        (format nil "method not allowed~%")
        :headers (list (cons "Allow" "GET, HEAD"))))
      ((or (zerop (length path))
           (not (char= (char path 0) #\/))
           (find #\% path)
           (find #\\ path))
       (release-server--not-found))
      ((string= path "/health")
       (release-server--response 200 "text/plain; charset=utf-8"
                                 (format nil "ok~%")
                                 :headers (list (cons "Cache-Control" "no-store"))))
      ((and (release-server-configuration-source-tag configuration)
            (release-server-configuration-source-commit configuration)
            (string=
             path
             (format nil "/health/~A/~A"
                     (release-server-configuration-source-tag configuration)
                     (release-server-configuration-source-commit configuration))))
       (release-server--response 200 "text/plain; charset=utf-8"
                                 (format nil "ok~%")
                                 :headers (list (cons "Cache-Control" "no-store"))))
      ((string= path "/autolith")
       (let ((installer
               (merge-pathnames
                "script/install"
                (release-server-configuration-source-root configuration))))
         (if (uiop:file-exists-p installer)
             (release-server--response
              200
              "text/x-shellscript; charset=utf-8"
              installer
              :headers (list (cons "Cache-Control" "no-cache")))
             (release-server--not-found))))
      ((string= path "/autolith.ps1")
       (let ((installer
               (merge-pathnames
                "script/install.ps1"
                (release-server-configuration-source-root configuration))))
         (if (uiop:file-exists-p installer)
             (release-server--response
              200
              "text/plain; charset=utf-8"
              installer
              :headers (list (cons "Cache-Control" "no-cache")))
             (release-server--not-found))))
      ((uiop:string-prefix-p "/releases/" path)
       (release-server--release-route configuration path))
      (t
       (release-server--not-found)))))


;;;; -- HTTP Transport --

(-> release-server--status-reason (integer) string)
(defun release-server--status-reason (status)
  "Return the fixed HTTP reason phrase for STATUS."
  (case status
    (200 "OK")
    (302 "Found")
    (400 "Bad Request")
    (404 "Not Found")
    (405 "Method Not Allowed")
    (500 "Internal Server Error")
    (503 "Service Unavailable")
    (t "Error")))

(-> release-server--body-length ((or null string pathname)) integer)
(defun release-server--body-length (body)
  "Return BODY's exact encoded byte length."
  (etypecase body
    (null 0)
    (string (length (sb-ext:string-to-octets body :external-format ':utf-8)))
    (pathname
     (with-open-file (stream body :direction ':input
                                  :element-type '(unsigned-byte 8))
       (file-length stream)))))

(-> release-server--write-octets (stream string) null)
(defun release-server--write-octets (stream text)
  "Write UTF-8 TEXT to a binary HTTP STREAM."
  (write-sequence (sb-ext:string-to-octets text :external-format ':utf-8)
                  stream)
  nil)

(-> release-server--write-response
    (stream release-server-response boolean)
    null)
(defun release-server--write-response (stream response head-p)
  "Write RESPONSE to binary STREAM, omitting its body when HEAD-P is true."
  (let* ((body (release-server-response-body response))
         (status (release-server-response-status response))
         (length (release-server--body-length body)))
    (release-server--write-octets
     stream
     (format nil
             (concatenate
              'string
              "HTTP/1.1 ~D ~A~C~C"
              "Content-Type: ~A~C~C"
              "Content-Length: ~D~C~C"
              "Connection: close~C~C"
              "X-Content-Type-Options: nosniff~C~C")
             status
             (release-server--status-reason status)
             #\Return #\Newline
             (release-server-response-content-type response)
             #\Return #\Newline
             length #\Return #\Newline
             #\Return #\Newline
             #\Return #\Newline))
    (dolist (header (release-server-response-headers response))
      (release-server--write-octets
       stream
       (format nil "~A: ~A~C~C"
               (first header) (rest header) #\Return #\Newline)))
    (release-server--write-octets stream (format nil "~C~C" #\Return #\Newline))
    (unless head-p
      (etypecase body
        (null)
        (string
         (release-server--write-octets stream body))
        (pathname
         (with-open-file (file body :direction ':input
                                    :element-type '(unsigned-byte 8))
           (let ((buffer (make-array 65536 :element-type '(unsigned-byte 8))))
             (loop for count = (read-sequence buffer file)
                   while (plusp count)
                   do (write-sequence buffer stream :end count)))))))
    (finish-output stream))
  nil)

(-> release-server--read-request-head (stream) string)
(defun release-server--read-request-head (stream)
  "Read and return one bounded HTTP request head from binary STREAM."
  (let ((octets (make-array 1024
                            :element-type '(unsigned-byte 8)
                            :adjustable t
                            :fill-pointer 0)))
    (loop
      for byte = (read-byte stream nil nil)
      unless byte
        do (error 'release-server-request-error
                  :message "The request ended before its headers.")
      do (vector-push-extend byte octets)
         (when (> (length octets) *release-server-maximum-request-head-size*)
           (error 'release-server-request-error
                  :message "The request headers are too large."))
         (let ((length (length octets)))
           (when (and (>= length 4)
                      (= (aref octets (- length 4)) 13)
                      (= (aref octets (- length 3)) 10)
                      (= (aref octets (- length 2)) 13)
                      (= (aref octets (1- length)) 10))
             (return
               (sb-ext:octets-to-string octets :external-format ':latin-1)))))))

(-> release-server--parse-request-head (string) (values string string))
(defun release-server--parse-request-head (head)
  "Return the method and target from a validated HTTP request HEAD."
  (let* ((line-end (search (format nil "~C~C" #\Return #\Newline) head))
         (line (and line-end (subseq head 0 line-end)))
         (parts (and line
                     (remove-if (lambda (part) (zerop (length part)))
                                (uiop:split-string line :separator '(#\Space))))))
    (unless (and (= (length parts) 3)
                 (member (third parts) '("HTTP/1.0" "HTTP/1.1")
                         :test #'string=)
                 (plusp (length (second parts)))
                 (<= (length (second parts)) 4096)
                 (char= (char (second parts) 0) #\/))
      (error 'release-server-request-error
             :message "The HTTP request line is malformed."))
    (values (first parts) (second parts))))

(-> release-server--handle-connection
    (release-server-configuration sb-bsd-sockets:socket)
    null)
(defun release-server--handle-connection (configuration socket)
  "Read, route, and close one HTTP connection SOCKET."
  (let ((stream nil))
    (unwind-protect
         (handler-case
             (progn
               (setf stream
                     (sb-bsd-sockets:socket-make-stream
                      socket
                      :input t
                      :output t
                      :element-type '(unsigned-byte 8)
                      :buffering ':full))
               (multiple-value-bind (method target)
                   (release-server--parse-request-head
                    (release-server--read-request-head stream))
                 (release-server--write-response
                  stream
                  (release-server-route configuration method target)
                  (string= method "HEAD"))))
           (release-server-request-error (condition)
             (declare (ignore condition))
             (when stream
               (ignore-errors
                 (release-server--write-response
                  stream
                  (release-server--response
                   400 "text/plain; charset=utf-8"
                   (format nil "bad request~%"))
                  nil))))
           (error (condition)
             (format *error-output* "~&Release request failed: ~A~%" condition)
             (when stream
               (ignore-errors
                 (release-server--write-response
                  stream
                  (release-server--response
                   500 "text/plain; charset=utf-8"
                   (format nil "internal error~%"))
                  nil)))))
      (if stream
          (ignore-errors (close stream))
          (ignore-errors (sb-bsd-sockets:socket-close socket)))))
  nil)

(-> release-server-serve (release-server-configuration) null)
(defun release-server-serve (configuration)
  "Serve release HTTP requests until the process is terminated."
  (let ((listener (make-instance 'sb-bsd-sockets:inet-socket
                                 :type ':stream
                                 :protocol ':tcp)))
    (unwind-protect
         (progn
           (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
           (sb-bsd-sockets:socket-bind
            listener
            (sb-bsd-sockets:make-inet-address
             (release-server-configuration-address configuration))
            (release-server-configuration-port configuration))
           (sb-bsd-sockets:socket-listen listener 128)
           (format t "~&Autolith release server listening on ~A:~D.~%"
                   (release-server-configuration-address configuration)
                   (release-server-configuration-port configuration))
           (finish-output)
           (loop
             for client = (sb-bsd-sockets:socket-accept listener)
             do (let ((connection client))
                  (make-thread
                   (lambda ()
                     (release-server--handle-connection configuration connection))
                   :name "autolith release request"))))
      (ignore-errors (sb-bsd-sockets:socket-close listener))))
  nil)
