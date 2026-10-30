;;;; -- Reusable Terminal Host Policy --

(in-package #:autolith-supervisor)

(serapeum/types:-> terminal-ui-recognize-image-path (string) (option pathname))
(defun terminal-ui-recognize-image-path (text)
  "Recognize a user-pasted image path without storing or sending its contents."
  (handler-case
      (let ((path (clinedi:pasted-path text)))
        (when (and (stringp path) (plusp (length path)))
          (let ((absolute
                  (ls-compat.posix:resolve-pathname
                   (uiop:ensure-pathname path :defaults (uiop:getcwd)
                                             :ensure-absolute t :want-non-wild t))))
            (multiple-value-bind (format width height)
                (yolokuva:image-inspect absolute :maximum-octets (* 1024 1024 1024))
              (declare (ignore format))
              (when (and (plusp width) (plusp height)) absolute)))))
    (error () nil)))

(defclass terminal-ui-host ()
  ((image-path-function :initarg       :image-path-function
                        :initform      #'terminal-ui-recognize-image-path
                        :reader        terminal-ui-host-image-path-function
                        :documentation "Trusted callback recognizing pasted image paths.")
   (inference-tool-function :initarg       :inference-tool-function
                            :initform      (lambda (name)
                                             (member name '("rlm.infer" "rlm.map" "rlm.complete")
                                                     :test #'string=))
                            :reader        terminal-ui-host-inference-tool-function
                            :documentation "Classify inert tool names for presentation.")
   (startup-tip-function :initarg       :startup-tip-function
                         :initform      (constantly nil)
                         :reader        terminal-ui-host-startup-tip-function
                         :documentation "Return trusted startup advice spans.")
   (resolve-path-function :initarg       :resolve-path-function
                          :initform      #'ls-compat.posix:resolve-pathname
                          :reader        terminal-ui-host-resolve-path-function
                          :documentation "Resolve a local path through the host adapter.")
   (fullscreen-enable-function :initarg       :fullscreen-enable-function
                               :initform      (constantly nil)
                               :reader        terminal-ui-host-fullscreen-enable-function
                               :documentation "Enter native fullscreen output mode and return its token.")
   (fullscreen-restore-function :initarg       :fullscreen-restore-function
                                :initform      (constantly nil)
                                :reader        terminal-ui-host-fullscreen-restore-function
                                :documentation "Restore native output mode from its token."))
  (:documentation "Trusted host functions needed by the reusable terminal editor."))

(defparameter *terminal-ui-host* (make-instance 'terminal-ui-host)
  "Host policy in this process. Bind it around terminal UI operations.")

(defparameter *terminal-ui-recent-tool-limit* 8
  "Maximum inert tool names accepted in one activity presentation.")

(serapeum/types:-> terminal-ui-non-empty-string-p (t) boolean)
(defun terminal-ui-non-empty-string-p (value)
  "Return true when VALUE contains a non-whitespace character."
  (and (stringp value)
       (not (every (lambda (character)
                     (find character '(#\Space #\Tab #\Newline #\Return #\Page)))
                   value))))

(serapeum/types:-> terminal-ui-host-image-path (string) (option pathname))
(defun terminal-ui-host-image-path (text)
  "Recognize a pasted path using trusted host policy."
  (funcall (terminal-ui-host-image-path-function *terminal-ui-host*) text))

(serapeum/types:-> terminal-ui-host-inference-tool-p (string) boolean)
(defun terminal-ui-host-inference-tool-p (name)
  "Classify NAME without resolving any agent function."
  (not (null (funcall (terminal-ui-host-inference-tool-function *terminal-ui-host*) name))))

(serapeum/types:-> terminal-ui-host-startup-tip-spans () list)
(defun terminal-ui-host-startup-tip-spans ()
  "Return trusted startup advice without loading application code."
  (funcall (terminal-ui-host-startup-tip-function *terminal-ui-host*)))

(serapeum/types:-> terminal-ui-host-truename (terminal-ui-host pathname) pathname)
(defun terminal-ui-host-truename (host pathname)
  "Resolve PATHNAME through HOST's path adapter."
  (funcall (terminal-ui-host-resolve-path-function host) pathname))

(serapeum/types:-> terminal-ui-host-enable-fullscreen (terminal-ui-host) t)
(defun terminal-ui-host-enable-fullscreen (host)
  "Enter HOST's native output mode and return the restoration token."
  (funcall (terminal-ui-host-fullscreen-enable-function host)))

(serapeum/types:-> terminal-ui-host-restore-fullscreen (terminal-ui-host t) null)
(defun terminal-ui-host-restore-fullscreen (host token)
  "Restore HOST's native output mode from TOKEN."
  (funcall (terminal-ui-host-fullscreen-restore-function host) token)
  nil)
