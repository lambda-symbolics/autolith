;;;; -- Agent Terminal Host Policy --

(in-package #:autolith)

(setf *terminal-ui-host*
      (make-instance 'terminal-ui-host
                     :image-path-function     #'image-input-recognize-pasted-path
                     :inference-tool-function #'tool-recursive-inference-p
                     :startup-tip-function
                     (lambda () (application--startup-tip-spans))
                     :resolve-path-function
                     (lambda (pathname) (platform-truename *platform* pathname))
                     :fullscreen-enable-function
                     (lambda () (platform-terminal-enable-fullscreen *platform*))
                     :fullscreen-restore-function
                     (lambda (token) (platform-terminal-restore-fullscreen *platform* token))))
