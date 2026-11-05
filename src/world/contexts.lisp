(in-package #:autolith)

;;;; -- Parked Agent Contexts --

;;; A parked agent resides nowhere, yet keeps the provider context it built.
;;; That context waits in one private file per agent until a move brings the
;;; agent into a channel, which adopts it durably and removes the file.

(defparameter *world-context-maximum-octets* (* 64 1024 1024)
  "The largest parked agent context file read back.")

(-> world-context-pathname (configuration string) pathname)
(defun world-context-pathname (configuration participant)
  "Return the file holding the context parked agent PARTICIPANT carries."
  (unless (every (lambda (character)
                   (or (alphanumericp character) (char= character #\-)))
                 participant)
    (error 'world-error
           :message    (format nil "~A is not a participant identifier." participant)
           :pathname   nil
           :operation  ':context
           :identifier participant))
  (merge-pathnames (make-pathname :directory '(:relative "world" "contexts")
                                  :name      participant
                                  :type      "sexp")
                   (config :data-root configuration)))

(-> world-context-write (configuration string &key (:source (option string)) (:rows list)) null)
(defun world-context-write (configuration participant &key source rows)
  "Keep ROWS, the context parked agent PARTICIPANT carries from channel SOURCE, until a move adopts it."
  (let ((pathname (world-context-pathname configuration participant)))
    (ensure-directories-exist pathname)
    (handler-case
        (sexp-store:snapshot-write pathname
                                   (apply #'sexp-store:make-record ':agent-context 1
                                          :participant participant
                                          :rows        rows
                                          (and source (list :source source))))
      (sexp-store:store-error (cause)
        (error 'world-error
               :message    (format nil "The context of agent ~A could not be kept: ~A"
                                   participant (sexp-store:store-error-message cause))
               :pathname   pathname
               :operation  ':context
               :identifier participant))))
  nil)

(-> world-context-read (configuration string) (values list (option string)))
(defun world-context-read (configuration participant)
  "Return the context rows parked agent PARTICIPANT carries and the channel they came from.

An agent with no kept context carries none."
  (let ((pathname (world-context-pathname configuration participant)))
    (if (probe-file pathname)
        (let ((record (handler-case
                          (sexp-store:snapshot-read-record
                           pathname :tag ':agent-context :versions '(1)
                                    :keyword-keys-p t
                                    :maximum-octets *world-context-maximum-octets*)
                        (sexp-store:store-error (cause)
                          (error 'world-error
                                 :message    (format nil "The context of agent ~A is unreadable: ~A"
                                                     participant (sexp-store:store-error-message cause))
                                 :pathname   pathname
                                 :operation  ':context
                                 :identifier participant)))))
          (unless (and (equal (getf (rest record) :participant) participant)
                       (listp (getf (rest record) :rows)))
            (error 'world-error
                   :message    (format nil "The context of agent ~A belongs to another agent." participant)
                   :pathname   pathname
                   :operation  ':context
                   :identifier participant))
          (values (getf (rest record) :rows) (getf (rest record) :source)))
        (values nil nil))))

(-> world-context-delete (configuration string) null)
(defun world-context-delete (configuration participant)
  "Forget the context parked agent PARTICIPANT carried, once a channel adopted it."
  (let ((pathname (world-context-pathname configuration participant)))
    (when (probe-file pathname)
      (delete-file pathname)))
  nil)
