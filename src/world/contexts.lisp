(in-package #:autolith)

;;;; -- Agent Contexts in Transit --

;;; A parked agent resides nowhere, yet keeps the provider context it built.
;;; That context waits in one private file per agent. A move writes the same
;;; file before it commits, naming the channel the agent goes to and the
;;; transition, so the context survives any failure after the commit: the
;;; destination adopts it when it is next served, or before the agent's next
;;; turn there, and removes the file. A file whose destination is not where
;;; the agent resides belongs to a move that never committed and is ignored.

(defparameter *world-context-maximum-octets* (* 64 1024 1024)
  "The largest agent context file read back.")

(defvar *world-transition-lock* (make-recursive-lock "Autolith world transitions")
  "Serializes this process's moves and parks; a file lock serializes them across processes.")

(-> world-call-with-transition (configuration function) t)
(defun world-call-with-transition (configuration function)
  "Call FUNCTION while no other move or park of CONFIGURATION's world runs, and return its values."
  (let ((pathname (merge-pathnames (make-pathname :directory '(:relative "world")
                                                  :name      "transition"
                                                  :type      "lock")
                                   (config :data-root configuration))))
    (with-recursive-lock-held (*world-transition-lock*)
      (ensure-directories-exist pathname)
      (call-with-file-lock pathname function))))

(-> world-context-pathname (configuration string) pathname)
(defun world-context-pathname (configuration participant)
  "Return the file holding the context agent PARTICIPANT carries."
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

(-> world-context-write
    (configuration string &key (:source (option string)) (:rows list) (:goal list)
                               (:destination (option string)) (:transition (option string)))
    null)
(defun world-context-write (configuration participant &key source rows goal destination transition)
  "Keep ROWS and GOAL, the context and goal agent PARTICIPANT carries from channel SOURCE, until channel DESTINATION adopts them.

A NIL DESTINATION keeps them for a parked agent. TRANSITION names the move
that writes them, so their adoption happens once."
  (let ((pathname (world-context-pathname configuration participant)))
    (ensure-directories-exist pathname)
    (handler-case
        (sexp-store:snapshot-write pathname
                                   (apply #'sexp-store:make-record ':agent-context 1
                                          :participant participant
                                          :rows        rows
                                          (append (and source (list :source source))
                                                  (and goal (list :goal goal))
                                                  (and destination (list :destination destination))
                                                  (and transition (list :transition transition)))))
      (sexp-store:store-error (cause)
        (error 'world-error
               :message    (format nil "The context of agent ~A could not be kept: ~A"
                                   participant (sexp-store:store-error-message cause))
               :pathname   pathname
               :operation  ':context
               :identifier participant))))
  nil)

(-> world-context-read (configuration string)
    (values list (option string) (option string) (option string) list))
(defun world-context-read (configuration participant)
  "Return the context rows agent PARTICIPANT carries, the channel they came from, the channel they go to, the transition taking them and the agent's goal.

An agent with no kept context carries none."
  (let ((pathname (world-context-pathname configuration participant)))
    (if (probe-file pathname)
        (let* ((record     (handler-case
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
                                      :identifier participant))))
               (properties (rest record)))
          (unless (and (equal (getf properties :participant) participant)
                       (listp (getf properties :rows)))
            (error 'world-error
                   :message    (format nil "The context of agent ~A belongs to another agent or has invalid rows." participant)
                   :pathname   pathname
                   :operation  ':context
                   :identifier participant))
          (values (getf properties :rows) (getf properties :source)
                  (getf properties :destination) (getf properties :transition)
                  (getf properties :goal)))
        (values nil nil nil nil nil))))

(-> world-context-waiting-p (configuration participant) boolean)
(defun world-context-waiting-p (configuration participant)
  "Return true when agent PARTICIPANT's kept context is the one it carries now.

That is the parked agent's context, or the context a committed move took
into the channel where it now resides. A context written by a move that
never committed names another channel, and the agent's own conversation
holds what it carries."
  (let ((identifier (participant-identifier participant)))
    (and (probe-file (world-context-pathname configuration identifier))
         (equal (nth-value 2 (world-context-read configuration identifier))
                (participant-residence participant)))))

(-> world-context-delete (configuration string) null)
(defun world-context-delete (configuration participant)
  "Forget the context agent PARTICIPANT carried, once a channel adopted it."
  (let ((pathname (world-context-pathname configuration participant)))
    (when (probe-file pathname)
      (delete-file pathname)))
  nil)
