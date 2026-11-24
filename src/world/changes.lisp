(in-package #:autolith)

;;;; -- World Change Notifications --

(defvar *world-change-listeners* nil
  "Process-local (DATA-ROOT . FUNCTION) registrations for world changes.")

(defvar *world-change-lock* (make-lock "Autolith world changes")
  "Protects listener registration, never callbacks or transport I/O.")

(defparameter *world-change-relay-function* nil
  "The cross-process notification adapter, installed by the world transport.")

(-> world-change-listen (configuration function) cons)
(defun world-change-listen (configuration function)
  "Register FUNCTION for CONFIGURATION's changes and return its removable registration."
  (let ((registration (cons (namestring (config :data-root configuration)) function)))
    (with-lock-held (*world-change-lock*)
      (push registration *world-change-listeners*))
    registration))

(-> world-change-unlisten (cons) null)
(defun world-change-unlisten (registration)
  "Remove REGISTRATION without waiting for callbacks already in progress."
  (with-lock-held (*world-change-lock*)
    (setf *world-change-listeners* (cl:delete registration *world-change-listeners* :test #'eq)))
  nil)

(-> world-change-notify (configuration &key (:relay-p boolean)) null)
(defun world-change-notify (configuration &key (relay-p t))
  "Announce a committed world or runtime change outside storage and presentation locks.

Local observers receive invalidations, not snapshots. Without local observers,
RELAY-P permits the transport to notify the process serving this world."
  (let* ((key       (namestring (config :data-root configuration)))
         (listeners (with-lock-held (*world-change-lock*)
                      (loop for registration in *world-change-listeners*
                            when (string= (first registration) key)
                              collect (rest registration)))))
    (dolist (listener listeners)
      (funcall listener))
    (when (and (null listeners) relay-p *world-change-relay-function*)
      (funcall *world-change-relay-function* configuration)))
  nil)
