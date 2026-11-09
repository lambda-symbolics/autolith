(in-package #:autolith)

;;;; -- Channel Catalogue --

(-> world-channel-statuses (configuration &key (:entry t)) list)
(defun world-channel-statuses (configuration &key entry)
  "Return the server's live channel statuses, or NIL when it is unavailable."
  (let ((entry (or entry (world-client-entry configuration))))
    (when entry
      (handler-case
          (mapcar #'rest (getf (rest (localgroup-query-record entry ':channels)) :channels))
        (autolith-error ()
          nil)))))

(-> world-channel-catalogue
    (configuration &key (:world world) (:statuses list)) world)
(defun world-channel-catalogue (configuration &key (world (world-load configuration))
                                                 (statuses (world-channel-statuses configuration)))
  "Project WORLD and live hosted STATUSES into one read-only channel catalogue.

The returned world has its own index. Newly hosted channels can precede their
first durable world record; durable channels always win over those projections.
No presentation discovery is written back to the world store."
  (let* ((catalogue (world--copy world))
         (person    (world-human-participant world))
         (now       (get-universal-time)))
    (dolist (status statuses)
      (let ((identifier (getf status :session-id))
            (directory  (getf status :cwd)))
        (when (and (stringp identifier) (stringp directory)
                   (not (world-channel-find catalogue identifier)))
          (setf (gethash identifier (world--channel-table catalogue))
                (make-instance 'channel
                               :identifier identifier
                               :owner      (if person (participant-identifier person) identifier)
                               :workspace  (world-workspace-name directory)
                               :title      (getf status :conversation-title)
                               :created-at now
                               :updated-at now)))))
    catalogue))

(-> world-channel-served-identifiers (configuration) list)
(defun world-channel-served-identifiers (configuration)
  "Return channel identities served by live processes or hosted by the world server."
  (remove-duplicates
   (append (mapcar (lambda (status) (getf status :session-id))
                   (world-channel-statuses configuration))
           (loop for entry in (handler-case (localgroup-endpoint-records configuration)
                                (autolith-error ()
                                  nil))
                 for properties = (rest (rest entry))
                 unless (eq (image-daemon:daemon-process-state (getf properties :pid)) ':dead)
                   collect (getf properties :session-id)))
   :test #'equal))
