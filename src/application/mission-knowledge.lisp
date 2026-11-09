(in-package #:autolith)

;;;; -- Mission Observations --


(-> mission-knowledge--identity (mission-context) string)
(defun mission-knowledge--identity (context)
  "Return the entity ID used for CONTEXT's mission in facts."
  (format nil "mission/~A/~D"
          (conversation-identifier
           (application-conversation (mission-context-application context)))
          (getf (mission-context-goal context) :created-at)))

(-> mission-knowledge--record
    (mission-context string &key (:arguments list) (:detail string)) null)
(defun mission-knowledge--record (context predicate &key arguments detail)
  "Record PREDICATE with ARGUMENTS for CONTEXT's mission, using DETAIL as the description.
Record only when knowledge capture is enabled. Knowledge storage errors do not
fail the mission; only the observation is lost."
  (let* ((application   (mission-context-application context))
         (configuration (application-configuration application)))
    (when (knowledge-capture-p configuration)
      (handler-case
          (let* ((identity (mission-knowledge--identity context))
                 (schema   (knowledge-base-schema (knowledge-base-read configuration) predicate)))
            (knowledge-replace-observation
             configuration
             (knowledge-make-proposition schema (cons identity arguments))
             (knowledge-observe :source  (list :mission :id identity :detail detail)
                                :adapter "mission/1")
             :source-conversation (conversation-identifier
                                   (application-conversation application))))
        (knowledge-error ()
          nil))))
  nil)

(-> mission-knowledge-record-status (mission-context keyword) null)
(defun mission-knowledge-record-status (context status)
  "Record that CONTEXT's mission now has STATUS."
  (let ((name (string-downcase (symbol-name status))))
    (mission-knowledge--record context "autolith.mission.status"
                               :arguments (list name)
                               :detail    name)))

(-> mission-knowledge-record-gate (mission-context string keyword) null)
(defun mission-knowledge-record-gate (context gate status)
  "Record that gate GATE of CONTEXT's mission now has STATUS."
  (let ((name (string-downcase (symbol-name status))))
    (mission-knowledge--record context "autolith.mission.gate"
                               :arguments (list gate name)
                               :detail    (format nil "~A ~A" gate name))))
