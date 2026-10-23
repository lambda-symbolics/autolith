(in-package #:autolith)

;;;; -- Mission Observations --

;;; A mission's status and the result of each acceptance gate are recorded as
;;; facts about the mission, so rules and plans can depend on them. Each new
;;; report replaces the previous one from the same mission. Recording follows
;;; the knowledge setting and never fails the mission: a knowledge storage
;;; failure only loses the observation.

(-> mission-knowledge--identity (mission-context) string)
(defun mission-knowledge--identity (context)
  "Return the entity identifying CONTEXT's mission in facts."
  (format nil "mission/~A/~D"
          (conversation-identifier
           (application-conversation (mission-context-application context)))
          (getf (mission-context-goal context) :created-at)))

(-> mission-knowledge--record (mission-context string list string) null)
(defun mission-knowledge--record (context predicate arguments detail)
  "Record that CONTEXT's mission reported PREDICATE of ARGUMENTS, described by DETAIL."
  (let* ((application (mission-context-application context))
         (configuration (application-configuration application)))
    (when (knowledge-capture-p configuration)
      (handler-case
          (let* ((identity (mission-knowledge--identity context))
                 (schema (knowledge-base-schema (knowledge-base-read configuration) predicate)))
            (knowledge-replace-observation
             configuration
             (knowledge-make-proposition schema (cons identity arguments))
             (knowledge-observe :source (list :mission :id identity :detail detail)
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
    (mission-knowledge--record context "autolith.mission.status" (list name) name)))

(-> mission-knowledge-record-gate (mission-context string keyword) null)
(defun mission-knowledge-record-gate (context gate status)
  "Record that gate GATE of CONTEXT's mission now has STATUS."
  (let ((name (string-downcase (symbol-name status))))
    (mission-knowledge--record context "autolith.mission.gate" (list gate name)
                               (format nil "~A ~A" gate name))))
