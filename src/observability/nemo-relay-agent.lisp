(in-package #:autolith)

;;;; -- Agent Adapter --

(-> observability-agent-turn-metadata
    (agent &key (:automatic-p boolean))
    json-object)
(defmethod observability-agent-turn-metadata
    ((agent agent) &key automatic-p)
  "Return the backend-neutral metadata projected from AGENT's turn state."
  (json-object
   "agent" "autolith"
   "agent_name" (observability-agent-name agent)
   "parent_agent" (observability-agent-parent-name agent)
   "child" (if (observability-agent-child-p agent) t false)
   "session_id" (agent-session-id agent)
   "conversation_id"
   (conversation-identifier (agent-conversation agent))
   "turn"
   (1+ (conversation-user-turn-count (agent-conversation agent)))
   "model"
   (configuration-model (agent-configuration agent))
   "reasoning_effort"
   (configuration-reasoning-effort (agent-configuration agent))
   "workspace"
   (namestring
    (configuration-working-directory (agent-configuration agent)))
   "automatic" (if automatic-p t false)))

(-> observability-agent-turn-start-data (agent) (option json-object))
(defmethod observability-agent-turn-start-data ((agent agent))
  "Return the backend-neutral start data for AGENT's turn."
  (json-object
   "conversation_id"
   (conversation-identifier (agent-conversation agent))
   "turn"
   (1+ (conversation-user-turn-count (agent-conversation agent)))))
