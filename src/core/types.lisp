(in-package #:autolith)

;;;; -- Fundamental Types --

(deftype tool-conversation-persistence ()
  "The lifetime of one tool call and its correlated provider result."
  '(member :durable :next-response))
