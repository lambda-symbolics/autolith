(in-package #:autolith-supervisor)

(serapeum/types:-> knowledge-node-text (knowledge-node) string)
(defun knowledge-node-text (node)
  "Return NODE's claim as a sentence."
  (or (knowledge-node--text node)
      (setf (knowledge-node--text node)
            (knowledge-proposition-display (knowledge-node-proposition node)
                                           (knowledge-node-schema node)))))
