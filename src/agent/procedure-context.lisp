(in-package #:autolith)

;;;; -- Procedure Catalog --

(-> procedure-catalog-line (list) string)
(defun procedure-catalog-line (procedure)
  "Return the catalog entry of PROCEDURE: its name, description and parameters."
  (let ((properties (rest procedure)))
    (format nil "- ~A: ~:[This procedure was learned from repeated calls to ~{~A~^, ~}.~;~:*~A~*~]~{~%  ~A~}"
            (getf properties :id)
            (getf properties :description)
            (mapcar (lambda (step) (getf step :tool)) (getf properties :steps))
            (mapcar (lambda (parameter)
                      (format nil "~A (~(~A~))~@[: ~A~]" (getf parameter :name)
                              (getf parameter :type) (getf parameter :description)))
                    (getf properties :parameters)))))

(-> procedure-catalog-context (request-context) (option context-contribution))
(defun procedure-catalog-context (request)
  "Return the catalog of the active procedures of the workspace of REQUEST, or NIL if there are none."
  (let ((configuration (request-context-configuration request)))
    (when (and (not (request-context-compaction-p request))
               (knowledge-active-p configuration))
      (when-let ((procedures
                  (loop for entry in (handler-case
                                         (knowledge-base-procedures
                                          (knowledge-base-read configuration)
                                          (knowledge--workspace configuration))
                                       (knowledge-error ()
                                         nil))
                        for version = (procedure-entry-runnable-version entry)
                        when version
                          collect (getf version :procedure))))
        (change-class
         (make-context-contribution
          :identifier        "procedure-catalog"
          :instruction       (format nil "## Procedures~2%The procedures of this workspace follow, each with its parameters. memory:procedures contains their steps.~2%~{~A~^~%~}"
                                     (mapcar #'procedure-catalog-line procedures))
          :priority          800
          :class             ':mandatory
          :deduplication-key "procedure-catalog")
         'anchored-context-contribution)))))

(register-context-contributor "procedure-catalog" 'procedure-catalog-context :source ':built-in)


;;;; -- Procedure Learning --

(-> agent-learn-procedures (agent agent-observer integer) null)
(defun agent-learn-procedures (agent observer tool-calls)
  "Learn procedures after a primary AGENT's turn that made TOOL-CALLS calls, reporting new ones.

A turn with fewer than two calls teaches nothing, so it reads no traces. A
failure while learning is not the turn's failure: nothing is reported."
  (let ((child-class (find-class 'task-child-agent nil)))
    (when (and (>= tool-calls 2)
               (not (and child-class (typep agent child-class))))
      (when-let ((learned (handler-case (procedure-learn (agent-configuration agent))
                            (error ()
                              nil))))
        (agent-observer-status observer :procedures-learned
                               (list :procedures learned)))))
  nil)
