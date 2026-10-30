(in-package #:autolith-supervisor)

;;;; -- Procedure Read Projections --

(serapeum/types:-> procedure-template-text (list) string)
(defun procedure-template-text (template)
  "Return TEMPLATE as JSON-like text: parameters as {NAME} and earlier results as <call N PATH>."
  (ecase (first template)
    (:json
     (second template))
    (:param
     (format nil "{~A}" (second template)))
    (:text
     (let ((prefix (argo:json-encode (second template)))
           (suffix (argo:json-encode (fourth template))))
       (format nil "~A{~A}~A" (subseq prefix 0 (1- (length prefix))) (third template)
               (subseq suffix 1))))
    (:from
     (format nil "<call ~D ~{~A~^.~}>" (1+ (second template)) (third template)))
    (:object
     (format nil "{~{~A~^,~}}"
             (mapcar (lambda (entry)
                       (format nil "~A:~A" (argo:json-encode (first entry))
                               (procedure-template-text (second entry))))
                     (rest template))))
    (:array
     (format nil "[~{~A~^,~}]" (mapcar #'procedure-template-text (rest template))))))

(serapeum/types:-> procedure-input-text (list) (option string))
(defun procedure-input-text (procedure)
  "Return PROCEDURE's request pattern with {NAME} for parameters and * for any word, or NIL."
  (let ((input (getf (rest procedure) :input)))
    (and input
         (format nil "~{~A~^ ~}"
                 (mapcar (lambda (element)
                           (cond
                             ((stringp element)
                              element)
                             ((eq (first element) :param)
                              (format nil "~A{~A}~A" (third element) (second element)
                                      (fourth element)))
                             (t
                              (format nil "~A*~A" (second element) (third element)))))
                         input)))))

(serapeum/types:-> procedure-entry-status (list) keyword)
(defun procedure-entry-status (entry)
  "Return where the procedure ENTRY describes stands."
  (cond
    ((getf entry :retired-p)
     ':retired)
    ((null (getf entry :admitted))
     ':candidate)
    ((>= (getf entry :consecutive-failures) *procedure-suspension-limit*)
     ':suspended)
    (t
     ':admitted)))

(serapeum/types:-> procedure-entry-version (list &optional (option integer)) list)
(defun procedure-entry-version (entry &optional version)
  "Return ENTRY's VERSION record, by default the newest one."
  (if version
      (nth (1- version) (getf entry :versions))
      (first (last (getf entry :versions)))))

(serapeum/types:-> knowledge-base-procedures (knowledge-base string) list)
(defun knowledge-base-procedures (base workspace)
  "Return WORKSPACE's procedure entries in BASE, ordered by name."
  (let ((entries nil))
    (maphash (lambda (key entry)
               (when (equal (first key) workspace)
                 (push entry entries)))
             (knowledge-base-procedures-table base))
    (sort entries #'string< :key (lambda (entry) (getf entry :id)))))
