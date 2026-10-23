(in-package #:autolith)

;;;; -- Mutable State Change Presentation --

(-> application--single-resource-operation-p (t) boolean)
(defun application--single-resource-operation-p (operations)
  "Return whether OPERATIONS is exactly one JSON operation vector."
  (and (vectorp operations)
       (not (stringp operations))
       (= (length operations) 1)))


;;; Agenda changes

(-> application--agenda-item-document (agenda-status string list) string)
(defun application--agenda-item-document (status text memory-identifiers)
  "Return one stable editable line document for an agenda item."
  (format nil "status: ~(~A~)~%text: ~A~%memory-ids: ~:[none~;~:*~{~A~^, ~}~]"
          status text memory-identifiers))

(-> application--agenda-item->document (agenda-item) string)
(defun application--agenda-item->document (item)
  "Return ITEM as one stable editable line document."
  (application--agenda-item-document
   (agenda-item-status item)
   (agenda-item-text item)
   (agenda-item-memory-identifiers item)))

(-> application--agenda-record-item
    ((option workspace-agenda) non-empty-string)
    (option agenda-item))
(defun application--agenda-record-item (record identifier)
  "Return agenda item IDENTIFIER from RECORD, when present."
  (and record
       (find identifier
             (workspace-agenda-items record)
             :test #'string=
             :key #'agenda-item-identifier)))

(-> application--agenda-resource-record
    (application string string)
    (values (option workspace-agenda) boolean))
(defun application--agenda-resource-record (application uri revision)
  "Return the exact observed agenda record for URI and REVISION, when retained."
  (block nil
    (unless (and (slot-boundp application 'conversation)
                 (non-empty-string-p uri)
                 (non-empty-string-p revision))
      (return (values nil nil)))
    (let ((conversation (application-conversation application)))
      (unless (typep conversation 'conversation)
        (return (values nil nil)))
      (with-recursive-lock-held
          ((conversation-resource-observation-lock conversation))
        (let ((state
                (resource-observation-state-find
                 (conversation-resource-observations conversation)
                 revision
                 'agenda-observation-state)))
          (unless state
            (return (values nil nil)))
          (let ((observation (resource-observation-state-observation state)))
            (unless (and (typep observation 'agenda-observation)
                         (string= uri (resource-observation-uri observation)))
              (return (values nil nil)))
            (let* ((snapshot (agenda-observation-snapshot observation))
                   (form (getf snapshot :record)))
              (cond
                ((null form)
                 (values nil t))
                ((agenda--record-form-p form *agenda-version*)
                 (values (agenda--record-form->record form *agenda-version*) t))
                (t
                 (values nil nil))))))))))

(-> application--agenda-operation-change-documents
    (json-object (option workspace-agenda))
    (values (option string) (option string)
            (option integer) (option integer) boolean))
(defun application--agenda-operation-change-documents (operation record)
  "Return before and after agenda item documents for OPERATION against RECORD."
  (handler-case
      (let ((normalized (agenda-resource--normalize-operation operation)))
        (case (getf normalized :kind)
          (:add
           (values
            nil
            (application--agenda-item-document
             (getf normalized :status)
             (getf normalized :text)
             (getf normalized :memory-identifiers))
            nil 1 t))
          (:update
           (let ((item
                   (application--agenda-record-item
                    record (getf normalized :identifier))))
             (if item
                 (values
                  (application--agenda-item->document item)
                  (application--agenda-item-document
                   (or (getf normalized :status) (agenda-item-status item))
                   (or (getf normalized :text) (agenda-item-text item))
                   (if (getf normalized :memory-identifiers-supplied-p)
                       (getf normalized :memory-identifiers)
                       (agenda-item-memory-identifiers item)))
                  1 1 t)
                 (values nil nil nil nil nil))))
          (:remove
           (let ((item
                   (application--agenda-record-item
                    record (getf normalized :identifier))))
             (if item
                 (values (application--agenda-item->document item)
                         nil 1 nil t)
                 (values nil nil nil nil nil))))
          (otherwise
           (values nil nil nil nil nil))))
    (error ()
      (values nil nil nil nil nil))))

(-> application--agenda-operation-change-rows
    (json-object (option workspace-agenda))
    (option list))
(defun application--agenda-operation-change-rows (operation record)
  "Return the shared colored line view for one agenda OPERATION."
  (multiple-value-bind (old-text new-text old-start-line new-start-line available-p)
      (application--agenda-operation-change-documents operation record)
    (and available-p
         (change-viewer-render
          :removed-content old-text
          :added-content new-text
          :removed-start-line old-start-line
          :added-start-line new-start-line))))



;;; Memory changes

(-> application--memory-document
    (&key (:title string) (:content string) (:scope memory-scope) (:tags list))
    string)
(defun application--memory-document (&key title content scope tags)
  "Return one stable editable line document for a persistent memory."
  (format nil "title: ~A~%scope: ~(~A~)~%tags: ~:[none~;~:*~{~A~^, ~}~]~%content:~%~A"
          title scope tags content))

(-> application--memory->document (memory) string)
(defun application--memory->document (memory)
  "Return MEMORY as one stable editable line document."
  (application--memory-document
   :title (memory-title memory)
   :content (memory-content memory)
   :scope (memory-scope memory)
   :tags (memory-tags memory)))


(-> application--memory-resource-observation
    (application string string)
    (values (option memory-observation) boolean))
(defun application--memory-resource-observation (application uri revision)
  "Return the exact retained memory observation for URI and REVISION."
  (block nil
    (unless (and (slot-boundp application 'conversation)
                 (non-empty-string-p uri)
                 (non-empty-string-p revision))
      (return (values nil nil)))
    (let ((conversation (application-conversation application)))
      (unless (typep conversation 'conversation)
        (return (values nil nil)))
      (with-recursive-lock-held
          ((conversation-resource-observation-lock conversation))
        (let ((state
                (resource-observation-state-find
                 (conversation-resource-observations conversation)
                 revision
                 'memory-observation-state)))
          (unless state
            (return (values nil nil)))
          (let ((observation (resource-observation-state-observation state)))
            (if (and (typep observation 'memory-observation)
                     (string= uri (resource-observation-uri observation)))
                (values observation t)
                (values nil nil))))))))

(-> application--memory-observation-item
    (memory-observation)
    (option memory))
(defun application--memory-observation-item (observation)
  "Return the exact item value carried by OBSERVATION, when it is an item."
  (when (eq (memory-observation-kind observation) ':item)
    (let ((record (getf (memory-observation-snapshot observation) :record)))
      (and record
           (handler-case
               (memory--record->memory #P"memories.sexp" record)
             (error ()
               nil))))))

(-> application--memory-resource-operation-change-documents
    (json-object memory-observation)
    (values (option string) (option string)
            (option integer) (option integer) boolean))
(defun application--memory-resource-operation-change-documents
    (operation observation)
  "Return before and after memory documents for OPERATION and OBSERVATION."
  (handler-case
      (let ((normalized (memory-resource--normalize-operation operation)))
        (case (getf normalized :kind)
          (:remember
           (let ((scope
                   (and (eq (memory-observation-kind observation) ':collection)
                        (cond
                          ((string= (memory-observation-identifier observation)
                                    "workspace")
                           ':workspace)
                          ((string= (memory-observation-identifier observation)
                                    "global")
                           ':global)
                          (t
                           nil)))))
             (if scope
                 (values
                  nil
                   (application--memory-document
                    :title (getf normalized :title)
                    :content (getf normalized :content)
                    :scope scope
                    :tags (getf normalized :tags))
                  nil 1 t)
                 (values nil nil nil nil nil))))
          (:replace
           (let ((memory (application--memory-observation-item observation)))
             (if memory
                 (values
                  (application--memory->document memory)
                   (application--memory-document
                    :title (getf normalized :title)
                    :content (getf normalized :content)
                    :scope (or (getf normalized :scope) (memory-scope memory))
                    :tags (getf normalized :tags))
                  1 1 t)
                 (values nil nil nil nil nil))))
          (:forget
           (let ((memory (application--memory-observation-item observation)))
             (if memory
                 (values (application--memory->document memory)
                         nil 1 nil t)
                 (values nil nil nil nil nil))))
          (otherwise
           (values nil nil nil nil nil))))
    (error ()
      (values nil nil nil nil nil))))

(-> application--memory-resource-operation-change-rows
    (json-object memory-observation)
    (option list))
(defun application--memory-resource-operation-change-rows
    (operation observation)
  "Return the shared colored line view for one memory resource OPERATION."
  (multiple-value-bind (old-text new-text old-start-line new-start-line available-p)
      (application--memory-resource-operation-change-documents operation observation)
    (and available-p
         (change-viewer-render
          :removed-content old-text
          :added-content new-text
          :removed-start-line old-start-line
          :added-start-line new-start-line))))

;;; Resource edit dispatch

(defmethod application-tool-call-entry
    ((tool resource-edit-tool) (application application) (call hash-table))
  "Present resource.edit operations through resource-specific change adapters."
  (declare (ignore tool))
  (let* ((arguments (application--function-call-arguments call))
         (uri (let ((value (and arguments (json-get arguments "uri"))))
                (if (stringp value) value "")))
         (revision
           (let ((value (and arguments (json-get arguments "base-revision"))))
             (if (stringp value) value "")))
         (operations (and arguments (json-get arguments "operations")))
         (path (application--file-resource-path uri)))
    (application--tool-entry
     application
     :style ':tool
     :header "▸ resource.edit"
     :rows
     (append
      (application--generic-argument-rows
       application arguments :excluded-keys '("operations"))
      (list nil)
      (cond
        (path
         (application--workspace-resource-operation-rows
          application operations
          :uri uri :revision revision :path path))
        ((string= uri "agenda:current")
         (multiple-value-bind (record observed-p)
             (application--agenda-resource-record application uri revision)
           (application--resource-operation-rows
            application
            operations
            :change-rows-function
            (and observed-p
                 (application--single-resource-operation-p operations)
                 (lambda (operation)
                   (application--agenda-operation-change-rows
                    operation record))))))
        ((uiop:string-prefix-p "memory:" uri)
         (multiple-value-bind (observation observed-p)
             (application--memory-resource-observation application uri revision)
           (application--resource-operation-rows
            application
            operations
            :change-rows-function
            (and observed-p
                 (application--single-resource-operation-p operations)
                 (lambda (operation)
                   (application--memory-resource-operation-change-rows
                    operation observation))))))
        (t
         (application--resource-operation-rows application operations)))))))
