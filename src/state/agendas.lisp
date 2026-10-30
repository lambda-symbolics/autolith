;;;; -- Legacy Agenda Configuration Adapter --

(in-package #:autolith)

(defmethod autolith-supervisor::agenda-context-path ((configuration configuration))
  "Use the private agenda path from trusted legacy configuration."
  (configuration-agenda-path configuration))

(defmethod autolith-supervisor::agenda-context-directory
           ((configuration configuration))
  "Return the canonical workspace key from trusted legacy configuration."
  (agenda-directory-name configuration (config :working-directory configuration)
                         :require-existing-p t))

(defmethod autolith-supervisor::agenda-context-memory-valid-p
           ((configuration configuration) identifier)
  "Validate linked memory identity in the legacy permitted memory scope."
  (not (null (memory-find configuration identifier))))

(defmethod agenda-directory-name
           ((configuration configuration) location &key require-existing-p)
  "Return LOCATION as an absolute directory key.

Existing locations are canonicalized. A transported source may remain missing
when REQUIRE-EXISTING-P is false, but it must still name an absolute path."
  (handler-case
   (let ((existing (uiop/filesystem:directory-exists-p location)))
     (cond
      (existing
       (namestring
        (uiop/pathname:ensure-directory-pathname
         (platform-truename *platform* existing))))
      (require-existing-p
       (error 'agenda-error :message
              (format nil "Agenda directory ~A does not exist." location) :pathname
              (configuration-agenda-path configuration) :operation ':validate-directory
              :cause nil))
      (t
       (let ((pathname
              (uiop/pathname:ensure-pathname (platform-pathname location)
                                             :ensure-absolute t :ensure-directory t
                                             :want-non-wild t)))
         (namestring pathname)))))
   (agenda-error (condition) (error condition))
   (error (cause)
          (error 'agenda-error :message
                 (format nil "Cannot use ~A as an agenda directory." location)
                 :pathname (configuration-agenda-path configuration) :operation
                 ':validate-directory :cause cause))))

(-> agenda--prompt-item-line (agenda-item) string)
(defun agenda--prompt-item-line (item)
  "Return one untrusted agenda item line for the system prompt."
  (format nil "- [~(~A~)] ~A  ~A~@[  memory_ids: ~A~]" (agenda-item-status item)
          (agenda-item-identifier item) (json-encode (agenda-item-text item))
          (and (agenda-item-memory-identifiers item)
               (json-encode (coerce (agenda-item-memory-identifiers item) 'vector)))))

(-> agenda--prompt-visible-item-p (agenda-item) boolean)
(defun agenda--prompt-visible-item-p (item)
  "Return true when ITEM is active state worth injecting into request context."
  (not (null (member (agenda-item-status item) '(:todo :doing :blocked :note)))))

(-> agenda-prompt-item-lines (configuration) (option string))
(defun agenda-prompt-item-lines (configuration)
  "Return active agenda item lines without the lead-in, or NIL when empty."
  (with-recursive-lock-held (*agenda-lock*)
    (let* ((state  (agenda-load configuration))
           (record (agenda-current configuration state))
           (items
            (and record
                 (remove-if-not #'agenda--prompt-visible-item-p
                                (workspace-agenda-items record)))))
      (when items
        (format nil "~{~A~^~%~}" (mapcar #'agenda--prompt-item-line items))))))
