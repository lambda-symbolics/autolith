;;;; -- Trusted Session Network Approval --

(in-package #:autolith-supervisor)

(serapeum/types:-> parent-application-network-p (parent-application) boolean)
(defun parent-application-network-p (application)
  "Read the parent-owned whole-session network policy, independent of file access."
  (not (null (parent-configuration-get (parent-application-configuration application) ':network-p))))

(serapeum/types:-> parent-terminal-approve-network
    (parent-terminal &key (:timeout real) (:cancelled (or null function))) boolean)
(defun parent-terminal-approve-network (terminal &key (timeout 60) cancelled)
  "Ask the trusted owner for broad session network authority. Only YES approves."
  (funcall
   (parent-terminal-exclusive-input terminal)
   (lambda ()
     (bordeaux-threads:with-lock-held ((parent-terminal-render-lock terminal))
       (when (parent-terminal-closed-p terminal)
         (error 'parent-terminal-error :reason ':closed))
       (handler-case
           (sb-ext:with-timeout (parent-terminal-write-timeout terminal)
             (funcall (parent-terminal-renderer terminal) ':trusted ':network-approval
                      (list :text "Allow unrestricted outbound network access for this session? This includes private and local network addresses. The child must restart to apply this policy. Type YES to approve.")))
         (sb-ext:timeout ()
           (error 'parent-terminal-error :reason ':blocked-output)))
       (multiple-value-bind (line state)
           (parent-terminal-read-input terminal :timeout timeout :cancelled cancelled)
         (and (eq state ':line) (string= line "YES") t))))))

(serapeum/types:-> parent-application-set-network
    (parent-application boolean &key (:terminal (or null parent-terminal))) keyword)
(defun parent-application-set-network (application enabled &key terminal)
  "Request explicit broad network policy through the trusted owner interface.
Approve in the parent, stop the old process, then replace its immutable native
policy. Exact endpoint grants are separate and do not imply this authority.
Return :UNCHANGED, :DENIED or :RESTARTING. No child message invokes this API."
  (unless (member enabled '(nil t))
    (parent-configuration--reject ':invalid-network-setting))
  (when (eq enabled (parent-application-network-p application))
    (return-from parent-application-set-network ':unchanged))
  (let ((backend (parent-application-backend application)))
    (unless backend
      (parent-configuration--reject ':application-not-running))
    (access-native-check-network backend enabled))
  (when (and enabled
             (or (null terminal)
                 (not (parent-terminal-approve-network terminal))))
    (return-from parent-application-set-network ':denied))
  (parent-application-defer
   application
   (lambda ()
     (let* ((configuration (parent-application-configuration application))
            (values        (copy-tree (parent-configuration-values configuration))))
       (setf (getf values :network-p) enabled
             (slot-value configuration 'values) values
             (parent-application-policy application) nil
             (parent-application-backend application) nil
             (parent-application-access-session application) nil))))
  ':restarting)
