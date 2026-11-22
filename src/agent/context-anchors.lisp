(in-package #:autolith)

;;;; -- Durable Context Anchors --

;;; An anchored contribution keeps the history boundary where it was first
;;; projected, so a provider's cached prefix through it survives later requests.
;;; Anchors name contributions and boundary items by SHA-256 digest and index
;;; only. They persist beside the conversation, so resuming it reuses the same
;;; boundaries; each one is validated against the current history before use.

(defparameter *context-anchors-maximum-octets* (* 256 1024)
  "Maximum size of one conversation's context anchor sidecar.")

(-> context-anchors-current (conversation) list)
(defun context-anchors-current (conversation)
  "Return CONVERSATION's context anchors, reading its durable sidecar once.

Callers hold the conversation append lock."
  (unless (conversation-context-anchors-loaded-p conversation)
    (setf (conversation-context-anchors conversation) (context-anchors--read conversation)
          (conversation-context-anchors-loaded-p conversation) t))
  (conversation-context-anchors conversation))

(-> context-anchors-update (conversation list) null)
(defun context-anchors-update (conversation anchors)
  "Install ANCHORS for CONVERSATION and publish them when they changed.

Callers hold the conversation append lock. Only a persisted conversation has a
sidecar; a failed publication keeps the anchors for this process only, which
costs at most a cache miss after resuming."
  (unless (equal anchors (conversation-context-anchors conversation))
    (setf (conversation-context-anchors conversation) anchors)
    (when (conversation-persisted-p conversation)
      (handler-case (context-anchors--write conversation anchors)
        (error ()
          nil))))
  (setf (conversation-context-anchors-loaded-p conversation) t)
  nil)

(-> context-anchors-key-digest (list) string)
(defun context-anchors-key-digest (key)
  "Return the durable identity of contribution KEY as a SHA-256 digest."
  (memory-source-text-digest
   (with-standard-io-syntax
     (let ((*print-readably* nil))
       (prin1-to-string key)))))

(-> context-anchors-item-digest (t) string)
(defun context-anchors-item-digest (item)
  "Return the durable identity of history ITEM as a SHA-256 digest of its JSON."
  (memory-source-text-digest (json-encode item)))

(-> context-anchors--pathname (conversation) pathname)
(defun context-anchors--pathname (conversation)
  "Return CONVERSATION's context anchor sidecar."
  (conversation-context-anchors-pathname (conversation-pathname conversation)))

(-> context-anchors--grammar () t)
(defun context-anchors--grammar ()
  "Return the bounded data-only dialect of anchor sidecars."
  (make-source-grammar
   :label                     "Context anchors"
   :maximum-depth 8 :maximum-nodes 65536
   :maximum-string-characters 128
   :allowed-atom-predicate
   (lambda (value)
     (or (null value) (keywordp value) (stringp value) (integerp value)))))

(-> context-anchors--valid-p (t) boolean)
(defun context-anchors--valid-p (anchor)
  "Return true when ANCHOR is one well-formed persisted anchor."
  (and (listp anchor)
       (stringp (getf anchor :key))
       (let ((index (getf anchor :index))
             (item  (getf anchor :item)))
         (or (and (null index) (null item))
             (and (typep index '(integer 0)) (stringp item))))
       t))

(-> context-anchors--read (conversation) list)
(defun context-anchors--read (conversation)
  "Read CONVERSATION's persisted anchors, or NIL when absent, foreign or damaged."
  (let ((pathname (context-anchors--pathname conversation)))
    (when (probe-file pathname)
      (handler-case
          (let ((record (sexp-store:snapshot-read-record
                         pathname :grammar        (context-anchors--grammar)
                                  :maximum-octets *context-anchors-maximum-octets*
                                  :properties-p   t :keyword-keys-p t)))
            (when (and (eql (getf record :version) 1)
                       (equal (getf record :id) (conversation-identifier conversation))
                       (eql (getf record :created-at) (conversation-created-at conversation))
                       (listp (getf record :anchors))
                       (every #'context-anchors--valid-p (getf record :anchors)))
              (getf record :anchors)))
        (error ()
          nil)))))

(-> context-anchors--write (conversation list) null)
(defun context-anchors--write (conversation anchors)
  "Atomically publish CONVERSATION's ANCHORS."
  (let ((pathname (context-anchors--pathname conversation)))
    (ensure-directories-exist pathname)
    (snapshot-write-text
     pathname
     (with-standard-io-syntax
       (let ((*print-readably* nil))
         (format nil "~S~%"
                 (list :version    1
                       :id         (conversation-identifier conversation)
                       :created-at (conversation-created-at conversation)
                       :anchors    anchors))))))
  nil)
