(in-package #:autolith)

;;;; -- Durable Conversation Search Tools --

(defparameter *session-search-default-result-limit* 20
  "The default number of durable conversation matches returned by session.search.")

(defparameter *session-search-maximum-result-limit* 100
  "The maximum number of durable conversation matches returned by session.search.")

(defparameter *session-read-default-context* 2
  "The default number of durable message records shown on either side of a hit.")

(defparameter *session-read-maximum-context* 20
  "The maximum number of durable message records shown on either side of a hit.")

(defclass session-search-tool (tool) ())
(defclass session-read-tool (tool) ())

(defmethod tool-storm-guard-exempt-p ((tool session-search-tool))
  (declare (ignore tool)) t)
(defmethod tool-storm-guard-exempt-p ((tool session-read-tool))
  (declare (ignore tool)) t)

(defun session-tool--string (tool arguments name &key required)
  (let ((value (tool-argument arguments name :required required)))
    (unless (or (null value) (stringp value))
      (error 'tool-error :tool-name (tool-canonical-name tool)
             :message (format nil "~A requires string argument ~S."
                              (tool-canonical-name tool) name)))
    value))

(defun session-tool--integer (arguments name fallback maximum)
  (let ((value (workspace-tool-integer-argument arguments name)))
    (min maximum (max 0 (or value fallback)))))

(defun session-tool--messages (configuration identifier)
  "Return durable text messages for IDENTIFIER, numbered in storage order."
  (let ((conversation (conversation-replay-load configuration identifier))
        (messages nil))
    (conversation-replay--map-records
     conversation
     (lambda (record)
       (when (and (eq (first record) :message)
                  (stringp (getf (rest record) :content)))
         (push (list :role (getf (rest record) :role)
                     :content (getf (rest record) :content))
               messages))))
    (nreverse messages)))

(defun session-tool--render-message (index message)
  (format nil "~D [~(~A~)] ~A"
          index (getf message :role) (getf message :content)))

(defmethod tool-execute ((tool session-search-tool) (context tool-context) arguments)
  (let* ((query (session-tool--string tool arguments "query" :required t))
         (needle (string-downcase query))
         (limit (session-tool--integer arguments "max-results"
                                       *session-search-default-result-limit*
                                       *session-search-maximum-result-limit*))
         (configuration (tool-context-configuration context))
         (matches nil))
    (unless (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) query)))
      (error 'tool-error :tool-name "session.search"
             :message "session.search query must be a non-empty string."))
    (loop for pathname in (conversation-list configuration)
          while (< (length matches) limit)
          do
             (let* ((header (ignore-errors (conversation-peek-header pathname)))
                    (identifier (and header (getf (rest header) :id))))
               (when identifier
                 (loop for message in (ignore-errors
                                        (session-tool--messages configuration identifier))
                       for index from 0
                       while (< (length matches) limit)
                       when (search needle (string-downcase (getf message :content)))
                         do (push (list identifier index message) matches)))))
    (setf matches (nreverse matches))
    (make-instance 'tool-result :success-p t
                   :content
                   (if matches
                       (with-output-to-string (stream)
                         (loop for (identifier index message) in matches
                               for shown from 0
                               while (< shown limit)
                               do (format stream "~A:~D ~A~%"
                                          identifier index
                                          (session-tool--render-message index message))))
                       "No durable conversation matches."))))

(defmethod tool-execute ((tool session-read-tool) (context tool-context) arguments)
  (let* ((identifier (session-tool--string tool arguments "conversation-id" :required t))
         (offset (session-tool--integer arguments "offset" 0 most-positive-fixnum))
         (radius (session-tool--integer arguments "context"
                                        *session-read-default-context*
                                        *session-read-maximum-context*))
         (messages (session-tool--messages (tool-context-configuration context) identifier)))
    (when (>= offset (length messages))
      (error 'tool-error :tool-name "session.read"
             :message (format nil "Conversation ~A has no message at offset ~D."
                              identifier offset)))
    (let ((start (max 0 (- offset radius)))
          (end (min (length messages) (+ offset radius 1))))
      (make-instance 'tool-result :success-p t
                     :content
                     (with-output-to-string (stream)
                       (format stream "Conversation ~A, messages ~D through ~D:~%"
                               identifier start (1- end))
                       (loop for index from start below end
                             do (format stream "~A~%"
                                        (session-tool--render-message
                                          index (nth index messages)))))))))

(defun session-augment-tool-registry (registry)
  "Register bounded durable conversation search and paging tools in REGISTRY."
  (unless (tool-registry-find registry "session" "search")
    (tool-registry-describe-namespace registry "session"
                                     "Search and read durable past conversations.")
    (tool-registry-register
     registry
     (make-instance 'session-search-tool :namespace "session" :name "search"
                    :description "Search durable past conversation text. Results contain a conversation id and message offset for session.read."
                    :parameters
                    (tool-object-schema
                     (json-object
                      "query" (tool-string-property "Plain text matched case-insensitively.")
                      "max-results" (tool-integer-property "Bounded result count."))
                     '("query")))))
  (unless (tool-registry-find registry "session" "read")
    (tool-registry-register
     registry
     (make-instance 'session-read-tool :namespace "session" :name "read"
                    :description "Read a bounded message window from one durable conversation."
                    :parameters
                    (tool-object-schema
                     (json-object
                      "conversation-id" (tool-string-property "Conversation id from session.search.")
                      "offset" (tool-integer-property "Zero-based message offset."))
                     '("conversation-id" "offset")))))
  registry)
