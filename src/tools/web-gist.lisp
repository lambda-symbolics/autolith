(in-package #:autolith)

;;;; -- Standalone Web Page Retrieval --

(defclass web-gist-tool (tool)
  ()
  (:documentation
   "A tool that fetches one web page and returns it as Markdown."))

(-> web-gist-parameters () json-object)
(defun web-gist-parameters ()
  "Return the web.gist parameter schema."
  (tool-object-schema
   (json-object
    "url" (tool-string-property
           "The absolute HTTP or HTTPS URL of the page to retrieve."))
   '("url")))

(-> web-gist--retrieve (string) string)
(defun web-gist--retrieve (url)
  "Fetch URL and return its Markdown representation."
  (unless (or (uiop:string-prefix-p "http://" url)
              (uiop:string-prefix-p "https://" url))
    (error "web.gist only retrieves HTTP and HTTPS URLs."))
  (fetch-gist:markdown-from-url url))

(defmethod tool-execute ((tool web-gist-tool)
                         (context tool-context)
                         (arguments hash-table))
  "Fetch one web page and return its Markdown representation."
  (declare (ignore tool context))
  (let ((url (tool-argument arguments "url" :required t)))
    (unless (non-empty-string-p url)
      (error 'tool-error
             :message "web.gist requires a non-empty string url."
             :tool-name "web.gist"))
    (handler-case
        (tool-success (web-gist--retrieve url))
      (error (condition)
        (error 'tool-error
               :message (format nil "web.gist could not retrieve ~A: ~A"
                                url condition)
               :tool-name "web.gist")))))
