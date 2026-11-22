(in-package #:autolith)

;;;; -- Bounded Native Resource Views --

(-> application-client-resource--integer
    (list keyword &key (:default integer) (:minimum integer) (:maximum integer)) integer)
(defun application-client-resource--integer (params name &key default minimum maximum)
  "Read a bounded native resource window parameter."
  (let ((value (or (getf params name) default)))
    (unless (and (integerp value) (<= minimum value maximum))
      (error 'application-client-unavailable
             :reason (format nil "Resource parameter ~S must be an integer between ~D and ~D."
                             name minimum maximum)))
    value))

(defgeneric application-client-resource--read (resource context params)
  (:documentation "Project an authorized resource as bounded native content."))

(defmethod application-client-resource--read
    ((resource resource) (context tool-context) params)
  "Reject resources without a structured projection."
  (declare (ignore resource context params))
  (error 'application-client-unavailable :reason "Resource has no structured projection."))

(defmethod application-client-resource--read
    ((resource workspace-file-resource) (context tool-context) params)
  "Read the requested lines from a single resource snapshot."
  (when (or (getf params :byte-offset) (getf params :byte-count))
    (error 'application-client-unavailable :reason "Workspace resources require line windows."))
  (let* ((start       (application-client-resource--integer
                       params ':start-line :default 1 :minimum 1 :maximum most-positive-fixnum))
         (count       (application-client-resource--integer
                       params ':line-count :default 400 :minimum 1 :maximum 1000))
         (observation (resource-observe resource context))
         (lines       (workspace-file-observation-lines observation))
         (total       (length lines))
         (first       (min total (1- start)))
         (last        first)
         (characters  0))
    (unless (eq (workspace-file-observation-kind observation) ':file)
      (error 'application-client-unavailable :reason "Workspace reads require a regular text file."))
    (loop while (< last (min total (+ first count)))
          for size = (+ (length (aref lines last)) (if (= last first) 0 1))
          while (<= (+ characters size) 65536)
          do (incf characters size) (incf last))
    (when (and (= first last) (< first total))
      (error 'application-client-unavailable :reason "Resource line exceeds the 65536-character window limit."))
    (list :uri      (resource-uri resource)
          :revision (resource-observation-revision observation)
          :encoding "utf-8" :mime "text/plain"
          :content  (format nil "~{~A~^~%~}" (coerce (subseq lines first last) 'list))
          :window   (list :kind ':lines :start-line (1+ first) :line-count (- last first)
                          :total-lines total :next-line (and (< last total) (1+ last))
                          :truncated-p (< last total)))))

(defmethod application-client-resource--read
    ((resource shell-log-resource) (context tool-context) params)
  "Read a bounded raw byte window from an exact shell-log reference."
  (when (or (getf params :start-line) (getf params :line-count))
    (error 'application-client-unavailable :reason "Shell logs require byte windows."))
  (let* ((offset   (application-client-resource--integer
                    params ':byte-offset :default 0 :minimum 0 :maximum most-positive-fixnum))
         (count    (application-client-resource--integer
                    params ':byte-count :default 4096 :minimum 1 :maximum 4096))
         (metadata (shell-log-resource-metadata resource))
         (capture  (shell-log-resource--capture resource))
         (file     (getf capture :file)))
    (when (or (member (getf metadata :state) '(:missing :pruned)) (null file))
      (error 'application-client-unavailable :reason "Shell log capture is unavailable."))
    (unless (shell-log--capture-file-p file)
      (shell-log--fail "Invalid shell capture filename in manifest."))
    (multiple-value-bind (bytes total next)
        (shell-log-resource--read-bytes
         (tool-context-configuration context)
         (merge-pathnames file (shell-log-resource-directory resource))
         :offset offset :count count)
      (list :uri (resource-uri resource) :revision nil
            :encoding "utf-8" :mime "text/plain"
            :content (cl-exec-sandbox:decode-capture-bytes bytes)
            :window  (list :kind ':bytes :byte-offset (min offset total)
                           :byte-count (length bytes) :total-bytes total
                           :next-byte-offset (and (< next total) next)
                           :truncated-p      (< next total))))))

(-> application-client-resource
    (application string &key (:start-line (option integer)) (:line-count (option integer))
                             (:byte-offset (option integer)) (:byte-count (option integer))) list)
(defun application-client-resource (application uri &key start-line line-count byte-offset byte-count)
  "Return bounded structured content for the requested URI."
  (let ((context (application-tool-context application)))
    (unless (and (stringp uri) (plusp (length uri)))
      (error 'application-client-unavailable :reason "Resource URI must be a nonempty string."))
    (multiple-value-bind (scheme identifier) (resource-uri-parse uri)
      (declare (ignore identifier))
      (unless (member scheme '("workspace" "shell-log") :test #'string=)
        (error 'application-client-unavailable :reason "Resource reads support workspace and shell-log URIs."))
      (let ((registry (tool-registry-resource-registry (tool-context-registry context))))
        (application-client-resource--read
         (if (string= scheme "shell-log")
             (cl-resources:resource-registry-resolve registry uri context)
             (resource-registry-resolve registry uri context))
         context
         (list :start-line start-line :line-count line-count
               :byte-offset byte-offset :byte-count byte-count))))))
