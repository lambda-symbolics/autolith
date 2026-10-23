(in-package #:autolith)

(-> test-contract-requirement-bindings () null)
(defun test-contract-requirement-bindings ()
  "Exercise admitted bindings, ambiguity, and mapping diagnostics through dispatch and reload."
  (with-test-configuration (base root)
    (let* ((configuration (configuration-copy base :working-directory root :knowledge-mode "active"))
           (registry (make-default-tool-registry :configuration configuration))
           (conversation (conversation-create configuration))
           (context (make-instance 'tool-context :configuration configuration :worker nil
                                                 :registry registry :conversation conversation))
           (mail (make-instance 'knowledge-tests--mail-tool
                                :namespace "mail" :name "send" :description "Send."
                                :parameters (tool-object-schema
                                             (json-object "report" (tool-string-property "Report."))
                                             '("report")))))
      (tool-registry-register registry mail)
      (dolist (predicate '("report.owner" "report.predicted" "report.observed"))
        (knowledge-define-schema
         configuration (knowledge-make-schema
                        :predicate predicate
                        :arguments '((:name "report" :type :entity)
                                     (:name "owner" :type :entity)))))
      (knowledge-tests--said configuration "report.owner" '("q3" "alice") "Alice owns q3")
      (knowledge-define-schema
       configuration (knowledge-make-schema
                      :predicate "report.owner"
                      :arguments '((:name "report" :type :entity) (:name "owner" :type :entity)))
       :scope ':global)
      (knowledge-assert configuration '("report.owner" "q3" "alice")
                        :scope ':global :basis ':user-assertion
                        :evidence (knowledge-observe :source '(:user :quote "Alice owns q3 globally")
                                                     :adapter "user/1"))
      (flet ((define-mapping (reference)
               (knowledge-define-contract
                configuration :id "send-owner" :tool "mail.send"
                :when (json-object "report" "?report")
                :requires #(#("report.owner" "?report" "?owner"))
                :effects #(#("report.predicted" "?report" "?owner"))
                :observes (vector (json-object "predicate" "report.observed"
                                               "arguments" (vector "?report" reference)))))

             (send ()
               (memory-resource-tests--call registry context "mail" "send" "report" "q3")))
        (define-mapping "?owner")
        (test-assert (tool-result-success-p (send)) "the requirement-bound observation dispatch succeeds")
        (let ((*knowledge-base-cache* (make-hash-table :test #'equal :synchronized t)))
          (let* ((view (knowledge-view-current configuration))
                 (observed (first (knowledge-view-holds view '("report.observed" "q3" "alice"))))
                 (predicted (first (knowledge-view-holds view '("report.predicted" "q3" "alice"))))
                 (support (and observed (first (knowledge-node-supports observed))))
                 (evidence (and support
                                (knowledge-base-evidence-record
                                 (knowledge-view-base view) (getf support :evidence)))))
            (test-assert (and observed predicted (knowledge-node-observed-p observed))
                         "requirement bindings survive in observations and effects after replay")
            (test-assert (= 2 (length (getf (rest (getf evidence :source)) :requirements)))
                         "equal binding environments admit both proof alternatives and retain both claim identities")))
        (dolist (reference '("$result.missing" "$result"))
          (define-mapping reference)
          (let* ((result (send))
                 (decoded (json-decode (tool-result-content result))))
            (test-assert (and (tool-result-success-p result)
                              (eq t (json-get decoded "externalSuccess"))
                              (equal "failed" (json-get decoded "contractRecording"))
                              (= 1 (length (json-get decoded "contractErrors")))
                              (search "msg-1" (json-get decoded "externalResult")))
                         "missing and mistyped mappings report failure separately from successful external execution")
            (conversation-append-tool-result
             conversation (make-identifier) :tool-name "mail.send"
             :output (tool-result-content result) :success-p t)
            (let* ((reloaded (conversation-load (conversation-pathname conversation)))
                   (record (first (conversation-records-newest reloaded 1))))
              (test-assert (and (eq (getf (rest record) :status) ':ok)
                                (search "contractErrors" (getf (rest record) :output)))
                           "durable mapping diagnostics preserve the successful external outcome"))))
        (knowledge-tests--said configuration "report.owner" '("q3" "bob") "Bob also owns q3")
        (let ((before (knowledge-tests--mail-tool-calls mail))
              (result (send)))
          (test-assert (and (not (tool-result-success-p result))
                            (search "ambiguous requirement bindings" (tool-result-content result))
                            (= before (knowledge-tests--mail-tool-calls mail)))
                       "ambiguous bindings refuse before external execution")))))
  (test-contract-recording-result-protocol)
  nil)


(defclass contract-binding-tests--result-tool (tool)
  ((result :initarg :result :accessor contract-binding-tests--result
           :documentation "The successful external result returned by this fixture."))
  (:documentation "A tool returning plain or ordered rich results through normal dispatch."))

(defmethod tool-execute ((tool contract-binding-tests--result-tool) (context tool-context)
                         (arguments hash-table))
  "Return the fixture's original external result."
  (declare (ignore context arguments))
  (contract-binding-tests--result tool))

(-> test-contract-recording-result-protocol () null)
(defun test-contract-recording-result-protocol ()
  "Retain valid bounded diagnostics, ordered images, and durable result subtype metadata."
  (with-test-configuration (base root)
    (let* ((configuration (configuration-copy base :working-directory root :knowledge-mode "active"))
           (registry (make-default-tool-registry :configuration configuration))
           (conversation (conversation-create configuration))
           (context (make-instance 'tool-context :configuration configuration :worker nil
                                                 :registry registry :conversation conversation))
           (tool (make-instance 'contract-binding-tests--result-tool
                                :namespace "receipt" :name "fetch" :description "Return a receipt."
                                :parameters (tool-object-schema (json-object) nil)
                                :result (tool-success (make-string 8000 :initial-element #\")))))
      (tool-registry-register registry tool)
      (knowledge-define-schema configuration
                               (knowledge-make-schema :predicate "receipt.identifier"
                                                      :arguments '((:name "id" :type :string))))
      (knowledge-define-contract
       configuration :id "receipt-identifier" :tool "receipt.fetch"
       :observes (vector (json-object "predicate" "receipt.identifier" "arguments" #("$result.id"))))
      (let* ((result (memory-resource-tests--call registry context "receipt" "fetch"))
             (diagnostic (json-decode (tool-result-content result))))
        (test-assert (and (tool-result-success-p result)
                          (<= (length (tool-result-content result)) 8000)
                          (eq t (json-get diagnostic "externalSuccess"))
                          (eq t (json-get diagnostic "externalResultTruncated"))
                          (equal "failed" (json-get diagnostic "contractRecording"))
                          (= 1 (length (json-get diagnostic "contractErrors"))))
                     "escaped long results retain valid bounded JSON diagnostics"))
      (let ((original (change-class (tool-success (make-string 8000 :initial-element #\a))
                                    'mcp-content-result :sections '((:text 0 8000)))))
        (setf (contract-binding-tests--result tool) original)
        (let* ((result (memory-resource-tests--call registry context "receipt" "fetch"))
               (content (tool-result-content result))
               (details (tool-result-details result))
               (separator (position #\Newline content :from-end t)))
          (test-assert (and (<= (length content) 8000)
                            separator
                            (json-get (json-decode (subseq content (1+ separator))) "externalSuccess")
                            (equal (getf (rest details) :external-details) (tool-result-details original))
                            (application--mcp-content-sections details content))
                       "plain provider text includes the diagnostic with adapted ranges and original metadata")))
      (let* ((image (image-input-prepare
                     (merge-pathnames "docs/autolith-banner.png" (asdf:system-source-directory "autolith"))
                     (conversation-image-artifact-root conversation)))
             (original (change-class
                        (tool-success "before imageafter image"
                                      :content-blocks (list "before image" image "after image"))
                        'mcp-content-result :sections '((:text 0 12) (:text 12 23)))))
        (setf (contract-binding-tests--result tool) original)
        (let* ((result (memory-resource-tests--call registry context "receipt" "fetch"))
               (details (tool-result-details result)))
          (test-assert (and (tool-result-success-p result)
                            (equal (tool-result-content original) (tool-result-content result))
                            (equal (tool-result-content-blocks original)
                                   (subseq (tool-result-content-blocks result) 0 3))
                            (equal (tool-result-image-attachments original) (tool-result-image-attachments result))
                            (equal (getf (rest details) :sections) '((:text 0 12) (:text 12 23)))
                            (json-get (json-decode (getf (rest details) :contract-recording)) "externalSuccess"))
                       "a recording failure preserves ordered MCP content and section metadata")
          (let* ((blocks (tool-result-content-blocks result))
                 (wire (conversation--tool-content-output blocks))
                 (diagnostic (json-decode (json-get (aref wire (1- (length wire))) "text"))))
            (test-assert (and (<= (loop for block in blocks when (stringp block) sum (length block)) 8000)
                              (eq t (json-get diagnostic "externalSuccess"))
                              (equal "failed" (json-get diagnostic "contractRecording")))
                         "the authoritative ordered provider transport includes a bounded recording diagnostic"))
          (conversation-append-tool-result conversation (make-identifier)
                                           :tool-name "receipt.fetch" :success-p t
                                           :output (tool-result-content result) :details details
                                           :content-blocks (tool-result-content-blocks result))
          (let* ((reloaded (conversation-load (conversation-pathname conversation)))
                 (record nil))
            (conversation-replay--map-records
             reloaded (lambda (item) (when (eq (first item) ':tool-result) (setf record item))))
            (test-assert (and (eq (getf (rest record) :status) ':ok)
                              (equal (mapcar #'first (getf (rest record) :content-blocks)) '(:text :image :text :text))
                              (equal (getf (rest record) :details) details)
                              (equal (application--mcp-content-sections details (getf (rest record) :output))
                                     '((:text 0 12) (:text 12 23))))
                         "replay retains ordered rich blocks, valid section ranges, and structured diagnostics"))))))
  nil)
