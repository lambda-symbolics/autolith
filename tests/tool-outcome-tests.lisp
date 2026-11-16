(in-package #:autolith)

;;;; -- Fullscreen Outcome Transactions --

(-> tool-outcome-test--call (t string) list)
(defun tool-outcome-test--call (identifier call-id)
  "Return a clickable pending invocation presentation."
  (list identifier
        (list (termdown:make-widget ':tool "▸ (probe " '(:copy "original call"))
              (terminal-span ':tool (format nil "~S)" call-id)))
        (list :kind ':call :call-key (list :call "owner" call-id) :state ':pending :entry-kind ':tool-call :group "probe")))

(-> tool-outcome-test--result
    (t string &key (:state keyword) (:body list) (:job-id (option string))) list)
(defun tool-outcome-test--result (identifier call-id &key (state ':success) body job-id)
  "Return an outcome with independently controllable retained BODY and job identity."
  (list identifier nil
        (list :kind ':result :call-key (list :call "owner" call-id)
              :state state :body body :retain-output-p (not (null body))
              :job-key (and job-id (list :job "owner" job-id)))))

(-> tool-outcome-test--text (fullscreen-terminal-ui) string)
(defun tool-outcome-test--text (ui)
  "Return all resident committed text in append order."
  (let ((viewport (fullscreen-terminal-ui-viewport ui)))
    (with-output-to-string (output)
      (loop for index below (clinedi:transcript-viewport-chunk-count viewport)
            do (write-string (clinedi:transcript-viewport-chunk-text viewport index) output)))))

(-> test-fullscreen-outcome-batch () null)
(defun test-fullscreen-outcome-batch ()
  "Replace interleaved invocations in one paint without losing their clickable arguments."
  (with-terminal-ui (ui (fullscreen-test--ui
                         (make-instance 'recording-terminal :columns 80 :rows 20)))
    (let ((paints 0)
          (paint  (fdefinition 'terminal-ui-fullscreen-paint)))
      (test-call-with-function-replacements
       (list (list 'terminal-ui-fullscreen-paint
                   (lambda (&rest arguments)
                     (incf paints)
                     (apply paint arguments))))
       (lambda ()
         (test-assert
          (= 4 (terminal-ui-present-records
                ui (list (tool-outcome-test--call 1 "first")
                         (tool-outcome-test--call 2 "second")
                         (tool-outcome-test--result 3 "second" :state ':failure
                                                    :body             (list (terminal-span ':plain
                                                                                           (format nil "~%  │ error"))))
                         (tool-outcome-test--result 4 "first"))))
          "a batch uses each call and its hidden outcome")
         (test-assert (= paints 1) "one publication paints once")))
      (test-assert (and (= (fullscreen-test--chunk-count ui) 2)
                        (search "✓ (probe \"first\")" (tool-outcome-test--text ui))
                        (search "! (probe \"second\")" (tool-outcome-test--text ui))
                        (search "  │ error" (tool-outcome-test--text ui)))
                   "outcomes replace their own original invocations")
      (test-assert (equal '(:copy "original call")
                          (terminal-ui-fullscreen--click-action ui 4 1))
                   "replacing a header preserves its widget action")
      (test-assert (zerop (terminal-ui-present-records
                           ui (list (tool-outcome-test--result 4 "first"))))
                   "completed record identities deduplicate")))
  nil)

(-> test-fullscreen-annotation-restoration () null)
(defun test-fullscreen-annotation-restoration ()
  "Restore an annotation's call, deduplicate it, and preserve it through completion."
  (with-terminal-ui (ui (fullscreen-test--ui (make-instance 'recording-terminal :columns 80)))
    (let* ((origin (tool-outcome-test--call 1 "restored"))
           (entry  (append (second origin)
                           (list (terminal-span ':dim (format nil "~%operation 1: edit"))
                                 (terminal-span ':dim (format nil "~%operation 2: edit")))))
           (annotation
             (list 2 "verdict for original command"
                   (list :kind ':annotation :call-key '(:call "owner" "restored")
                         :annotations     (list (terminal-span ':annotation (format nil "~%permission verdict")))
                         :origin-function (lambda () origin)))))
      (setf (second origin) entry
            (getf (third origin) :entry-layout) (lambda (width) (declare (ignore width)) entry))
      (terminal-ui-present-records ui (list annotation))
      (test-assert (and (= 1 (fullscreen-test--chunk-count ui))
                        (search "restored" (tool-outcome-test--text ui))
                        (search "permission verdict" (tool-outcome-test--text ui)))
                   "A judgment restores its original invocation in one chunk.")
      (test-assert (zerop (terminal-ui-present-records ui (list annotation)))
                   "Repeated annotation identities do not duplicate the verdict.")
      (let ((result (tool-outcome-test--result
                     3 "restored" :state ':failure :body (list (terminal-span ':plain (format nil "~%output"))))))
        (setf (getf (third result) :entry-transform) #'application--resource-edit-failure-entry)
        (terminal-ui-present-records ui (list result)))
      (dolist (width '(40 80))
        (terminal-ui-resize ui width :rows 20)
        (let* ((text     (tool-outcome-test--text ui))
               (position (search "permission verdict" text)))
          (test-assert (and position
                            (not (search "permission verdict" text :start2 (1+ position)))
                            (search "output" text))
                       "Per-operation failure bodies and relayout retain one call-owned judgment.")))
      (terminal-ui-present-records
       ui (list (list 4 "verdict for missing command in /workspace"
                      (list :kind ':annotation :call-key '(:call "owner" "missing")
                            :origin-function (lambda () nil)))))
      (test-assert (and (= 2 (fullscreen-test--chunk-count ui))
                        (search "missing command in /workspace" (tool-outcome-test--text ui)))
                   "A missing origin displays the self-identifying fallback atomically."))))

(-> test-fullscreen-outcome-deferred-order () null)
(defun test-fullscreen-outcome-deferred-order ()
  "Publish mixed suspended output in order and associate tags with the queued chunks."
  (with-terminal-ui (ui (fullscreen-test--ui
                         (make-instance 'recording-terminal :columns 80 :rows 20)))
    (terminal-ui-present-records ui (list (tool-outcome-test--call 1 "resident")))
    (setf (terminal-ui-live-output-suspended-p ui) t)
    (terminal-ui-append-finalized ui 2 "before")
    (terminal-ui-present-records ui (list (tool-outcome-test--call 3 "queued")))
    (terminal-ui-stream-update ui :rows (list (list (terminal-span ':plain "stream"))))
    (terminal-ui-append-finalized ui 4 "after")
    (terminal-ui-present-records ui (list (tool-outcome-test--result 5 "resident")
                                         (tool-outcome-test--result 6 "queued")))
    (test-assert (and (= (fullscreen-test--chunk-count ui) 1)
                      (search "▸ (probe" (tool-outcome-test--text ui)))
                 "suspended operations do not mutate resident chunks")
    (test-assert (zerop (terminal-ui-present-records
                         ui (list (tool-outcome-test--result 6 "queued"))))
                 "queued outcomes deduplicate before publication")
    (setf (terminal-ui-live-output-suspended-p ui) nil)
    (terminal-ui--present-live ui)
    (let* ((text   (tool-outcome-test--text ui))
           (before (search "before" text))
           (call   (search "✓ (probe \"queued\")" text))
           (stream (search "stream" text))
           (after  (search "after" text)))
      (test-assert (and before call stream after (< before call stream after)
                        (search "✓ (probe \"resident\")" text))
                   "mixed output retains chronology and both invocations complete"))
    (test-assert (null (fullscreen-terminal-ui-deferred-operations ui))
                 "successful publication drains suspended operations"))
  nil)

(-> test-fullscreen-outcome-retag () null)
(defun test-fullscreen-outcome-retag ()
  "Keep replacement identity valid through chunk retags and job reassignment."
  (let ((ui (fullscreen-test--ui (make-instance 'recording-terminal :columns 80))))
    (terminal-ui-present-records ui (list (tool-outcome-test--call 1 "tagged")))
    (terminal-ui-mark-finalized ui 2)
    (terminal-ui-present-records ui (list (tool-outcome-test--result 3 "tagged")))
    (test-assert (and (= (fullscreen-test--chunk-count ui) 1)
                      (search "✓ (probe" (tool-outcome-test--text ui)))
                 "adding another finalized identity does not stale the replacement tag")
    (terminal-ui-present-records
     ui (list (tool-outcome-test--result 4 "tagged" :state ':pending :job-id "old")
              (tool-outcome-test--result 5 "tagged" :state ':pending :job-id "new")))
    (terminal-ui-present-records
     ui (list (list 6 nil (list :kind ':completion :job-key '(:job "owner" "old")
                                :state ':failure))))
    (test-assert (and (= 3 (hash-table-count (fullscreen-terminal-ui-presentation-records ui)))
                      (search "▸ (probe" (tool-outcome-test--text ui)))
                 "job reassignment drops obsolete aliases and ignores their later completion")
    (terminal-ui-present-records
     ui (list (list 7 nil (list :kind ':completion :job-key '(:job "owner" "new")
                                :state ':success))))
    (test-assert (search "✓ (probe" (tool-outcome-test--text ui))
                 "the current job completes its invocation"))
  nil)

(-> test-fullscreen-outcome-publication-retry () null)
(defun test-fullscreen-outcome-publication-retry ()
  "Restore viewport and semantic bookkeeping after a failed paint, then retry once."
  (let* ((terminal (make-instance 'failing-recording-terminal :columns 80 :rows 20))
         (ui       (fullscreen-test--ui terminal)))
    (with-terminal-ui (ui ui)
      (terminal-ui-present-records ui (list (tool-outcome-test--call 1 "retry")))
      (let ((text   (tool-outcome-test--text ui))
            (record (gethash 1 (fullscreen-terminal-ui-presentation-records ui)))
            (result (tool-outcome-test--result
                     2 "retry" :state ':failure
                     :body            (list (terminal-span ':plain (format nil "~%  │ failed"))))))
        (setf (failing-recording-terminal-fail-next-write-p terminal) t)
        (test-assert
         (handler-case (progn (terminal-ui-present-records ui (list result)) nil)
           (terminal-error () t))
         "failed outcome publication propagates the terminal condition")
        (test-assert (and (string= text (tool-outcome-test--text ui))
                          (eq record (gethash 1 (fullscreen-terminal-ui-presentation-records ui)))
                          (not (terminal-ui--finalized-p ui 2))
                          (null (fullscreen-terminal-ui-deferred-operations ui)))
                     "failure restores original spans, aliases, deduplication and pending work")
        (terminal-ui-present-records ui (list result))
        (terminal-ui-append-finalized ui 3 "later")
        (test-assert (and (= (fullscreen-test--chunk-count ui) 2)
                          (search "! (probe" (tool-outcome-test--text ui))
                          (search "later" (tool-outcome-test--text ui)))
                     "replacement and append succeed after rollback"))))
  nil)

(-> test-fullscreen-handoff-origin () null)
(defun test-fullscreen-handoff-origin ()
  "Recover evicted calls for handoffs and completions, including deferred publication."
  (with-test-configuration (configuration)
    (dolist (deferred-p '(nil t))
      (let* ((*fullscreen-transcript-character-limit* 4000)
             (conversation                            (conversation-create configuration))
             (ui                                      (fullscreen-test--ui (make-instance 'recording-terminal :columns 100 :rows 24)))
             (application                             (make-instance 'application :configuration configuration
                                                                    :conversation conversation :ui ui
                                                                    :tool-registry               (make-default-tool-registry)))
             (call                                    (conversation-append-record
                                                       conversation
                                                       (list :provider-item :wire-json
                                                             (json-encode (json-object "type" "function_call" "call_id" "origin"
                                                                                       "namespace" "shell" "name" "run"
                                                                                       "arguments" "{\"command\":\"echo original-command\"}")))))
             (handoff                                 (conversation-append-record
                                                       conversation
                                                       (list :tool-result :tool "shell.run" :call-id "origin" :status ':ok :output ""
                                                             :details '(:tool-execution :handed-off-p t :handoff-reason :grace-expired
                                                                        :job (:id "exec:origin" :state :running)))))
             (call-key                                (application--tool-outcome-key application ':call "origin"))
             (job-key                                 (application--tool-outcome-key application ':job "exec:origin")))
        (setf (application-compact-view-p application) nil)
        (with-terminal-ui (ui ui)
          (terminal-ui-present-records ui (list (application--record-presentation application call)))
          (setf (terminal-ui-live-output-suspended-p ui) deferred-p)
          (loop for identifier from 100 to 180
                do (terminal-ui-append-finalized ui identifier (make-string 80 :initial-element #\x)))
          (unless deferred-p
            (test-assert (null (gethash call-key (fullscreen-terminal-ui-presentation-records ui)))
                         "the invocation is evicted before its background result"))
          (terminal-ui-present-records ui (list (application--record-presentation application handoff)))
          (setf (terminal-ui-live-output-suspended-p ui) nil)
          (terminal-ui--present-live ui)
          (let* ((index    (fullscreen-terminal-ui-presentation-records ui))
                 (resident (gethash call-key index))
                 (viewport (fullscreen-terminal-ui-viewport ui))
                 (matching-chunks
                   (loop for offset below (clinedi:transcript-viewport-chunk-count viewport)
                         for text = (clinedi:transcript-viewport-chunk-text viewport offset)
                         when (search "original-command" text) collect text)))
            (test-assert (and resident (eq resident (gethash job-key index))
                              (eq (getf resident :state) ':pending)
                              (= (length matching-chunks) 1)
                              (search "exec:origin" (first matching-chunks)))
                         "the background note and original command share one indexed chunk")
            (test-assert (zerop (terminal-ui-present-records
                                 ui (list (application--record-presentation application call))))
                         "paging the restored invocation does not duplicate it"))
          (loop for identifier from 200 to 280
                do (terminal-ui-append-finalized ui identifier (make-string 80 :initial-element #\y)))
          (let ((completion
                  (conversation-append-record
                   conversation
                   (list :message :role ':user :content "completed" :automatic-p t
                         :job-completion '(:job-id "exec:origin" :outcome :success :summary "completed")))))
            (terminal-ui-present-records ui (list (application--record-presentation application completion)))
            (let ((resident (gethash call-key (fullscreen-terminal-ui-presentation-records ui))))
              (test-assert (and resident (eq (getf resident :state) ':success)
                                (search "original-command" (tool-outcome-test--text ui))
                                (member (application--record-presentation-identifier application completion)
                                        (getf resident :identifiers) :test #'equal))
                           "a completion recovers its durable invocation after another eviction")))))))
  nil)

(-> test-fullscreen-outcome-eviction-and-reset () null)
(defun test-fullscreen-outcome-eviction-and-reset ()
  "Forget semantic aliases and associated result identities at the residency boundary."
  (let* ((*fullscreen-transcript-character-limit* 1000)
         (ui                                      (fullscreen-test--ui (make-instance 'recording-terminal :columns 80))))
    (terminal-ui-present-records
     ui (list (tool-outcome-test--call '(:conversation "owner" 1) "old")
              (tool-outcome-test--result '(:conversation "owner" 2) "old" :job-id "job")))
    (loop for sequence from 3 to 40
          do (terminal-ui-append-finalized ui (list :conversation "owner" sequence)
                                           (make-string 80 :initial-element #\x)))
    (test-assert (and (zerop (hash-table-count (fullscreen-terminal-ui-presentation-records ui)))
                      (not (terminal-ui--finalized-p ui '(:conversation "owner" 1)))
                      (not (terminal-ui--finalized-p ui '(:conversation "owner" 2)))
                      (>= (terminal-ui-take-evicted-sequence ui "owner") 2))
                 "eviction removes call and job aliases together with their outcomes")
    (terminal-ui-present-records ui (list (tool-outcome-test--call 100 "new")))
    (setf (terminal-ui-live-output-suspended-p ui) t)
    (terminal-ui-present-records ui (list (tool-outcome-test--result 101 "new")))
    (terminal-ui-reset-transcript ui)
    (test-assert (and (zerop (fullscreen-test--chunk-count ui))
                      (zerop (hash-table-count (fullscreen-terminal-ui-presentation-records ui)))
                      (null (fullscreen-terminal-ui-deferred-operations ui)))
                 "reset clears resident aliases and queued outcomes"))
  nil)

(-> test-transcript-entry-semantics () null)
(defun test-transcript-entry-semantics ()
  "Use explicit groups regardless of glyphs, styles, or widget representation."
  (dolist (fullscreen-p '(nil t))
    (let* ((terminal (make-instance 'recording-terminal :columns 100 :rows 24))
           (ui       (terminal-ui-create :terminal terminal :fullscreen-p fullscreen-p))
           (entries  (list
                      (list 1 (list (terminal-span ':failure "first"))
                            '(:entry-kind :tool-call :group "same"))
                      (list 2 (list (termdown:make-widget ':notice "second" '(:copy "value")))
                            '(:entry-kind :tool-call :group "same"))
                      (list 3 (list (terminal-span ':tool "▸ (fake.call)"))
                            '(:entry-kind :message :group nil)))))
      (with-terminal-ui (ui ui)
        (recording-terminal-reset terminal)
        (setf (terminal-interactive-p terminal) nil)
        (terminal-ui-present-records ui entries)
        (let ((text (if fullscreen-p (tool-outcome-test--text ui)
                        (clinedi:ansi-strip (recording-terminal-output terminal)))))
          (test-assert (string= text (format nil "first~%second~%~%▸ (fake.call)~%~%"))
                       "explicit groups, including NIL, determine transcript spacing"))
        (when fullscreen-p
          (test-assert (equal '(:copy "value")
                              (terminal-ui-fullscreen--click-action ui 1 2))
                       "semantic grouping preserves widget actions"))
        (let ((entry (list (terminal-span ':success "✓ qualified.tool"))))
          (test-assert
           (and (string= (terminal-ui--finalized-content ui entry :group nil)
                         (format nil "✓ qualified.tool~%~%"))
                (string= (terminal-ui--finalized-content ui entry :group ':success)
                         (format nil "✓ qualified.tool~%")))
           "explicit NIL is a block even when the entry looks compact")))))
  (dolist (group '(nil "probe" :call))
    (let* ((ui   (fullscreen-test--ui (make-instance 'recording-terminal :columns 100)))
           (call (tool-outcome-test--call 2 "spacing"))
           (key  '(:call "owner" "spacing")))
      (setf (getf (third call) :group) group)
      (terminal-ui-present-records
       ui (list (list 1 "before" '(:entry-kind :message :group "before")) call))
      (terminal-ui-present-records
       ui (list (tool-outcome-test--result 3 "spacing" :state ':pending :job-id "job")))
      (terminal-ui-present-records
       ui (list (list 4 nil (list :kind ':completion :job-key '(:job "owner" "job")
                                 :state ':failure :retain-output-p t
                                 :body (list (terminal-span ':plain (format nil "~%  │ completed")))))))
      (let ((resident (gethash key (fullscreen-terminal-ui-presentation-records ui))))
        (test-assert (and (equal group (getf resident :group))
                          (eq (getf resident :entry-kind) ':tool-call)
                          (eq (getf resident :state) ':failure)
                          (string= (getf resident :prefix) (string #\Newline))
                          (uiop:string-suffix-p
                           (tool-outcome-test--text ui)
                           (format nil "  │ completed~%~:[~%~;~]" group)))
                     "job completion preserves the invocation's group, prefix, and entry kind"))))
  nil)

(-> test-fullscreen-origin-reverse-search () null)
(defun test-fullscreen-origin-reverse-search ()
  "Recover nearby origins without reading older segments, and exhaust genuine misses."
  (with-test-configuration (configuration)
    (let* ((application  (application-tool-outcome-tests--application configuration))
           (conversation (application-conversation application)))
      (dotimes (index 2)
        (conversation-append-record
         conversation (list :message :role ':user :content (format nil "old segment ~D" index)))
        (conversation-append-summary conversation (make-continuation :intent "older checkpoint")))
      (let* ((call        (application-tool-outcome-history-tests--append
                           conversation (application-tool-outcome-tests--call 0 "nearby")))
             (handoff     (application-tool-outcome-tests--result 0 "nearby"))
             (active      (conversation-storage-active-pathname (conversation-pathname conversation)))
             (segment-map (fdefinition 'sexp-store:segment-map))
             (reads       0))
        (setf (getf (rest handoff) :details)
              '(:tool-execution :handed-off-p t :job (:id "exec:nearby")))
        (setf handoff (application-tool-outcome-history-tests--append conversation handoff))
        (let ((completion (application-tool-outcome-history-tests--append
                           conversation (application-tool-outcome-tests--completion
                                         0 "exec:nearby" :outcome ':failure))))
          (test-call-with-function-replacements
           (list (list 'conversation-map-records
                       (lambda (&rest arguments)
                         (declare (ignore arguments))
                         (error "Origin lookup used forward lifetime mapping.")))
                 (list 'conversation--map-storage-records
                       (lambda (&rest arguments)
                         (declare (ignore arguments))
                         (error "Origin lookup used forward storage mapping.")))
                 (list 'sexp-store:segment-map
                       (lambda (function segment &rest arguments)
                         (test-assert (equal segment active)
                                      "a nearby origin does not open archived segments")
                         (incf reads)
                         (apply segment-map function segment arguments))))
           (lambda ()
             (dolist (outcome (list handoff completion))
               (let* ((presentation (application--record-presentation application outcome))
                      (origin       (funcall (getf (third presentation) :origin-function))))
                 (test-assert (equal (first origin)
                                     (application--record-presentation-identifier application call))
                              "result and job completion recover the same durable invocation")))
             (test-assert (= reads 2)
                          "each origin lookup visits its newest segment only once"))))
        (dolist (kind '(:call :job))
          (test-assert
           (null (application--tool-outcome-origin
                  application
                  (list (if (eq kind ':call) ':call-key ':job-key)
                        (application--tool-outcome-key application kind "missing"))))
           "an absent origin returns NIL after searching archived segments")))))
  nil)

(-> test-fullscreen-origin-chronology () null)
(defun test-fullscreen-origin-chronology ()
  "Resolve reused call and job identifiers at the outcome's historical position."
  (with-test-configuration (configuration)
    (let* ((application  (application-tool-outcome-tests--application configuration))
           (conversation (application-conversation application))
           (pairs        nil))
      (dotimes (index 2)
        (let* ((call    (application-tool-outcome-history-tests--append
                         conversation (application-tool-outcome-tests--call 0 "reused")))
               (handoff (application-tool-outcome-tests--result 0 "reused")))
          (setf (getf (rest handoff) :details)
                '(:tool-execution :handed-off-p t :job (:id "exec:0")))
          (setf handoff (application-tool-outcome-history-tests--append conversation handoff))
          ;; This reused call follows the handoff and cannot be its origin.
          (application-tool-outcome-history-tests--append
           conversation (application-tool-outcome-tests--call 0 "reused"))
          (let ((completion (application-tool-outcome-history-tests--append
                             conversation (application-tool-outcome-tests--completion
                                           0 "exec:0" :outcome ':failure))))
            (push (list call handoff completion) pairs)))
        (conversation-append-summary conversation (make-continuation :intent "reused identifiers")))
      (dolist (pair pairs)
        (dolist (outcome (rest pair))
          (let* ((presentation (application--record-presentation application outcome))
                 (origin       (funcall (getf (third presentation) :origin-function))))
            (test-assert
             (equal (first origin) (application--record-presentation-identifier application (first pair)))
             "historical outcomes cannot attach to a later reused call or job identifier"))))))
  nil)

(-> test-tool-outcome-selection-metadata () null)
(defun test-tool-outcome-selection-metadata ()
  "Select lifecycle metadata without decoding a supplied item or formatting failure bodies."
  (with-test-configuration (configuration)
    (let* ((application (application-tool-outcome-tests--application configuration))
           (call        (application-tool-outcome-tests--call 1 "raw"))
           (item        (json-decode (getf (rest call) :wire-json)))
           (completion  (application-tool-outcome-tests--completion 2 "exec:raw" :outcome ':failure)))
      (test-call-with-function-replacements
       (list (list 'json-decode
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (error "Selection decoded a supplied provider item.")))
             (list 'application--failure-result-rows
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (error "Selection formatted an unselected failure body."))))
       (lambda ()
         (let ((metadata (application--record-tool-outcome-basic
                          application call nil :item item :metadata-only-p t)))
           (test-assert (equal (getf metadata :call-key)
                               (application--tool-outcome-key application ':call "raw"))
                        "raw provider metadata uses the decoded item"))
         (let ((metadata (application--record-tool-outcome-basic
                          application completion nil :metadata-only-p t)))
           (test-assert (and (eq (getf metadata :state) ':failure)
                             (null (getf metadata :body)))
                        "raw completion metadata retains lifecycle state without a body"))))))
  nil)
