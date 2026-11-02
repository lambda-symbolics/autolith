(in-package #:autolith)

;;;; -- Application Tool-Outcome History --

(-> application-tool-outcome-history-tests--append (conversation list) list)
(defun application-tool-outcome-history-tests--append (conversation record)
  "Append RECORD without fixture-owned sequence or timestamp properties."
  (conversation-append-record
   conversation
   (cons (first record)
         (loop for (key value) on (rest record) by (lambda (tail) (rest (rest tail)))
              unless (member key '(:seq :time))
                append (list key value)))))

(-> test-application-tool-outcomes-history-replay () null)
(defun test-application-tool-outcomes-history-replay ()
  "Page an original invocation after its later result or completion was displayed."
  (dolist (compact-p '(t nil))
    (dolist (mode '(:success :failure :job-success :job-failure))
      (with-test-configuration (configuration)
        (let* ((application  (application-tool-outcome-tests--application
                              configuration :compact-view-p compact-p))
               (conversation (application-conversation application))
               (job-p        (member mode '(:job-success :job-failure)))
               (success-p    (member mode '(:success :job-success)))
               (result       (application-tool-outcome-tests--result
                              0 "old" :status (if success-p ':ok ':error) :output "history body")))
          (application-tool-outcome-history-tests--append
           conversation (application-tool-outcome-tests--call 0 "old"))
          (when job-p
            (setf (getf (rest result) :details)
                  (list :tool-execution :handed-off-p t :job (list :id "history-job"))))
          (setf result (application-tool-outcome-history-tests--append conversation result))
          (let* ((later                           (if job-p
                                                      (application-tool-outcome-history-tests--append
                                                       conversation (application-tool-outcome-tests--completion
                                                                     0 "history-job" :outcome (if success-p ':success ':failure)
                                                                                     :summary "history body"))
                                                      result))
                 (*application-history-page-size* (if job-p 2 1)))
            (application-tool-outcome-tests--present application later)
            (setf (application-rendered-sequence application) (application--record-sequence later)
                  (application-history-floor-sequence application) (application--record-sequence later))
            (application-render-history application)
            (let* ((viewport (fullscreen-terminal-ui-viewport (application-ui application)))
                   (call     (loop for index below (clinedi:transcript-viewport-chunk-count viewport)
                                   for text = (clinedi:transcript-viewport-chunk-text viewport index)
                                   when (search "workspace:x" text) return text)))
              (test-assert
               (and call (search (if success-p "✓ (resource.read :uri" "! (resource.read :uri") call)
                    (eql (or (not compact-p) (not success-p))
                         (not (null (search "history body" call)))))
               "paging resolves the original call and retains the appropriate body")))))))
  nil)

(-> test-application-tool-outcomes-history-bounded () null)
(defun test-application-tool-outcomes-history-bounded ()
  "Keep only selected call outcomes while scanning a long conversation."
  (with-test-configuration (configuration)
    (let ((application (application-tool-outcome-tests--application configuration
                                                                    :identifier "history-bounded")))
      (dotimes (index 20)
        (application-tool-outcome-history-tests--append
         (application-conversation application)
         (application-tool-outcome-tests--call (* 2 index) (format nil "call-~D" index)))
        (application-tool-outcome-history-tests--append
         (application-conversation application)
         (application-tool-outcome-tests--result (1+ (* 2 index))
                                                 (format nil "call-~D" index))))
      (multiple-value-bind (records count outcomes)
          (application--history-page-records application most-positive-fixnum 3)
        (test-assert (= 3 (length records)) "history page remains bounded")
        (test-assert (= 3 (hash-table-count outcomes))
                     "history outcomes remain bounded to selected calls")
        (test-assert (= 20 count) "hidden lifecycle records do not change candidates")))))

(-> test-application-tool-outcomes-startup-history () null)
(defun test-application-tool-outcomes-startup-history ()
  "Fill startup pages with visible calls while applying hidden successful outcomes."
  (with-test-configuration (configuration)
    (let* ((application                     (application-tool-outcome-tests--application configuration))
           (*application-history-page-size* 2))
      (dotimes (index 3)
        (let ((call-id (format nil "startup-~D" index)))
          (application-tool-outcome-history-tests--append
           (application-conversation application) (application-tool-outcome-tests--call 0 call-id))
          (application-tool-outcome-history-tests--append
           (application-conversation application) (application-tool-outcome-tests--result 0 call-id))))
      (application-render-records application)
      (let ((text (application-tool-outcome-tests--text application)))
        (test-assert (= 2 (terminal-tests--substring-count "✓ (resource.read :uri" text))
                     "hidden compact results do not crowd visible startup calls out of the page")
        (test-assert (= 6 (application-rendered-sequence application))
                     "startup advances past hidden results"))))
  (dolist (compact-p '(t nil))
    (dolist (mode '(:success :failure :job-success :job-failure))
      (with-test-configuration (configuration)
        (let* ((application                     (application-tool-outcome-tests--application
                                                 configuration :compact-view-p compact-p))
               (conversation                    (application-conversation application))
               (*application-history-page-size* 6)
               (job-p                           (member mode '(:job-success :job-failure)))
               (success-p                       (member mode '(:success :job-success))))
          (application-tool-outcome-history-tests--append
           conversation (application-tool-outcome-tests--call 0 "older"))
          (when job-p
            (let ((handoff (application-tool-outcome-tests--result 0 "older")))
              (setf (getf (rest handoff) :details)
                    (list :tool-execution :handed-off-p t :job (list :id "rotated-job")))
              (application-tool-outcome-history-tests--append conversation handoff)))
          (conversation-append-summary conversation "rotation checkpoint")
          (application-tool-outcome-history-tests--append
           conversation
           (if job-p
               (application-tool-outcome-tests--completion
                0 "rotated-job" :outcome (if success-p ':success ':failure)
                                :summary "cross-segment body")
               (application-tool-outcome-tests--result
                0 "older" :status (if success-p ':ok ':error) :output "cross-segment body")))
          (application-tool-outcome-history-tests--append
           conversation (application-tool-outcome-tests--call 0 "newer"))
          (application-tool-outcome-history-tests--append
           conversation (application-tool-outcome-tests--result 0 "newer"))
          (application-render-records application)
          (let* ((viewport (fullscreen-terminal-ui-viewport (application-ui application)))
                 (older    (clinedi:transcript-viewport-chunk-text viewport 0))
                 (text     (application-tool-outcome-tests--text application)))
            (test-assert
             (and (search (if success-p "✓ (resource.read :uri" "! (resource.read :uri") older)
                  (search "workspace:x" older)
                  (eql (or (not compact-p) (not success-p))
                       (not (null (search "cross-segment body" older)))))
             "startup resolves a selected older call from the active segment")
            (test-assert
             (= (if success-p 2 1)
                (terminal-tests--substring-count "✓ (resource.read :uri" text))
             "the active segment's selected call also receives its outcome")
            (test-assert
             (= (1- (conversation-next-sequence conversation))
                (application-rendered-sequence application))
             "startup retains the active scan cursor across segment outcome resolution"))))))
  nil)

(-> test-application-tool-outcomes-history-job-alias () null)
(defun test-application-tool-outcomes-history-job-alias ()
  "Ignore obsolete background-job completions while replaying a retagged call."
  (dolist (startup-p '(t nil))
    (with-test-configuration (configuration)
      (let* ((application                     (application-tool-outcome-tests--application configuration))
             (conversation                    (application-conversation application))
             (*application-history-page-size* 6))
        (application-tool-outcome-history-tests--append
         conversation (application-tool-outcome-tests--call 0 "retagged"))
        (dolist (job-id '("old" "new"))
          (let ((handoff (application-tool-outcome-tests--result 0 "retagged")))
            (setf (getf (rest handoff) :details)
                  (list :tool-execution :handed-off-p t :job (list :id job-id)))
            (application-tool-outcome-history-tests--append conversation handoff)))
        (when startup-p
          (conversation-append-summary conversation "rotation checkpoint"))
        (let ((obsolete (application-tool-outcome-history-tests--append
                         conversation (application-tool-outcome-tests--completion
                                       0 "old" :outcome ':failure))))
          (if startup-p
              (application-render-records application)
              (progn
                (application-tool-outcome-tests--present application obsolete)
                (setf (application-rendered-sequence application)
                      (application--record-sequence obsolete)
                      (application-history-floor-sequence application)
                      (application--record-sequence obsolete))
                (application-render-history application))))
        (let* ((viewport (fullscreen-terminal-ui-viewport (application-ui application)))
               (call     (loop for index below (clinedi:transcript-viewport-chunk-count viewport)
                               for text = (clinedi:transcript-viewport-chunk-text viewport index)
                               when (search "workspace:x" text) return text)))
          (test-assert (and call (search "▸ (resource.read :uri" call))
                       "an obsolete completion cannot resolve the selected call's current job")))))
  nil)
