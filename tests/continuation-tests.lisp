(in-package #:autolith)

;;;; -- Continuation Checkpoint Tests --

(-> continuation-tests--checkpoint (&rest t) list)
(defun continuation-tests--checkpoint (&rest replacements)
  "Return a valid checkpoint citing one record, with REPLACEMENTS for its properties."
  (let ((checkpoint
          (make-continuation
           :intent     "Ship the parser."
           :notes      (list (list :kind ':completed :text "Tests pass." :refs '("r1"))
                             (list :kind ':completed :text "Docs are updated." :refs nil)
                             (list :kind ':open :text "Tag the release." :refs nil))
           :next       (list :text "Tag it." :refs '("r1"))
           :references (list (list :handle "r1" :kind ':tool-result :label "A test run."
                                   :conversation "elsewhere" :sequence 3)))))
    (loop for (key value) on replacements by (partial #'nthcdr 2)
          do (setf (getf checkpoint key) value))
    checkpoint))

(-> test-continuation-validity () null)
(defun test-continuation-validity ()
  "Accept only well-formed checkpoints that keep what they cite and stay within their bounds."
  (test-assert (continuation-p (continuation-tests--checkpoint))
               "a checkpoint that keeps the reference it cites is valid")
  (test-assert (continuation-p (make-continuation :unsummarized '(("elsewhere" 1 9))))
               "a checkpoint that summarizes nothing needs no intent")
  (dolist (invalid
            (list (continuation-tests--checkpoint :format 0)
                  (continuation-tests--checkpoint :intent "")
                  (continuation-tests--checkpoint :intent nil)
                  (continuation-tests--checkpoint
                   :notes (list (list :kind ':done :text "Tests pass." :refs nil)))
                  (continuation-tests--checkpoint
                   :notes (list (list :kind ':open :text "" :refs nil)))
                  (continuation-tests--checkpoint :references nil)
                  (continuation-tests--checkpoint :unsummarized '(("elsewhere" 0 9)))
                  (continuation-tests--checkpoint
                   :references (list (list :handle "r1" :kind ':tool-result
                                           :label  (make-string *continuation-maximum-octets*
                                                                :initial-element #\x))))
                  (append (continuation-tests--checkpoint) (list :verified t))
                  "Ship the parser."))
    (test-assert (not (continuation-p invalid))
                 "a malformed, unbounded or dangling checkpoint is rejected"))
  nil)

(-> test-continuation-rendering () null)
(defun test-continuation-rendering ()
  "Render what a checkpoint cites, what it leaves unsubstantiated and what it did not summarize."
  (let* ((checkpoint (continuation-tests--checkpoint
                      :unsummarized '(("elsewhere" 5 9))
                      :references   (list (list :handle "r1" :kind ':plan-step
                                                :label "plan step \"Tag\"" :observed "doing"))))
         (text       (continuation-render checkpoint)))
    (dolist (expected '("Tests pass. [r1]"
                        "Docs are updated. (unsubstantiated)"
                        "r1: plan step \"Tag\" (doing)"
                        "Records 5 to 9 of conversation elsewhere"))
      (test-assert (search expected text)
                   "the text shows citations, unsupported completions, references and gaps"))
    (test-assert (not (search "Tag the release. (unsubstantiated)" text))
                 "only a completed note needs a reference")
    (test-assert (string= text (continuation-render
                                (with-standard-io-syntax
                                  (read-from-string (prin1-to-string checkpoint)))))
                 "a stored checkpoint renders the same text again"))
  nil)
