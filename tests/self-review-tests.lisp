(in-package #:autolith)

;;;; -- Periodic Self-Modification Review Tests --

(-> self-review-tests--request
    (configuration conversation &key (:compaction-p boolean))
    request-context)
(defun self-review-tests--request
    (configuration conversation &key compaction-p)
  "Return one request snapshot for the review gate tests."
  (make-instance 'request-context
                 :configuration configuration
                 :conversation conversation
                 :tool-namespaces #()
                 :compaction-p compaction-p))

(-> test-self-review-reminder () null)
(defun test-self-review-reminder ()
  "Test the turn and working-time gates of the periodic review reminder."
  (let* ((configuration (test-configuration))
         (root (test-configuration-root configuration))
         (*self-review-receipts* (make-hash-table :test #'equal))
         (*context-contributors* nil)
         (*context-next-request-delivered* (make-hash-table :test #'equal))
         (*context-last-deliveries* (make-hash-table :test #'equal))
         (*context-last-delivery-order* nil))
    (unwind-protect
         (let ((conversation (conversation-create configuration
                                                  :identifier "review")))
           (register-context-contributor "self-improvement-review"
                                         'self-review-context
                                         :source ':built-in)
           (dolist (case '((0 0 nil "a fresh conversation")
                           (9 3600 nil "too few user turns")
                           (10 599 nil "too little working time")
                           (10 600 t "both gates passed")))
             (destructuring-bind (turns worked expected description)
                 case
               (let ((*self-review-receipts*
                       (make-hash-table :test #'equal)))
                 (setf (conversation-user-turn-count conversation) turns
                       (conversation-working-seconds conversation) worked)
                 (test-assert
                  (eq (not (null (self-review-context
                                  (self-review-tests--request configuration
                                                              conversation))))
                      expected)
                  (format nil "review gates: ~A" description)))))
           (setf (conversation-user-turn-count conversation) 10
                 (conversation-working-seconds conversation) 700)
           (test-assert
            (not (null (self-review-context
                        (self-review-tests--request configuration
                                                    conversation))))
            "the reminder fires when both gates pass")
           (test-assert
            (not (null (self-review-context
                        (self-review-tests--request configuration
                                                    conversation))))
            "the reminder repeats through its whole trigger turn")
           (setf (conversation-user-turn-count conversation) 11)
           (test-assert
            (null (self-review-context
                   (self-review-tests--request configuration conversation)))
            "the reminder stays silent after its trigger turn")
           (setf (conversation-user-turn-count conversation) 22
                 (conversation-working-seconds conversation) 1200)
           (test-assert
            (null (self-review-context
                   (self-review-tests--request configuration conversation)))
            "a repeat reminder requires more working time since the last one")
           (setf (conversation-working-seconds conversation) 1300)
           (test-assert
            (not (null (self-review-context
                        (self-review-tests--request configuration
                                                    conversation))))
            "a repeat reminder fires once both period gates pass again")
           (setf (conversation-user-turn-count conversation) 34
                 (conversation-working-seconds conversation) 2000)
           (test-assert
            (null (self-review-context
                   (self-review-tests--request configuration
                                               conversation
                                               :compaction-p t)))
            "a compaction request never carries the reminder")
           (let ((*self-review-enabled-p* nil))
             (test-assert
              (null (self-review-context
                     (self-review-tests--request configuration conversation)))
              "the disabled reminder stays silent"))
           (let ((immutable
                   (configuration-create
                    :source-root (configuration-source-root configuration)
                    :working-directory (configuration-working-directory
                                        configuration)
                    :immutable-p t)))
             (test-assert
              (null (self-review-context
                     (self-review-tests--request immutable conversation)))
              "an immutable session never carries the reminder"))
           (setf (conversation-user-turn-count conversation) 46
                 (conversation-working-seconds conversation) 2600)
           (let ((delivery
                   (context-resolve-request configuration conversation #())))
             (test-assert
              (not
               (null
                (find "self-improvement-review"
                      (context-delivery-contributions delivery)
                      :key #'context-contribution-identifier
                      :test #'string=)))
              "a triggered reminder reaches the request context")))
      (platform-delete-directory-tree *platform* root :validate t :if-does-not-exist ':ignore)))
  nil)
