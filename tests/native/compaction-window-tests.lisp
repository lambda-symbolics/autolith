(in-package #:autolith)

;;;; -- Stored Native Compaction Windows --

(-> native-compaction-tests--opaque (string) json-object)
(defun native-compaction-tests--opaque (content)
  "Return a native item containing CONTENT as opaque provider state."
  (json-object "type" "compaction" "encrypted_content" content))

(-> test-native-compaction-window-replay () null)
(defun test-native-compaction-window-replay ()
  "Replay stored native windows and project checkpoints across provider families."
  (with-test-configuration (configuration)
    (let* ((conversation (conversation-create configuration))
           (window       (list (native-compaction-tests--opaque "first")
                               (json-object "type" "reasoning" "summary" (json-array)
                                            "encrypted_content" "retained-reasoning")
                               (native-compaction-tests--opaque "last"))))
      (conversation-append-user-message conversation "original")
      (let ((loaded (test-conversation-with-native-window
                     configuration conversation window
                     :family ':responses :continuation (make-continuation :intent "portable handoff"))))
        (dolist (family '(:responses :messages))
          (let ((items (conversation-input-items-for-family loaded family)))
            (test-assert
             (and (test-object-contains-string-p (last items) "portable handoff")
                  (equal (mapcar #'json-encode (butlast items))
                         (and (eq family ':responses) (mapcar #'json-encode window))))
             "the source family receives the full ordered window before the checkpoint and other families only the checkpoint")
            (test-assert (= (if (eq family ':responses) 2 0)
                            (count-if #'native-compaction-item-p items))
                         "all opaque items belong exclusively to their originating family"))))
      (let ((legacy (conversation-create configuration)))
        (conversation-append-record
         legacy (list :native-compaction :family ':responses
                      :through-seq       (1- (conversation-next-sequence legacy))
                      :wire-json         (json-encode (native-compaction-tests--opaque "legacy"))
                      :summary           "legacy handoff"))
        (let ((loaded (conversation-load-by-id configuration (conversation-identifier legacy))))
          (test-assert (equal '(1 0)
                              (mapcar (lambda (family)
                                        (count-if #'native-compaction-item-p
                                                  (conversation-input-items-for-family
                                                   loaded family)))
                                      '(:responses :messages)))
                       "legacy one-object archives are readable")
          (test-assert (equal '(nil t)
                              (mapcar (lambda (family)
                                        (test-object-contains-string-p
                                         (conversation-input-items-for-family loaded family)
                                         "legacy handoff"))
                                      '(:responses :messages)))
                       "a legacy text handoff stays hidden from the family that owns the window")))))
  nil)
