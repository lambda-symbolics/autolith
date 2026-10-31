(in-package #:autolith)

;;;; -- Retained Working Sets --

;;; Long sessions keep bounded working sets resident and read the rest from
;;; disk. This report shows what each set holds, so growth is visible.

(-> application-retention (application) list)
(defun application-retention (application)
  "Return the resident counts and character weights of APPLICATION's bounded working sets.

The plist holds :TRANSCRIPT-CHUNKS, :TRANSCRIPT-CHARACTERS and
:TRANSCRIPT-EVICTED, all NIL without a fullscreen transcript;
:FINALIZED-IDENTIFIERS; :INPUT-RECALL-ENTRIES and :INPUT-RECALL-CHARACTERS;
:TRACE-CONVERSATIONS and :TRACE-CHARACTERS; :KNOWLEDGE-CLAIMS,
:KNOWLEDGE-SUPPORTS, :KNOWLEDGE-EVIDENCE and :KNOWLEDGE-ARCHIVED, the retracted
supports kept only in the log, all NIL when the knowledge log cannot be read; and :HEAP-BYTES, the dynamic space in use."
  (let* ((ui       (application-ui application))
         (viewport (and (typep ui 'fullscreen-terminal-ui)
                        (fullscreen-terminal-ui-viewport ui)))
         (history  (and ui (line-editor-history (terminal-ui-editor ui))))
         (base     (handler-case (knowledge-base-read (application-configuration application))
                     (knowledge-error ()
                       nil))))
    (list :transcript-chunks       (and viewport (clinedi:transcript-viewport-chunk-count viewport))
          :transcript-characters   (and viewport
                                        (clinedi:transcript-viewport-resident-characters viewport))
          :transcript-evicted      (and viewport (clinedi:transcript-viewport-evicted-count viewport))
          :finalized-identifiers   (if ui (hash-table-count (terminal-ui-finalized-identifiers ui)) 0)
          :input-recall-entries    (length history)
          :input-recall-characters (reduce #'+ history :key #'length)
          :trace-conversations     (hash-table-count *action-trace-cache*)
          :trace-characters        (loop for trace-history being the hash-values of *action-trace-cache*
                                         sum (action-trace--history-weight trace-history))
          :knowledge-claims        (and base (hash-table-count (knowledge-base-claims base)))
          :knowledge-supports      (and base (hash-table-count (knowledge-base-supports base)))
          :knowledge-evidence      (and base (hash-table-count (knowledge-base-evidence base)))
          :knowledge-archived      (and base (knowledge-base-archived-supports base))
          :heap-bytes              (sb-kernel:dynamic-usage))))

(-> application--retention-description (application) string)
(defun application--retention-description (application)
  "Return APPLICATION's retained working sets as one compact status line."
  (let ((retention (application-retention application)))
    (flet ((quantity (key)
             (application--token-count-description (getf retention key))))
      (format nil "~{~A~^; ~}"
              (remove nil
                      (list (format nil "heap ~D MB"
                                    (round (getf retention :heap-bytes) (* 1024 1024)))
                            (and (getf retention :transcript-characters)
                                 (format nil "transcript ~A chars~[~:;, ~:*~D chunks evicted~]"
                                         (quantity :transcript-characters)
                                         (getf retention :transcript-evicted)))
                            (format nil "input recall ~A entries" (quantity :input-recall-entries))
                            (format nil "traces ~A chars" (quantity :trace-characters))
                            (if (getf retention :knowledge-claims)
                                (format nil "knowledge ~A claims" (quantity :knowledge-claims))
                                "knowledge unreadable")))))))
