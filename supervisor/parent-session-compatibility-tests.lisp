;;;; -- Parent Session Migration and Historical Fork Checks --

(in-package #:autolith-supervisor)

(serapeum/types:-> parent-session-test--cleanup (pathname) null)
(defun parent-session-test--cleanup (root)
  "Remove the ordinary files and directories owned by one session fixture."
  (when (uiop:directory-exists-p root)
    (dolist (file (uiop:directory-files root)) (delete-file file))
    (dolist (directory (uiop:subdirectories root)) (parent-session-test--cleanup directory))
    (uiop:delete-empty-directory root))
  nil)

(serapeum/types:-> parent-session-test--write (pathname list &key (:tail (or null string))) pathname)
(defun parent-session-test--write (path forms &key tail)
  "Write portable legacy fixture forms, optionally followed by a torn final form."
  (ensure-directories-exist path)
  (with-open-file (stream path :direction ':output :if-exists ':supersede)
    (let ((*print-readably* t) (*print-pretty* nil))
      (dolist (form forms) (write form :stream stream) (terpri stream)))
    (when tail (write-string tail stream)))
  path)

(serapeum/types:-> parent-session-test--rejected-p (function keyword) boolean)
(defun parent-session-test--rejected-p (function code)
  "Check that malformed private data receives the expected fixed failure category."
  (handler-case (progn (funcall function) nil)
    (parent-session-compatibility-error (condition)
      (eq (parent-session-compatibility-error-code condition) code))))

(serapeum/types:-> run-parent-session-compatibility-reader-tests () integer)
(defun run-parent-session-compatibility-reader-tests ()
  "Exercise the real inert legacy reader, chunk continuity, bounds, and crash tails."
  (let* ((root    (merge-pathnames (format nil "session-reader-~A/" (symbol-name (gensym)))
                                   (uiop:temporary-directory)))
         (flat    (merge-pathnames "flat.sexp" root))
         (message '(:message :seq 1 :time 42 :role :user :content "hello"))
         (answer  '(:message :seq 2 :time 43 :role :assistant :content "answer")))
    (unwind-protect
         (progn
           (parent-session-test--write flat
                                       (list '(:conversation :version 1 :id "flat" :created-at 42)
                                             message answer)
                                       :tail "(:message :seq 3 :content \"unfinished")
           (let ((before (uiop:read-file-string flat)))
             (multiple-value-bind (header records incomplete) (parent-session-read-legacy root "flat")
               (assert (eq (first header) ':conversation))
               (assert (equal records (list message answer)))
               (assert incomplete))
             (assert (equal before (uiop:read-file-string flat))))
           (dolist (value (list nil t 42 1.25d0 "text" :keyword '(1 :two "three") #(1 :two "three")))
             (assert (equalp value (parent-session--untagged (parent-session--tagged value)))))
           (dolist (text '("#.(error \"must not execute\")" "#1=(:message :x #1#)"
                           "#S(secret :value 1)" "(:message :value CL:OPEN)"
                           "(:message :value '(:foo))"))
             (assert (parent-session-test--rejected-p
                      (lambda () (parent-session--portable-input text)) ':invalid-reader-syntax)))
           (assert (parent-session-test--rejected-p
                    (lambda () (parent-session--portable-input (make-string 33 :initial-element (code-char 40))))
                    ':record-limit))
           (let ((first  (merge-pathnames "chunks/00000000000000000001.sexp" root))
                 (second (merge-pathnames "chunks/00000000000000000002.sexp" root)))
             (parent-session-test--write first
                                         (list '(:conversation :version 2 :id "chunks" :chunk-start-sequence 1)
                                               message))
             (parent-session-test--write second
                                         (list '(:conversation :version 2 :id "chunks" :chunk-start-sequence 2)
                                               answer))
             (assert (equal (nth-value 1 (parent-session-read-legacy root "chunks"))
                            (list message answer)))
             (parent-session-test--write second
                                         (list '(:conversation :version 2 :id "chunks" :chunk-start-sequence 4)
                                               '(:message :seq 4 :role :assistant :content "gap")))
             (assert (parent-session-test--rejected-p
                      (lambda () (parent-session-read-legacy root "chunks")) ':invalid-segment-header))
             (parent-session-test--write first
                                         (list '(:conversation :version 2 :id "chunks" :chunk-start-sequence 1)
                                               message) :tail "(:message")
             (assert (parent-session-test--rejected-p
                      (lambda () (parent-session-read-legacy root "chunks")) ':incomplete-interior-segment)))
           (parent-session-test--write (merge-pathnames "Aabcdef.sexp" root)
                                       (list '(:conversation :version 1 :id "Aabcdef") message))
           (assert (= 1 (length (nth-value 1 (parent-session-read-legacy root "A-abcdef")))))
           (dolist (identifier '("../escape" ".." "x/y" "x\\y" "*"))
             (assert (parent-session-test--rejected-p
                      (lambda () (parent-session--component identifier)) ':invalid-session-id)))
           21)
      (parent-session-test--cleanup root))))

(serapeum/types:-> run-parent-session-compatibility-tests () integer)
(defun run-parent-session-compatibility-tests ()
  "Exercise migration, real replay content, fork prefixes and read-only recovery."
  (run-parent-session-compatibility-reader-tests)
  (let* ((root   (merge-pathnames (format nil "session-migration-~A/" (parent-application--identifier))
                                  (uiop:temporary-directory)))
         (legacy (merge-pathnames "legacy/" root))
         (configuration
           (make-instance 'parent-configuration
                          :values (list :private-root              (namestring (merge-pathnames "private/" root))
                                        :legacy-conversations-root (namestring legacy))))
         (path   (merge-pathnames "source.sexp" legacy))
         (forms  '((:conversation :version 1 :id "source" :created-at 42 :title "History")
                   (:message :seq 1 :time 42 :role :user :content "owner input")
                   (:tool-call :seq 2 :time 43 :call-id "call" :tool "shell.run" :arguments "{}")
                   (:tool-result :seq 3 :time 44 :call-id "call" :status :ok :content "done"))))
    (unwind-protect
         (progn
           (parent-session-test--write path forms :tail "(:message :seq 4")
           (let* ((before   (uiop:read-file-string path))
                  (store    (parent-session-ensure configuration "source"))
                  (snapshot (parent-session-snapshot store))
                  (locals   (remove nil (mapcar #'parent-session-record->local snapshot))))
             (assert (equal before (uiop:read-file-string path)))
             (assert (= 5 (length snapshot)))
             (assert (= 3 (length locals)))
             (assert (every (lambda (record) (eq (sixth record) ':agent)) (subseq snapshot 1 4)))
             (assert (equal "owner input" (getf (rest (first locals)) ':content)))
             (assert (= 42 (getf (rest (first locals)) ':time)))
             (assert (equal "History" (getf (rest (parent-session-legacy-header store)) ':title)))
             (assert (equal snapshot (parent-session-snapshot (parent-session-ensure configuration "source")))))
           (let* ((store  (parent-session-ensure configuration "source" :read-only-p t))
                  (replay (parent-session-replay-open store)))
             (assert (= 2 (first (parent-session-replay-select replay '("turn" "1")))))
             (assert (= 3 (first (parent-session-replay-select replay '("sequence" "3")))))
             (assert (= 1 (first (parent-session-replay-move replay -999))))
             (assert (= 5 (first (parent-session-replay-move replay 999))))
             (assert (parent-session-test--rejected-p
                      (lambda () (parent-session-replay-select replay '("turn" "9"))) ':selection-not-found))
             (assert (eq ':completed
                         (parent-session-replay-run store :input (make-string-input-stream "quit")
                                                         :output (make-broadcast-stream)))))
           (let* ((fork     (parent-session-fork configuration "source" :selection '("sequence" "2")
                                                                      :identifier  "fork"))
                  (snapshot (parent-session-snapshot fork))
                  (locals   (remove nil (mapcar #'parent-session-record->local snapshot))))
             (assert (equal (session-store-id fork) "fork"))
             (assert (= 2 (length locals)))
             (assert (equal "owner input" (getf (rest (first locals)) ':content)))
             (assert (eq (first (second locals)) ':fork))
             (assert (eq (fourth (first (last snapshot))) ':turn-outcome))
             (assert (equal "interrupted"
                            (second (session-store--field (fifth (first (last snapshot))) "outcome")))))
           (let* ((path   (parent-session-path configuration "source" "session.sexp"))
                  (source (uiop:read-file-string path)))
             (with-open-file (stream path :direction ':output :if-exists ':append)
               (write-string "(:session-entry" stream))
             (let* ((before (uiop:read-file-string path))
                    (store  (session-store-open path "source" :read-only-p t)))
               (assert (= 5 (length (parent-session-snapshot store))))
               (assert (equal before (uiop:read-file-string path)))
               (assert (handler-case (progn (session-store-start-turn store "forbidden" "input") nil)
                         (session-store-error (condition) (eq (session-store-error-code condition) ':read-only)))))
             (with-open-file (stream path :direction ':output :if-exists ':supersede)
               (write-string source stream)))
           44)
      (parent-session-test--cleanup root))))

(serapeum/types:-> run-parent-session-artifact-tests () integer)
(defun run-parent-session-artifact-tests ()
  "Exercise usable attachment bytes, rebased task paths, digest manifests and scoped forks."
  (let* ((root          (merge-pathnames (format nil "session-artifacts-~A/" (parent-application--identifier))
                                         (uiop:temporary-directory)))
         (legacy        (merge-pathnames "legacy/" root))
         (configuration (make-instance 'parent-configuration
                                       :values (list :private-root              (namestring (merge-pathnames "private/" root))
                                                     :legacy-conversations-root (namestring legacy))))
         (image         (merge-pathnames "conversation-images/source/image.png" root))
         (task          (merge-pathnames "tasks/source/job/result.sexp" root))
         (descriptor    '(:id "image" :artifact "image.png" :source "/private/original/image.png"
                          :mime "image/png" :detail "high" :width 1 :height 1)))
    (unwind-protect
         (progn
           (ensure-directories-exist image)
           (with-open-file (stream image :direction ':output :element-type '(unsigned-byte 8))
             (write-sequence #(137 80 78 71 13 10 26 10) stream))
           (parent-session-test--write task '((:result :value "usable")))
           (parent-session-test--write
            (merge-pathnames "source.sexp" legacy)
            (list '(:conversation :version 1 :id "source")
                  (list ':message :seq 1 :time 42 :role ':user :content "picture" :images (list descriptor))
                  (list ':task-completion :seq 2 :time 43 :artifact-path (namestring task))))
           (let* ((store      (parent-session-ensure configuration "source"))
                  (header     (parent-session-legacy-header store))
                  (projection (pathname (getf (rest header) ':artifact-root)))
                  (locals     (remove nil (mapcar #'parent-session-record->local (parent-session-snapshot store)))))
             (assert (equal projection (parent-session-artifact-root configuration "source")))
             (assert (= 8 (with-open-file (stream (merge-pathnames "image.png" projection)
                                                 :element-type '(unsigned-byte 8)) (file-length stream))))
             (assert (equal "image.png" (getf (first (getf (rest (first locals)) ':images)) ':source)))
             (assert (equal (uiop:read-file-string task)
                            (uiop:read-file-string (getf (rest (second locals)) ':artifact-path))))
             (assert (probe-file (merge-pathnames "manifest.sexp" projection)))
             (assert (member (namestring projection)
                             (parent-configuration-get (parent-session-configuration configuration store) ':runtime-roots)
                             :test #'equal))
             (let* ((fork      (parent-session-fork configuration "source" :selection '("sequence" "2") :identifier "fork"))
                    (fork-root (pathname (getf (rest (parent-session-legacy-header fork)) ':artifact-root))))
               (assert (not (equal projection fork-root)))
               (assert (probe-file (merge-pathnames "image.png" fork-root)))
               (assert (not (probe-file (merge-pathnames "tools/tasks/source/job/result.sexp" fork-root))))))
           (dolist (path '("../outside" "/absolute" "a//b" "a/*" "a/../b" "a\\b"))
             (assert (parent-session-test--rejected-p
                      (lambda () (parent-session--literal-relative path)) ':invalid-artifact-path)))
           15)
      (parent-session-test--cleanup root))))

(serapeum/types:-> run-parent-session-cli-tests () integer)
(defun run-parent-session-cli-tests ()
  "Exercise preserved CLI flags, result validation, machine-event order and malformed data."
  (let* ((request (make-parent-cli-request :command ':fork :options '(:fullscreen t :immutable t :id "new")))
         (copy    (parent-session--resume-request request "chosen")))
    (assert (equal "chosen" (parent-cli-option copy ':session-id)))
    (assert (parent-cli-option copy ':fullscreen))
    (assert (parent-cli-option copy ':immutable))
    (assert (null (parent-cli-option request ':session-id)))
    (assert (parent-job--result-valid-p (parent-job--failure ':process-failure)))
    (assert (not (parent-job--result-valid-p '(:autolith-job-result :version 2))))
    (let* ((output   (make-string-output-stream))
           (callback (parent-job--event-callback output))
           (fields   (make-hash-table :test #'equal)))
      (setf (gethash "run_job_event" fields)
            "(:autolith-event :version 1 :run-id \"run\" :seq 1 :time \"now\" :kind :progress :data (:text \"a
b\"))")
      (funcall callback ':presentation fields)
      (assert (eq (first (parent-job--read-form (get-output-stream-string output))) ':autolith-event))
      (assert (parent-session-test--rejected-p
               (lambda () (funcall callback ':presentation fields)) ':invalid-job-event)))
    (dolist (kind '("message" "acp-prompt" "configure" "command"))
      (let ((payload (make-hash-table :test #'equal)))
        (setf (gethash "text" payload) "transport-only input"
              (gethash "application_kind" payload) kind)
        (assert (null (parent-session-record->local
                       (list 1 "event" "turn" ':user-input
                             (session-store--value-data (protocol-data->value payload)) ':parent))))))
    12))
