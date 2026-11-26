(in-package #:autolith)

;;;; -- Old Tool Evidence Provenance --

(-> shell-log-legacy-tests--persist
    (configuration list string
     &key (:digest (option string)) (:valid-from (option timestamp))
          (:valid-until (option timestamp)) (:evidence-until (option timestamp))
          (:retracted-p boolean) (:support-p boolean))
    (values string string))
(defun shell-log-legacy-tests--persist
    (configuration source output
     &key (digest (memory-source-text-digest output)) valid-from valid-until
       evidence-until retracted-p (support-p t))
  "Append real old-format rows, bypassing new-source constructor normalization."
  (let* ((workspace (knowledge--workspace configuration))
         (id        (knowledge-digest-identifier #\e (list source digest "legacy/1" workspace)))
         (support   (make-identifier))
         (claim     (knowledge-claim-identifier '("legacy.fixture" "capture")
                                               :scope ':workspace :workspace workspace))
         (now       (get-universal-time))
         (path      (configuration-knowledge-path configuration)))
    (ensure-directories-exist path)
    (sexp-store:log-append path
                           (list :evidence :version 1 :id id :source source :workspace workspace
                                 :digest digest :observed-at now :valid-until evidence-until
                                 :excerpt output :adapter "legacy/1")
                           :initial-forms (list (knowledge--header)))
    (when support-p
      (sexp-store:log-append
       path (list :claim-asserted :version 1 :support support :claim claim
                  :scope ':workspace :workspace workspace
                  :proposition '("legacy.fixture" "capture") :evidence id :basis ':observation
                  :valid-from valid-from :valid-until valid-until :time now))
      (when retracted-p
        (sexp-store:log-append path
                               (list :support-retracted :version 1 :support support :time now))))
    (values id support)))

(-> shell-log-legacy-tests--append-result
    (conversation string t &key (:tool string) (:call string) (:details-p boolean)) null)
(defun shell-log-legacy-tests--append-result
    (conversation output details &key (tool "shell.run") (call "provider-call") (details-p t))
  "Persist a result with an actual provider call ID and optional old metadata."
  (if details-p
      (conversation-append-tool-result conversation call :tool-name tool
                                                        :output output :details details :success-p t)
      (conversation-append-record conversation
                                  (list :tool-result :call-id call :tool tool
                                        :status ':ok :output output)))
  nil)

(-> shell-log-legacy-tests--metadata (string) list)
(defun shell-log-legacy-tests--metadata (artifact)
  "Return canonical persisted capture identities whose raw storage is already missing."
  (list :shell-logs
        (list (list :execution-id "legacy-execution" :artifact-id artifact
                    :captures (list (list :stream    ':output
                                          :reference (format nil "shell-log:legacy-owner/legacy-execution/~A/output"
                                                             artifact)))))))

(-> test-shell-log-legacy-exact-provenance () null)
(defun test-shell-log-legacy-exact-provenance ()
  "Cold-fold old rows and match exact owner/tool/digest, never synthetic calls or text URIs."
  (with-test-configuration (configuration)
    (let* ((*shell-log-legacy-evidence-cache* nil)
           (*knowledge-base-cache*            (make-hash-table :test #'equal :synchronized t))
           (*shell-log-active*                (make-hash-table :test #'equal))
           (context                           (shell-log-tests--context configuration "legacy-owner"))
           (owner                             (tool-context-conversation context))
           (artifact                          (shell-log-allocate context :merge-output-p nil))
           (metadata                          (shell-log-tests--finish artifact #(65 66) :error-bytes #(67)))
           (output                            "Preview mentions shell-log:unrelated/run/decoy/output, which is not evidence.")
           (source                            (list :tool :name "shell.run" :conversation (conversation-identifier owner)
                                                    :call "contract.fixture/0123456789abcdef"))
           (expected                          (sort (list (shell-log-reference artifact) (shell-log-reference artifact ':error))
                                                    #'string<)))
      (shell-log-legacy-tests--append-result owner output (list :shell-logs (list metadata))
                                              :call "provider-actual-one")
      (shell-log-legacy-tests--append-result owner output (list :shell-logs (list metadata))
                                              :call "provider-actual-two")
      (multiple-value-bind (id support)
          (shell-log-legacy-tests--persist configuration source output)
        (let* ((base       (knowledge-tests--reread configuration))
               (before     (shell-log-legacy-evidence--file-stamp (configuration-knowledge-path configuration)))
               (references (shell-log-legacy-evidence-references configuration base))
               (evidence   (gethash id (knowledge-base-evidence base))))
          (test-assert (equal expected (gethash id references))
                       "Two same-digest deliveries with identical exact stream sets resolve once")
          (test-assert (shell-log-legacy-evidence-cache-valid-p configuration base references)
                       "A freshly warmed exact map is valid")
          (test-assert (and (equal source (getf evidence :source))
                            (gethash support (knowledge-base-supports base))
                            (equal before (shell-log-legacy-evidence--file-stamp
                                           (configuration-knowledge-path configuration))))
                       "Backfill preserves old evidence identity, support and append-only audit")
          (test-call-with-function-replacements
           (list (list 'platform-path-status (lambda (&rest arguments)
                                              (declare (ignore arguments))
                                              (error "Cached access performed I/O")))
                 (list 'conversation--map-storage-records (lambda (&rest arguments)
                                                           (declare (ignore arguments))
                                                           (error "Cached access read a transcript"))))
           (lambda ()
             (multiple-value-bind (refs map)
                 (shell-log-legacy-evidence-cached-references configuration base evidence)
               (test-assert (and (equal refs expected) (eq map references))
                            "Write-side cache access returns exact refs plus the warmed map without I/O"))))
          (delete-file (merge-pathnames "output.bin" (shell-log-capture-directory artifact)))
          (test-assert (equal expected
                              (gethash id (shell-log-legacy-evidence-references configuration base)))
                       "Existing audit keeps exact canonical identity after raw bytes are missing")))))
  nil)

(-> test-shell-log-legacy-refusals () null)
(defun test-shell-log-legacy-refusals ()
  "Refuse unproved reuse while admitting new captures and retaining possible dependencies."
  (dolist (case '(:missing-digest :missing-owner :different-digest :different-tool
                  :different-owner :missing-details :omitted-details :omitted-logs
                  :malformed-details :ambiguous))
    (with-test-configuration (configuration)
      (let* ((*shell-log-legacy-evidence-cache* nil)
             (*knowledge-base-cache*            (make-hash-table :test #'equal :synchronized t))
             (*shell-log-active*                (make-hash-table :test #'equal))
             (context                           (shell-log-tests--context configuration "legacy-owner"))
             (owner                             (tool-context-conversation context))
             (artifact                          (shell-log-allocate context))
             (output                            "legacy preview")
             (details                           (case case
                                                  (:omitted-details
                                                   '(:omitted-p t :reason :metadata-size-limit))
                                                  (:omitted-logs
                                                   '(:shell-logs-omitted 1))
                                                  (:malformed-details
                                                   '(:shell-logs-omitted "unknown"))
                                                  (otherwise
                                                   (shell-log-legacy-tests--metadata "artifact-one"))))
             (source                            (list :tool         :name "shell.run"
                                                      :conversation (unless (eq case ':missing-owner)
                                                                      (conversation-identifier owner))
                                                      :call         "not-a-provider-call")))
        (shell-log-tests--finish artifact #(65))
        (shell-log-legacy-tests--append-result
         (if (eq case ':different-owner)
             (conversation-create configuration :identifier "foreign-owner")
             owner)
         output details :tool      (if (eq case ':different-tool) "lisp.eval" "shell.run")
                        :details-p (not (eq case ':missing-details)))
        (when (eq case ':ambiguous)
          (shell-log-legacy-tests--append-result owner output
                                                (shell-log-legacy-tests--metadata "artifact-two")
                                                :call "another-provider-call"))
        (shell-log-legacy-tests--persist
         configuration source output
         :digest (case case
                   (:missing-digest
                    nil)
                   (:different-digest
                    (memory-source-text-digest "different output"))
                   (otherwise
                    (memory-source-text-digest output))))
        (test-assert
         (eq ':unresolved-legacy-shell-evidence
             (knowledge-tests--error-code
              (lambda ()
                (shell-log-legacy-evidence-references configuration (knowledge-tests--reread configuration)))))
         (format nil "~A refuses instead of silently abandoning possible raw dependencies" case))
        (let* ((*shell-log-retention-age* -1)
               (fresh                     (shell-log-allocate context)))
          (shell-log-tests--finish fresh #(66))
          (shell-log-prune configuration)
          (test-assert (and (probe-file (merge-pathnames "output.bin" (shell-log-capture-directory artifact)))
                            (probe-file (merge-pathnames "output.bin" (shell-log-capture-directory fresh))))
                       "Unresolved old proof protects captures without preventing new allocation")
          (let ((*shell-log-retention-byte-limit* 1))
            (test-assert
             (handler-case (progn (shell-log-allocate context) nil)
               (shell-log-error () t))
             "Unresolved dependencies cannot be pruned to exceed the storage budget"))))))
  nil)

(-> test-shell-log-legacy-cache-invalidation () null)
(defun test-shell-log-legacy-cache-invalidation ()
  "Invalidate on transcript append/replacement, knowledge changes and bounded-cache eviction."
  (with-test-configuration (configuration)
    (let* ((*shell-log-legacy-evidence-cache*       nil)
           (*shell-log-legacy-evidence-cache-limit* 2)
           (*knowledge-base-cache*                  (make-hash-table :test #'equal :synchronized t))
           (owner                                   (conversation-create configuration :identifier "legacy-owner"))
           (source                                  (list :tool :name "shell.run" :conversation (conversation-identifier owner)
                                                          :call "synthetic-call"))
           (scans                                   0)
           (mapper                                  (symbol-function 'conversation--map-storage-records)))
      (shell-log-legacy-tests--append-result owner "preview" nil)
      (let* ((id   (shell-log-legacy-tests--persist configuration source "preview"))
             (base (knowledge-tests--reread configuration)))
        (test-call-with-function-replacements
         (list (list 'conversation--map-storage-records
                     (lambda (identity function &key on-header)
                       (incf scans)
                       (funcall mapper identity function :on-header on-header))))
         (lambda ()
           (let ((first (shell-log-legacy-evidence-references configuration base)))
             (test-assert (and (eq first (shell-log-legacy-evidence-references configuration base))
                               (= scans 1) (nth-value 1 (gethash id first)))
                          "Warm cache reuses exact map, including proved NIL dependency")
             (conversation-append-record owner '(:note :text "append changes source size"))
             (test-assert (not (shell-log-legacy-evidence-cache-valid-p configuration base first))
                          "A transcript append invalidates the warmed proof")
             (let* ((second      (shell-log-legacy-evidence-references configuration base))
                    (active      (conversation-storage-active-pathname (conversation-pathname owner)))
                    (replacement (merge-pathnames "replacement.sexp" (uiop:pathname-directory-pathname active))))
               (test-assert (and (= scans 2) (not (eq first second)))
                            "Append causes one new owner scan")
               (uiop:copy-file active replacement)
               (uiop:rename-file-overwriting-target replacement active)
               (test-assert (not (shell-log-legacy-evidence-cache-valid-p configuration base second))
                            "A same-size transcript replacement invalidates by file identity")
               (let ((third (shell-log-legacy-evidence-references configuration base)))
                 (sexp-store:log-append (configuration-knowledge-path configuration)
                                        (list :evidence-renewed :id id
                                              :observed-at (1+ (get-universal-time)) :valid-until nil))
                 (test-assert (not (shell-log-legacy-evidence-cache-valid-p configuration base third))
                              "An authoritative knowledge append invalidates the old map")
                 (let ((current (knowledge-base-read configuration)))
                   (test-assert (and (not (eq current base))
                                     (eq ':unresolved-legacy-shell-evidence
                                         (knowledge-tests--error-code
                                          (lambda ()
                                            (shell-log-legacy-evidence-cached-references
                                             configuration current (gethash id (knowledge-base-evidence current)))))))
                                "A replacement folded snapshot cannot borrow old cached provenance")
                   (shell-log-legacy-evidence-references configuration current)
                   (shell-log-legacy-evidence-references configuration (knowledge--copy-base current))
                   (test-assert (and (= 2 (length *shell-log-legacy-evidence-cache*))
                                     (not (shell-log-legacy-evidence-cache-valid-p configuration base third)))
                                "Bounded cache eviction removes the oldest snapshot's authority"))))))))))
  nil)

(-> test-shell-log-legacy-active-supports () null)
(defun test-shell-log-legacy-active-supports ()
  "Include future and boundary-valid supports; skip retracted/expired/new-format rows."
  (with-test-configuration (configuration)
    (let* ((*shell-log-legacy-evidence-cache* nil)
           (*knowledge-base-cache*            (make-hash-table :test #'equal :synchronized t))
           (owner                             (conversation-create configuration :identifier "legacy-owner"))
           (now                               (get-universal-time))
           (included                          nil)
           (excluded                          nil))
      (shell-log-legacy-tests--append-result owner "preview" nil)
      (dolist (case '(:future :boundary :expired-support :expired-evidence :retracted :new-format :unused))
        (let* ((needed-p (member case '(:future :boundary)))
               (source   (append (list :tool         :name "shell.run"
                                       :conversation (when needed-p (conversation-identifier owner))
                                       :call         (string-downcase case))
                                 (when (eq case ':new-format) (list :shell-logs nil))))
               (id       (shell-log-legacy-tests--persist
                          configuration source "preview"
                          :digest         (when needed-p (memory-source-text-digest "preview"))
                          :valid-from     (when (eq case ':future) (+ now 1000))
                          :valid-until    (case case (:boundary
                                                      now)
                                                     (:expired-support
                                                      (1- now)))
                          :evidence-until (case case (:boundary
                                                      now)
                                                     (:expired-evidence
                                                      (1- now)))
                          :retracted-p (eq case ':retracted) :support-p (not (eq case ':unused)))))
          (if needed-p (push id included) (push id excluded))))
      (let* ((base       (knowledge-tests--reread configuration))
             (required   (shell-log-legacy-evidence--required base now))
             (references (shell-log-legacy-evidence-references configuration base)))
        (test-assert (and (= 2 (length required))
                          (every (lambda (id) (find id required :key (lambda (item) (getf item :id))
                                                        :test        #'equal)) included))
                     "Future and exactly boundary-valid legacy observations require resolution")
        (test-assert (some (lambda (id) (and (nth-value 1 (gethash id references))
                                           (null (gethash id references)))) included)
                     "A future-valid support with explicit NIL details proves no raw dependency")
        (dolist (id excluded)
          (test-assert (not (nth-value 1 (gethash id references)))
                       "Irrelevant old rows and explicit new-format NIL need no guessed provenance")))))
  nil)

(-> test-shell-log-legacy-explicit-reuse () null)
(defun test-shell-log-legacy-explicit-reuse ()
  "Warm dormant exact evidence explicitly and reuse it through a cold knowledge transaction."
  (dolist (case '(:orphan :retracted :expired-support :expired-evidence))
    (with-test-configuration (configuration)
      (let* ((*shell-log-legacy-evidence-cache* nil)
             (*knowledge-base-cache*            (make-hash-table :test #'equal :synchronized t))
             (*shell-log-active*                (make-hash-table :test #'equal))
             (schema                            (knowledge-make-schema :predicate "legacy.fixture"
                                                                       :arguments '((:name "capture" :type :entity))
                                                                       :template  "{capture}"))
             (context                           (shell-log-tests--context configuration "legacy-owner"))
             (owner                             (tool-context-conversation context))
             (artifact                          (shell-log-allocate context))
             (metadata                          (shell-log-tests--finish artifact #(65)))
             (reference                         (shell-log-reference artifact))
             (source                            (list :tool :name "shell.run" :conversation (conversation-identifier owner)
                                                      :call (format nil "legacy/~(~A~)" case)))
             (now                               (get-universal-time)))
        (knowledge-define-schema configuration schema)
        (shell-log-legacy-tests--append-result owner "preview" (list :shell-logs (list metadata)))
        (let* ((id        (shell-log-legacy-tests--persist
                           configuration source "preview"
                           :support-p      (not (eq case ':orphan))
                           :retracted-p    (eq case ':retracted)
                           :valid-until    (when (eq case ':expired-support) (1- now))
                           :evidence-until (when (eq case ':expired-evidence) (1- now))))
               (base      (knowledge-tests--reread configuration))
               (evidence  (gethash id (knowledge-base-evidence base)))
               (empty     (shell-log-legacy-evidence-references configuration base))
               (requested (shell-log-legacy-evidence-references configuration base
                                                                :include-evidence (list evidence))))
          (test-assert (and (zerop (hash-table-count empty))
                            (equal (list reference) (gethash id requested))
                            (shell-log-legacy-evidence-cache-valid-p configuration base requested))
                       (format nil "~A evidence resolves when explicitly requested despite no live support" case))
          (setf *shell-log-legacy-evidence-cache* nil)
          (let ((record (cons ':evidence (copy-list evidence))))
            (when (eq case ':expired-evidence)
              (setf (getf (rest record) :observed-at) (1+ (getf evidence :observed-at))
                    (getf (rest record) :valid-until) nil))
            (multiple-value-bind (claim support)
                (knowledge-assert configuration '("legacy.fixture" "capture")
                                  :evidence record :basis ':observation
                                  :source-conversation (conversation-identifier owner))
              (declare (ignore claim))
              (let* ((current  (knowledge-tests--reread configuration))
                     (asserted (gethash support (knowledge-base-supports current)))
                     (retained (gethash id (knowledge-base-evidence current))))
                (test-assert (and (equal id (getf asserted :evidence))
                                  (not (getf asserted :retracted-at))
                                  (equal source (getf retained :source))
                                  (not (getf retained :valid-until)))
                             (format nil "Cold ~A reuse commits exact old identity with retained raw evidence" case)))))))))
  nil)
