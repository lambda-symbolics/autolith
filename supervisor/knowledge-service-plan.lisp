(in-package #:autolith-supervisor)

;;;; -- Parent Plan Evaluation --

(serapeum/types:-> knowledge-service--plan-pattern (t function boolean) list)
(defun knowledge-service--plan-pattern (value schema-function canonical-p)
  "Parse new input by schema; retain valid canonical patterns after schema changes."
  (if canonical-p
      (let ((pattern (knowledge-plan-data->term value)))
        (labels ((valid-p (items negatable-p)
                   (and (alexandria:proper-list-p items)
                        (if (eq (first items) ':not)
                            (and negatable-p (= (length items) 2) (valid-p (second items) nil))
                            (and (<= 2 (length items) 9)
                                 (non-empty-string-p (first items))
                                 (<= (length (first items)) 256)
                                 (every #'knowledge-term-p (rest items)))))))
          (unless (valid-p pattern t) (knowledge-service--reject ':invalid-request)))
        pattern)
      (knowledge--pattern-from-input value schema-function :negatable t)))

(serapeum/types:-> knowledge-service--plan-steps (t function boolean) list)
(defun knowledge-service--plan-steps (raw schema-function canonical-p)
  "Validate at most 32 bounded public steps before evaluating any step."
  (unless (and (simple-vector-p raw) (<= (length raw) *plan-maximum-steps*))
    (knowledge-service--reject ':resource-limit))
  (let ((identifiers nil))
    (map 'list
         (lambda (item)
           (unless (hash-table-p item) (knowledge-service--reject ':invalid-request))
           (knowledge-service--fields item '("text" "step" "status" "id" "requires" "verify" "receipt")
                                      '("status"))
           (let* ((text     (or (gethash "text" item) (gethash "step" item)))
                  (status   (plan--status (gethash "status" item)))
                  (id       (gethash "id" item))
                  (requires (gethash "requires" item #()))
                  (verify   (gethash "verify" item))
                  (receipt  (gethash "receipt" item)))
             (unless (and (non-empty-string-p text) (<= (length text) *plan-step-text-limit*)
                          status (or (null id) (and (non-empty-string-p id) (<= (length id) 256)))
                          (or (null receipt) (and (stringp receipt) (<= (length receipt) 256)))
                          (simple-vector-p requires) (<= (length requires) 32))
               (knowledge-service--reject ':invalid-request))
             (when (and id (member id identifiers :test #'equal))
               (knowledge-service--reject ':invalid-request))
             (when id (push id identifiers))
             (list :text text :status status :id id :receipt receipt
                   :requires (map 'list (lambda (pattern)
                                         (knowledge-service--plan-pattern pattern schema-function canonical-p))
                                  requires)
                   :verify   (and verify (not (and (vectorp verify) (zerop (length verify))))
                                  (knowledge-service--plan-pattern verify schema-function canonical-p)))))
         raw)))

(serapeum/types:-> knowledge-service--plan-object (list (option string)) workspace-plan)
(defun knowledge-service--plan-object (steps explanation)
  "Construct an in-memory plan without reading or writing agent plan storage."
  (make-instance 'workspace-plan :directory "workspace" :updated-at (get-universal-time)
                 :explanation explanation
                 :steps       (mapcar (lambda (step)
                                        (make-instance 'plan-step :text (getf step :text)
                                                       :status (getf step :status) :identifier (getf step :id)
                                                       :requires (getf step :requires) :verify (getf step :verify)
                                                       :receipt         (getf step :receipt))) steps)))

(serapeum/types:-> knowledge-service--plan-receipt-key (list) list)
(defun knowledge-service--plan-receipt-key (step)
  "Bind a receipt to the exact step, not merely its child-provided identifier."
  (list :id (getf step :id) :text (getf step :text)
        :requires (getf step :requires) :verify (getf step :verify)))

(serapeum/types:-> knowledge-service--plan-receipt-form-p (t) boolean)
(defun knowledge-service--plan-receipt-form-p (form)
  "Check the bounded parent-private receipt snapshot envelope."
  (and (alexandria:proper-list-p form) (eq (first form) ':plan-receipts)
       (eql (getf (rest form) :version) 1)
       (let ((entries (getf (rest form) :entries)))
         (and (alexandria:proper-list-p entries) (<= (length entries) 32)
              (every (lambda (entry)
                       (and (alexandria:proper-list-p entry)
                            (stringp (getf entry :session)) (stringp (getf entry :workspace))
                            (alexandria:proper-list-p (getf entry :receipts))
                            (<= (length (getf entry :receipts)) 128))) entries)))
       t))

(serapeum/types:-> knowledge-service--plan-receipt-update (knowledge-service knowledge-view function) list)
(defun knowledge-service--plan-receipt-update (service view function)
  "Atomically publish parent receipt state separately from expendable viewer snapshots."
  (let* ((context (knowledge-service-context service))
         (path    (merge-pathnames "plan-receipts.sexp"
                                   (uiop:pathname-directory-pathname (memory-context-knowledge-path context))))
         (store   (make-instance
                   'sexp-store:snapshot-store :pathname path
                   :lock-pathname                       (memory--state-lock-pathname path "plan-receipts.lock")
                   :initial-state #'list :validator #'knowledge-service--plan-receipt-form-p
                   :decoder                             (lambda (form) (copy-tree (getf (rest form) :entries)))
                   :encoder                             (lambda (entries) (list :plan-receipts :version 1 :entries entries)))))
    (knowledge--translate-store-errors
     (lambda ()
       (sexp-store:store-transact
        store
        (lambda (entries)
          (let* ((session   (knowledge-service-session-id service))
                 (workspace (knowledge-view-workspace view))
                 (entry     (find workspace entries :key (lambda (item) (getf item :workspace))
                                 :test                   #'equal)))
            (multiple-value-bind (receipts result)
                (funcall function (getf entry :receipts) (knowledge-view-now view) (null entry))
              (if (and (null entry) (null receipts))
                  (values entries result nil)
                  (let ((updated (cons (list :session session :workspace workspace :receipts receipts)
                                       (remove entry entries))))
                    (unless (and (<= (length updated) 32)
                                 (<= (reduce #'+ updated :key (lambda (item) (length (getf item :receipts)))) 1024)
                                 (<= (length (with-standard-io-syntax (write-to-string updated))) 8388608))
                      (knowledge-service--reject ':resource-limit))
                    (values updated result t)))))))))))

(serapeum/types:-> knowledge-service--plan-receipts
    (knowledge-service list &key (:prepare-p boolean) (:view knowledge-view)) list)
(defun knowledge-service--plan-receipts (service steps &key prepare-p view)
  "Resolve opaque receipts under parent authority and durably issue newly done receipts.

The private plan-receipts snapshot survives service reopen and does not share
viewer eviction or damaged-viewer recovery. At most 128 receipts survive one
workspace update; active tokens are retained first. Tokens are bound to the
workspace, global-access grant and exact step. The issuer session is audit metadata."
  (knowledge-service--plan-receipt-update
   service view
   (lambda (previous time first-p)
     (declare (ignore first-p))
     (let ((issued nil) (resolved nil))
       (dolist (step steps)
         (let* ((token (getf step :receipt))
                (entry (and token (find token previous :key (lambda (item) (getf item :token))
                                       :test                #'equal)))
                (key   (knowledge-service--plan-receipt-key step)))
           (when (and token (not (and entry (equal key (getf entry :key))
                                      (eql (getf entry :allow-global-p)
                                           (knowledge-service-allow-global-p service)))))
             (knowledge-service--reject ':permission-denied))
           (cond
             ((and entry (eq (getf step :status) ':done))
              (push entry issued))
             ((and prepare-p (eq (getf step :status) ':done)
                   (or (getf step :requires) (getf step :verify)))
              (let* ((with      (first (plan-steps-with-receipts (list step) nil view)))
                     (new-token (memory-context-make-identifier (knowledge-service-context service))))
               (setf entry (list :token new-token :key key :receipt (getf with :receipt) :time time
                                 :issuer-session (knowledge-service-session-id service)
                                 :allow-global-p (knowledge-service-allow-global-p service)))
               (push entry issued)))
             (t
              (setf entry nil)))
           (let ((copy (copy-list step)))
             (setf (getf copy :receipt) (and entry (getf entry :receipt))
                   (getf copy :parent-receipt) (and entry (getf entry :token)))
             (push copy resolved))))
       (let* ((keep   (append (nreverse issued) previous))
              (unique (remove-duplicates keep :key (lambda (item) (getf item :token))
                                         :test #'equal :from-end t)))
         (values (subseq unique 0 (min 128 (length unique))) (nreverse resolved)))))))

(serapeum/types:-> knowledge-service--plan-wire-step (list) hash-table)
(defun knowledge-service--plan-wire-step (step)
  "Return only normalized step data and its opaque parent receipt."
  (let ((data (make-hash-table :test #'equal)))
    (setf (gethash "text" data) (getf step :text)
          (gethash "status" data) (string-downcase (symbol-name (getf step :status)))
          (gethash "requires" data) (knowledge-service--expansion-data (getf step :requires))
          (gethash "verify" data) (knowledge-service--expansion-data (getf step :verify)))
    (when (getf step :id) (setf (gethash "id" data) (getf step :id)))
    (when (getf step :parent-receipt)
      (setf (gethash "receipt" data) (getf step :parent-receipt)))
    data))

(serapeum/types:-> knowledge-service-plan-response
    (knowledge-service hash-table keyword) parent-proto:knowledge-response)
(defun knowledge-service-plan-response (service fields scope)
  "Evaluate fixed bounded plan actions without granting graph mutation authority."
  (knowledge-service--fields fields '("action" "steps" "previous" "explanation" "canonical" "active" "turn")
                             '("action" "steps" "canonical"))
  (let* ((action      (gethash "action" fields))
         (canonical   (gethash "canonical" fields))
         (explanation (gethash "explanation" fields))
         (active      (gethash "active" fields))
         (turn        (gethash "turn" fields)))
    (unless (and (eq scope ':workspace)
                 (member action '("prepare" "render" "changes" "delivered") :test #'equal)
                 (typep canonical 'boolean) (typep active 'boolean)

                 (eq canonical (not (equal action "prepare")))
                 (or (equal action "prepare") (not (nth-value 1 (gethash "previous" fields))))
                 (or (null explanation) (and (stringp explanation) (<= (length explanation) 4096)))
                 (or (null turn) (typep turn '(integer 0 1000000000))))
      (knowledge-service--reject ':invalid-request))
    (let* ((context         (knowledge-service-context service))
           (base            (knowledge-base-read context))
           (schema-function (lambda (predicate)
                              (knowledge-base-schema base predicate :workspace (knowledge--workspace context))))
           (steps           (knowledge-service--plan-steps (gethash "steps" fields) schema-function canonical))

           (previous        (knowledge-service--plan-steps (gethash "previous" fields #()) schema-function t))
           (prepared
             (if (equal action "prepare")
                 (mapcar (lambda (step)
                           (let ((old (and (getf step :id)
                                           (find (knowledge-service--plan-receipt-key step) previous
                                                 :key #'knowledge-service--plan-receipt-key :test #'equal))))
                             (when (and old (eq (getf old :status) ':done)
                                        (eq (getf step :status) ':done))
                               (setf (getf step :receipt) (getf old :receipt)))
                             step)) steps)
                 steps))
           (view            (knowledge-service--context-view service (knowledge-service--context-memories service)))
           (resolved        (knowledge-service--plan-receipts service prepared :prepare-p (equal action "prepare") :view view))
           (plan            (knowledge-service--plan-object resolved explanation))
           (data            (make-hash-table :test #'equal)))
      (setf (gethash "steps" data) (map 'vector #'knowledge-service--plan-wire-step resolved)
            (gethash "rendered" data) (plan-render plan :view view)
            (gethash "changes" data) #())
      (when (or (member action '("prepare" "delivered") :test #'equal)
                (and (equal action "changes") active (knowledge-active-p context)))
        (let ((changes
                (knowledge-viewer-update
                 context (format nil "plan:~A" (knowledge-service-session-id service))
                 (knowledge-view-workspace view) :time (knowledge-view-now view)
                 :function
                 (lambda (previous time first-p)
                   (declare (ignore time))
                   (let* ((current (plan-step-snapshot plan view))
                          (changes (and (equal action "changes") (not first-p)
                                        (plan-snapshot-changes previous current))))
                     (values current changes))))))
          (setf (gethash "changes" data) (coerce changes 'vector))))
      (when (> (length (gethash "rendered" data)) 131072)
        (knowledge-service--reject ':resource-limit))
      (parent-proto:make-knowledge-response :fields (protocol-data->map data)))))
