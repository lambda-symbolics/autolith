(in-package #:autolith)

;;;; -- Knowledge Viewers --

;;; A viewer is someone who looks at knowledge repeatedly: a person through
;;; (what-changed), or a conversation through recall and memory:changes. The
;;; last snapshot each viewer saw in each workspace is kept, so changes are
;;; reported once, when that viewer next looks. Snapshots are derived state:
;;; a damaged file is replaced, losing only the record of what was last seen.

(defparameter *knowledge-viewer-format-version* 1
  "The readable knowledge viewer state format version.")

(defparameter *knowledge-viewer-limit* 32
  "The most viewer snapshots retained, the least recently seen dropped first.")

(-> configuration-knowledge-viewers-path (configuration) pathname)
(defun configuration-knowledge-viewers-path (configuration)
  "Return the pathname of the last knowledge snapshot each viewer saw."
  (merge-pathnames "knowledge-viewers.sexp" (config :state-root configuration)))

(-> knowledge-viewer--entry-p (t) boolean)
(defun knowledge-viewer--entry-p (entry)
  "Return true when ENTRY is one viewer's portable snapshot record."
  (and (proper-list-p entry)
       (evenp (length entry))
       (non-empty-string-p (getf entry :viewer))
       (non-empty-string-p (getf entry :workspace))
       (typep (getf entry :time) 'timestamp)
       (proper-list-p (getf entry :snapshot))
       t))

(-> knowledge-viewer--form-p (t) boolean)
(defun knowledge-viewer--form-p (form)
  "Return true when FORM is the complete viewer state record."
  (and (proper-list-p form)
       (eq (first form) :knowledge-viewers)
       (eql (getf (rest form) :version) *knowledge-viewer-format-version*)
       (proper-list-p (getf (rest form) :viewers))
       (every #'knowledge-viewer--entry-p (getf (rest form) :viewers))
       t))

(-> knowledge-viewer--store (configuration) sexp-store:snapshot-store)
(defun knowledge-viewer--store (configuration)
  "Return the locked snapshot store of viewer state."
  (let ((pathname (configuration-knowledge-viewers-path configuration)))
    (make-instance 'sexp-store:snapshot-store
                   :pathname pathname
                   :lock-pathname (readable-state-lock-pathname pathname "knowledge-viewers.lock")
                   :initial-state #'list
                   :validator #'knowledge-viewer--form-p
                   :decoder (lambda (form) (copy-tree (getf (rest form) :viewers)))
                   :encoder (lambda (viewers)
                              (list :knowledge-viewers
                                    :version *knowledge-viewer-format-version*
                                    :viewers viewers))
                   :recover-read-error (lambda (condition)
                                         (declare (ignore condition))
                                         nil))))

(-> knowledge-viewer-update
    (configuration string string timestamp function)
    t)
(defun knowledge-viewer-update (configuration viewer workspace time function)
  "Replace VIEWER's snapshot in WORKSPACE by what FUNCTION returns, and return its result.

FUNCTION receives the previous snapshot, the time it was recorded, and whether
there was none, and returns the new snapshot, or :KEEP to leave it, and a
result."
  (knowledge--translate-store-errors
   (lambda ()
     (values
      (sexp-store:store-transact
       (knowledge-viewer--store configuration)
       (lambda (viewers)
         (let ((entry (find-if (lambda (entry)
                                 (and (equal (getf entry :viewer) viewer)
                                      (equal (getf entry :workspace) workspace)))
                               viewers)))
           (multiple-value-bind (snapshot result)
               (funcall function (getf entry :snapshot) (getf entry :time) (null entry))
             (if (eq snapshot ':keep)
                 (values viewers result nil)
                 (let ((others (remove entry viewers)))
                   (values (cons (list :viewer viewer
                                       :workspace workspace
                                       :time time
                                       :snapshot snapshot)
                                 (subseq others 0 (min (length others)
                                                       (1- *knowledge-viewer-limit*))))
                           result
                           t)))))))))))

(-> knowledge-viewer-changes
    (configuration string knowledge-view &key (:record boolean))
    (values list (option timestamp) boolean))
(defun knowledge-viewer-changes (configuration viewer view &key (record t))
  "Return what changed in VIEW since VIEWER last looked from its workspace.

The second value is when VIEWER last looked, and the third is true when it
never had. When RECORD, VIEW becomes what VIEWER last saw."
  (values-list
   (knowledge-viewer-update
    configuration viewer (knowledge-view-workspace view) (knowledge-view-now view)
    (lambda (previous time first-p)
      (values (if record (knowledge-view-snapshot view) ':keep)
              (list (and (not first-p) (knowledge-view-changes previous view))
                    time
                    first-p))))))
