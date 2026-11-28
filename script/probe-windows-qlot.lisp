;;;; Diagnostic only. Load after Qlot and Autolith's cleanup override.

(defvar *qlot-probe-source* nil)
(defvar *qlot-probe-presenter* nil)

(defun qlot-probe-mark (phase edge detail)
  "Flush one independently timestamped marker, without using Qlot's progress stream."
  (let* ((thread sb-thread:*current-thread*)
         (presentation (or *qlot-probe-presenter*
                           (equal (sb-thread:thread-name thread) "qlot progress manager")))
         (path (merge-pathnames (if presentation "progress.log" "installer.log")
                                (uiop:ensure-directory-pathname
                                 (uiop:getenv "AUTOLITH_QLOT_PROBE_DIR")))))
    (with-open-file (out path :direction ':output :if-exists ':append
                              :if-does-not-exist ':create :external-format ':utf-8)
      (format out "~D ~S ~S ~A ~A ~S~%" (get-internal-real-time)
              (sb-thread:thread-name thread) *qlot-probe-source* phase edge detail)
      (finish-output out))))

(defun qlot-probe-wrap (package name &key detail source-p presenter-p)
  "Record normal entry and return without altering the wrapped call or its values."
  (let* ((symbol (find-symbol name package))
         (original (fdefinition symbol)))
    (setf (fdefinition symbol)
          (lambda (&rest arguments)
            (let ((*qlot-probe-source*
                    (if source-p
                        (uiop:symbol-call '#:qlot/source '#:source-project-name (first arguments))
                        *qlot-probe-source*))
                  (*qlot-probe-presenter* (or presenter-p *qlot-probe-presenter*)))
              (qlot-probe-mark name "before" (when detail (funcall detail arguments)))
              (multiple-value-prog1 (apply original arguments)
                (qlot-probe-mark name "after" nil)))))))

(qlot-probe-mark "startup" "ready"
                 (list :sbcl (lisp-implementation-version)
                       :runtime sb-ext:*runtime-pathname*
                       :win32 (not (null (member ':win32 *features*)))
                       :qlot-version (asdf:component-version (asdf:find-system "qlot"))
                       :qlot-source (asdf:system-source-file "qlot")
                       :qlot-no-cache (uiop:getenv "QLOT_NO_CACHE")
                       :project (first (uiop:command-line-arguments))
                       :update (not (null (member "--update" (uiop:command-line-arguments)
                                                   :test #'string=)))))

(qlot-probe-wrap "QLOT/DISTIFY/GIT" "DISTIFY-GIT" :source-p t)
(qlot-probe-wrap "QLOT/DISTIFY/GIT" "WRITE-METADATA-FILES"
                 :detail (lambda (arguments) (rest arguments)))
(qlot-probe-wrap "QLOT/UTILS/DISTIFY" "SYSTEMS.TXT" :detail #'identity)
(qlot-probe-wrap "QLOT/UTILS/DISTIFY" "RELEASES.TXT" :detail #'identity)
(qlot-probe-wrap "QLOT/UTILS/DISTIFY" "NORMALIZE-PATHNAME" :detail #'first)
(qlot-probe-wrap "QLOT/UTILS/ASDF" "DIRECTORY-SYSTEM-FILES" :detail #'first)
(qlot-probe-wrap "QLOT/UTILS/ASDF" "READ-ASD-FILE" :detail #'first)
(qlot-probe-wrap "IRONCLAD" "DIGEST-FILE" :detail #'identity)
(qlot-probe-wrap "IRONCLAD" "DIGEST-STREAM" :detail #'first)
(qlot-probe-wrap "QLOT/UTILS/TMP" "DELETE-TMP-DIRECTORY" :detail #'first)
(qlot-probe-wrap "QLOT/SERVER" "MAKE-HANDLER" :detail #'first)
(qlot-probe-wrap "QLOT/INSTALL" "INSTALL-SOURCE" :source-p t
                 :detail (lambda (arguments)
                           (declare (ignore arguments))
                           (symbol-value (find-symbol "*CACHE-ENABLED*" "QLOT/CACHE"))))
(qlot-probe-wrap "QLOT/PROGRESS" "REFRESH-PROGRESS-LINE" :presenter-p t
                 :detail (lambda (arguments)
                           (list (uiop:symbol-call "QLOT/PROGRESS" "PROGRESS-LINE-HEADER"
                                                   (second arguments))
                                 (third arguments))))
(qlot-probe-mark "hooks" "ready" nil)
