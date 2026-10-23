(in-package #:autolith)

;;;; -- Evidence Applicability --

;;; Evidence is an immutable record of what was observed; whether it still
;;; applies is decided now. File evidence requires the observed content digest,
;;; including when it quotes a passage: surrounding edits may change its meaning.
;;; Web pages and tool output expire without background retrieval. A user's
;;; statement and a recorded run stay as they were.

(defparameter *knowledge-url-lifetime* (* 7 86400)
  "Seconds a web observation supports claims before it expires.")

(defparameter *knowledge-tool-lifetime* 86400
  "Seconds a tool output observation supports claims before it expires.")

(defvar *knowledge-clock* #'get-universal-time
  "A function returning the current universal time for knowledge observations and checks.

Replays and fixtures bind it to evaluate expiry deterministically.")

(-> knowledge-now () timestamp)
(defun knowledge-now ()
  "Return the current time as knowledge sees it."
  (funcall *knowledge-clock*))

(deftype knowledge-applicability ()
  "Whether evidence or a support applies now."
  '(member :current :changed :missing :expired :retracted :pending :unverified))

(defclass evidence-check ()
  ((now
    :initarg :now
    :reader evidence-check-now
    :type timestamp
    :documentation "The time the check evaluates validity and expiry against.")
   (memories
    :initarg :memories
    :initform (make-hash-table :test #'equal)
    :reader evidence-check-memories
    :type hash-table
    :documentation "Active prose memories by identifier, for memory evidence.")
   (files
    :initform (make-hash-table :test #'equal)
    :reader evidence-check-files
    :type hash-table
    :documentation "Each cited file's (TEXT FAILURE STATUS-KEY), read at most once per check.")
   (results
    :initform (make-hash-table :test #'equal)
    :reader evidence-check-results
    :type hash-table
    :documentation "Evidence outcomes by identifier, computed at most once."))
  (:documentation "One consistent pass deciding which evidence applies now."))


;;;; -- Lifetimes --

(-> knowledge-source-lifetime (list) (option integer))
(defun knowledge-source-lifetime (source)
  "Return the seconds evidence from SOURCE stays valid, or NIL when it does not expire."
  (case (first source)
    (:url *knowledge-url-lifetime*)
    (:tool *knowledge-tool-lifetime*)
    (otherwise nil)))

(-> knowledge-observe
    (&key (:source list) (:workspace (option string)) (:digest (option string))
          (:excerpt (option string)) (:adapter string) (:time timestamp))
    list)
(defun knowledge-observe (&key source workspace digest excerpt adapter (time (knowledge-now)))
  "Return evidence of observing SOURCE at TIME, expiring after the source's lifetime."
  (let ((lifetime (knowledge-source-lifetime source)))
    (knowledge-make-evidence :source      source
                             :workspace   workspace
                             :digest      digest
                             :observed-at time
                             :valid-until (and lifetime (+ time lifetime))
                             :excerpt     excerpt
                             :adapter     adapter)))


;;;; -- Checking --

(-> evidence-check-create (&key (:now timestamp) (:memories list)) evidence-check)
(defun evidence-check-create (&key (now (knowledge-now)) memories)
  "Return a check at NOW that resolves memory evidence against MEMORIES."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (memory memories)
      (setf (gethash (memory-identifier memory) table) memory))
    (make-instance 'evidence-check :now now :memories table)))

(-> knowledge-file-status-key (pathname) t)
(defun knowledge-file-status-key (pathname)
  "Return a value that changes whenever the file at PATHNAME changes, or :MISSING."
  (let ((status (ignore-errors (platform-path-status *platform* pathname :follow-links-p t))))
    (if status
        (list (platform-file-status-identity status)
              (platform-file-status-size status)
              (platform-file-status-modification-time status)
              (platform-file-status-change-time status))
        ':missing)))

(-> evidence-check--file (evidence-check pathname) (values (option string) (option keyword)))
(defun evidence-check--file (check pathname)
  "Return PATHNAME's text and read failure, reading it at most once per CHECK.

The file's status is taken before reading, so a later change to the file is
never mistaken for the content that was read."
  (let* ((key (namestring pathname))
         (entry (or (gethash key (evidence-check-files check))
                    (setf (gethash key (evidence-check-files check))
                          (let ((status (knowledge-file-status-key pathname)))
                            (multiple-value-bind (text failure) (memory-source-read-file pathname)
                              (list text failure status)))))))
    (values (first entry) (second entry))))

(-> evidence-check--file-source (evidence-check list (option string) (option string))
    (values keyword (option keyword)))
(defun evidence-check--file-source (check source workspace digest)
  "Return whether file SOURCE still has the observed DIGEST and quoted passage."
  (let ((pathname (memory-source-file-pathname source workspace)))
    (multiple-value-bind (text failure) (evidence-check--file check pathname)
      (let ((quote (getf (rest source) :quote)))
        (cond
          ((and text (null digest))
           (values ':unverified ':missing-digest))
          ((and text (equal (memory-source-text-digest text) digest)
                (or (null quote) (memory-source-quote-found-p quote text)))
           (values ':current nil))
          (text
           (values ':changed ':changed))
          ((eq failure ':missing)
           (values ':missing ':missing))
          (t
           (values ':unverified failure)))))))

(-> evidence-check-evidence (evidence-check list) (values keyword (option keyword)))
(defun evidence-check-evidence (check evidence)
  "Return whether EVIDENCE, an evidence plist, applies at CHECK's time, and why not.

The first value is a KNOWLEDGE-APPLICABILITY; the second refines it, such as
the read failure of an unverified file."
  (let* ((identifier (getf evidence :id))
         (cached (gethash identifier (evidence-check-results check))))
    (if cached
        (values (first cached) (second cached))
        (multiple-value-bind (state reason)
            (let ((source (getf evidence :source))
                  (valid-until (getf evidence :valid-until)))
              (cond
                ((and valid-until (> (evidence-check-now check) valid-until))
                 (values ':expired nil))
                ((eq (first source) :file)
                 (evidence-check--file-source check source
                                              (getf evidence :workspace)
                                              (getf evidence :digest)))
                ((eq (first source) :memory)
                 (let ((memory (gethash (getf (rest source) :id)
                                        (evidence-check-memories check))))
                   (cond
                     ((null memory)
                      (values ':missing ':forgotten))
                     ((string= (memory-content-digest (memory-title memory)
                                                      (memory-content memory))
                               (getf (rest source) :digest))
                      (values ':current nil))
                     (t
                      (values ':changed ':changed)))))
                (t
                 (values ':current nil))))
          (setf (gethash identifier (evidence-check-results check)) (list state reason))
          (values state reason)))))

(-> evidence-check-support (evidence-check knowledge-base list)
    (values keyword (option keyword)))
(defun evidence-check-support (check base support)
  "Return whether SUPPORT, a support plist, applies at CHECK's time, and why not."
  (let ((now (evidence-check-now check))
        (evidence (and (getf support :evidence)
                       (knowledge-base-evidence-record base (getf support :evidence)))))
    (cond
      ((getf support :retracted-at)
       (values ':retracted nil))
      ((and (getf support :valid-from) (< now (getf support :valid-from)))
       (values ':pending nil))
      ((and (getf support :valid-until) (> now (getf support :valid-until)))
       (values ':expired nil))
      ((null evidence)
       (values ':current nil))
      (t
       (evidence-check-evidence check evidence)))))

(-> knowledge-observe-file
    (pathname &key (:quote (option string)) (:workspace (option string)) (:time timestamp))
    list)
(defun knowledge-observe-file (pathname &key quote workspace (time (knowledge-now)))
  "Return evidence that file PATHNAME contains QUOTE, or its whole current text.

A file inside WORKSPACE is recorded relative to it. Signal MEMORY-ERROR
correctively when the file cannot support anything."
  (let* ((observed (memory-source-observe-file pathname :quote quote :time time))
         (relative (and workspace
                        (uiop:subpathp pathname (uiop:ensure-directory-pathname workspace)))))
    (knowledge-observe :source (list :file
                                     :path (if relative
                                               (uiop:unix-namestring relative)
                                               (getf (rest observed) :path))
                                     :relative (and relative t)
                                     :quote quote)
                       :workspace (and relative workspace)
                       :digest (getf (rest observed) :digest)
                       :excerpt quote
                       :adapter "file/1"
                       :time time)))
