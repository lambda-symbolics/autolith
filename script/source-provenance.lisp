(in-package #:cl-user)

(defun autolith-source-jj-p (source-root)
  "Return true for a jj workspace rooted at SOURCE-ROOT."
  (not (null (probe-file (merge-pathnames ".jj/" source-root)))))

(defun autolith-source-blob (pathname)
  "Return the Git SHA1 blob identity of PATHNAME without a repository process."
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (let ((digest (uiop:symbol-call '#:ironclad '#:make-digest :sha1))
          (buffer (make-array 65536 :element-type '(unsigned-byte 8))))
      (uiop:symbol-call
       '#:ironclad '#:update-digest
       digest (map '(vector (unsigned-byte 8)) #'char-code
                   (format nil "blob ~D~C" (file-length stream) #\Null)))
      (loop for count = (read-sequence buffer stream)
            while (plusp count)
            do (uiop:symbol-call '#:ironclad '#:update-digest digest buffer :end count))
      (uiop:symbol-call '#:ironclad '#:byte-array-to-hex-string
                        (uiop:symbol-call '#:ironclad '#:produce-digest digest)))))

(defun autolith-source-command (source-root arguments fallback &key ignore-error-status)
  "Adapt the image library's Git provenance operations for jj workspaces.

The commit is the workspace's parent, cleanliness covers only the requested
inputs, and file identities retain the Git blob format. Other source trees
use FALLBACK unchanged, including the Git metadata supplied by Nix builds."
  (if (not (autolith-source-jj-p source-root))
      (funcall fallback source-root arguments)
      (labels ((jj (&rest arguments)
                 (string-trim '(#\Space #\Tab #\Newline #\Return)
                              (uiop:run-program
                               (append (list "jj" "--no-pager" "--color" "never"
                                             "--repository" (namestring source-root))
                                       arguments)
                               :directory source-root
                                :output :string :error-output :output
                                :ignore-error-status ignore-error-status))))
        (cond
          ((equal arguments '("rev-parse" "HEAD"))
           (jj "log" "--no-graph" "-r" "@-" "-T" "commit_id"))
          ((equal arguments '("status" "--porcelain"))
           (jj "diff" "--from" "@-" "--to" "@" "--name-only"))
          ((equal (subseq arguments 0 (min 3 (length arguments)))
                  '("status" "--porcelain" "--"))
           (apply #'jj "diff" "--from" "@-" "--to" "@" "--name-only" "--"
                  (cdddr arguments)))
          ((string= (first arguments) "hash-object")
           (format nil "~{~A~^~%~}"
                   (mapcar (lambda (path)
                             (autolith-source-blob (merge-pathnames path source-root)))
                           (remove "--" (rest arguments) :test #'string=))))
          (t (error "Unsupported jj image provenance operation: ~S" arguments))))))
