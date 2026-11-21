;;;; Dispatch overlay only. Run with the extracted release's SBCL and Qlot.
(require :asdf)
(load (uiop:getenv "AUTOLITH_PROJECT_SETUP"))

(defun smoke--check (condition control &rest arguments)
  "Fail the dispatch with the requested diagnostic if CONDITION is false."
  (unless condition
    (apply #'error control arguments)))

(defun smoke--absolute-file (value)
  "Return the canonical pathname of the supplied absolute file."
  (let ((pathname (pathname value)))
    (smoke--check (uiop:absolute-pathname-p pathname) "Expected absolute path: ~S" value)
    (truename pathname)))

(defun smoke--load-pinned-clifff ()
  "Require ASDF to select the exact Clifff release from the packaged Qlot tree."
  (let* ((setup (smoke--absolute-file (uiop:getenv "AUTOLITH_PROJECT_SETUP")))
         (qlot (uiop:pathname-directory-pathname setup))
         (pin (uiop:getenv "CLIFFF_COMMIT"))
         (dist (uiop:symbol-call :ql-dist :find-dist "clifff"))
         (expected (merge-pathnames
                    (format nil "dists/clifff/software/clifff-ref-~A/clifff.asd" pin)
                    qlot)))
    (smoke--check
     (and pin dist
          (string= (uiop:symbol-call :ql-dist :version dist) (concatenate 'string "git-" pin))
          (equal (truename (uiop:symbol-call :ql-dist :base-directory dist))
                 (truename (merge-pathnames "dists/clifff/" qlot))))
     "Active Clifff dist is not the pinned packaged dist: ~A" pin)
    (uiop:symbol-call :ql :quickload :clifff :silent t)
    (let ((actual (truename (asdf:system-source-file (asdf:find-system :clifff)))))
      (smoke--check (equal actual (truename expected))
                    "Clifff was loaded from ~A instead of ~A" actual expected)
      #+win32
      (smoke--check (uiop:subpathp actual (truename (merge-pathnames "dists/clifff/" qlot)))
                    "The packaged Clifff source resolves outside its dist: ~A" actual)
      (format t "CLIFFF: active dist git-~A, system=~A~%" pin actual))))

(smoke--load-pinned-clifff)

(defun smoke--write-fixture (directory)
  "Write one searchable file and one unrelated file beneath DIRECTORY."
  (dolist (entry '(("src/clifff-index-needle.txt" "first line" "FffSmokeUnique42" "last line")
                   ("docs/unrelated.txt" "no match here")))
    (let ((file (merge-pathnames (first entry) directory)))
      (ensure-directories-exist file)
      (with-open-file (stream file :direction :output :if-exists :error :external-format :utf-8)
        (dolist (line (rest entry))
          (write-line line stream))))))

(defun smoke--cache-populated-p (cache)
  "Check that the native frecency and history databases exist under CACHE."
  (every (lambda (name) (probe-file (merge-pathnames name cache)))
         '("frecency/data.mdb" "history/data.mdb")))

(defun smoke--worker-cycle (command &key library fixture cache log cold-p)
  "Search a fixture and verify worker reuse, then close the worker and both pipes."
  (let ((worker (clifff:make-worker :command command :log-pathname log))
        (process nil)
        (input nil)
        (output nil))
    (labels ((request (operation arguments)
               ;; The pinned client validates the helper's ownership acknowledgement
               ;; against the exact (DLL, workspace, cache) paths on every reply.
               (clifff:worker-request worker :library-path library
                                             :base-path fixture
                                             :cache-directory cache
                                             :log-pathname log
                                             :operation operation
                                             :arguments arguments))

             (check-content (result)
               (smoke--check (and (eql 0 (search "1 match;" result))
                                  (search ", 2 indexed." result)
                                  (search "clifff-index-needle.txt:2:" result)
                                  (search "FffSmokeUnique42" result)
                                  (not (search "unrelated.txt" result)))
                             "Unexpected indexed content result:~%~A" result)))
      (unwind-protect
           (progn
             (if cold-p
                 (smoke--check (not (probe-file cache)) "Cold cache already exists: ~A" cache)
                 (smoke--check (smoke--cache-populated-p cache) "Warm cache is missing: ~A" cache))
             (let ((files (request ':files '("**/clifff-index-needle.txt" :glob-p t))))
               (smoke--check (and (eql 0 (search "1 result shown, 1 matched, 2 indexed;" files))
                                  (search "clifff-index-needle.txt" files)
                                  (not (search "unrelated.txt" files)))
                             "Unexpected indexed file result:~%~A" files))
             (setf process (clifff:worker-process worker))
             (smoke--check (and process (uiop:process-alive-p process)) "Search helper is not alive.")
             (setf input (uiop:process-info-input process)
                   output (uiop:process-info-output process))
             (smoke--check (and input output (open-stream-p input) (open-stream-p output))
                           "Search helper protocol streams are not open.")
             (smoke--check (smoke--cache-populated-p cache) "Native cache was not populated: ~A" cache)
             (format t "CLIFFF PASS: ~A indexed file lookup (~A).~%"
                     (if cold-p "cold" "reopened cache") cache)
             (let* ((arguments '("FffSmokeUnique42" :mode :plain))
                    (first-result (request ':content arguments))
                    (warm-result (request ':content arguments)))
               (check-content first-result)
               (check-content warm-result)
               (smoke--check (and (eq process (clifff:worker-process worker))
                                  (uiop:process-alive-p process))
                             "Warm query did not reuse the same live search helper."))
             (format t "CLIFFF PASS: repeated indexed content lookup reused the same helper.~%"))
        (clifff:worker-close worker))
      (smoke--check (and (null (clifff:worker-process worker))
                         (not (uiop:process-alive-p process))
                         (not (open-stream-p input))
                         (not (open-stream-p output)))
                    "Search helper or protocol streams survived worker-close.")
      (format t "CLIFFF PASS: worker-close reaped the helper and closed both protocol pipes.~%"))))

(destructuring-bind (worker-file private-root) (uiop:command-line-arguments)
  (let* ((worker (smoke--absolute-file worker-file))
         (runtime (smoke--absolute-file (uiop:getenv "AUTOLITH_SBCL")))
         (library (smoke--absolute-file (uiop:getenv "AUTOLITH_FFF_LIBRARY")))
         (root (uiop:ensure-directory-pathname (truename private-root)))
         (fixture (merge-pathnames "workspace/" root))
         (cache (merge-pathnames "index/" root))
         (log (merge-pathnames "worker.log" root))
         (command (list (namestring runtime) "--script" (namestring worker))))
    (smoke--write-fixture fixture)
    (format t "CLIFFF: helper=~A DLL=~A~%" worker library)
    (smoke--worker-cycle command :library library :fixture fixture :cache cache :log log :cold-p t)
    (smoke--worker-cycle command :library library :fixture fixture :cache cache :log log :cold-p nil)))
