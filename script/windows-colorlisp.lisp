(require :asdf)
(load (merge-pathnames "roots.lisp" (uiop:pathname-directory-pathname *load-truename*)))

;;;; -- Windows ColorLisp Release Library --

(defun windows-colorlisp--unsigned (bytes offset &optional (count 2))
  "Read COUNT little-endian octets from BYTES at OFFSET."
  (loop for index below count
        sum (ash (aref bytes (+ offset index)) (* index 8))))

(defun windows-colorlisp--validate-image (pathname)
  "Require PATHNAME to contain an x64 PE32+ DLL before invoking the loader."
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (let ((dos (make-array 64 :element-type '(unsigned-byte 8)))
          (pe  (make-array 26 :element-type '(unsigned-byte 8))))
      (unless (and (= (read-sequence dos stream) 64)
                   (= (windows-colorlisp--unsigned dos 0) #x5a4d))
        (error "ColorLisp library is not a PE image: ~A" pathname))
      (let ((offset (windows-colorlisp--unsigned dos 60 4)))
        (unless (and (<= 64 offset (- (file-length stream) 26))
                     (file-position stream offset)
                     (= (read-sequence pe stream) 26)
                     (= (windows-colorlisp--unsigned pe 0 4) #x4550)
                     (= (windows-colorlisp--unsigned pe 4) #x8664)
                     (>= (windows-colorlisp--unsigned pe 20) 2)
                     (logtest #x2000 (windows-colorlisp--unsigned pe 22))
                     (= (windows-colorlisp--unsigned pe 24) #x20b))
          (error "ColorLisp library is not an x64 PE32+ DLL: ~A" pathname))))))

(defun windows-colorlisp--highlight-probe (pathname)
  "Exercise the exact DLL at PATHNAME with composer input and two grammars."
  (autolith-script-setenv "COLORLISP_NATIVE_LIBRARY" (uiop:native-namestring pathname))
  (uiop:symbol-call '#:colorlisp '#:highlight-spans "(" :language ':common-lisp)
  (dolist (case '((:common-lisp "(defun answer () 42)")
                  (:rust "fn main() { 42 }")))
    (unless (find ':number
                  (uiop:symbol-call '#:colorlisp '#:highlight-spans
                                    (second case) :language (first case))
                  :key (lambda (span)
                         (uiop:symbol-call '#:colorlisp '#:span-category span)))
      (error "Packaged ColorLisp did not highlight a ~A number." (first case))))
  (format t "~&Packaged ColorLisp highlighting passed: ~A~%" pathname))

(let* ((arguments (uiop:command-line-arguments))
       (action    (first arguments))
       (target    (and (second arguments)
                       (uiop:parse-native-namestring (second arguments))))
       (source    (uiop:pathname-parent-directory-pathname
                   (uiop:pathname-directory-pathname *load-truename*))))
  (unless (and (= (length arguments) 2)
               (member action '("copy" "validate") :test #'string=))
    (error "Usage: windows-colorlisp.lisp copy|validate DLL"))
  (load (merge-pathnames ".qlot/setup.lisp" source))
  (uiop:symbol-call '#:ql '#:quickload :colorlisp :silent t)
  (when (string= action "copy")
    (let ((library (uiop:symbol-call '#:colorlisp '#:native-library-path)))
      (windows-colorlisp--validate-image library)
      (ensure-directories-exist target)
      (uiop:copy-file library target)))
  (windows-colorlisp--validate-image target)
  (windows-colorlisp--highlight-probe target))
