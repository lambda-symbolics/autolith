;;;; -- Windows Stat --

;;; Safe filesystem stat for the supported Windows x86-64 SBCL runtime.

(in-package #:cl-user)

;; SBCL 2.6.6 wrap.c copies the UTF-8 pathname into char[MAX_PATH] with
;; strcpy. Bypass that wrapper, including its lstat alias, before ASDF loads.
;; The UCRT _stat64 ABI is documented in <sys/stat.h>: 56 bytes, with
;; 64-bit size and timestamps. Resolve stat and errno in the same DLL.
;; Remove this adapter when the minimum Windows SBCL handles unbounded stat paths.
#+win32
(progn
  (sb-alien:define-alien-type autolith-windows--stat64
    (sb-alien:struct autolith-windows--stat64
      (device sb-alien:unsigned-int)
      (inode sb-alien:unsigned-short)
      (mode sb-alien:unsigned-short)
      (links sb-alien:short)
      (user sb-alien:short)
      (group sb-alien:short)
      (special-device sb-alien:unsigned-int)
      (size sb-alien:long-long)
      (access-time sb-alien:long-long)
      (modification-time sb-alien:long-long)
      (creation-time sb-alien:long-long)))

  (defun autolith-windows--crt-entry (name)
    "Resolve NAME in the UCRT on this process, including after core reload."
    (let* ((module (sb-alien:alien-funcall
                    (sb-alien:extern-alien
                     "GetModuleHandleA"
                     (function sb-alien:system-area-pointer sb-alien:c-string))
                    "ucrtbase.dll"))
           (entry  (sb-alien:alien-funcall
                    (sb-alien:extern-alien
                     "GetProcAddress"
                     (function sb-alien:system-area-pointer
                               sb-alien:system-area-pointer sb-alien:c-string))
                    module name)))
      (when (zerop (sb-sys:sap-int entry))
        (error "UCRT entry point ~S is unavailable." name))
      entry))

  (defun autolith-windows--errno ()
    "Read errno from the same UCRT that supplies _wstat64."
    (sb-alien:deref
     (sb-alien:alien-funcall
      (sb-alien:sap-alien (autolith-windows--crt-entry "_errno")
                          (function (* sb-alien:int))))))

  (defun autolith-windows--path-error ()
    "Translate the last Win32 path error into UCRT errno."
    (let ((code (sb-alien:alien-funcall
                 (sb-alien:extern-alien "GetLastError"
                                        (function sb-alien:unsigned-int)))))
      (sb-alien:alien-funcall
       (sb-alien:sap-alien
        (autolith-windows--crt-entry "_dosmaperr")
        (function sb-alien:void sb-alien:unsigned-int))
       code)
      (values nil (autolith-windows--errno))))

  (defun autolith-windows--full-path (name)
    "Expand NAME with Windows drive/UNC rules. Return NIL and errno on failure."
    (let ((input (sb-ext:string-to-octets name :external-format ':utf-16le
                                             :null-terminate    t)))
      (sb-sys:with-pinned-objects (input)
        (loop with capacity = 512
              for output = (make-array (* 2 capacity)
                                       :element-type '(unsigned-byte 8))
              for used = (sb-sys:with-pinned-objects (output)
                           (sb-alien:alien-funcall
                            (sb-alien:extern-alien
                             "GetFullPathNameW"
                             (function sb-alien:unsigned-int
                                       sb-alien:system-area-pointer
                                       sb-alien:unsigned-int
                                       sb-alien:system-area-pointer
                                       sb-alien:system-area-pointer))
                            (sb-sys:vector-sap input) capacity
                            (sb-sys:vector-sap output) (sb-sys:int-sap 0)))
              do (cond
                   ((zerop used)
                    (return (autolith-windows--path-error)))
                   ((< used capacity)
                    (return (sb-ext:octets-to-string
                             output :external-format ':utf-16le :end (* 2 used))))
                   (t
                    (setf capacity used)))))))

  (defun autolith-windows--path-root-end (native)
    "Return the end of a drive, volume or UNC share root in NATIVE."
    (if (and (>= (length native) 8)
             (string-equal native "\\\\?\\UNC\\" :end1 8))
        (let* ((server-end (position #\\ native :start 8))
               (share-end  (and server-end
                                (position #\\ native :start (1+ server-end)))))
          (if share-end
              (1+ share-end)
              (length native)))
        (let ((end (position #\\ native :start 4)))
          (if end
              (1+ end)
              (length native)))))

  (defun autolith-windows--extended-path (name)
    "Return a wide-API pathname without imposing MAX_PATH.
Ordinary paths undergo Windows absolute-path normalization before adding the
extended prefix. Already extended/device paths retain their native semantics."
    (let* ((name   (substitute #\\ #\/ name))
           (native (if (or (and (>= (length name) 4)
                                (string= name "\\\\?\\" :end1 4))
                           (and (>= (length name) 4)
                                (string= name "\\\\.\\" :end1 4)))
                       name
                       (multiple-value-bind (full errno)
                           (autolith-windows--full-path name)
                         (unless full
                           (return-from autolith-windows--extended-path
                             (values nil errno)))
                         (if (and (>= (length full) 2)
                                  (string= full "\\\\" :end1 2))
                             (concatenate 'string "\\\\?\\UNC\\" (subseq full 2))
                             (concatenate 'string "\\\\?\\" full))))))
      (let ((end      (length native))
            (root-end (autolith-windows--path-root-end native)))
        (loop while (and (> end root-end)
                         (char= (char native (1- end)) #\\))
              do (decf end))
        (subseq native 0 end))))

  (defun autolith-windows--stat-device (native mode fallback)
    "Preserve CRT disk drive numbers after adding an extended-path prefix.
The UCRT recognizes a drive letter only at the start of its input. For disk
files on an extended drive path, report that drive rather than the current one."
    (let ((drive (and (>= (length native) 7)
                      (string= native "\\\\?\\" :end1 4)
                      (char= (char native 5) #\:)
                      (position (char-upcase (char native 4))
                                "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))))
      (if (and drive (member (logand mode #o170000) '(#o40000 #o100000)))
          drive
          fallback)))

  (defun autolith-windows--unix-stat (name)
    "Return SBCL's fourteen stat values, or NIL and UCRT errno.
As in SBCL's Windows wrapper, lstat follows the CRT's stat behavior and the
inode, uid, gid, block-size and block-count results are zero."
    (when (or (zerop (length name))
              (and (= (length name) 2)
                   (find (char-upcase (char name 0)) "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
                   (char= (char name 1) #\:)))
      (return-from autolith-windows--unix-stat (values nil 2)))
    (when (find #\Null name)
      (return-from autolith-windows--unix-stat (values nil 22)))
    (multiple-value-bind (native errno)
        (autolith-windows--extended-path name)
      (unless native
        (return-from autolith-windows--unix-stat (values nil errno)))
      (let ((wide  (sb-ext:string-to-octets native :external-format ':utf-16le
                                                 :null-terminate    t))
            (entry (autolith-windows--crt-entry "_wstat64")))
        (sb-alien:with-alien ((result autolith-windows--stat64))
          (let ((status
                  (sb-sys:with-pinned-objects (wide)
                    (sb-alien:alien-funcall
                     (sb-alien:sap-alien
                      entry
                      (function sb-alien:int sb-alien:system-area-pointer
                                (* autolith-windows--stat64)))
                     (sb-sys:vector-sap wide) (sb-alien:addr result)))))
            (if (minusp status)
                (values nil (autolith-windows--errno))
                (values t
                        (autolith-windows--stat-device
                         native (sb-alien:slot result 'mode)
                         (sb-alien:slot result 'device))
                        0
                        (sb-alien:slot result 'mode)
                        (sb-alien:slot result 'links)
                        0 0
                        (autolith-windows--stat-device
                         native (sb-alien:slot result 'mode)
                         (sb-alien:slot result 'special-device))
                        (sb-alien:slot result 'size)
                        (sb-alien:slot result 'access-time)
                        (sb-alien:slot result 'modification-time)
                        (sb-alien:slot result 'creation-time)
                        0 0)))))))

  (sb-alien:load-shared-object "ucrtbase.dll")
  (assert (= 56 (sb-alien:alien-size autolith-windows--stat64 :bytes)))
  (sb-ext:with-unlocked-packages (sb-unix)
    (defun sb-unix:unix-stat (name)
      "Filesystem stat through the bounds-safe Windows wide-character CRT."
      (autolith-windows--unix-stat name))
    (defun sb-unix:unix-lstat (name)
      "Windows lstat, with the same CRT target behavior as stat."
      (autolith-windows--unix-stat name))))
