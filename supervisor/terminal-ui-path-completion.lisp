(in-package #:autolith-supervisor)

;;;; -- Workspace Path Completion --

(defparameter *terminal-path-completion-limit* 32
  "The most workspace path candidates returned for one token.")

(serapeum/types:-> terminal-path--whitespace-p (character) boolean)
(defun terminal-path--whitespace-p (character)
  "Return true when CHARACTER is a whitespace token boundary."
  (and (find character '(#\Space #\Tab #\Newline #\Return #\Page))
       t))

(serapeum/types:-> terminal-path--boundary-p (character) boolean)
(defun terminal-path--boundary-p (character)
  "Return true when CHARACTER ends an @ path token."
  (or (terminal-path--whitespace-p character)
      (and (find character '(#\" #\' #\( #\)))
           t)))

(serapeum/types:-> terminal-path-token
    (string (integer 0))
    (values (option (integer 0)) (option (integer 0)) (option string)))
(defun terminal-path-token (text cursor)
  "Return the @ path token at CURSOR in TEXT.

Values are start index, end index, and the token string. CURSOR may equal
TEXT's length. A cursor on a boundary does not use the previous token. Only a
token that starts with @ is returned."
  (let ((length (length text)))
    (unless (<= 0 cursor length)
      (return-from terminal-path-token (values nil nil nil)))
    (when (and (< cursor length)
               (terminal-path--boundary-p (char text cursor)))
      (return-from terminal-path-token (values nil nil nil)))
    (let* ((start
             (let ((separator
                     (position-if #'terminal-path--boundary-p
                                  text
                                  :from-end t
                                  :end      cursor)))
               (if separator
                   (1+ separator)
                   0)))
           (end
             (or (position-if #'terminal-path--boundary-p text :start cursor)
                 length))
           (token (subseq text start end)))
      (if (terminal-path-token-p token)
          (values start end token)
          (values nil nil nil)))))

(serapeum/types:-> terminal-path-token-p (string) boolean)
(defun terminal-path-token-p (token)
  "Return true when TOKEN is an @ workspace path mention."
  (and (terminal-ui-non-empty-string-p token) (char= (char token 0) #\@) t))

(serapeum/types:-> terminal-path--relative (string) string)
(defun terminal-path--relative (token)
  "Return TOKEN's workspace-relative remainder after @."
  (let ((body (subseq token 1)))
    (if (and (plusp (length body))
             (char= (char body 0) #\/))
        (subseq body 1)
        body)))

(serapeum/types:-> terminal-path--unix-namestring (pathname) string)
(defun terminal-path--unix-namestring (pathname)
  "Return PATHNAME as a slash-separated namestring."
  (substitute #\/ #\\ (namestring pathname)))

(serapeum/types:-> terminal-path--safe-relative-p (string) boolean)
(defun terminal-path--safe-relative-p (relative)
  "Return true when RELATIVE stays under the completion root."
  (not (or (string= relative "..")
           (uiop:string-prefix-p "../" relative)
           (search "/../" relative))))

(serapeum/types:-> terminal-path--child-basename (pathname boolean) string)
(defun terminal-path--child-basename (pathname directory-p)
  "Return PATHNAME's visible name, with a trailing slash for directories."
  (if directory-p
      (format nil "~A/"
              (first (last (pathname-directory pathname))))
      (file-namestring pathname)))

(serapeum/types:-> terminal-path--hidden-name-p (string) boolean)
(defun terminal-path--hidden-name-p (name)
  "Return true when NAME is a dotted filesystem entry."
  (and (plusp (length name))
       (char= (char name 0) #\.)
       t))

(serapeum/types:-> terminal-path--skip-child-p (string string) boolean)
(defun terminal-path--skip-child-p (name prefix)
  "Return true when NAME should not appear for PREFIX."
  (let ((bare (string-right-trim "/" name)))
    (or (and (member bare '("." ".." ".git") :test #'string=)
             t)
        (and (terminal-path--hidden-name-p bare)
             (not (uiop:string-prefix-p "." prefix))))))

(serapeum/types:-> terminal-path--under-root-p (pathname pathname) boolean)
(defun terminal-path--under-root-p (pathname root)
  "Return true when PATHNAME is ROOT or a descendant of ROOT."
  (let ((enough (uiop:enough-pathname pathname root)))
    (or (zerop (length (namestring enough)))
        (and (not (uiop:absolute-pathname-p enough))
             (terminal-path--safe-relative-p
              (terminal-path--unix-namestring enough))))))

(serapeum/types:-> terminal-path--list-children (pathname) list)
(defun terminal-path--list-children (directory)
  "Return (pathname . directory-p) pairs directly under DIRECTORY."
  (when (uiop:directory-exists-p directory)
    (append
     (mapcar (lambda (pathname) (cons pathname t))
             (uiop:subdirectories directory))
     (mapcar (lambda (pathname) (cons pathname nil))
             (uiop:directory-files directory)))))

(serapeum/types:-> terminal-path--search-entries
    (list &key (:token-start (integer 0)) (:token-end (integer 0)) (:limit (integer 1)))
    list)
(defun terminal-path--search-entries (paths &key token-start token-end limit)
  "Return insert-only completion plists for ranked workspace PATHS."
  (let ((entries
         (loop for path in paths
               when (terminal-ui-non-empty-string-p path)
               collect (list :name path :argument nil :description
                             (if (uiop/utility:string-suffix-p path "/")
                                 "directory"
                                 "file")
                             :kind ':path :submit-p nil :token-start token-start
                             :token-end token-end))))
    (subseq entries 0 (min limit (length entries)))))

(serapeum/types:-> terminal-path-completion-entries
    (pathname string &key (:token-start (integer 0)) (:token-end (integer 0))
              (:limit (integer 1)) (:search-function (option function)))
    list)
(defun terminal-path-completion-entries
       (root token
        &key (token-start 0) (token-end (length token))
        (limit *terminal-path-completion-limit*) search-function)
  "Return path completion plists for an @ TOKEN under workspace ROOT.

The @ is not part of the inserted name. SEARCH-FUNCTION, when provided, is a
fuzzy path search of QUERY. Directories keep a trailing slash. Entries do not
submit."
  (unless (terminal-path-token-p token)
    (return-from terminal-path-completion-entries nil))
  (when search-function
    (return-from terminal-path-completion-entries
      (terminal-path--search-entries
       (or (funcall search-function (terminal-path--relative token)) 'nil) :token-start
       token-start :token-end token-end :limit limit)))
  (let ((root (uiop/pathname:ensure-directory-pathname root)))
    (unless (uiop/filesystem:directory-exists-p root)
      (return-from terminal-path-completion-entries nil))
    (setf root
            (uiop/pathname:ensure-directory-pathname
             (terminal-ui-host-truename *terminal-ui-host* root)))
    (let* ((relative         (terminal-path--relative token))
           (slash            (position #\/ relative :from-end t))
           (directory-relative
            (if slash
                (subseq relative 0 (1+ slash))
                ""))
           (prefix
            (if slash
                (subseq relative (1+ slash))
                relative))
           (directory
            (uiop/pathname:ensure-directory-pathname
             (uiop/pathname:merge-pathnames* directory-relative root)))
           (downcased-prefix (string-downcase prefix))
           (matches
            (and (terminal-path--under-root-p directory root)
                 (loop for (pathname . directory-p) in (terminal-path--list-children
                                                        directory)
                       for basename = (terminal-path--child-basename pathname
                                                                     directory-p)
                       for enough = (uiop/pathname:enough-pathname pathname root)
                       for relative-name = (and enough
                                                (not
                                                 (uiop/pathname:absolute-pathname-p
                                                  enough))
                                                (terminal-path--unix-namestring enough))
                       when (and relative-name (plusp (length relative-name))
                                 (terminal-path--safe-relative-p relative-name)
                                 (not (terminal-path--skip-child-p basename prefix))
                                 (uiop/utility:string-prefix-p downcased-prefix
                                                               (string-downcase
                                                                basename)))
                       collect (let ((insert
                                      (if (and directory-p
                                               (not
                                                (uiop/utility:string-suffix-p
                                                 relative-name "/")))
                                          (concatenate 'string relative-name "/")
                                          relative-name)))
                                 (list :name insert :argument nil :description
                                       (if directory-p
                                           "directory"
                                           "file")
                                       :kind ':path :submit-p nil :token-start
                                       token-start :token-end token-end))))))
      (subseq
       (sort matches
             (lambda (left right)
               (let ((left-directory-p  (string= (getf left :description) "directory"))
                     (right-directory-p (string= (getf right :description) "directory")))
                 (cond ((and left-directory-p (not right-directory-p))
                        t)
                       ((and right-directory-p (not left-directory-p))
                        nil)
                       (t
                        (string-lessp (getf left :name) (getf right :name)))))))
       0 (min limit (length matches))))))
