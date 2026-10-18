(in-package #:autolith)

;;;; -- Plugin Store --

;; The plugin store is computed from the state root. No function in this file
;; creates a directory. Phase 4 creates checkouts when it fetches a plugin.

(-> plugin-store-root (configuration) pathname)
(defun plugin-store-root (configuration)
  "Return CONFIGURATION's per-user plugin store root."
  (merge-pathnames "plugins/" (config :state-root configuration)))

(-> plugin-store-checkout-root (configuration) pathname)
(defun plugin-store-checkout-root (configuration)
  "Return the directory that holds plugin checkouts."
  (merge-pathnames "checkouts/" (plugin-store-root configuration)))

(-> plugin-store--slug (string) string)
(defun plugin-store--slug (value)
  "Return a filesystem-safe lowercase slug for VALUE.

Letters and digits are kept. Every run of other characters becomes one
hyphen. Leading and trailing hyphens are removed. An empty result becomes
the word plugin."
  (let ((stream (make-string-output-stream))
        (started-p nil)
        (separator-p nil))
    (loop for character across value
          for lower = (char-downcase character)
          do (cond
               ((or (char<= #\a lower #\z)
                    (char<= #\0 lower #\9))
                (when (and separator-p started-p)
                  (write-char #\- stream))
                (setf separator-p nil
                      started-p t)
                (write-char lower stream))
               (t
                (when started-p
                  (setf separator-p t)))))
    (let ((slug (get-output-stream-string stream)))
      (if (plusp (length slug))
          slug
          "plugin"))))

(-> plugin-store-checkout-name (plugin-identity) string)
(defun plugin-store-checkout-name (identity)
  "Return the checkout directory name for IDENTITY."
  (format nil "~A-~A"
          (plugin-store--slug (plugin-identity-source identity))
          (plugin-store--slug (plugin-identity-commit identity))))

(-> plugin-store-checkout-directory (configuration plugin-identity) pathname)
(defun plugin-store-checkout-directory (configuration identity)
  "Return the checkout directory for IDENTITY under CONFIGURATION's store.

This only computes the path. It does not create the directory."
  (merge-pathnames
   (format nil "~A/" (plugin-store-checkout-name identity))
   (plugin-store-checkout-root configuration)))
