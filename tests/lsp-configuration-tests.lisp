(in-package #:autolith)

;;;; -- Native LSP Configuration Tests --

(-> lsp-configuration-tests--write (configuration string) pathname)
(defun lsp-configuration-tests--write (configuration contents)
  "Write CONTENTS to CONFIGURATION's native LSP file."
  (test-write-text (lsp-configuration-path configuration) contents :external-format ':default))

(-> lsp-configuration-tests--signals-p (configuration string) boolean)
(defun lsp-configuration-tests--signals-p (configuration contents)
  "Return true when CONTENTS is rejected as LSP configuration."
  (lsp-configuration-tests--write configuration contents)
  (handler-case
      (progn (lsp-load-configurations configuration) nil)
    (lsp-configuration-error () t)))

(-> test-lsp-configuration () null)
(defun test-lsp-configuration ()
  "Test strict native LSP configuration decoding."
  (with-test-configuration (configuration)
    (test-assert (null (lsp-load-configurations configuration))
                 "missing lsp.sexp returns NIL")
    (test-assert (equal (file-namestring (lsp-configuration-path configuration)) "lsp.sexp")
                 "LSP configuration has the expected filename")
    (test-assert (lsp-configuration-tests--signals-p configuration
                  "(:version 2 :servers ())")
                 "library configuration failures map to the application condition"))
  nil)
