;;; autolith.el --- Develop in Autolith's image with sly and agent-shell  -*- lexical-binding: t; -*-

;; Author: Lambda Symbolics
;; URL: https://autolith.rocks
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: lisp, tools

;; This file is part of Autolith and shares its ISC license.

;;; Commentary:

;; `autolith-ide' opens one frame with three windows: your code, a sly
;; REPL connected to Autolith's own Lisp image, and an agent-shell
;; conversation with the same Autolith.  Whatever you define in the REPL
;; lives in the image the agent runs in, so it can inspect and advise on it.
;;
;; Autolith is started by agent-shell as `autolith acp' with three
;; environment variables:
;;
;;   AUTOLITH_SLYNK_DIRECTORY  this sly's own slynk/ directory, so the
;;                             server matches the client exactly
;;   AUTOLITH_SLYNK_PORT_FILE  where Autolith writes the Slynk port
;;   AUTOLITH_EMACS_SERVER     this Emacs's server socket, which enables
;;                             Autolith's emacs.* tools
;;
;; Requires sly and agent-shell.  `autolith-agent-shell' starts only the
;; conversation, with the Emacs tools but without Slynk.

;;; Code:

(require 'cl-lib)
(require 'server)

(declare-function agent-shell-make-agent-config "agent-shell")
(declare-function agent-shell--make-acp-client "agent-shell")
(declare-function agent-shell-start "agent-shell")
(declare-function sly-connect "sly")
(declare-function sly-slynk-path "sly")
(declare-function sly-current-connection "sly")
(declare-function sly-mrepl--find-buffer "sly-mrepl")
(defvar sly-connected-hook)

(defgroup autolith nil
  "Autolith, the live Common Lisp agent, inside Emacs."
  :group 'tools
  :prefix "autolith-")

(defcustom autolith-command '("autolith" "acp")
  "The command and arguments that start Autolith's ACP endpoint."
  :type '(repeat string))

(defcustom autolith-environment nil
  "Extra environment variables for Autolith, as \"NAME=VALUE\" strings."
  :type '(repeat string))

(defcustom autolith-slynk-timeout 120
  "Seconds to wait for Autolith's Slynk to report its port.
The first start compiles Slynk, which takes longer than later ones."
  :type 'integer)

(defvar autolith--pending nil
  "The pending IDE launch: a plist of :port-file, :code, :shell and :timer.")

;;;; Agent configuration

(defun autolith-agent-shell-config (&optional environment)
  "Return an agent-shell configuration that runs Autolith with ENVIRONMENT.
ENVIRONMENT is a list of \"NAME=VALUE\" strings added to
`autolith-environment'."
  (agent-shell-make-agent-config
   :identifier 'autolith
   :mode-line-name "Autolith"
   :buffer-name "Autolith"
   :shell-prompt "Autolith> "
   :shell-prompt-regexp "Autolith> "
   :client-maker (lambda (buffer)
                   (agent-shell--make-acp-client
                    :command (car autolith-command)
                    :command-params (cdr autolith-command)
                    :environment-variables (append environment autolith-environment)
                    :context-buffer buffer))
   :install-instructions "Install Autolith from https://autolith.rocks and sign in with `autolith auth'."))

;;;###autoload
(defun autolith-agent-shell ()
  "Start an agent-shell conversation with Autolith and its Emacs tools."
  (interactive)
  (autolith--require 'agent-shell)
  (agent-shell-start :config (autolith-agent-shell-config
                              (list (autolith--server-variable)))))

;;;; IDE

;;;###autoload
(defun autolith-ide ()
  "Develop in Autolith's image: code, a sly REPL and an Autolith conversation.
The current buffer stays as the code window.  Autolith starts under
agent-shell, serves its image over Slynk, and sly connects to it once
the port is known."
  (interactive)
  (autolith--require 'sly)
  (autolith--require 'agent-shell)
  (autolith--cancel-pending)
  (let* ((code (current-buffer))
         (port-file (make-temp-file "autolith-slynk-" nil ".port"))
         (shell (agent-shell-start
                 :config (autolith-agent-shell-config
                          (list (concat "AUTOLITH_SLYNK_DIRECTORY="
                                        (expand-file-name (sly-slynk-path)))
                                (concat "AUTOLITH_SLYNK_PORT_FILE=" port-file)
                                (autolith--server-variable))))))
    (setq autolith--pending (list :port-file port-file :code code :shell shell))
    (autolith--await-port (float-time))
    (message "Autolith is starting; sly connects once Slynk is ready.")))

(defun autolith-ide-reconnect ()
  "Connect sly again to the Slynk of the running `autolith-ide' launch.
Use it after a checkpoint, which closes sly connections."
  (interactive)
  (let ((port (and autolith--pending
                   (autolith--read-port (plist-get autolith--pending :port-file)))))
    (unless (integerp port)
      (user-error "No running Autolith Slynk is known; start one with M-x autolith-ide"))
    (sly-connect "127.0.0.1" port)))

;;;; Internals

(defun autolith--require (feature)
  "Load FEATURE or explain that Autolith needs it."
  (unless (require feature nil t)
    (user-error "Autolith needs the %s package; install it first" feature)))

(defun autolith--server-variable ()
  "Start this Emacs's server when needed and return AUTOLITH_EMACS_SERVER=SOCKET."
  (unless (server-running-p)
    (server-start))
  (when server-use-tcp
    (user-error "Autolith's Emacs tools need a local-socket server; unset `server-use-tcp'"))
  (concat "AUTOLITH_EMACS_SERVER=" (expand-file-name server-name server-socket-dir)))

(defun autolith--read-port (port-file)
  "Return the port in PORT-FILE, an error string, or nil while it is empty."
  (when (file-exists-p port-file)
    (let ((text (string-trim (with-temp-buffer
                               (insert-file-contents port-file)
                               (buffer-string)))))
      (cond ((string-match-p "\\`[0-9]+\\'" text) (string-to-number text))
            ((string-prefix-p "error: " text) (substring text 7))
            (t nil)))))

(defun autolith--await-port (started)
  "Poll the pending port file, connecting sly when it names a port.
STARTED is when the launch began, for `autolith-slynk-timeout'."
  (when autolith--pending
    (let ((port (autolith--read-port (plist-get autolith--pending :port-file))))
      (cond
       ((integerp port)
        (autolith--connect port))
       ((stringp port)
        (setq autolith--pending nil)
        (message "Autolith could not start Slynk: %s" port))
       ((> (- (float-time) started) autolith-slynk-timeout)
        (setq autolith--pending nil)
        (message "Autolith's Slynk did not start within %d seconds" autolith-slynk-timeout))
       (t
        (plist-put autolith--pending :timer
                   (run-at-time 0.3 nil #'autolith--await-port started)))))))

(defun autolith--connect (port)
  "Connect sly to PORT and lay the frame out once its REPL exists."
  (let ((pending autolith--pending))
    (cl-labels ((arrange ()
                  (remove-hook 'sly-connected-hook #'arrange)
                  (let ((repl (sly-mrepl--find-buffer (sly-current-connection))))
                    (autolith--layout (plist-get pending :code)
                                      repl
                                      (plist-get pending :shell)))))
      (add-hook 'sly-connected-hook #'arrange 90)
      (sly-connect "127.0.0.1" port))))

(defun autolith--layout (code repl shell)
  "Show CODE on the left, REPL top right and SHELL bottom right."
  (when (and (buffer-live-p code) (buffer-live-p shell))
    (delete-other-windows)
    (switch-to-buffer code)
    (let* ((right (split-window-right))
           (bottom (split-window right nil 'below)))
      (set-window-buffer right (if (buffer-live-p repl) repl shell))
      (set-window-buffer bottom shell))))

(defun autolith--cancel-pending ()
  "Stop waiting for a previous launch's Slynk."
  (when-let* ((timer (plist-get autolith--pending :timer)))
    (cancel-timer timer))
  (setq autolith--pending nil))

(provide 'autolith)
;;; autolith.el ends here
