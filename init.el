;;; init.el --- Entry of whole configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(when (version< emacs-version "25.1")
  (error "This requires Emacs 25.1 and above!"))

;;
;; Speed up startup
;;

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

(unless (or (daemonp) noninteractive init-file-debug)
  ;; Prevent flashing of messages at startup
  (when (display-graphic-p)
    (setq-default inhibit-redisplay t
                  inhibit-message t)
    (defun reset-inhibit-vars ()
      (setq-default inhibit-redisplay nil
                    inhibit-message nil)
      (redraw-frame))
    (add-hook 'window-setup-hook #'reset-inhibit-vars)
    (define-advice startup--load-user-init-file (:after (&rest _) reset-inhibit-vars)
      (and init-file-had-error (reset-inhibit-vars))))

  ;; Suppress file handlers operations at startup
  ;; `file-name-handler-alist' is consulted on each call to `require' and `load'
  (let ((old-value file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist old-value))))
              101)))

;; Load path
;; Optimize: Force "lisp" and "site-lisp" at head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'.

Don't put large files in `site-lisp' directory, e.g. EAF.
Otherwise the startup will be very slow."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;; Packages
;; Without this comment Emacs25 adds (package-initialize) here
(require 'init-package)

;; Preferences
(require 'init-base)

(require 'init-ivy)
(require 'init-company)
(require 'init-windows)
(require 'init-ibuffer)
(require 'init-edit)
(require 'init-yasnippet)
(require 'init-flycheck)
(require 'init-ui)
(require 'init-lsp)
(require 'init-org)
(require 'init-sessions)
(require 'init-git)
(require 'init-dired)

;; Programming Language
(require 'init-prog)
(require 'init-cc-mode)
(require 'init-latex)
(require 'init-haskell)
(require 'init-lisp)
(require 'init-rust)
(require 'init-python)
(require 'init-go)
;; (require 'init-dap)



;;; init.el ends here
