;;; init.el --- Entry of whole configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(when (version< emacs-version "25.1")
  (error "This requires Emacs 25.1 and above!"))

;; Speed up startup
(setq auto-mode-case-fold nil)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-percentage 0.1
                  gc-cons-threshold (* 50 1024 1024))))

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
(require 'init-basic)

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
(require 'init-cc-mode)
(require 'init-latex)
(require 'init-haskell)
(require 'init-lisp)
(require 'init-rust)
(require 'init-python)
(require 'init-go)
(require 'init-dap)

;;; init.el ends here
