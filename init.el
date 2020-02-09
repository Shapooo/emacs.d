;;; -*- lexical-binding: t -*-
;;; init.el --- Entry of whole configuration
;;; Commentary:

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; Constants
(require 'init-const)

;; Functions
(require 'init-custom)

;; Customization
(require 'init-funcs)

;; Packages
;; Without this comment Emacs25 adds (package-initialize) here
(require 'init-package)

;; Preferences
(require 'init-basic)

(require 'init-ivy)
(require 'init-company)
(require 'init-windows)
(require 'init-ibuffer)
(require 'init-editing-utils)
(require 'init-cc-mode)
(require 'init-yasnippet)
(require 'init-flycheck)
(require 'init-lisp)
(require 'init-ui)
(require 'init-lsp)
(require 'init-org)
;;; init.el ends here
