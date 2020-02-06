;;; init.el --- Entry of whole configuration
;;; Commentary:

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'init-custom)
(require 'init-package)
(require 'init-basic)
(require 'init-lisp)
(require 'init-ui)
(require 'init-ivy)
(require 'init-company)
(require 'init-windows)
(require 'init-ibuffer)
(require 'init-editing-utils)
(require 'init-cc-mode)
