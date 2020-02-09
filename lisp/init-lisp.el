;;; init-lisp.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lispy
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1))))

(provide 'init-lisp)
;;; init-lisp ends here
