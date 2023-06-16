;;; init-lisp.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lispy
  :diminish
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1))))

(use-package eldoc
  :ensure nil
  :diminish)

(provide 'init-lisp)
;;; init-lisp ends here
