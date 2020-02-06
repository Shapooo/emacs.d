
(use-package lispy
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1))))

(provide 'init-lisp)
