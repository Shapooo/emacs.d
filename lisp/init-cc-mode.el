;;; init-cc-mode.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package cc-mode
  :ensure nil
  :hook (c-mode-common . (lambda () (c-set-style "gnu")
                           (setq c-basic-offset 2)))
  ;; :init (setq-default c-basic-offset 2)
  :config
  (use-package modern-cpp-font-lock
    :diminish
    :init (modern-c++-font-lock-global-mode t))
  (setq c-auto-newline nil))

(use-package counsel-gtags
  :diminish
  :hook (c-mode-common . counsel-gtags-mode)
  :bind (:map counsel-gtags-mode-map
              ("M-t" . counsel-gtags-find-definition)
              ("M-r" . counsel-gtags-find-reference)
              ("M-s" . counsel-gtags-find-symbol)
              ("M-," . counsel-gtags-go-backward)))

(provide 'init-cc-mode)
;;; init-cc-mode.el ends here
