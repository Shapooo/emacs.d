;;; init-cc-mode.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package cc-mode
  :ensure nil
  :init
  (setq-default c-basic-offset 2)
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "linux")))

  :config
  (use-package modern-cpp-font-lock
    :diminish
    :init (modern-c++-font-lock-global-mode t))
  (setq c-auto-newline nil))

;; (use-package clang-format
;;   :init
;;   (setq clang-format-style-option "mozilla"))

(use-package counsel-gtags
  :diminish
  :hook (c-mode-common . counsel-gtags-mode)
  :bind (:map counsel-gtags-mode-map
              ("M-t" . counsel-gtags-find-definition)
              ("M-r" . counsel-gtags-find-reference)
              ("M-s" . counsel-gtags-find-symbol)
              ("M-," . counsel-gtags-go-backward)))

(use-package cmake-mode
  :hook (cmake-mode .  (lambda ()
                         (add-to-list 'company-backends 'company-cmake))))

;; (add-hook 'cmake-mode-hook (lambda ()
;;                              (add-to-list 'company-backends 'company-cmake)))

(provide 'init-cc-mode)
;;; init-cc-mode.el ends here
