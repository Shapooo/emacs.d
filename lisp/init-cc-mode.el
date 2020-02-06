
(use-package cc-mode
  :hook (c-mode-common . (lambda () (c-set-style "linux")))
  :init (setq-default c-basic-offset 2)
  :config
  (use-package modern-cpp-font-lock
    :diminish
    :init (modern-c++-font-lock-global-mode t))
  (defun my-common-cc-mode-setup ()
    "setup shared by all languages (java/groovy/c++ ...)"
    (setq c-basic-offset 2)
    ;; give me NO newline automatically after electric expressions are entered
    (setq c-auto-newline nil)

    ;; syntax-highlight aggressively
    ;; (setq font-lock-support-mode 'lazy-lock-mode)
    (setq lazy-lock-defer-contextually t)
    (setq lazy-lock-defer-time 0)

                                        ;make DEL take all previous whitespace with it
    (c-toggle-hungry-state 1)
    ;; indent
    ;; google "C/C++/Java code indentation in Emacs" for more advanced skills
    ;; C code:
    ;;   if(1) // press ENTER here, zero means no indentation
    (fix-c-indent-offset-according-to-syntax-context 'substatement 0)
    ;;   void fn() // press ENTER here, zero means no indentation
    (fix-c-indent-offset-according-to-syntax-context 'func-decl-cont 0))
  (add-hook 'c-mode-common-hook 'my-common-cc-mode-setup))

(use-package counsel-gtags
  :ensure t
  :diminish
  :hook (c-mode-common . counsel-gtags-mode)
  :bind (:map counsel-gtags-mode-map
              ("M-t" . counsel-gtags-find-definition)
              ("M-r" . counsel-gtags-find-reference)
              ("M-s" . counsel-gtags-find-symbol)
              ("M-," . counsel-gtags-go-backward)))

(provide 'init-cc-mode)
