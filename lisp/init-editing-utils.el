(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package anzu
  :ensure t
  :diminish
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook (after-init . global-anzu-mode))

(use-package electric-pair-mode
  :ensure nil
  :hook after-init
  :diminish)

(use-package electric-indent-mode
  :ensure nil
  :hook after-init
  :diminish)

(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(global-set-key (kbd "RET") 'newline-and-indent)
(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "S-<return>") 'sanityinc/newline-at-end-of-line)

(setq inhibit-splash-screen t)
(setq-default initial-scratch-message
              (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

(use-package which-key
  :diminish t
  :config (which-key-mode))

(add-hook 'after-init-hook 'show-paren-mode)
(setq show-paren-delay 0)


(provide 'init-editing-utils)
