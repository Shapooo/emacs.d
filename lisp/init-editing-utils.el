;;; init-editing-utils.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package nyan-mode
  :config
  (nyan-mode))

(use-package rainbow-delimiters
  :config
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package anzu
  :diminish
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook (after-init . global-anzu-mode))

;; Minor mode to aggressively keep your code always indented
;; (use-package aggressive-indent
;;   :diminish
;;   :hook ((after-init . global-aggressive-indent-mode)
;;          ;; FIXME: Disable in big files due to the performance issues
;;          ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
;;          (find-file . (lambda ()
;;                         (if (> (buffer-size) (* 3000 80))
;;                             (aggressive-indent-mode -1)))))
;;   :config
;;   ;; Disable in some modes
;;   (dolist (mode '(asm-mode web-mode html-mode css-mode go-mode prolog-inferior-mode))
;;     (push mode aggressive-indent-excluded-modes))

;;   ;; Disable in some commands
;;   (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t)

;;   ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
;;   (add-to-list
;;    'aggressive-indent-dont-indent-if
;;    '(and (derived-mode-p 'c-mode 'c++-mode 'csharp-mode
;;                          'java-mode 'go-mode 'swift-mode)
;;          (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
;;                              (thing-at-point 'line))))))

;; A comprehensive visual interface to diff & patch
(use-package ediff
  :ensure nil
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

;; Hungry deletion
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line] . mwim-end-of-code-or-line)))

(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode)
  :config (setq global-auto-revert-non-file-buffers t
                auto-revert-verbose nil))

;; Handling capitalized subwords in a nomenclature
(use-package subword
  :ensure nil
  :diminish
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))

(use-package show-paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :init (setq show-paren-delay 0))

;; Hideshow
;; (use-package hideshow
;;   :ensure nil
;;   :diminish hs-minor-mode
;;   :bind (:map hs-minor-mode-map
;;          ("C-`" . hs-toggle-hiding)))

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
  :diminish
  :config (which-key-mode))


(provide 'init-editing-utils)
;;; init-editing-utils.el ends here
