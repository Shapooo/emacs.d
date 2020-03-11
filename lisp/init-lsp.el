;; init-lsp.el --- Initialize LSP configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package lsp-mode
  :diminish
  ;; :hook (prog-mode . (lambda ()
  ;;                      (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
  ;;                        (lsp-deferred))))
  ;; :hook ((c-mode-common python-mode) . ((lsp-deferred)))
  ;; :hook (c-common-mode . lsp-deferred-hook)
  :hook ((c-mode-common python-mode) . (lambda () (lsp-deferred)))
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point))
  :init (setq lsp-auto-guess-root t        ; Detect project root
              lsp-keep-workspace-alive nil ; Auto-kill LSP server
              lsp-prefer-flymake nil       ; Use lsp-ui and flycheck

              flymake-fringe-indecator-position 'right-fringe)
  :config
  ;; Configure LSP clients
  (use-package lsp-clients
    :ensure nil
    :functions (lsp-format-buffer lsp-orgnize-imports)
    :init
    (setq lsp-clients-python-library-directories '("/usr/local/" "/usr/"))
    ;; (unless (executable-find "rls")
    ;;   (setq lsp-rust-rls-server-command '("rustup" "run" "stable" "rls")))
    )
  :commands lsp
  )

(use-package company-lsp
  :config (push 'company-lsp company-backends))

(use-package lsp-ivy
  :after lsp-mode
  :bind (:map lsp-mode-map
              ([remap xref-find-apropos] . lsp-ivy-workspace-symbol)
              ("C-s-." . lsp-ivy-global-workspace-symbol)))



(provide 'init-lsp)
;;; init-lsp.el ends here
