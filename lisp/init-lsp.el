;; init-lsp.el --- Initialize LSP configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package lsp-mode
  :diminish
  :hook (c-mode-common . (lambda () (lsp-deferred)))
  ;; (python-mode . (lambda () (require 'lsp-python-ms) (lsp-deferred)))
  ;; :hook (c-common-mode . lsp-deferred-hook)
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
    (setq lsp-clients-python-library-directories '("/usr/local/" "/usr/")))
  :commands lsp
  )

(use-package company-lsp
  :init (setq company-lsp-cache-candidates 'auto)
  :config (push 'company-lsp company-backends)
  (add-to-list 'company-lsp-filter-candidates '(mspyls)))

(use-package lsp-python-ms
  :hook (python-mode . (lambda () (require 'lsp-python-ms)
                         (lsp)))
  :init
  (setq lsp-python-ms-executable "~/Git/python-language-server/output/bin/Debug/Microsoft.Python.LanguageServer"))

(use-package lsp-ivy
  :after lsp-mode
  :bind (:map lsp-mode-map
              ([remap xref-find-apropos] . lsp-ivy-workspace-symbol)
              ("C-s-." . lsp-ivy-global-workspace-symbol)))

(provide 'init-lsp)
;;; init-lsp.el ends here
