;; init-lsp.el --- Initialize LSP configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package lsp-mode
  :diminish
  :hook (c-mode-common . (lambda () (lsp-deferred)))
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point))
  :init
  (setq lsp-auto-guess-root t           ; Detect project root
        lsp-keep-workspace-alive nil    ; Auto-kill LSP server
        lsp-prefer-flymake nil          ; Use lsp-ui and flycheck

        flymake-fringe-indecator-position 'right-fringe
        lsp-idle-delay 0.500)
  :config
  (setq lsp-clients-clangd-args
        '("--header-insertion=never"
          "-log=error"
          "--clang-tidy"
          "--fallback-style=mozilla"
          "--completion-style=bundled"))

  :commands lsp)

(use-package lsp-pyright
  :preface
  (defun lsp-pyright-format-buffer ()
    (interactive)
    (when (and (executable-find "yapf") buffer-file-name)
      (call-process "yapf" nil nil nil "-i" buffer-file-name)))
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (add-hook 'after-save-hook #'lsp-pyright-format-buffer t t)))
  :init (when (executable-find "python3")
          (setq lsp-pyright-python-executable-cmd "python3")))

(use-package lsp-ivy
  :after lsp-mode
  :bind (:map lsp-mode-map
              ([remap xref-find-apropos] . lsp-ivy-workspace-symbol)
              ("C-s-." . lsp-ivy-global-workspace-symbol)))

(use-package ccls
  :hook ((c-mode c++-mode) .
         (lambda () (require 'ccls))))

(provide 'init-lsp)
;;; init-lsp.el ends here
