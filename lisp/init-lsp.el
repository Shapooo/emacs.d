;; init-lsp.el --- Initialize LSP configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

;; Performace tuning
;; @see https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq read-process-output-max (* 1024 1024))
(setenv "LSP_USE_PLISTS" "true")

(use-package lsp-mode
  :diminish
  :defines (lsp-diagnostics-disabled-modes lsp-clients-python-library-directories)
  :commands (lsp-format-buffer lsp-organize-imports lsp lsp-deferred)
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                          (lsp-deferred))))
         (rustic-mode .  lsp-inlay-hints-mode))

  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point)
              ([remap xref-find-definitions] . lsp-find-definition)
              ([remap xref-find-references] . lsp-find-references))
  :init
  (setq lsp-auto-guess-root t           ; Detect project root
        lsp-keep-workspace-alive nil    ; Auto-kill LSP server
        lsp-prefer-flymake nil          ; Use lsp-ui and flycheck

        lsp-semantic-tokens-enable t
        lsp-progress-spinner-type 'progress-bar-filled
        lsp-signature-render-documentation nil
        lsp-modeline-code-actions-enable nil
        flymake-fringe-indecator-position 'right-fringe
        lsp-idle-delay 0.500
        lsp-diagnostics-disabled-modes '(markdown-mode gfm-mode)
        lsp-ui-sideline-enable nil

        ;; For clients
        lsp-clients-python-library-directories '("/usr/local/" "/usr/"))
  :config
  (setq lsp-clients-clangd-args
        '("--header-insertion=never"
          "-log=error"
          "--clang-tidy"
          "--fallback-style=mozilla"
          "--completion-style=bundled")
        lsp-inlay-hint-enable t
        lsp-rust-analyzer-completion-auto-import-enable nil
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-display-chaining-hints t)
  )

(use-package lsp-lens
  :ensure nil
  :diminish)

(use-package lsp-ui
  :custom-face
  (lsp-ui-sideline-code-action ((t (:inherit warning))))
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-doc-show-with-cursor nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-delay 0.1)
  ;; Set correct color to borders
  (defun my-lsp-ui-doc-set-border ()
    "Set the border color of lsp doc."
    (setq lsp-ui-doc-border
          (if (facep 'posframe-border)
              (face-background 'posframe-border nil t)
            (face-background 'region nil t))))
  (my-lsp-ui-doc-set-border)
  (add-hook 'after-load-theme-hook #'my-lsp-ui-doc-set-border t)
  :config
  (with-no-warnings
    ;; Display peek in child frame if possible
    ;; @see https://github.com/emacs-lsp/lsp-ui/issues/441
    (defvar lsp-ui-peek--buffer nil)
    (defun lsp-ui-peek--peek-display (fn src1 src2)
      (if (childframe-workable-p)
          (-let* ((win-width (frame-width))
                  (lsp-ui-peek-list-width (/ (frame-width) 2))
                  (string (-some--> (-zip-fill "" src1 src2)
                            (--map (lsp-ui-peek--adjust win-width it) it)
                            (-map-indexed 'lsp-ui-peek--make-line it)
                            (-concat it (lsp-ui-peek--make-footer)))))
            (setq lsp-ui-peek--buffer (get-buffer-create " *lsp-peek--buffer*"))
            (posframe-show lsp-ui-peek--buffer
                           :string (mapconcat 'identity string "")
                           :min-width (frame-width)
                           :internal-border-color (face-background 'posframe-border nil t)
                           :internal-border-width 1
                           :poshandler #'posframe-poshandler-frame-center))
        (funcall fn src1 src2)))
    (defun lsp-ui-peek--peek-destroy (fn)
      (if (childframe-workable-p)
          (progn
            (when (bufferp lsp-ui-peek--buffer)
              (posframe-hide lsp-ui-peek--buffer))
            (setq lsp-ui-peek--last-xref nil))
        (funcall fn)))
    (advice-add #'lsp-ui-peek--peek-new :around #'lsp-ui-peek--peek-display)
    (advice-add #'lsp-ui-peek--peek-hide :around #'lsp-ui-peek--peek-destroy)

    ;; Handle docs
    (defun my-lsp-ui-doc--handle-hr-lines nil
      (let (bolp next before after)
        (goto-char 1)
        (while (setq next (next-single-property-change (or next 1) 'markdown-hr))
          (when (get-text-property next 'markdown-hr)
            (goto-char next)
            (setq bolp (bolp)
                  before (char-before))
            (delete-region (point) (save-excursion (forward-visible-line 1) (point)))
            (setq after (char-after (1+ (point))))
            (insert
             (concat
              (and bolp (not (equal before ?\n)) (propertize "\n" 'face '(:height 0.5)))
              (propertize "\n" 'face '(:height 0.5))
              (propertize " "
                          ;; :align-to is added with lsp-ui-doc--fix-hr-props
                          'display '(space :height (1))
                          'lsp-ui-doc--replace-hr t
                          'face `(:background ,(face-foreground 'font-lock-comment-face nil t)))
              ;; :align-to is added here too
              (propertize " " 'display '(space :height (1)))
              (and (not (equal after ?\n)) (propertize " \n" 'face '(:height 0.5)))))))))
    (advice-add #'lsp-ui-doc--handle-hr-lines :override #'my-lsp-ui-doc--handle-hr-lines))
  )

(use-package lsp-pyright
  :preface
  ;; (defun lsp-pyright-format-buffer ()
  ;;   (interactive)
  ;;   (when (and (executable-find "black") buffer-file-name)
  ;;     (call-process "black" nil nil nil buffer-file-name)))
  :config (setq lsp-pyright-venv-path "/home/shapo/.virtualenvs/")
  (setq lsp-pyright-venv-directory ".virtualenvs")
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred)
                         ;; (add-hook 'after-save-hook #'lsp-pyright-format-buffer t t)
                         ))
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

(use-package lsp-haskell
  :after lsp-mode
  :hook (haskell-mode . (lambda () (require 'lsp-haskell))))

(use-package consult-lsp
  :diminish)

(provide 'init-lsp)
;;; init-lsp.el ends here
