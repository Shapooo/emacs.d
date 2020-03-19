;;; init-ivy.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package counsel
  :diminish ivy-mode counsel-mode
  :bind (;; ("C-s"   . swiper-isearch)
         ;; ("C-r"   . swiper-isearch-backward)
         ;; ("s-f"   . swiper)
         ;; ("C-S-s" . swiper-all)

         ;; ("C-c C-r" . ivy-resume)
         ;; ("C-c v p" . ivy-push-view)
         ;; ("C-c v o" . ivy-pop-view)
         ;; ("C-c v ." . ivy-switch-view)

         :map counsel-mode-map
         ([remap swiper] . counsel-grep-or-swiper)
         ([remap swiper-backward] . counsel-grep-or-swiper-backward)
         ([remap dired] . counsel-dired)
         ([remap set-variable] . counsel-set-variable)
         ([remap insert-char] . counsel-unicode-char)

         ;; ("C-x C-r" . counsel-buffer-or-recentf)
         ("C-x j"   . counsel-mark-ring)
         ;; ("C-h F"   . counsel-faces)

         ;; ("C-c B" . counsel-bookmarked-directory)
         ;; ("C-c L" . counsel-load-library)
         ;; ("C-c O" . counsel-find-file-extern)
         ;; ("C-c P" . counsel-package)
         ("C-c R" . counsel-list-processes)
         ;; ("C-c f" . counsel-find-library)
         ;; ("C-c g" . counsel-grep)
         ;; ("C-c h" . counsel-command-history)
         ("C-c i" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ;; ("C-c l" . counsel-locate)
         ("C-c o" . counsel-outline)
         ("C-c r" . counsel-rg)
         ;; ("C-c z" . counsel-fzf)

         ;; ("C-c c B" . counsel-bookmarked-directory)
         ;; ("C-c c F" . counsel-faces)
         ;; ("C-c c L" . counsel-load-library)
         ;; ("C-c c O" . counsel-find-file-extern)
         ;; ("C-c c P" . counsel-package)
         ;; ("C-c c R" . counsel-list-processes)
         ;; ("C-c c a" . counsel-apropos)
         ;; ("C-c c e" . counsel-colors-emacs)
         ;; ("C-c c f" . counsel-find-library)
         ;; ("C-c c g" . counsel-grep)
         ;; ("C-c c h" . counsel-command-history)
         ;; ("C-c c i" . counsel-git)
         ;; ("C-c c j" . counsel-git-grep)
         ;; ("C-c c l" . counsel-locate)
         ;; ("C-c c m" . counsel-minibuffer-history)
         ;; ("C-c c o" . counsel-outline)
         ;; ("C-c c p" . counsel-pt)
         ;; ("C-c c r" . counsel-rg)
         ;; ("C-c c s" . counsel-ag)
         ;; ("C-c c t" . counsel-load-theme)
         ;; ("C-c c u" . counsel-unicode-char)
         ;; ("C-c c w" . counsel-colors-web)
         ;; ("C-c c v" . counsel-set-variable)
         ;; ("C-c c z" . counsel-fzf)

         :map ivy-minibuffer-map
         ("C-w" . ivy-yank-word)

         :map counsel-find-file-map
         ("C-h" . counsel-up-directory)

         :map swiper-map
         ("M-s" . swiper-isearch-toggle)
         ("M-%" . swiper-query-replace)

         :map isearch-mode-map
         ("M-s" . swiper-isearch-toggle))
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :init
  (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers

  (setq ivy-use-selectable-prompt t
        ivy-virtual-abbreviate 'fullpath
        ivy-use-virtual-buffers t       ; Enable bookmarks and recentf
        ivy-height 10
        ivy-fixed-height-minibuffer t
        ivy-count-format "(%d/%d) "
        ivy-on-del-error-function nil
        ivy-initial-inputs-alist nil)

  (setq swiper-action-recenter t)

  (setq counsel-find-file-at-point t
        counsel-yank-pop-separator "\n────────\n")

  ;; Use the faster search tool: ripgrep (`rg')
  (when (executable-find "rg")
    (setq counsel-grep-base-command "rg -S --no-heading --line-number --color never %s %s")
    (when (and sys/macp (executable-find "gls"))
      (setq counsel-find-file-occur-use-find nil
            counsel-find-file-occur-cmd
            "gls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 gls -d --group-directories-first")))

  :config
  (with-no-warnings

    ;; Enhance M-x
    (use-package amx
      :init (setq amx-history 20))

    ;; Better sorting and filtering
    (use-package prescient
      :commands prescient-persist-mode
      :init
      (setq prescient-filter-method '(literal regexp initialism fuzzy))
      (prescient-persist-mode 1))

    ;; Integrate yasnippet
    ;; (use-package ivy-yasnippet
    ;;   :commands ivy-yasnippet--preview
    ;;   :bind ())
    (use-package avy
      :bind ("C-:" . avy-goto-char)
      ("M-g f" . avy-goto-line))
    )
  )

(provide 'init-ivy)
;;; init-ivy ends here
