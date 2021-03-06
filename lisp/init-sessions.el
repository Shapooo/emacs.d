
(use-package desktop
  :ensure nil
  :init (setq desktop-path (list user-emacs-directory)
              desktop-auto-save-timeout 600
              desktop-load-locked-desktop t)
  :config
  (desktop-save-mode 1)
  (setq desktop-globals-to-save
        '((comint-input-ring        . 50)
          (compile-history          . 30)
          desktop-missing-file-warning
          (dired-regexp-history     . 20)
          (extended-command-history . 30)
          (face-name-history        . 20)
          (file-name-history        . 100)
          (grep-find-history        . 30)
          (grep-history             . 30)
          (ido-buffer-history       . 100)
          (ido-last-directory-list  . 100)
          (ido-work-directory-list  . 100)
          (ido-work-file-list       . 100)
          (ivy-history              . 100)
          (magit-read-rev-history   . 50)
          (minibuffer-history       . 50)
          (org-clock-history        . 50)
          (org-refile-history       . 50)
          (org-tags-history         . 50)
          (query-replace-history    . 60)
          (read-expression-history  . 60)
          (regexp-history           . 60)
          (regexp-search-ring       . 20)
          register-alist
          (search-ring              . 20)
          (shell-command-history    . 50)
          tags-file-name
          tags-table-list)))

(provide 'init-sessions)
