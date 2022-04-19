;;; init-sessions.el --- Initialize desktop. -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Save sessions
;;
;;; Code:

(use-package desktop
  :ensure nil
  :init (setq desktop-path (list user-emacs-directory)
              desktop-auto-save-timeout 600
              desktop-load-locked-desktop t)
  :hook (kill-emacs . (lambda () (desktop-save user-emacs-directory)))
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
          (ivy-history              . 100)
          (magit-read-rev-history   . 50)
          (query-replace-history    . 60)
          (read-expression-history  . 60)
          (regexp-history           . 20)
          (regexp-search-ring       . 20)
          (search-ring              . 20)
          (shell-command-history    . 50)
          tags-file-name
          tags-table-list))
  (setq desktop-restore-frames nil))

(provide 'init-sessions)
;;; init-sessions.el ends here
