;;; init-ui.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1) )

(defun shapo/set-gui-font ()
  (set-face-attribute 'default nil
                      :family "Source Code Pro"
                      :height 180
                      :weight 'normal
                      :width 'normal)

  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset (font-spec :family "WenQuanYi Micro Hei"
                                         :height 180))))
(when (display-graphic-p)
  (shapo/set-gui-font))
(add-hook 'server-after-make-frame-hook 'shapo/set-gui-font)

;; (use-package display-line-numbers
;;   :ensure nil
;;   :hook (prog-mode . display-line-numbers-mode))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; (use-package all-the-icons
;;   :if (display-graphic-p)
;;   :init (unless (or sys/win32p (font-installed-p "all-the-icons"))
;;           (all-the-icons-install-fonts t)))

(provide 'init-ui)
;;; init-ui.el ends here
