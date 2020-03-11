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
                      ;; :family "Cascadia Code"
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

;; (set-face-attribute 'default nil
;;                     :family "Source Code Pro"
;;                     ;; :family "Cascadia Code"
;;                     :height 180
;;                     :weight 'normal
;;                     :width 'normal)

;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font)
;;                     charset (font-spec :family "WenQuanYi Micro Hei"
;;                                        :height 180)))

(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-colors")
                                        ; use the colorful treemacs theme
  ;; (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(provide 'init-ui)
;;; init-ui.el ends here
