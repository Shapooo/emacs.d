;;; init-ui.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'init-const)
(require 'custom)
(require 'init-funcs)

(defun shapo/set-gui-font ()
  (set-face-attribute 'default nil
                      :family "Inconsolata"
                      ;; :family "Source Code Pro"
                      :height 160
                      :weight 'normal
                      :width 'normal)
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font "fontset-default"
                      charset (font-spec
                               :family "Source Han Sans"
                               :height 160)))
  (set-fontset-font "fontset-default"
                    'symbol (font-spec :family "Noto Emoji"))
  (set-fontset-font "fontset-default"
                    'symbol (font-spec :family "Noto Color Emoji") nil 'append))

(when (display-graphic-p)
  (shapo/set-gui-font))

(if shapo-server
    (add-hook 'server-after-make-frame-hook 'shapo/set-gui-font)
  (add-hook 'after-make-frame-functions 'shapo/set-gui-font))

;; Optimization
(setq idle-update-delay 1.0)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1) )

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t      ; if nil, bold is universally disabled
        doom-themes-enable-italic t)   ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t)

(provide 'init-ui)
;;; init-ui.el ends here
