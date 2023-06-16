;; init-ui.el --- Better lookings and appearances.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2023 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Visual (UI) configurations for better lookings and appearances.
;;

;;; Code:

(require 'init-const)
(require 'custom)
(require 'init-funcs)

(defun shapo/set-gui-font ()
  "Setup gui font of Emacs."
  (set-face-attribute 'default nil
                      :family "Inconsolata"
                      ;; :family "Source Code Pro"
                      :height 160
                      :weight 'normal
                      :width 'normal)
  (dolist (charset '(kana han cjk-misc bopomofo))
    ;; (setq face-font-rescale-alist `((,font . 1.3)))
    (set-fontset-font t charset (font-spec :family "Source Han Sans")))
  (set-fontset-font t '(#x4e00 . #x9fff) (font-spec :family "Source Han Sans"))
  (set-fontset-font t 'symbol (font-spec :family "Noto Emoji") nil 'append)
  (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'append))

;; (when (display-graphic-p)
;;   (shapo/set-gui-font))
(if shapo-server
    (add-hook 'server-after-make-frame-hook 'shapo/set-gui-font)
  (add-hook 'window-setup-hook 'shapo/set-gui-font))

;; Optimization
(setq idle-update-delay 1.0)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; (when (fboundp 'tool-bar-mode)
;;   (tool-bar-mode -1))
;; (when (fboundp 'menu-bar-mode)
;;   (menu-bar-mode -1))
;; (when (fboundp 'scroll-bar-mode)
;;   (scroll-bar-mode -1) )
;; (when (fboundp 'horizontal-scroll-bar-mode)
;;   (horizontal-scroll-bar-mode -1))

;; Menu/Tool/Scroll bars
(unless emacs/>=27p
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist)
  (when (featurep 'ns)
    (push '(ns-transparent-titlebar . t) default-frame-alist)))

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

(use-package hide-mode-line
  :hook (((completion-list-mode
           completion-in-region-mode
           eshell-mode shell-mode
           ;; term-mode vterm-mode
           ;; treemacs-mode
           lsp-ui-imenu-mode) . hide-mode-line-mode)))


;; A minor-mode menu for mode-line
;; (use-package minions
;;   :hook (doom-modeline-mode . minions-mode))

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      ;; initial-scratch-message nil
      )

(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000
      auto-window-vscroll nil
      scroll-preserve-screen-position t)

(use-package page-break-lines
  :diminish
  :hook (after-init . global-page-break-lines-mode))

(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

(provide 'init-ui)
;;; init-ui.el ends here
