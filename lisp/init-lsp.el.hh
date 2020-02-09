;; init-lsp.el --- Initialize LSP configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
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
;; Language Server Protocol (LSP) configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))
(setq centaur-lsp 'lsp-mode)
(pcase centaur-lsp
  ('eglot
   (use-package eglot
     :hook (prog-mode . eglot-ensure)))

  ('lsp-mode
   ;; Emacs client for the Language Server Protocol
   ;; https://github.com/emacs-lsp/lsp-mode#supported-languages
   (use-package lsp-mode
     :diminish
     :hook (prog-mode . (lambda ()
                          (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
                            (lsp-deferred))))
     :bind (:map lsp-mode-map
                 ("C-c C-d" . lsp-describe-thing-at-point))
     :init (setq lsp-auto-guess-root t        ; Detect project root
                 lsp-keep-workspace-alive nil ; Auto-kill LSP server
                 lsp-prefer-flymake nil       ; Use lsp-ui and flycheck
                 flymake-fringe-indicator-position 'right-fringe)
     :config
     ;; Configure LSP clients
     ;; (use-package lsp-clients
     ;;   :ensure nil
     ;;   :functions (lsp-format-buffer lsp-organize-imports)
     ;;   :hook (go-mode . (lambda ()
     ;;                      "Format and add/delete imports."
     ;;                      (add-hook 'before-save-hook #'lsp-format-buffer t t)
     ;;                      (add-hook 'before-save-hook #'lsp-organize-imports t t)))
     ;;   :init
     ;;   (setq lsp-clients-python-library-directories '("/usr/local/" "/usr/"))
     ;;   (unless (executable-find "rls")
     ;;     (setq lsp-rust-rls-server-command '("rustup" "run" "stable" "rls"))))
     )

   ;; Completion
   (use-package company-lsp
     :init (setq company-lsp-cache-candidates 'auto)
     :config
     ;; WORKAROUND:Fix tons of unrelated completion candidates shown
     ;; when a candidate is fulfilled
     ;; @see https://github.com/emacs-lsp/lsp-python-ms/issues/79
     ;; (add-to-list 'company-lsp-filter-candidates '(mspyls))

     (with-no-warnings
       (defun my-company-lsp--on-completion (response prefix)
         "Handle completion RESPONSE.

PREFIX is a string of the prefix when the completion is requested.

Return a list of strings as the completion candidates."
         (let* ((incomplete (and (hash-table-p response) (gethash "isIncomplete" response)))
                (items (cond ((hash-table-p response) (gethash "items" response))
                             ((sequencep response) response)))
                (candidates (mapcar (lambda (item)
                                      (company-lsp--make-candidate item prefix))
                                    (lsp--sort-completions items)))
                (server-id (lsp--client-server-id (lsp--workspace-client lsp--cur-workspace)))
                (should-filter (or (eq company-lsp-cache-candidates 'auto)
                                   (and (null company-lsp-cache-candidates)
                                        (company-lsp--get-config company-lsp-filter-candidates server-id)))))
           (when (null company-lsp--completion-cache)
             (add-hook 'company-completion-cancelled-hook #'company-lsp--cleanup-cache nil t)
             (add-hook 'company-completion-finished-hook #'company-lsp--cleanup-cache nil t))
           (when (eq company-lsp-cache-candidates 'auto)
             ;; Only cache candidates on auto mode. If it's t company caches the
             ;; candidates for us.
             (company-lsp--cache-put prefix (company-lsp--cache-item-new candidates incomplete)))
           (if should-filter
               (company-lsp--filter-candidates candidates prefix)
             candidates)))
       (advice-add #'company-lsp--on-completion :override #'my-company-lsp--on-completion)))

   ;; Ivy integration
   ;; (use-package lsp-ivy
   ;;   :after lsp-mode
   ;;   :bind (:map lsp-mode-map
   ;;               ([remap xref-find-apropos] . lsp-ivy-workspace-symbol)
   ;;               ("C-s-." . lsp-ivy-global-workspace-symbol)))
   ))

(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
