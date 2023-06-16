;;; init-package.el --- Initialize package configurations. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-const)
(require 'init-custom)
(require 'init-funcs)

;; Load `custom-file'
(and (file-readable-p custom-file) (load custom-file))

;; ;; Load custom-post file
;; (defun load-custom-post-file ()
;;   "Load custom-post file."
;;   (cond ((file-exists-p centaur-custom-post-org-file)
;;          (and (fboundp 'org-babel-load-file)
;;               (org-babel-load-file centaur-custom-post-org-file)))
;;         ((file-exists-p centaur-custom-post-file)
;;          (load centaur-custom-post-file))))
;; (add-hook 'after-init-hook #'load-custom-post-file)

;; HACK: DO NOT save package-selected-packages to `custom-file'.
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun my-package--save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to variable `custom-file'."
  (when value
    (setq package-selected-packages value))
  (unless after-init-time
    (add-hook 'after-init-hook #'my-package--save-selected-packages)))
(advice-add 'package--save-selected-packages :override #'my-package--save-selected-packages)

;; Set ELPA packages
(set-package-archives shapo-package-archives nil nil t)

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

(use-package diminish)
;; (use-package bind-key)
;; (use-package auto-package-update)

(provide 'init-package)
;;; init-package.el ends here
