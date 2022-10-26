;;; init-package.el --- Initialize package configurations. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'init-const)
(require 'init-custom)
(require 'init-funcs)


(require 'package)

;; (setq package-archives '(("gnu"   . "http://mirrors.163.com/elpa/gnu/")
;;                          ("melpa" . "http://mirrors.163.com/elpa/melpa/")))
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)


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
(use-package bind-key)

(use-package auto-package-update)

(provide 'init-package)
;;; init-package.el ends here
