;; init-dired.el --- Initialize dired configurations.	-*- lexical-binding: t -*-

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
;; Directory configurations.
;;

;;; Code:

(require 'init-const)
(require 'init-funcs)

(defun open-dired-marked ()
  "Open marked files in dired."
  (interactive)
  (mapc 'find-file (dired-get-marked-files)))

;; Show git info in dired
(use-package dired-git-info
  :bind (:map dired-mode-map
         (")" . dired-git-info-mode)))

;; Allow rsync from dired buffers
(use-package dired-rsync
  :bind (:map dired-mode-map
         ("C-c C-r" . dired-rsync)))

;; Colorful dired
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (use-package fd-dired))

(provide 'init-dired)
;;; init-dired.el ends here
