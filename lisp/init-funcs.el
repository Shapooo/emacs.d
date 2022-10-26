;;; init-funcs.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)

(require 'init-const)
(require 'init-custom)


(unless (fboundp 'caadr)
  (defalias 'caadr #'cl-caadr))



;; (eval-when-compile
;;   (require 'init-const))

;; Font
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))


;; Dos2Unix/Unix2Dos
(defun dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun delete-carrage-returns ()
  "Delete `^M' characters in the buffer.
Same as `replace-string C-q C-m RET RET'."
  (interactive)
  (save-excursion
    (goto-char 0)
    (while (search-forward "\r" nil :noerror)
      (replace-match ""))))

;; File and buffer
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))
(global-set-key (kbd "C-x K") #'delete-this-file)

(defun rename-this-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (if-let ((filename (if (equal major-mode 'dired-mode)
                         default-directory
                       (buffer-file-name))))
      (progn
        (kill-new filename)
        (message "Copied '%s'" filename))
    (warn "Current buffer is not attached to a file!")))

;; Reload configurations
(defun reload-init-file ()
  "Reload Emacs configurations."
  (interactive)
  (load user-init-file))
;; (defalias 'centaur-reload-init-file #'reload-init-file)
(global-set-key (kbd "C-c C-l") #'reload-init-file)

(defun lc-format-testcase ()
  "Format leetcode testcase to match specific PL syntax."
  (interactive)
  (if (region-active-p)
      (let* ((from (region-beginning))
             (to (region-end))
             (testcase-string (buffer-substring-no-properties from to)))

        (setq lang-string (cond ((eq 'rustic-mode major-mode) "rust")
                                ((eq 'c++-mode major-mode) "cpp")
                                ("")))
        (setq output-string (shell-command-to-string (format "python ~/.config/emacs/lisp/leetcode-formater.py '%s' '%s'" lang-string testcase-string)))
        (save-excursion (delete-region from to)
                        (goto-char from)
                        (insert output-string)))
    nil))

(provide 'init-funcs)
;;; init-funcs.el ends here
