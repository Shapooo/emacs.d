;;; init-windows.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))

(use-package winner
  :bind (("C-c u" . winner-undo)
         ("C-c r" . winner-redo))
  :hook (after-init . winner-mode)
  :init (setq winner-boring-buffers '("*Completions*"
                                      "*Compile-Log*"
                                      "*inferior-lisp*"
                                      "*Fuzzy Completions*"
                                      "*Apropos*"
                                      "*Help*"
                                      "*cvs*"
                                      "*Buffer List*"
                                      "*Ibuffer*"
                                      "*esh command on file*")))

(use-package switch-window
  :diminish
  :bind ("C-x o" . switch-window)
  :config
  (setq-default switch-window-shortcut-style 'alphabet
                switch-window-timeout nil))



(provide 'init-windows)
;;; init-windows.el ends here
