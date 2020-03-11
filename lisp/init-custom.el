;;; init-custom.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; (let ((custom-example-file
;;        (expand-file-name "custom-example.el" user-emacs-directory)))
;;   (if (and (file-exists-p custom-example-file)
;;            (not (file-exists-p custom-file)))
;;       (copy-file custom-example-file custom-file)))

;; (if (file-exists-p custom-file)
;;     (load-file custom-file))

(provide 'init-custom)
;;; init-custom.el ends here
