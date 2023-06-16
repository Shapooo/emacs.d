;;; init-yasnippet.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

;; Collection of yasnippet snippets
(use-package yasnippet-snippets)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
