;;; init-flycheck.el --- -*- lexical-binding : t -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :diminish
  :config
  ;; (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list
  ;;       flycheck-display-errors-delay 0.5
  ;;       flycheck-check-syntax-automatically '(save mode-enable))
  (use-package flycheck-color-mode-line
    :diminish
    :hook (flycheck-mode-hook . flycheck-color-mode-line-mode)))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
