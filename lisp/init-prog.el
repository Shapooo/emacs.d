;; init-prog.el --- Initialize programming configurations.	-*- lexical-binding: t -*-
;;; Commentary:

;;; Code:


(require 'init-custom)
(require 'init-const)
(require 'init-funcs)

;; Prettify Symbols
;; e.g. display “lambda” as “λ”
(use-package prog-mode
  :ensure nil
  :hook (prog-mode . prettify-symbols-mode)
  :init (setq prettify-symbols-unprettify-at-point 'right-edge))

;; Run commands quickly
;; (use-package quickrun
;;   :bind (("C-<f5>" . quickrun)
;;          ("C-c x" . quickrun)))

(provide 'init-prog)

;;; init-prog.el ends here
