;; init-latex.el --- Initialize LaTeX configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:


(use-package tex
  :ensure auctex
  :config
  (add-to-list 'TeX-command-list
     		   '("XeLaTeX" "xelatex -interaction=nonstopmode %s"
		         TeX-run-command t t :help "Run xelatex") t)
  (add-to-list 'TeX-view-program-list '("zathura" "zathura %o"))
  (setq TeX-view-program-selection '((output-pdf "zathura")
                                     (output-div "zathura"))))

(provide 'init-latex)
;;; init-latex.el ends here
