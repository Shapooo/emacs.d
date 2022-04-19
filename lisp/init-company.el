;;; init-company.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package company
  :diminish
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-abort
  :bind (("M-/" . company-complete)
         ("<backtab>" . company-yasnippet)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("<tab>" . company-complete-common)
         ("<backtab>" . my-company-yasnippet)
         ;; ("C-c C-y" . my-company-yasnippet)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :hook (after-init . global-company-mode)
  :init
  (defun my-company-yasnippet ()
    "Hide the current completeions and show snippets."
    (interactive)
    (company-abort)
    (call-interactively 'company-yasnippet))
  :config
  (setq company-tooltip-align-annotations t
        company-tooltip-limit 12
        company-idle-delay 0
        company-echo-delay (if (display-graphic-p) nil 0)
        company-minimum-prefix-length 2
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-global-modes '(not erc-mode message-mode help-mode gud-mode eshell-mode shell-mode)
        company-backends '(company-capf company-files)
        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend))

  ;; (use-package company-c-headers
  ;;   ;; :after company
  ;;   :config (add-to-list 'company-backends 'company-c-headers))

  (when emacs/>=26p
    ;; (use-package company-box
    ;;   :diminish
    ;;   :hook (company-mode . company-box-mode)
    ;;   :init (setq company-box-backends-colors nil
    ;;               company-box-show-single-candidate t
    ;;               company-box-max-candidates 50
    ;;               company-box-doc-delay 0.5)
    ;;   :config
    ;;   (with-no-warnings
    ;;     ;; Highlight `company-common'
    ;;     (defun my-company-box--make-line (candidate)
    ;;       (-let* (((candidate annotation len-c len-a backend) candidate)
    ;;               (color (company-box--get-color backend))
    ;;               ((c-color a-color i-color s-color) (company-box--resolve-colors color))
    ;;               (icon-string (and company-box--with-icons-p (company-box--add-icon candidate)))
    ;;               (candidate-string (concat (propertize (or company-common "") 'face 'company-tooltip-common)
    ;;                                         (substring (propertize candidate 'face 'company-box-candidate)
    ;;                                                    (length company-common) nil)))
    ;;               (align-string (when annotation
    ;;                               (concat " " (and company-tooltip-align-annotations
    ;;                                                (propertize " " 'display `(space :align-to (- right-fringe ,(or len-a 0) 1)))))))
    ;;               (space company-box--space)
    ;;               (icon-p company-box-enable-icon)
    ;;               (annotation-string (and annotation (propertize annotation 'face 'company-box-annotation)))
    ;;               (line (concat (unless (or (and (= space 2) icon-p) (= space 0))
    ;;                               (propertize " " 'display `(space :width ,(if (or (= space 1) (not icon-p)) 1 0.75))))
    ;;                             (company-box--apply-color icon-string i-color)
    ;;                             (company-box--apply-color candidate-string c-color)
    ;;                             align-string
    ;;                             (company-box--apply-color annotation-string a-color)))
    ;;               (len (length line)))
    ;;         (add-text-properties 0 len (list 'company-box--len (+ len-c len-a)
    ;;                                          'company-box--color s-color)
    ;;                              line)
    ;;         line))
    ;;     (advice-add #'company-box--make-line :override #'my-company-box--make-line)

    ;;     ;; Prettify icons
    ;;     (defun my-company-box-icons--elisp (candidate)
    ;;       (when (derived-mode-p 'emacs-lisp-mode)
    ;;         (let ((sym (intern candidate)))
    ;;           (cond ((fboundp sym) 'Function)
    ;;                 ((featurep sym) 'Module)
    ;;                 ((facep sym) 'Color)
    ;;                 ((boundp sym) 'Variable)
    ;;                 ((symbolp sym) 'Text)
    ;;                 (t . nil)))))
    ;;     (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)))
    )

  ;; Better sorting and filtering
  (use-package company-prescient
    :init (company-prescient-mode 1))

  ;; Popup documentation for completion candidates
  (when (and (not emacs/>=26p) (display-graphic-p))
    (use-package company-quickhelp
      :defines company-quickhelp-delay
      :bind (:map company-active-map
                  ([remap company-show-doc-buffer] . company-quickhelp-manual-begin))
      :hook (global-company-mode . company-quickhelp-mode)
      :init (setq company-quickhelp-delay 2))))

(provide 'init-company)
;;; init-company.el ends here
