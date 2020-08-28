;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package org
  :ensure nil
  :init
  (setq org-src-fontify-natively t
        org-todo-keywords '((sequence "TODO" "HAND" "|" "DONE"))
        org-capture-templates
        `(("t" "todo" entry (file "~/Note/inbox.org") ; "" => `org-default-notes-file'
           "* HAND %?\n%U\n" :clock-resume t)
          ("n" "note" entry (file "~/Note/note.org")
           "* %? \n%U\n%a\n")
          ("j" "journeal" entry (file "~/Note/journal.org")
           "* %t\n")
          ("m" "memo" entry (file "~/Note/memo.org")
           "* %t %a\n")
          ))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((R . t)
     (ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . nil)
     (latex . t)
     (ledger . t)
     (ocaml . nil)
     (octave . t)
     (plantuml . t)
     (python . t)
     (ruby . t)
     (screen . nil)
     (,(if (locate-library "ob-sh") 'sh 'shell) . t)
     (sql . t)
     (sqlite . t)
     )))

(global-set-key (kbd "C-c c") 'org-capture)

(provide 'init-org)
;;; init-org.el ends here
