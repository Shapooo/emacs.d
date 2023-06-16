;;; init-custom.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defgroup shapo nil
  "My Emacs customization."
  :group 'convenience)

(defcustom shapo-full-name user-full-name
  "Set user full name."
  :group 'shapo
  :type 'string)

(defcustom shapo-mail-address user-mail-address
  "Set user email address."
  :group 'shapo
  :type 'string)

(defcustom shapo-proxy "127.0.0.1:10808"
  "Set HTTP/HTTPS proxy."
  :group 'shapo
  :type 'string)

(defcustom shapo-socks-proxy "127.0.0.1:10808"
  "Set SOCKS proxy."
  :group 'shapo
  :type 'string)

(defcustom shapo-server t
  "Enable `server-mode' or not."
  :group 'shapo
  :type 'boolean)

;; Emacs Lisp Package Archive (ELPA)
;; @see https://github.com/melpa/melpa and https://elpa.emacs-china.org/.
(defcustom shapo-package-archives-alist
  (let ((proto (if (gnutls-available-p) "https" "http")))
    `((melpa    . (("gnu"    . ,(format "%s://elpa.gnu.org/packages/" proto))
                   ("nongnu" . ,(format "%s://elpa.nongnu.org/nongnu/" proto))
                   ("melpa"  . ,(format "%s://melpa.org/packages/" proto))))
      (emacs-cn . (("gnu"    . "http://1.15.88.122/gnu/")
                   ("nongnu" . "http://1.15.88.122/nongnu/")
                   ("melpa"  . "http://1.15.88.122/melpa/")))
      (bfsu     . (("gnu"    . ,(format "%s://mirrors.bfsu.edu.cn/elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.bfsu.edu.cn/elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.bfsu.edu.cn/elpa/melpa/" proto))))
      (netease  . (("gnu"    . ,(format "%s://mirrors.163.com/elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.163.com/elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.163.com/elpa/melpa/" proto))))
      (sjtu     . (("gnu"    . ,(format "%s://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa/" proto))))
      (tuna     . (("gnu"    . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/" proto))))
      (ustc     . (("gnu"    . ,(format "%s://mirrors.ustc.edu.cn/elpa/gnu/" proto))
                   ("nongnu" . ,(format "%s://mirrors.ustc.edu.cn/elpa/nongnu/" proto))
                   ("melpa"  . ,(format "%s://mirrors.ustc.edu.cn/elpa/melpa/" proto))))))
  "A list of the package archives."
  :group 'shapo
  :type '(alist :key-type (symbol :tag "Archive group name")
                :value-type (alist :key-type (string :tag "Archive name")
                                   :value-type (string :tag "URL or directory name"))))

(defcustom shapo-package-archives 'tuna
  "Set package archives from which to fetch."
  :group 'shapo
  :set (lambda (symbol value)
         (set symbol value)
         (setq package-archives
               (or (alist-get value shapo-package-archives-alist)
                   (error "Unknown package archives: `%s'" value))))
  :type `(choice ,@(mapcar
                    (lambda (item)
                      (let ((name (car item)))
                        (list 'const
                              :tag (capitalize (symbol-name name))
                              name)))
                    shapo-package-archives-alist)))

(defcustom shapo-theme-alist
  '((default . doom-one)
    (pro     . doom-monokai-pro)
    (dark    . doom-vibrant)
    (light   . doom-one-light)
    (warm    . doom-solarized-light)
    (cold    . doom-palenight)
    (day     . doom-tomorrow-day)
    (night   . doom-tomorrow-night))
  "List of themes mapped to internal themes."
  :group 'shapo
  :type '(alist :key-type (symbol :tag "Theme")
                :value-type (symbol :tag "Internal theme")))

(defcustom shapo-auto-themes '(("8:00"  . doom-one-light)
				                 ("19:00" . doom-one))
  "List of themes mapped to the time they should be loaded.

The keywords `:sunrise' and `:sunset' can be used for the time
if `calendar-latitude' and `calendar-longitude' are set.
For example:
  \\='((:sunrise . doom-one-light)
    (:sunset  . doom-one))"
  :group 'shapo
  :type '(alist :key-type (string :tag "Time")
                :value-type (symbol :tag "Theme")))

(defcustom shapo-system-themes '((light . doom-one-light)
				                   (dark  . doom-one))
  "List of themes related the system appearance.

It's only available on macOS currently."
  :group 'shapo
  :type '(alist :key-type (symbol :tag "Appearance")
                :value-type (symbol :tag "Theme")))

(defcustom shapo-theme 'default
  "The color theme."
  :group 'shapo
  :type `(choice (const :tag "Auto" auto)
                 (const :tag "Random" random)
                 (const :tag "System" system)
                 ,@(mapcar
                    (lambda (item)
                      (let ((name (car item)))
                        (list 'const
                              :tag (capitalize (symbol-name name))
                              name)))
                    shapo-theme-alist)
                 symbol))

(defcustom shapo-completion-style 'childframe
  "Completion display style."
  :group 'shapo
  :type '(choice (const :tag "Minibuffer" minibuffer)
                 (const :tag "Child Frame" childframe)))

(defcustom shapo-dashboard (not (daemonp))
  "Display dashboard at startup or not.
If Non-nil, use dashboard, otherwise will restore previous session."
  :group 'shapo
  :type 'boolean)

(defcustom shapo-restore-frame-geometry t
  "Restore the frame's geometry at startup.
If Non-nil, save and restore the frame's geometry."
  :group 'shapo
  :type 'boolean)

(defcustom shapo-lsp 'lsp-mode
  "Set language server.

`lsp-mode': See https://github.com/emacs-lsp/lsp-mode.
`eglot': See https://github.com/joaotavora/eglot.
tags: Use tags file instead of language server. See https://github.com/universal-ctags/citre.
nil means disabled."
  :group 'shapo
  :type '(choice (const :tag "LSP Mode" lsp-mode)
                 (const :tag "Eglot" eglot)
                 (const :tag "Disable" nil)))

(defcustom shapo-tree-sitter t
  "Enable tree-sitter or not.
Native tree-sitter is introduced in 29."
  :group 'shapo
  :type 'boolean)

(defcustom shapo-lsp-format-on-save nil
  "Auto format buffers on save."
  :group 'shapo
  :type 'boolean)

(defcustom shapo-lsp-format-on-save-ignore-modes
  '(c-mode c++-mode python-mode markdown-mode)
  "The modes that don't auto format and organize imports while saving the buffers.
`prog-mode' means ignoring all derived modes."
  :group 'shapo
  :type '(repeat (symbol :tag "Major-Mode")))

(defcustom shapo-chinese-calendar nil
  "Enable Chinese calendar or not."
  :group 'shapo
  :type 'boolean)

(defcustom shapo-player nil
  "Enable players or not."
  :group 'shapo
  :type 'boolean)

(defcustom shapo-prettify-symbols-alist
  '(("lambda" . ?λ)
    ("<-"     . ?←)
    ("->"     . ?→)
    ("->>"    . ?↠)
    ("=>"     . ?⇒)
    ("map"    . ?↦)
    ("/="     . ?≠)
    ("!="     . ?≠)
    ("=="     . ?≡)
    ("<="     . ?≤)
    (">="     . ?≥)
    ("=<<"    . (?= (Br . Bl) ?≪))
    (">>="    . (?≫ (Br . Bl) ?=))
    ("<=<"    . ?↢)
    (">=>"    . ?↣)
    ("&&"     . ?∧)
    ("||"     . ?∨)
    ("not"    . ?¬))
  "A list of symbol prettifications.
Nil to use font supports ligatures."
  :group 'shapo
  :type '(alist :key-type string :value-type (choice character sexp)))

(defcustom shapo-prettify-org-symbols-alist
  '(("[ ]"            . ?)
    ("[-]"            . ?)
    ("[X]"            . ?)

    (":PROPERTIES:"   . ?)
    (":ID:"           . ?🪪)
    (":END:"          . ?🔚)

    ("#+ARCHIVE:"     . ?📦)
    ("#+AUTHOR:"      . ?👤)
    ("#+CREATOR:"     . ?💁)
    ("#+DATE:"        . ?📆)
    ("#+DESCRIPTION:" . ?⸙)
    ("#+EMAIL:"       . ?📧)
    ("#+HEADERS"      . ?☰)
    ("#+OPTIONS:"     . ?⚙)
    ("#+SETUPFILE:"   . ?⚒)
    ("#+TAGS:"        . ?🏷)
    ("#+TITLE:"       . ?📓)

    ("#+BEGIN_SRC"    . ?✎)
    ("#+END_SRC"      . ?□)
    ("#+BEGIN_QUOTE"  . ?«)
    ("#+END_QUOTE"    . ?»)
    ("#+RESULTS:"     . ?💻))
  "A list of symbol prettifications for `org-mode'."
  :group 'shapo
  :type '(alist :key-type string :value-type (choice character sexp)))

;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'init-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
