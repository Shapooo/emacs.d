;; init-funcs.el --- Define functions.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2023 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Define functions.
;;

;;; Code:
(require 'cl-lib)

(require 'init-const)
(require 'init-custom)

;; Suppress warnings
(defvar circadian-themes)
(defvar socks-noproxy)
(defvar socks-server)

(declare-function chart-bar-quickie 'chart)
(declare-function xwidget-webkit-current-session 'xwidget)



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
(defun revert-this-buffer ()
  "Revert the current buffer."
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (revert-buffer t t)
    (message "Reverted this buffer")))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

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

;; (defun browse-this-file ()
;;   "Open the current file as a URL using `browse-url'."
;;   (interactive)
;;   (let ((file-name (buffer-file-name)))
;;     (if (and (fboundp 'tramp-tramp-file-p)
;;              (tramp-tramp-file-p file-name))
;;         (error "Cannot open tramp file")
;;       (browse-url (concat "file://" file-name)))))

(defun copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (if filename
        (progn
          (kill-new filename)
          (message "Copied '%s'" filename))
      (warn "Current buffer is not attached to a file!"))))

;; Browse URL
;; (defun shapo-webkit-browse-url (url &optional pop-buffer new-session)
;;   "Browse URL with xwidget-webkit' and switch or pop to the buffer.

;; POP-BUFFER specifies whether to pop to the buffer.
;; NEW-SESSION specifies whether to create a new xwidget-webkit session."
;;   (interactive (progn
;;                  (require 'browse-url)
;;                  (browse-url-interactive-arg "xwidget-webkit URL: ")))
;;   (or (featurep 'xwidget-internal)
;;       (user-error "Your Emacs was not compiled with xwidgets support"))

;;   (xwidget-webkit-browse-url url new-session)
;;   (let ((buf (xwidget-buffer (xwidget-webkit-current-session))))
;;     (when (buffer-live-p buf)
;;       (and (eq buf (current-buffer)) (quit-window))
;;       (if pop-buffer
;;           (pop-to-buffer buf)
;;         (switch-to-buffer buf)))))

;; Mode line
(defun mode-line-height ()
  "Get the height of the mode-line."
  (- (elt (window-pixel-edges) 3)
     (elt (window-inside-pixel-edges) 3)
     (if (bound-and-true-p window-divider-mode)
         window-divider-default-bottom-width
       0)))

;; Reload configurations
(defun reload-init-file ()
  "Reload Emacs configurations."
  (interactive)
  (load user-init-file))
(defalias 'shapo-reload-init-file #'reload-init-file)

;; Browse the homepage
(defun browse-homepage ()
  "Browse the Github page of shapo Emacs."
  (interactive)
  (browse-url shapo-homepage))

;; Open custom file
(defun find-custom-file()
  "Open custom files."
  (interactive)
  (unless (file-exists-p custom-file)
    (if (file-exists-p shapo-custom-example-file)
        (copy-file shapo-custom-example-file custom-file)
      (user-error "The file `%s' doesn't exist" shapo-custom-example-file)))
  (when (file-exists-p custom-file)
    (find-file custom-file))
  (when (file-exists-p shapo-custom-post-file)
    (find-file-other-window shapo-custom-post-file)))

;; Misc
(defun create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

(defun save-buffer-gbk-as-utf8 ()
  "Revert a buffer with GBK and save as UTF-8."
  (interactive)
  (save-buffer-as-utf8 'gbk))

(defun byte-compile-elpa ()
  "Compile packages in elpa directory. Useful if you switch Emacs versions."
  (interactive)
  (if (fboundp 'async-byte-recompile-directory)
      (async-byte-recompile-directory package-user-dir)
    (byte-recompile-directory package-user-dir 0 t)))

(defun byte-compile-site-lisp ()
  "Compile packages in site-lisp directory."
  (interactive)
  (let ((dir (locate-user-emacs-file "site-lisp")))
    (if (fboundp 'async-byte-recompile-directory)
        (async-byte-recompile-directory dir)
      (byte-recompile-directory dir 0 t))))

(defun native-compile-elpa ()
  "Native-compile packages in elpa directory."
  (interactive)
  (if (fboundp 'native-compile-async)
      (native-compile-async package-user-dir t)))

(defun native-compile-site-lisp ()
  "Native compile packages in site-lisp directory."
  (interactive)
  (let ((dir (locate-user-emacs-file "site-lisp")))
    (if (fboundp 'native-compile-async)
        (native-compile-async dir t))))

(defun icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (and shapo-icon
       (or (featurep 'nerd-icons)
           (require 'nerd-icons nil t))))

(defun shapo-treesit-available-p ()
  "Check whether tree-sitter is available.
Native tree-sitter is introduced since 29."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)))

(defun shapo-set-variable (variable value &optional no-save)
  "Set the VARIABLE to VALUE, and return VALUE.

  Save to `custom-file' if NO-SAVE is nil."
  (customize-set-variable variable value)
  (when (and (not no-save)
             (file-writable-p custom-file))
    (with-temp-buffer
      (insert-file-contents custom-file)
      (goto-char (point-min))
      (while (re-search-forward
              (format "^[\t ]*[;]*[\t ]*(setq %s .*)" variable)
              nil t)
        (replace-match (format "(setq %s '%s)" variable value) nil nil))
      (write-region nil nil custom-file)
      (message "Saved %s (%s) to %s" variable value custom-file))))

(defun too-long-file-p ()
  "Check whether the file is too long."
  (if (fboundp 'buffer-line-statistics)
      (> (car (buffer-line-statistics)) 10000)
    (> (buffer-size) 100000)))

;; (define-minor-mode shapo-read-mode
;;   "Minor Mode for better reading experience."
;;   :init-value nil
;;   :group shapo
;;   (if shapo-read-mode
;;       (progn
;;         (and (fboundp 'olivetti-mode) (olivetti-mode 1))
;;         (and (fboundp 'mixed-pitch-mode) (mixed-pitch-mode 1))
;;         (text-scale-set +1))
;;     (progn
;;       (and (fboundp 'olivetti-mode) (olivetti-mode -1))
;;       (and (fboundp 'mixed-pitch-mode) (mixed-pitch-mode -1))
;;       (text-scale-set 0))))

;; Pakcage repository (ELPA)
(defun set-package-archives (archives &optional refresh async no-save)
  "Set the package archives (ELPA).

REFRESH is non-nil, will refresh archive contents.
ASYNC specifies whether to perform the downloads in the background.
Save to `custom-file' if NO-SAVE is nil."
  (interactive
   (list
    (intern
     (ivy-read "Select package archives: "
               (mapcar #'car shapo-package-archives-alist)
               :preselect (symbol-name shapo-package-archives)))))
  ;; Set option
  (shapo-set-variable 'shapo-package-archives archives no-save)

  ;; Refresh if need
  (and refresh (package-refresh-contents async))

  (message "Set package archives to `%s'" archives))
(defalias 'shapo-set-package-archives #'set-package-archives)

;; Refer to https://emacs-china.org/t/elpa/11192
(defun shapo-test-package-archives (&optional no-chart)
  "Test connection speed of all package archives and display on chart.

Not displaying the chart if NO-CHART is non-nil.
Return the fastest package archive."
  (interactive)

  (let* ((durations (mapcar
                     (lambda (pair)
                       (let ((url (concat (cdr (nth 2 (cdr pair)))
                                          "archive-contents"))
                             (start (current-time)))
                         (message "Fetching %s..." url)
                         (ignore-errors
                           (url-copy-file url null-device t))
                         (float-time (time-subtract (current-time) start))))
                     shapo-package-archives-alist))
         (fastest (car (nth (cl-position (apply #'min durations) durations)
                            shapo-package-archives-alist))))

    ;; Display on chart
    (when (and (not no-chart)
               (require 'chart nil t)
               (require 'url nil t))
      (chart-bar-quickie
       'vertical
       "Speed test for the ELPA mirrors"
       (mapcar (lambda (p) (symbol-name (car p))) shapo-package-archives-alist)
       "ELPA"
       (mapcar (lambda (d) (* 1e3 d)) durations) "ms"))

    (message "`%s' is the fastest package archive" fastest)

    ;; Return the fastest
    fastest))

;; WORKAROUND: fix blank screen issue on macOS.
(defun fix-fullscreen-cocoa ()
  "Address blank screen issue with child-frame in fullscreen.
This issue has been addressed in 28."
  (and sys/mac-cocoa-p
       (not emacs/>=28p)
       (bound-and-true-p ns-use-native-fullscreen)
       (setq ns-use-native-fullscreen nil)))



;; Update
(defun update-config ()
  "Update shapo Emacs configurations to the latest version."
  (interactive)
  (let ((dir (expand-file-name user-emacs-directory)))
    (unless (file-exists-p dir)
      (user-error "\"%s\" doesn't exist" dir))

    (message "Updating configurations...")
    (cd dir)
    (shell-command "git pull")
    (message "Updating configurations...done")))
(defalias 'shapo-update-config #'update-config)

(defun update-packages ()
  "Refresh package contents and update all packages."
  (interactive)
  (message "Updating packages...")
  (package-upgrade-all)
  (message "Updating packages...done"))
(defalias 'shapo-update-packages #'update-packages)

(defun update-config-and-packages()
  "Update confgiurations and packages."
  (interactive)
  (update-config)
  (update-packages))
(defalias 'shapo-update #'update-config-and-packages)

(defun update-dotfiles ()
  "Update the dotfiles to the latest version."
  (interactive)
  (let ((dir (or (getenv "DOTFILES")
                 (expand-file-name "~/.dotfiles/"))))
    (if (file-exists-p dir)
        (progn
          (message "Updating dotfiles...")
          (cd dir)
          (shell-command "git pull")
          (message "Updating dotfiles...done"))
      (message "\"%s\" doesn't exist" dir))))
(defalias 'shapo-update-dotfiles #'update-dotfiles)

(defun update-org ()
  "Update Org files to the latest version."
  (interactive)
  (let ((dir (expand-file-name "~/org/")))
    (if (file-exists-p dir)
        (progn
          (message "Updating org files...")
          (cd dir)
          (shell-command "git pull")
          (message "Updating org files...done"))
      (message "\"%s\" doesn't exist" dir))))
(defalias 'shapo-update-org #'update-org)

(defun update-all()
  "Update dotfiles, org files, configurations and packages to the latest."
  (interactive)
  (update-org)
  (update-dotfiles)
  (update-config-and-packages))
(defalias 'shapo-update-all #'update-all)


;; Fonts
(defun shapo-install-fonts ()
  "Install necessary fonts."
  (interactive)
  (nerd-icons-install-fonts))




;; UI
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defun run-after-load-theme-hook (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(advice-add #'load-theme :after #'run-after-load-theme-hook)

(defun childframe-workable-p ()
  "Whether childframe is workable."
  (or (not (or noninteractive
               emacs-basic-display
               (not (display-graphic-p))))
      (daemonp)))

(defun childframe-completion-workable-p ()
  "Whether childframe completion is workable."
  (and (eq shapo-completion-style 'childframe)
       (childframe-workable-p)))

(defun shapo--theme-name (theme)
  "Return internal THEME name."
  (or (alist-get theme shapo-theme-alist) theme 'doom-one))

(defun shapo-compatible-theme-p (theme)
  "Check if the THEME is compatible. THEME is a symbol."
  (or (memq theme '(auto random system))
      (string-prefix-p "doom" (symbol-name (shapo--theme-name theme)))))

(defun shapo-dark-theme-p ()
  "Check if the current theme is a dark theme."
  (eq (frame-parameter nil 'background-mode) 'dark))

(defun shapo-theme-enable-p (theme)
  "The THEME is enabled or not."
  (and theme
       (not (memq shapo-theme '(auto random system)))
       (memq (shapo--theme-name theme) custom-enabled-themes)))

(defun shapo--load-theme (theme)
  "Disable others and enable new one."
  (when-let ((theme (shapo--theme-name theme)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)))

(defun shapo--load-system-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (shapo--load-theme (alist-get appearance shapo-system-themes)))

(defun shapo-load-random-theme ()
  "Load the random theme."
  (interactive)
  (let* ((themes (mapcar #'cdr shapo-theme-alist))
         (theme (nth (random (length themes)) themes)))
    (if (eq theme shapo-theme)
        (shapo-load-random-theme)
      (shapo--load-theme theme))))

(defun shapo-load-theme (theme &optional no-save)
  "Load color THEME. Save to `custom-file' if NO-SAVE is nil."
  (interactive
   (list
    (intern
     (ivy-read "Load theme: "
               `(auto
                 random
                 system
                 ,@(mapcar #'car shapo-theme-alist))
               :preselect (symbol-name shapo-theme)))))

  ;; Disable system theme
  (when (bound-and-true-p auto-dark-mode)
    (setq auto-dark--last-dark-mode-state 'unknown)
    (auto-dark-mode -1))

  (pcase theme
    ('auto
     ;; Time-switching themes
     (use-package circadian
       :ensure t
       :functions circadian-setup
       :custom (circadian-themes shapo-auto-themes)
       :init (circadian-setup)))
    ('system
     ;; System-appearance themes
     (use-package auto-dark
       :ensure t
       :diminish
       :init
       (setq auto-dark-light-theme (alist-get 'light shapo-system-themes)
             auto-dark-dark-theme (alist-get 'dark shapo-system-themes))
       (when (and sys/macp (not (display-graphic-p)))
         (setq auto-dark-detection-method 'osascript))
       (auto-dark-mode 1)))
    ('random
     (shapo-load-random-theme))
    (_
     (shapo--load-theme theme)))

  ;; Set option
  (shapo-set-variable 'shapo-theme theme no-save))



;; Frame
(defvar shapo-frame--geometry nil)
(defun shapo-frame--save-geometry ()
  "Save current frame's geometry."
  (setq shapo-frame--geometry
        `((left   . ,(frame-parameter nil 'left))
          (top    . ,(frame-parameter nil 'top))
          (width  . ,(frame-parameter nil 'width))
          (height . ,(frame-parameter nil 'height))
          (fullscreen))))

(defun shapo-frame--fullscreen-p ()
  "Returns Non-nil if the frame is fullscreen."
  (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth)))

(defun shapo-frame-maximize ()
  "Maximize the frame."
  (interactive)
  (shapo-frame--save-geometry)
  (unless (eq (frame-parameter nil 'fullscreen) 'maximized)
    (set-frame-parameter nil 'fullscreen 'maximized)))

(defun shapo-frame-restore ()
  "Restore the frame's size and position."
  (interactive)
  (modify-frame-parameters nil shapo-frame--geometry))

(defun shapo-frame-left-half ()
  "Put the frame to the left-half."
  (interactive)
  (unless (shapo-frame--fullscreen-p)
    (shapo-frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (/ (nth 2 attr) 2) 20))
           (height (- (nth 3 attr) 30))
           (left (nth 0 attr))
           (top (nth 1 attr)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

(defun shapo-frame-right-half ()
  "Put the frame to the right-half."
  (interactive)
  (unless (shapo-frame--fullscreen-p)
    (shapo-frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (/ (nth 2 attr) 2) 20))
           (height (- (nth 3 attr) 30))
           (left (+ (nth 0 attr) width 20))
           (top (nth 1 attr)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

(defun shapo-frame-top-half ()
  "Put the frame to the top-half."
  (interactive)
  (unless (shapo-frame--fullscreen-p)
    (shapo-frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (nth 2 attr) 20))
           (height (- (/ (nth 3 attr) 2) 30))
           (left (nth 0 attr))
           (top (nth 1 attr)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

(defun shapo-frame-bottom-half ()
  "Put the frame to the bottom-half."
  (interactive)
  (unless (shapo-frame--fullscreen-p)
    (shapo-frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (nth 2 attr) 20))
           (height (- (/ (nth 3 attr) 2) 30))
           (left (nth 0 attr))
           (top (+ (nth 1 attr) height 30)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))



;; Network Proxy
(defun proxy-http-show ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is `%s'" shapo-proxy)
    (message "No HTTP proxy")))

(defun proxy-http-enable ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,shapo-proxy)
          ("https" . ,shapo-proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (proxy-http-show))

(defun proxy-http-disable ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (proxy-http-show))

(defun proxy-http-toggle ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if (bound-and-true-p url-proxy-services)
      (proxy-http-disable)
    (proxy-http-enable)))

(defun proxy-socks-show ()
  "Show SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (message "Current SOCKS%d proxy is %s:%s"
               (cadddr socks-server) (cadr socks-server) (caddr socks-server))
    (message "No SOCKS proxy")))

(defun proxy-socks-enable ()
  "Enable SOCKS proxy."
  (interactive)
  (require 'socks)
  (setq url-gateway-method 'socks
        socks-noproxy '("localhost"))
  (let* ((proxy (split-string shapo-socks-proxy ":"))
         (host (car proxy))
         (port (string-to-number (cadr proxy))))
    (setq socks-server `("Default server" ,host ,port 5)))
  (setenv "all_proxy" (concat "socks5://" shapo-socks-proxy))
  (proxy-socks-show))

(defun proxy-socks-disable ()
  "Disable SOCKS proxy."
  (interactive)
  (setq url-gateway-method 'native
        socks-noproxy nil
        socks-server nil)
  (setenv "all_proxy" "")
  (proxy-socks-show))

(defun proxy-socks-toggle ()
  "Toggle SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (proxy-socks-disable)
    (proxy-socks-enable)))

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
