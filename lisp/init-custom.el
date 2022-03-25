;;; init-custom.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defcustom shapo-proxy "127.0.0.1:10808"
  "Set HTTP/HTTPS proxy."
  :group 'convenience
  :type 'string)

(defcustom shapo-socks-proxy "127.0.0.1:10808"
  "Set SOCKS proxy."
  :group 'convenience
  :type 'string)

(defcustom shapo-server t
  "Enable `server-mode' or not."
  :group 'convenience
  :type 'boolean)

(provide 'init-custom)
;;; init-custom.el ends here
