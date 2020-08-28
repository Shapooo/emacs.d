
(defun open-dired-marked ()
  "Open marked files in dired."
  (interactive)
  (mapc 'find-file (dired-get-marked-files)))

(provide 'init-dired)
