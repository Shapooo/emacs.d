(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))

;; (require 'use-package)
;; (use-package diminish)
(require 'diminish)
(require 'bind-key)

(provide 'init-package)
