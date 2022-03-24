;; init-rust.el --- Initialize Rust configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Rust configurations.
;;

;;; Code:

;; Rust

(use-package rustic
  :config
  (setq rustic-analyzer-command '("~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/bin/rust-analyzer"))
  )

;; (use-package rust-playground)

(provide 'init-rust)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rust.el ends here
