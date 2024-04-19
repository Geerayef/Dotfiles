;;;; init.el --- Init -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'early-init)

(add-to-list 'load-path core-dir)

(setq read-process-output-max (* 4 1024 1024)
      process-adaptive-read-buffering nil)

(when (file-exists-p custom-file)
  (add-hook 'elpaca-after-init-hook (lambda () (load custom-file))))

;; ~  --------------------------------------------------------------------------------  ~ ;;

(require 'pacatim)

(use-package gcmh
  :ensure t
  :config (setq gcmh-high-cons-threshold (* 128 1024 1024))
  :hook ((after-init . gcmh-mode)))

;; ~  --------------------------------------------------------------------------------  ~ ;;

(let ((debug-on-error t)
      (debug-on-quit t)
      (file-name-handler-alist nil))
  (use-package core-init :ensure nil))

(provide 'init)
;;; init.el ends here
