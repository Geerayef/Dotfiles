;;; init.el --- Initial setup -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'early-init)
(add-to-list 'load-path core-dir)
(require 'pacatim)
(when (file-exists-p custom-file)
  (add-hook 'elpaca-after-init-hook (lambda () (load custom-file))))
(add-hook 'elpaca-after-init-hook (lambda () (setq-default gc-cons-threshold (* 16 1024 1024))))

;; ~ Variables ------------------------------------------------------------- ~ ;;

;; 'ef-elea-dark | 'doom-{plain-dark, tomorrow-night, spacegrey} | 'poet-dark{-monochrome}
(defvar gracs/theme "doom-plain-dark"
  "Default theme.")

(add-hook 'elpaca-after-init-hook (lambda () (load-theme (intern gracs/theme) t)))

(use-package core-init :ensure nil)

(provide 'init)
;;; init.el ends here
