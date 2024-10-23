;;; init.el --- Initial setup -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'early-init)
(add-to-list 'load-path core-dir)
(require 'pacatim)
(when (file-exists-p custom-file)
  (add-hook 'elpaca-after-init-hook (lambda () (load custom-file))))
(add-hook 'elpaca-after-init-hook (lambda ()
                                    (setq-default gc-cons-threshold (* 16 1024 1024))))

;; 'ef-elea-dark | 'doom-{plain-dark, tomorrow-night, sourcerer, pine, spacegrey}
(add-hook 'elpaca-after-init-hook (lambda () (load-theme 'doom-spacegrey t)))

(use-package core-init :ensure nil)

(provide 'init)
;;; init.el ends here
