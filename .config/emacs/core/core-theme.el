;;;; theme.el --- Theme -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq custom-safe-themes t)
(setq-default custom-enabled-themes '(doom-ayu-dark leuven-dark))
(add-hook 'elpaca-after-init-hook 'load-custom-themes)
;; (load-theme 'leuven-dark)

(provide 'theme)
;;; theme.el ends here
