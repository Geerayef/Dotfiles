;;;; mod-format.el --- Formatting -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1))

;; (use-package format-all
;;   :ensure t
;;   :commands format-all-mode
;;   :config
;;   (setq-default format-all-formatters
;;                 '(("C"        (clang-format "-i"))
;;                   ("Shell"    (shfmt "-i" "2" "-ci"))))
;;   :hook (prog-mode . format-all-mode))

(provide 'mod-format)
;;; mod-format.el ends here
