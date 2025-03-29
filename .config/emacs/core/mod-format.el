;;;; mod-format.el --- Formatting -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ~ Apheleia ------------------------------------------------------------- ~ ;;

(use-package apheleia
  :ensure t
  :bind (("C-c f" . apheleia-format-buffer)))

;; ~ Reformatter ---------------------------------------------------------- ~ ;;

;; (use-package reformatter
;;   :ensure t)

;; ~ Aggressive indent ---------------------------------------------------- ~ ;;

(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1))

;; ~ Format all ----------------------------------------------------------- ~ ;;

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
