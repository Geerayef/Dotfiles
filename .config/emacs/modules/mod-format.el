;;;; mod-format.el --- Formatting -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package format-all
  :ensure t
  :commands format-all-mode
  :config
  (setq-default format-all-formatters
                '(("C"        (clang-format "-i"))
                  ("Shell"    (shfmt "-i" "2" "-ci"))
                  ("Markdown" (prettier "--parser" "markdown"))))
  :hook (prog-mode . format-all-mode))

(provide 'mod-format)
;;; mod-format.el ends here
