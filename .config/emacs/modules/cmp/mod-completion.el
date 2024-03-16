;;;; mod-completion.el --- Completion configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq tab-always-indent 'complete)
(setq completion-cycle-threshold 1
(setq completions-detailed t)
(setq 'xref-show-definitions-function
      #'xref-show-definitions-completing-read)

;; ~  --------------------------------------------------------------------------------  ~ ;;

(require 'orderless)
(require 'marginalia)
(require 'cape)
(require 'vertico)
(require 'consult)
(require 'corfu)
(require 'embark)

(provide 'mod-completion)
;;; mod-completion.el ends here
