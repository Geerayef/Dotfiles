;;;; mod-ui.el --- UI -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq custom-safe-themes t)

(use-package ef-themes
  :ensure t
  :demand t
  :config
  ;; (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'ef-winter t))

(use-package nano-modeline
  :ensure t
  :init
  (nano-modeline-prog-mode t)
  :custom
  (nano-modeline-position 'nano-modeline-footer)
  :hook
  (prog-mode            . nano-modeline-prog-mode)
  (text-mode            . nano-modeline-text-mode)
  (org-mode             . nano-modeline-org-mode)
  (pdf-view-mode        . nano-modeline-pdf-mode)
  (mu4e-headers-mode    . nano-modeline-mu4e-headers-mode)
  (mu4e-view-mode       . nano-modeline-mu4e-message-mode)
  (elfeed-show-mode     . nano-modeline-elfeed-entry-mode)
  (elfeed-search-mode   . nano-modeline-elfeed-search-mode)
  (term-mode            . nano-modeline-term-mode)
  (xwidget-webkit-mode  . nano-modeline-xwidget-mode)
  (messages-buffer-mode . nano-modeline-message-mode)
  (org-capture-mode     . nano-modeline-org-capture-mode)
  (org-agenda-mode      . nano-modeline-org-agenda-mode))

(use-package indent-bars
  :ensure (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :config (require 'indent-bars-ts)
  :custom
  (indent-bars-prefer-character t)
  (indent-bars-display-on-blank-lines nil)
  (indent-bars-pattern ".")
  (indent-bars-width-frac 0.1)
  (indent-bars-pad-frac 0.1)
  (indent-bars-zigzag nil)
  (indent-bars-color '(highlight :face-bg nil :blend 0.4))
  (indent-bars-color-by-depth nil)
  (indent-bars-highlight-current-depth '(:blend 0.6))
  (indent-bars-no-descend-list t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module" "program" "translation_unit"))
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement if_statement with_statement while_statement)))
  (indent-bars-treesit-scope-min-lines 3)
  (indent-bars-treesit-wrap '((python argument_list parameters list list_comprehension
  										  dictionary dictionary_comprehension
  										  parenthesized_expression subscript)))
  :hook (prog-mode . indent-bars-mode))

;; ~ Font

(use-package jit-lock
  :ensure nil
  :config (setq jit-lock-defer-time 0))

(use-package faces
  :ensure nil
  :after (ef-themes)
  :config
  (set-face-attribute 'default nil :family "Iosevka Nerd Font Mono"
                      :height 160 :weight 'regular)
  (set-face-attribute 'line-number-current-line nil :foreground "yellow"
                      :inherit 'default :slant 'normal :weight 'heavy))

;; ~ Flash-indicate movement

(defun pulse-line (&rest _)
  "Pulse-highlight the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command scroll-down-command recenter-top-bottom other-window))
  (advice-add command :after #'pulse-line))

(provide 'mod-ui)
;;; mod-ui.el ends here
