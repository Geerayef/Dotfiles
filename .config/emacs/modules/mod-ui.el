;;;; mod-ui.el --- UI -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq custom-safe-themes t)

(use-package ef-themes
             :ensure t
             :demand t
             :config
             (mapc #'disable-theme custom-enabled-themes)
             (load-theme 'ef-tritanopia-dark t))

;; (use-package nano-modeline
;;              :ensure t
;;              :init
;;              (nano-modeline-prog-mode t)
;;              :custom
;;              (nano-modeline-position 'nano-modeline-footer)
;;              :hook
;;              (prog-mode            . nano-modeline-prog-mode)
;;              (text-mode            . nano-modeline-text-mode)
;;              (org-mode             . nano-modeline-org-mode)
;;              (pdf-view-mode        . nano-modeline-pdf-mode)
;;              (mu4e-headers-mode    . nano-modeline-mu4e-headers-mode)
;;              (mu4e-view-mode       . nano-modeline-mu4e-message-mode)
;;              (elfeed-show-mode     . nano-modeline-elfeed-entry-mode)
;;              (elfeed-search-mode   . nano-modeline-elfeed-search-mode)
;;              (term-mode            . nano-modeline-term-mode)
;;              (xwidget-webkit-mode  . nano-modeline-xwidget-mode)
;;              (messages-buffer-mode . nano-modeline-message-mode)
;;              (org-capture-mode     . nano-modeline-org-capture-mode)
;;              (org-agenda-mode      . nano-modeline-org-agenda-mode))

;; ~ Font ------------------------------------------------------------------ ~ ;;

(use-package jit-lock
             :ensure nil
             :config (setq jit-lock-defer-time 0))

(use-package faces
             :ensure nil
             :hook
             (elpaca-after-init . (lambda ()
                                    (progn (set-face-attribute 'default nil :family "ZedMono Nerd Font Mono"
                                                               :height 180 :weight 'regular)
                                           (set-face-attribute 'line-number-current-line nil :foreground "yellow"
                                                               :slant 'normal :weight  'regular)))))

(defun pulse-line (&rest _)
  "Pulse-highlight the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command scroll-down-command recenter-top-bottom other-window))
  (advice-add command :after #'pulse-line))

(provide 'mod-ui)
;;; mod-ui.el ends here
