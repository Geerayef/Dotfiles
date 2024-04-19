;;;; core-ui.el --- UI -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ~ Font

(use-package jit-lock
  :ensure nil
  :config (setq jit-lock-defer-time 0))

(use-package faces
  :ensure nil
  :demand t
  :init
  (set-face-attribute 'default nil :family "Iosevka Nerd Font Mono"
                      :height 160 :weight 'regular)
  (set-face-attribute 'line-number-current-line nil :foreground "yellow"
                      :slant 'normal :weight 'heavy))

;; ~ Flash-indicate movement

(defun pulse-line (&rest _)
  "Pulse-highlight the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command scroll-down-command recenter-top-bottom other-window))
  (advice-add command :after #'pulse-line))

(provide 'core-ui)
;;; core-ui.el ends here
