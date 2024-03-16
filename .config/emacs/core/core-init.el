;;;; core-init.el --- Core behaviour -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq user-full-name "Tibor Novakovic")

;; ~  -------------------------------------------------------------------------------- ~ ;;

(require 'core-eln)
(require 'core-backups)
(require 'core-util)
(require 'core-ui)
(require 'core-window)
(require 'core-editor)
(require 'core-global-keymaps)
(require 'core-modules)

;; ~  -------------------------------------------------------------------------------- ~ ;;

(defalias 'yes-or-no-p 'y-or-n-p)

;; Kill ring
(setq kill-do-not-save-duplicates t
      kill-ring-max 1000)

;; Repeat
(add-hook 'after-init-hook 'repeat-mode)

;; Dired
(setq dired-auto-revert-buffer t)
(setq-default dired-dwim-target t)

;; Long files
(add-hook 'after-init-hook 'so-long-enable)

;; Buffers
(setq switch-to-buffer-in-dedicated-window 'pop
      switch-to-buffer-obey-display-actions t)

;; Manual
(setq Man-notify-method 'aggressive)

;; ibuffer
;; Prefer the more full-featured built-in ibuffer for managing buffers.
;; (keymap-global-set "<remap> <list-buffers>" #'ibuffer-list-buffers)
;; Turn off forward and backward movement cycling
;; (customize-set-variable 'ibuffer-movement-cycle nil)
;; The number of hours before a buffer is considered "old" by ibuffer.
;; (customize-set-variable 'ibuffer-old-time 24)

(provide 'core-init)
;;; core-init.el ends here
