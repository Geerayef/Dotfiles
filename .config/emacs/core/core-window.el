;;;; core-window.el --- Window Management -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(winner-mode 1)
;; (define-prefix-command 'keymap-window)
;; (keymap-set 'keymap-window "u" 'winner-undo)
;; (keymap-set 'keymap-window "r" 'winner-redo)
;; (keymap-set 'keymap-window "n" 'windmove-down)
;; (keymap-set 'keymap-window "p" 'windmove-up)
;; (keymap-set 'keymap-window "b" 'windmove-left)
;; (keymap-set 'keymap-window "f" 'windmove-right)
;;
;; (keymap-global-set prefix-key-window 'keymap-window)

(setq auto-window-vscroll nil
      scroll-margin 4
      scroll-conservatively 101
      fast-but-imprecise-scrolling t
      scroll-preserve-screen-position t
      frame-inhibit-implied-resize t)

(setq-default
  window-resize-pixelwise t
  frame-resize-pixelwise t)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(add-to-list 'display-buffer-alist
             '("\\*Help\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)))

(add-to-list 'display-buffer-alist
             '("\\*Completions\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)
               (window-height . 10)))

(provide 'core-window)
;;; core-window.el ends here
