;;;; corfu.el --- Corfu -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto nil)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match t)
  ;; (corfu-separator ?\s)
  ;; (corfu-quit-at-boundary nil)
  ;; (corfu-preview-current nil)
  ;; (corfu-preselect 'prompt)
  ;; (corfu-on-exact-match nil)
  ;; (corfu-scroll-margin 5)
  :bind
  (:map corfu-map ("SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode 1))

(elpaca-wait)

(when (and (require 'corfu-popupinfo nil :noerror)
  (corfu-popupinfo-mode 1)
  (eldoc-add-command #'corfu-insert)
  (keymap-set corfu-map "M-p" #'corfu-popupinfo-scroll-down)
  (keymap-set corfu-map "M-n" #'corfu-popupinfo-scroll-up)
  (keymap-set corfu-map "M-d" #'corfu-popupinfo-toggle)))

(provide 'corfu)
;;; corfu.el ends here
