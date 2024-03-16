;;;; corfu.el --- Corfu -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package corfu
  :ensure 
  (corfu :files (:defaults "extensions/*")
         :includes (corfu-popupinfo))
  :demand t
  :custom
  (corfu-cycle t)
  (corfu-auto nil)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match t)
  :config
  (eldoc-add-command #'corfu-insert)
  ;; (corfu-separator ?\s)
  ;; (corfu-quit-at-boundary nil)
  ;; (corfu-preview-current nil)
  ;; (corfu-preselect 'prompt)
  ;; (corfu-on-exact-match nil)
  ;; (corfu-scroll-margin 5)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("M-d" . corfu-popupinfo-toggle)
        ("M-p" . corfu-popupinfo-scroll-down)
        ("M-n" . corfu-popupinfo-scroll-up))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(provide 'corfu)
;;; corfu.el ends here
