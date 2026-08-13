;;; mod-modeline.el --- Modeline plugin & settings   -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; (use-package nano-modeline
;;   :ensure t
;;   :demand t
;;   :hook
;;   (prog-mode            . nano-modeline-prog-mode)
;;   (text-mode            . nano-modeline-text-mode)
;;   (org-mode             . nano-modeline-org-mode)
;;   (pdf-view-mode        . nano-modeline-pdf-mode)
;;   (mu4e-headers-mode    . nano-modeline-mu4e-headers-mode)
;;   (mu4e-view-mode       . nano-modeline-mu4e-message-mode)
;;   (elfeed-show-mode     . nano-modeline-elfeed-entry-mode)
;;   (elfeed-search-mode   . nano-modeline-elfeed-search-mode)
;;   (term-mode            . nano-modeline-term-mode)
;;   (xwidget-webkit-mode  . nano-modeline-xwidget-mode)
;;   (messages-buffer-mode . nano-modeline-message-mode)
;;   (org-capture-mode     . nano-modeline-org-capture-mode)
;;   (org-agenda-mode      . nano-modeline-org-agenda-mode)
;;   :custom
;;   (nano-modeline-position 'nano-modeline-footer)
;;   (nano-modeline-padding '(0.2 . 0.2))
;;   :custom-face
;;   (nano-modeline-active ((t (:foreground ,gracs/theme-fg :background ,gracs/theme-bg)))))

(use-package lambda-line
  :ensure (:host github :repo "lambda-emacs/lambda-line")
  :custom
  (lambda-line-icon-time nil)
  (lambda-line-position 'bottom)
  (lambda-line-hspace "    ")
  (lambda-line-prefix t)
  (lambda-line-symbol-position +.5)
  (lambda-line-prefix-padding "    ")
  (lambda-line-tty-ro-symbol " ")
  (lambda-line-gui-ro-symbol " ")
  (lambda-line-tty-mod-symbol " •")
  (lambda-line-gui-mod-symbol " •")
  (lambda-line-space-top +.5)
  (lambda-line-space-bottom +.5)
  (lambda-line-vc-symbol "")
  (lambda-line-git-diff-mode-line t)
  :config
  (lambda-line-mode))

(provide 'mod-modeline)
;;; mod-modeline.el ends here
