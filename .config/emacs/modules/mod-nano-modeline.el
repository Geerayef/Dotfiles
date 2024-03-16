;;;; mod-nano-modeline.el --- NANO modeline -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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

(elpaca-wait)

(provide 'mod-nano-modeline)
;;; mod-nano-modeline.el ends here
