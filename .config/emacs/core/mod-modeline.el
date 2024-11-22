;;; mod-modeline.el --- Modeline plugin & settings   -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package nano-modeline
  :ensure t
  :init
  (nano-modeline-prog-mode t)
  :custom
  (nano-modeline-position 'nano-modeline-footer)
  (nano-modeline-padding '(0 . 0))
  :custom-face
  (nano-modeline-active ((t (:foreground ,gracs/theme-fg :background ,gracs/theme-bg))))
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

;; (use-package mood-line
;;   :ensure t
;;   :custom
;;   (mood-line-glyph-alist mood-line-glyphs-fira-code)
;;   (mood-line-format (mood-line-defformat
;;                      :left
;;                      ("    │ "
;;                       ((mood-line-segment-buffer-name) . " ")
;;                       (mood-line-segment-buffer-status))
;;                      :right
;;                      ((mood-line-segment-misc-info)
;;                       " "
;;                       (mood-line-segment-process)
;;                       " "
;;                       ((when (mood-line-segment-checker) " │ ") . " ")
;;                       (mood-line-segment-vc)
;;                       "    ")))
;;   :config
;;   (mood-line-mode))

(provide 'mod-modeline)
;;; mod-modeline.el ends here
