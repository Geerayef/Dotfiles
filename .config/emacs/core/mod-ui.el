;;;; mod-ui.el --- UI -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq custom-safe-themes t)

(use-package ef-themes
  :ensure t
  :demand t
  :config
  (setq ef-elea-dark-palette-overrides
        '((cursor "#FFF779")
          (bg-main "#0A0E14")))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'ef-elea-dark t))

;; (use-package kanagawa-theme
;;   :ensure (:host github :repo "Fabiokleis/emacs-kanagawa-theme")
;;   :demand t
;;   :preface
;;   (setq kanagawa-theme-custom-colors '((sumi-ink-1b "#0A0E14")
;;                                        (sumi-ink-1 "#0A0E14")))
;; :config
;; (load-theme 'kanagawa t))

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

(provide 'mod-ui)
;;; mod-ui.el ends here
