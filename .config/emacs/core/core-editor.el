;;;; core-editor.el --- Editor settings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Indent
(setq-default
  electric-indent-inhibit t
  indent-tabs-mode nil
  tab-width 4)

(setq-default
  bidi-paragraph-direction 'left-to-right
  bidi-inhibit-bpa t)

(add-hook 'elpaca-after-init-hook 'show-paren-mode)
(add-hook 'elpaca-after-init-hook 'electric-pair-mode)
(add-hook 'elpaca-after-init-hook 'delete-selection-mode)

(setq-default word-wrap t
              truncate-lines t)
(setq truncate-partial-width-windows nil
      sentence-end-double-space nil
      require-final-newline t)

(add-hook 'text-mode-hook #'visual-line-mode)

;; (keymap-set global-map "M-#" #'dictionary-lookup-definition)
;; (add-to-list 'display-buffer-alist
;;              '("^\\*Dictionary\\*"
;;                (display-buffer-in-side-window)
;;                (side . left)
;;                (window-width . 70)))

;; Spell check
(with-eval-after-load 'ispell
                      (when (executable-find ispell-program-name)
                        (add-hook 'text-mode-hook #'flyspell-mode)
                        (add-hook 'prog-mode-hook #'flyspell-prog-mode)))

(provide 'core-editor)
;;; core-editor.el ends here

