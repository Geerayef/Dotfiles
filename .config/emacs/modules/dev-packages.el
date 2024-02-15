;;;; dev-packages.el --- Development packages -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when (version< "29" emacs-version)
  (add-to-list 'package-selected-packages 'treesit-auto))

(add-to-list 'package-selected-packages 'editorconfig)

(add-to-list 'package-selected-packages 'aggressive-indent)

(add-to-list 'package-selected-packages 'ibuffer-project)

(provide 'dev-packages)
;;; dev-packages.el ends here
