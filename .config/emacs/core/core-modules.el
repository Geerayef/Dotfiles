;;;; core-modules.el --- Source plugins -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(util/recursive-add-to-load-path modules-dir)

;; ~  UI
(require 'mod-theme)
(require 'mod-all-the-icons)
(require 'mod-nerd-icons)
(require 'mod-nano-modeline)

;; ~  Completion
(require 'mod-completion)

;; ~  Editing
;; ~  Formatting
;; ~  Linting
(require 'mod-flycheck)
;; ~  TreeSitter
(require 'mod-treesitter)
;; ~  Eglot
(require 'mod-eglot)

;; ~  Languages
(require 'mod-lang)

;; ~  Navigation

;; ~  Version Control

;; ~  Project Management
;; ~  Workspace Management

;; ~  ORG mode

(provide 'core-modules)
;;; core-modules.el ends here
