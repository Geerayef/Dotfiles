;;;; init.el --- Init -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(setq custom-file (concat user-emacs-directory "custom.el"))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))
;; (when (and custom-file (file-exists-p custom-file))
;;  (load custom-file nil :nomessage))

;; Performance
(setq gc-cons-threshold (* 128 1024 1024))
(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

;; Behaviour
(setq inhibit-startup-screen t)
(setq visible-bell nil)
(save-place-mode t)
(global-auto-revert-mode t)
(setq mouse-wheel-progressive-speed nil)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; UI
(set-face-attribute 'default nil :font "Iosevka Nerd Font Mono" :height 120 :weight 'regular)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode -1)
(blink-cursor-mode 0)

;; Edit
;; (global-display-line-numbers-mode 1)
(custom-set-variables
  '(display-line-numbers-type 'relative))
(setq crafted-ui-display-line-numbers t)
(setq display-line-numbers 'relative)
(show-paren-mode 1)
(electric-pair-mode t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Crafted Emacs init
(load (expand-file-name "modules/init-config" crafted-emacs-home))

;; ~  --------------------------------------------------------------------------------  ~ ;;

;; ~  Packages

(require 'ui-packages)
(require 'dev-packages)
(require 'crafted-completion-packages)
(require 'lisp-packages)
;; (require 'crafted-writing-packages)
;; (require 'crafted-org-config)

(crafted-package-install-selected-packages)
(elpaca-wait)

;; ~  --------------------------------------------------------------------------------  ~ ;;

;; ~  Configuration

(require 'defaults-config)
(require 'ui-config)
(require 'dev-config)
(require 'crafted-completion-config)
(require 'lisp-config)
;; (require 'crafted-writing-config)
;; (require 'crafted-org-config)

(load-theme 'leuven-dark)

(provide 'init.el)
;;; init.el ends here
