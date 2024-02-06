;;; init.el --- Init -*- lexical-binding: t; -*-


(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))
(when (and custom-file
            (file-exists-p custom-file))
  (load custom-file nil :nomessage))

;; Adjust garbage collection threshold for early startup (see use of gcmh below)
(setq gc-cons-threshold (* 128 1024 1024))
;; Process performance tuning
(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

(setq inhibit-startup-message t)
(setq visible-bell nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-face-attribute 'default t :font "Iosevka Nerd Font Mono" :height 160)
(electric-pair-mode t)
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
;; (save-place-mode t)
;; (savehist-mode t)
;; (recentf-mode t)
;; (global-auto-revert-mode t)

(load (expand-file-name "modules/crafted-init-config" crafted-emacs-home))

;; ~  --------------------------------------------------------------------------------  ~ ;;

;; ~  Packages

;; (add-to-list 'package-selected-packages 'all-the-icons 'helpful 'breadcrumb)
(require 'crafted-ui-packages)
;; (require 'crafted-ide-packages)
(require 'crafted-completion-packages)
(require 'crafted-lisp-packages)
;; (require 'crafted-writing-packages)
;; (require 'crafted-org-config)

(crafted-package-install-selected-packages)
(elpaca-wait)

;; ~  --------------------------------------------------------------------------------  ~ ;;

;; ~  Configuration

(require 'crafted-ui-config)
(require 'crafted-defaults-config)
;; (require 'crafted-ide-config)
(require 'crafted-completion-config)
(require 'crafted-lisp-config)
;; (require 'crafted-writing-config)
;; (require 'crafted-org-config)
(load-theme 'modus-vivendi)
