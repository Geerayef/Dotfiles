;;; early-init.el --- Package manager and globals  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq package-enable-at-startup nil)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(setq load-prefer-newer t)
(setq frame-inhibit-implied-resize t)
(setq-default user-emacs-directory "~/.config/emacs")
(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
(defvar core-dir (expand-file-name "core" user-emacs-directory))
(defvar cache-dir (expand-file-name "cache" user-emacs-directory))
(defvar backup-dir (expand-file-name "backup" cache-dir))
(defvar modules-dir (expand-file-name "modules" user-emacs-directory))
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name "eln/" cache-dir)))

(provide 'early-init)
;;; early-init.el ends here
