;;; early-init.el --- Package manager and globals  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq
 package-enable-at-startup nil
 gc-cons-threshold most-positive-fixnum
 load-prefer-newer t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq-default
  user-emacs-directory "~/.config/emacs"
  custom-file (expand-file-name "custom.el" user-emacs-directory))
(defvar core-dir (expand-file-name "core" user-emacs-directory))
(defvar cache-dir (expand-file-name "cache" user-emacs-directory))
(defvar backup-dir (expand-file-name "backup" cache-dir))
(defvar modules-dir (expand-file-name "modules" user-emacs-directory))
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name "eln/" cache-dir)))

;;; early-init.el ends here
