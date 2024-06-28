;;; early-init.el --- Package manager and globals  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq package-enable-at-startup nil
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5
      read-process-output-max (* 4 1024 1024)
      process-adaptive-read-buffering nil
      load-prefer-newer t
      inhibit-startup-screen t   
      inhibit-startup-message t  
      initial-scratch-message nil
      use-dialog-box nil         
      use-file-dialog nil
      font-use-system-font nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(setq frame-title-format
      '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b"))))

(setq-default user-emacs-directory "~/.config/emacs/"
              custom-file (expand-file-name "custom.el" user-emacs-directory))
(defvar core-dir (expand-file-name "core/" user-emacs-directory))
(defvar cache-dir (expand-file-name "cache/" user-emacs-directory))
(defvar backup-dir (expand-file-name "backup/" cache-dir))
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name "eln/" cache-dir)))

(provide 'early-init)
;;; early-init.el ends here
