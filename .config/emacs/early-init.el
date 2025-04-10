;;; early-init.el --- Global settings & Locations -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(set-language-environment "UTF-8")

;; ~ Location ------------------------------------------------------------- ~ ;;

(setq-default user-emacs-directory "~/.config/emacs/"
              custom-file (expand-file-name "custom.el" user-emacs-directory)
              custom-theme-directory (expand-file-name "themes/" user-emacs-directory))
(defvar core-dir (expand-file-name "core/" user-emacs-directory))
(defvar cache-dir (expand-file-name "cache/" user-emacs-directory))
(defvar backup-dir (expand-file-name "backup/" cache-dir))
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name "eln/" cache-dir)))

;; ~ Features ------------------------------------------------------------- ~ ;;

(if (and (featurep 'native-compile)
         (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (setq native-comp-jit-compilation t
          package-native-compile t
          native-comp-speed 2
          comp-speed 2
          native-comp-async-report-warnings-errors nil
          comp-async-report-warnings-errors nil
          native-comp-warning-on-missing-source nil
          native-comp-async-query-on-exit t
          comp-async-query-on-exit t)
  (setq features (delq 'native-compile features)))
(if (fboundp #'json-parse-string)
    (push 'jansson features))
(if (string-match-p "HARFBUZZ" system-configuration-features)
    (push 'harfbuzz features))
(if (bound-and-true-p module-file-suffix)
    (push 'dynamic-modules features))

;; ~ Settings ------------------------------------------------------------- ~ ;;

(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right
              cursor-in-non-selected-windows nil)

(setq package-enable-at-startup nil
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5
      read-process-output-max (* 1024 1024)
      process-adaptive-read-buffering nil
      load-prefer-newer t
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      window-resize-pixelwise nil
      resize-mini-windows 'grow-only
      idle-update-delay 1.0
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      initial-buffer-choice nil
      inhibit-x-resources t
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t
      inhibit-splash-screen t
      inhibit-compacting-font-caches t
      ffap-machine-p-known 'reject ; Don't ping things that look like domain names.
      ad-redefinition-action 'accept
      comint-prompt-read-only t
      comint-buffer-maximum-size 2048
      ;; Warnings, Errors
      warning-suppress-types '((lexical-binding))
      debug-on-error nil
      jka-compr-verbose nil
      byte-compile-warnings nil
      byte-compile-verbose nil
      compilation-always-kill t
      compilation-ask-about-save nil
      compilation-scroll-output 'first-error
      ;;
      auto-mode-case-fold nil
      font-use-system-font nil
      default-input-method nil
      frame-title-format ""
      icon-title-format ""
      bidi-inhibit-bpa t
      use-dialog-box nil
      use-file-dialog nil
      blink-matching-paren nil
      x-stretch-cursor nil
      highlight-nonselected-windows nil)

;; (advice-add #'display-startup-echo-area-message :override #'ignore)
;; (advice-add #'display-startup-screen :override #'ignore)

;; ~ GUI ------------------------------------------------------------------ ~ ;;

(blink-cursor-mode -1)
;; (menu-bar-mode -1)
(push '(menu-bar-lines . 0) default-frame-alist)
(setq menu-bar-mode nil)
;; (tool-bar-mode -1)
(push '(tool-bar-lines . 0) default-frame-alist)
(setq tool-bar-mode nil)
;; (scroll-bar-mode -1)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(setq scroll-bar-mode nil)
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(unless noninteractive
  (dolist (buf (buffer-list))
    (with-current-buffer buf (setq mode-line-format nil)))
  (put 'mode-line-format 'initial-value (default-toplevel-value 'mode-line-format))
  (setq-default mode-line-format nil)
  (unless (eq system-type 'darwin)
    (setq command-line-ns-option-alist nil))
  (unless (memq initial-window-system '(x pgtk))
    (setq command-line-x-option-alist nil)))

(provide 'early-init)
;;; early-init.el ends here
