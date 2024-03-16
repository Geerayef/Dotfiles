;;;; core-ui.el --- UI Configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Startup
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message nil)

;; Dialogs
(setq use-dialog-box nil
      use-file-dialog nil)

;; Cursor
(blink-cursor-mode -1)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                 (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode))

(setq visible-bell nil)

;; ~  -------------------------------------------------------------------------------- ~ ;;

;; ~ Font

(add-hook 'elpaca-after-init-hook (lambda ()
                                    (set-face-attribute 'default nil
                                                        :family "Iosevka Nerd Font Mono"
                                                        :height 140
                                                        :weight 'regular)))

(add-hook 'elpaca-after-init-hook (lambda ()
                                    (set-face-attribute 'line-number-current-line nil
                                                        :foreground "yellow"
                                                        :slant 'normal
                                                        :weight 'heavy)))

;; ~  --------------------------------------------------------------------------------  ~ ;;

;; ~ Line

(setq line-numbers t)

(defcustom line-numbers-enabled-modes
           '(fundamental-mode conf-mode prog-mode org-mode)
           "Modes which should display line numbers."
           :type 'list
           :group 'ui)

(defcustom line-numbers-disabled-modes
           '()
           "Modes which should not display line numbers."
           :type 'list
           :group 'ui)

(defun enable-line-numbers ()
  "Turn on line numbers mode."
  (display-line-numbers-mode 1))

(defun disable-line-numbers ()
  "Turn off line numbers mode."
  (display-line-numbers-mode -1))

(defun update-line-numbers-display ()
  "Update configuration for line numbers display."
  (if line-numbers
    (progn
      (dolist (mode line-numbers-enabled-modes)
        (add-hook (intern (format "%s-hook" mode))
                  #'enable-line-numbers))
      (dolist (mode line-numbers-disabled-modes)
        (add-hook (intern (format "%s-hook" mode))
                  #'disable-line-numbers))
      (setq-default
        display-line-numbers-grow-only t
        display-line-numbers-type 'relative
        display-line-numbers-width 3))
    (progn
      (dolist (mode line-numbers-enabled-modes)
        (remove-hook (intern (format "%s-hook" mode))
                     #'enable-line-numbers))
      (dolist (mode line-numbers-disabled-modes)
        (remove-hook (intern (format "%s-hook" mode))
                     #'disable-line-numbers)))))

(defcustom line-numbers nil
           "Whether line numbers should be enabled."
           :type 'boolean
           :group 'ui
           :set (lambda (sym val)
                  (set-default sym val)
                  (update-line-numbers-display)))

(defun pulse-line (&rest _)
  "Pulse-highlight the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command
                    scroll-down-command
                    recenter-top-bottom
                    other-window))
  (advice-add command :after #'pulse-line))

;; ~  --------------------------------------------------------------------------------  ~ ;;

;; TODO: Move to plugin configurations
;; (add-to-list 'package-selected-packages 'helpful)
;; (add-to-list 'package-selected-packages 'breadcrumb)

;; (when (require 'breadcrumb nil :noerror)
;;   (breadcrumb-mode))

;; (when (require 'helpful nil :noerror)
;; (keymap-set helpful-mode-map "<remap> <revert-buffer>" #'helpful-update)
;; (keymap-global-set "<remap> <describe-command>"        #'helpful-command)
;; (keymap-global-set "<remap> <describe-function>"       #'helpful-callable)
;; (keymap-global-set "<remap> <describe-key>"            #'helpful-key)
;; (keymap-global-set "<remap> <describe-symbol>"         #'helpful-symbol)
;; (keymap-global-set "<remap> <describe-variable>"       #'helpful-variable)
;;   (keymap-global-set "C-h F"                             #'helpful-function))
;; (keymap-global-set "C-h K" #'describe-keymap)

(provide 'core-ui)
;;; core-ui.el ends here
