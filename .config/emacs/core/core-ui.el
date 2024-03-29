;;;; core-ui.el --- UI Configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ~ Font

(use-package faces
  :ensure nil
  :demand t
  :config
  (set-face-attribute 'default nil
                      :family "Iosevka Nerd Font Mono"
                      :height 160
                      :weight 'regular)
  (set-face-attribute 'line-number-current-line nil
                      :foreground "yellow"
                      :slant 'normal
                      :weight 'heavy))

;; ~  --------------------------------------------------------------------------------  ~ ;;

;; ~ Line

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
