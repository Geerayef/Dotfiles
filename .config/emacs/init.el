(setq gc-cons-threshold 100000000) ; 100 mb
(setq read-process-output-max (* 1024 1024)) ; 1mb
(setq inhibit-startup-message t)
(setq visible-bell nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil :nomessage))

;; Native compilation settings
(when (featurep 'native-compile)
  ;; Set the right directory to store the native compilation cache
  (let ((path (expand-file-name "eln-cache/" user-emacs-directory)))
    (setq-default native-comp-eln-load-path       (list path)
                  native-compile-target-directory path)
    (when (fboundp 'startup-redirect-eln-cache)
      (startup-redirect-eln-cache path)))
  (setq-default native-comp-async-report-warnings-errors nil  ;; Silence compiler warnings
                native-comp-deferred-compilation         t    ;; Make native compilation happens asynchronously
                package-native-compile                   t)   ;; Compile installed packages
  )

(set-face-attribute 'default t :font "JetBrainsMono Nerd Font Mono" :height 140)
(require 'uniquify)
(electric-pair-mode t)
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(global-auto-revert-mode t)

(add-to-list 'custom-theme-load-path "/home/tibor/.config/emacs/lisp/themes")
(let ((basedir "~/.config/emacs/lisp/themes"))
  (dolist (f (directory-files basedir))
    (if (and (not (or (equal f ".") (equal f "..")))
             (file-directory-p (concat basedir f)))
      (add-to-list 'custom-theme-load-path (concat basedir f)))))
(load-theme 'kanagawa)

(global-display-line-numbers-mode 1)
;; (display-line-numbers relative)
;; (display-line-numbers-mode relative)

;; (provide 'init)
