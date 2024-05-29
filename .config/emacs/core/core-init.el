;;;; core-init.el --- Core behaviour -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ~  Functions
(defun util/disable-indent-tabs ()
  "Disable indenting with tabs."
  (setq indent-tabs-mode nil))

(defun util/recursive-add-to-load-path (dir)
  "Add DIR and all its sub-directories to `load-path'."
  (add-to-list 'load-path dir)
  (dolist (f (directory-files dir))
    (let ((name (expand-file-name f dir)))
      (when (and (file-directory-p name) (not (string-prefix-p "." f)))
        (util/recursive-add-to-load-path name)))))

;; ~  Emacs settings (from C source)
(use-package emacs
  :ensure nil
  :hook
  ((prog-mode text-mode fundamental-mode) . util/disable-indent-tabs)
  :custom
  ;; UX
  (use-short-answers t)
  ;; UI
  (display-line-numbers-grow-only t)
  (display-line-numbers-type 'relative)
  (display-line-numbers-width 3)
  ;; Editing
  (tab-width 4)
  ;; Paragraph - Sentence
  (bidi-paragraph-direction 'left-to-right)
  (bidi-inhibit-bpa t)
  (truncate-partial-width-windows nil)
  (sentence-end-double-space nil)
  ;; Scroll
  (auto-window-vscroll nil)
  (scroll-margin 4)
  (scroll-conservatively 101)
  (fast-but-imprecise-scrolling t)
  (scroll-preserve-screen-position t)
  ;; Resize
  (frame-inhibit-implied-resize t)
  (window-resize-pixelwise t)
  (frame-resize-pixelwise t)
  ;; Miscellaneous
  (visible-bell nil))

;; ~  Keybindings

(global-set-key (kbd "C-v")
                (lambda ()
                  (interactive
                   (forward-line (/ (window-height (selected-window)) 3)))))

(global-set-key (kbd "M-v")
                (lambda ()
                  (interactive
                   (forward-line (- (/ (window-height (selected-window)) 3))))))

;; -------------------------------------------------------------------------------- ;;

;; ~  Editing

(use-package display-line-numbers
  :ensure nil
  :custom (global-display-line-numbers-mode 1))

(use-package paren
  :ensure nil
  :init (show-paren-mode 1))

(use-package electric
  :ensure nil
  :custom (electric-indent-inhibit t))

(use-package elec-pair
  :ensure nil
  :init (electric-pair-mode 1))

(use-package delsel
  :ensure nil
  :init (delete-selection-mode 1))

(use-package files
  :ensure nil
  :custom (require-final-newline t))

(use-package simple
  :ensure nil
  :custom
  (kill-do-not-save-duplicates t)
  (kill-ring-max 1000)
  :hook
  ((fundamental-mode prog-mode) . visual-line-mode))

(use-package text-mode
  :ensure nil
  :hook (text-mode . visual-line-mode))

;; -------------------------------------------------------------------------------- ;;

;; ~  QoL

(use-package repeat
  :ensure nil
  :hook (elpaca-after-init . repeat-mode))

(use-package so-long
  :ensure nil
  :config (global-so-long-mode))

(use-package man
  :ensure nil
  :custom
  (Man-notify-method 'thrifty))

(use-package autoinsert
  :config
  (setq auto-insert-query nil)
  (auto-insert-mode 1)
  (add-hook 'find-file-hook 'auto-insert))

(use-package window
  :ensure nil
  :custom
  (switch-to-buffer-in-dedicated-window 'pop)
  (switch-to-buffer-obey-display-actions t)
  :config
  (add-to-list 'display-buffer-alist
               '("\\*Help\\*"
                 (display-buffer-reuse-window display-buffer-pop-up-window)
                 (inhibit-same-window . t)))
  (add-to-list 'display-buffer-alist
               '("\\*Completions\\*"
                 (display-buffer-reuse-window display-buffer-pop-up-window)
                 (inhibit-same-window . t)
                 (window-height . 10))))

(use-package winner
  :ensure nil
  :demand t
  :custom (winner-mode 1))
;; (define-prefix-command 'keymap-window)
;; (keymap-set 'keymap-window "u" 'winner-undo)
;; (keymap-set 'keymap-window "r" 'winner-redo)
;; (keymap-set 'keymap-window "n" 'windmove-down)
;; (keymap-set 'keymap-window "p" 'windmove-up)
;; (keymap-set 'keymap-window "b" 'windmove-left)
;; (keymap-set 'keymap-window "f" 'windmove-right)
;; (keymap-global-set prefix-key-window 'keymap-window)

(use-package pixel-scroll
  :ensure nil
  :when (fboundp 'pixel-scroll-precision-mode)
  :config (pixel-scroll-precision-mode))

;; ~  -------------------------------------------------------------------------------- ~ ;;

;; ~  File system navigation

(use-package dired
  :ensure nil
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setf dired-kill-when-opening-new-dired-buffer t))

;; ~  -------------------------------------------------------------------------------- ~ ;;

;; ~  Modules

(util/recursive-add-to-load-path modules-dir)

(use-package core-backups :ensure nil)

;; ~  UI
(use-package mod-ui :ensure nil)
(use-package mod-icons :ensure nil)
;; ~  Completion
(use-package mod-completion :ensure nil)
;; ~  ORG mode, text editing
(use-package mod-write :ensure nil)
;; ~  Formatting
;; ~  Linting
(use-package mod-flycheck :ensure nil)
;; ~  Treesitter
(use-package mod-treesitter :ensure nil)
;; ~  Eglot
(use-package mod-eglot :ensure nil)
;; ~  Programming
(use-package mod-lang :ensure nil)
;; ~  Navigation
(use-package mod-nav :ensure nil)
;; ~  Version Control
(use-package mod-git :ensure nil)
;; ~  Project Management
;; ~  Workspace Management

(provide 'core-init)
;;; core-init.el ends here
