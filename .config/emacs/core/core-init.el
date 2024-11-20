;;; core-init.el --- Core behaviour -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ~ Utility functions ----------------------------------------------------- ~ ;;

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

(use-package emacs
  :ensure nil
  ;; :hook ((prog-mode text-mode fundamental-mode) . util/disable-indent-tabs)
  :config
  (setq-default left-fringe-width 8
                right-fringe-width 8
                indicate-buffer-boundaries nil
                indicate-empty-lines nil
                word-wrap t
                truncate-lines t
                fill-column 80
                global-text-scale-adjust-resizes-frames nil
                use-short-answers t
                whitespace-line-column nil
                indent-tabs-mode nil
                tab-width 4
                tab-always-indent 'complete
                truncate-partial-width-windows nil
                truncate-string-ellipsis "…"
                sentence-end-double-space nil
                delete-by-moving-to-trash (not noninteractive)
                visible-bell nil
                ring-bell-function #'ignore
                mouse-yank-at-point t
                split-width-threshold 170
                split-height-threshold nil
                comment-multi-line t
                comment-empty-lines t
                lazy-highlight-initial-delay 0
                display-time-default-load-average nil
                line-number-mode t
                column-number-mode t))

;; ~ Font ------------------------------------------------------------------ ~ ;;

(use-package jit-lock
  :ensure nil
  :custom (jit-lock-defer-time 0))

(use-package faces
  :ensure nil
  :hook
  (elpaca-after-init . (lambda ()
                         (progn (set-face-attribute 'default nil
                                                    :family "ZedMono Nerd Font Propo"
                                                    :height 180 :weight 'regular)))))

;; ~ Keys ------------------------------------------------------------------ ~ ;;

(global-set-key (kbd "C-v")
                (lambda ()
                  (interactive
                   (forward-line (/ (window-height (selected-window)) 2)))))

(global-set-key (kbd "M-v")
                (lambda ()
                  (interactive
                   (forward-line (- (/ (window-height (selected-window)) 2))))))

;; ~ Edit ------------------------------------------------------------------ ~ ;;

(use-package display-line-numbers
  :ensure nil
  :init (global-display-line-numbers-mode 1)
  :custom
  (display-line-numbers-type 'relative)
  (display-line-numbers-widen t)
  (display-line-numbers-width 3)
  (display-line-numbers-grow-only t))

(use-package paren
  :ensure nil
  :init (show-paren-mode 1)
  :custom
  (show-paren-delay 0.1)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  (delete-pair-blink-delay 0))

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
  :hook ((fundamental-mode prog-mode) . visual-line-mode))

(use-package text-mode
  :ensure nil
  :hook (text-mode . visual-line-mode))

;; ~ QoL ------------------------------------------------------------------- ~ ;;

(use-package repeat
  :ensure nil
  :hook (elpaca-after-init . repeat-mode))

(use-package so-long
  :ensure nil
  :init (global-so-long-mode))

(use-package man
  :ensure nil
  :custom
  (Man-notify-method 'thrifty))

(use-package autoinsert
  :ensure nil
  :custom
  (auto-insert-query nil)
  (auto-insert-mode 1)
  :hook (find-file . auto-insert))

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

(use-package frame
  :ensure nil
  :custom
  (window-divider-default-bottom-width 1)
  (window-divider-default-places t)
  (window-divider-default-right-width 1)
  :hook (after-init . window-divider-mode))

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
  :custom
  (pixel-scroll-precision-mode)
  :config
  (setq auto-window-vscroll nil
        scroll-margin 0
        scroll-conservatively 10
        scroll-error-top-bottom t
        fast-but-imprecise-scrolling t
        scroll-preserve-screen-position t
        hscroll-margin 2
        hscroll-step 1
        mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
        mouse-wheel-scroll-amount-horizontal 1))

(defun pulse-line (&rest _)
  "Pulse-highlight the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command scroll-down-command recenter-top-bottom other-window))
  (advice-add command :after #'pulse-line))


;; ~ Dired ----------------------------------------------------------------- ~ ;;

(use-package dired
  :ensure nil
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-listing-switches "-alhD --group-directories-first")
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-recursive-deletes 'top)
  (dired-recursive-copies  'always)
  (dired-create-destination-dirs 'ask)
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (put 'dired-find-alternate-file 'disabled nil))

;; Filetype
(setq python-indent-guess-indent-offset-verbose nil)
(setq sh-indent-after-continuation 'always)

;; ~ Modules --------------------------------------------------------------- ~ ;;

(util/recursive-add-to-load-path core-dir)

(use-package core-backups :ensure nil)
(use-package mod-theme :ensure nil)
(use-package mod-modeline :ensure nil)
;; (use-package mod-icons :ensure nil)
(use-package mod-completion :ensure nil)
;; (use-package mod-write :ensure nil)
;; (use-package mod-flycheck :ensure nil)
;; (use-package mod-treesitter :ensure nil)
;; (use-package mod-eglot :ensure nil)
;; (use-package mod-lang :ensure nil)
;; (use-package mod-dirvish :ensure nil)
;; (use-package mod-git :ensure nil)
(use-package mod-misc :ensure nil)

(provide 'core-init)
;;; core-init.el ends here
