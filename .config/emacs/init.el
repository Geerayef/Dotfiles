;;;; init.el --- Init -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'elpaca-installer)

(when (file-exists-p custom-file)
  (add-hook 'elpaca-after-init-hook (apply-partially #'load custom-file)))

;;; built-in features
(use-package emacs
  :ensure nil
  :custom
  (auto-save-include-big-deletions t)
  (auto-save-interval 200)
  (auto-save-timeout 10)
  (auto-window-vscroll nil)
  (bidi-inhibit-bpa t)
  (bidi-paragraph-direction 'left-to-right)
  (create-lockfiles nil)
  (display-line-numbers-grow-only t)
  (display-line-numbers-type 'relative)
  (display-line-numbers-width 3)
  (fast-but-imprecise-scrolling t)
  (frame-inhibit-implied-resize t)
  (frame-resize-pixelwise t)
  (history-delete-duplicates t)
  (history-length t)
  (line-numbers t)
  (process-adaptive-read-buffering nil)
  (read-process-output-max (* 4 1024 1024))
  (scroll-conservatively 101)
  (scroll-margin 4)
  (scroll-preserve-screen-position t)
  (tab-width 4)
  (truncate-lines t)
  (truncate-partial-width-windows nil)
  (use-short-answers t)
  (user-full-name "Tibor Novakovic")
  (visible-bell nil)
  (window-resize-pixelwise t)
  (word-wrap t)
  :config
  (when (boundp 'native-comp-eln-load-path)
    (startup-redirect-eln-cache (expand-file-name "eln/" cache-dir))
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln/" cache-dir))
    (setq native-comp-async-report-warnings-errors init-file-debug
          native-comp-warning-on-missing-source init-file-debug)
    ;; REVIEW: To be obsolete
    (unless (boundp 'native-comp-deferred-compilation-deny-list)
      (defvaralias 'native-comp-deferred-compilation-deny-list 'native-comp-jit-compilation-deny-list))
    (define-advice comp-effective-async-max-jobs (:before (&rest _) set-default-cpus)
      "Default to 1/4 of cores in interactive sessions and all of them otherwise."
      (and (null comp-num-cpus)
           (zerop native-comp-async-jobs-number)
           (setq comp-num-cpus
                 (max 1 (/ (num-processors) (if noninteractive 1 4))))))))
(use-package autorevert
  :ensure nil
  :demand t
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose t)
  (auto-revert-use-notify nil)
  (auto-revert-stop-on-user-input nil)
  :config
  (global-auto-revert-mode))
(use-package bookmark
  :ensure nil
  :defer 1
  :custom
  (bookmark-default-file (expand-file-name "bookmarks" cache-dir))
  (bookmark-save-flag 1)
  (bookmark-set-no-overwrite t))
(use-package delsel
  :ensure nil
  :hook (after-init-hook .  delete-selection-mode))
(use-package dired
  :ensure nil
  :custom
  (dired-auto-revert-buffer t)
  (dired-dwim-target t))
(use-package ediff
  :ensure nil
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain))
(use-package electric
  :ensure nil
  :custom
  (electric-indent-inhibit t))
(use-package elec-pair
  :ensure nil
  :hook (after-init-hook .  electric-pair-mode))
(use-package faces
  :ensure nil
  :demand t
  :config
  (set-face-attribute 'default nil
                      :family "Iosevka Nerd Font Mono"
                      :height 140
                      :weight 'regular)
  (set-face-attribute 'line-number-current-line nil
                      :foreground "yellow"
                      :slant 'normal
                      :weight 'heavy))
(use-package paragraphs
  :ensure nil
  :custom (sentence-end-double-space nil))
(use-package files
  :ensure nil
  :demand t
  :custom
  (require-final-newline t)
  (auto-save-file-name-transforms
   `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
      ;; Prefix tramp autosaves to prevent conflicts with local ones
      ,(concat auto-save-list-file-prefix "tramp-\\2") t)
     (".*" ,auto-save-list-file-prefix t)))
  (auto-save-default t)
  (make-backup-files t)
  (version-control t)
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-old-versions 5)
  (kept-new-versions 5)
  (revert-without-query '("."))
  (backup-directory-alist (list (cons "." backup-dir))))
(use-package ispell
  :ensure nil :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode))
(use-package jit-lock
  :ensure nil
  :custom (jit-lock-defer-time 0))
(use-package man
  :ensure nil
  :custom
  (Man-notify-method 'aggressive))
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode))
(use-package pixel-scroll
  :ensure nil
  :when
  (fboundp 'pixel-scroll-precision-mode)
  :config
  (pixel-scroll-precision-mode))
(use-package recentf
  :ensure nil
  :demand t
  :custom
  (recentf-auto-cleanup (and (daemonp) 300))
  (recentf-exclude '(+recentf-exclude-p))
  (recentf-save-file (expand-file-name "recentf" cache-dir))
  (recentf-max-saved-items 100)
  (recentf-max-menu-items 10)
  (recentf-auto-cleanup nil)
  (recentf-exclude '("\\.git.*" "\\.hg.*" "\\.svn.*"))
  :hook
  (kill-emacs . recentf-cleanup)
  (dired-mode . +recentf-add-dired-directory-h)
  :config
  (defun +recentf-exclude-p (file)
    "A predicate to decide whether to exclude FILE from recentf."
    (let ((file-dir (file-truename (file-name-directory file))))
      (cl-some (lambda (dir) (string-prefix-p dir file-dir))
               (mapcar 'file-truename (list cache-dir package-user-dir)))))
  (defun +recentf-add-dired-directory-h ()
    "Add dired directories to recentf file list."
    (recentf-add-file default-directory))
  (recentf-mode))
(use-package repeat
  :ensure nil
  :hook (after-init . repeat-mode))
(use-package savehist
  :ensure nil
  :demand t
  :custom
  (savehist-file (expand-file-name "savehist" cache-dir))
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-save-minibuffer-history t)
  (savehist-autosave-interval nil))
(use-package saveplace
  :demand t
  :custom
  (save-place-file (expand-file-name "saveplace" cache-dir)))
(use-package simple
  :ensure nil
  :custom
  (kill-do-not-save-duplicates t)
  (kill-ring-max 1000)
  :config
  (indent-tabs-mode -1))
(use-package startup
  :ensure nil
  :demand t
  :custom
  (auto-save-list-file-prefix (expand-file-name "autosave/" cache-dir)))
(use-package so-long
  :ensure nil
  :hook (after-init . so-long-enable))
(use-package text-mode
  :ensure nil
  :hook (text-mode . visual-line-mode))
(use-package tramp
  :ensure nil
  :custom
  (tramp-auto-save-directory (expand-file-name "tramp-autosave/" cache-dir))
  (tramp-backup-directory-alist backup-directory-alist))
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
  :custom
  (winner-mode))

;;; external features
(use-package gcmh
  :ensure t
  :custom
  (gcmh-high-cons-threshold (* 128 1024 1024))
  :config
  (add-hook 'elpaca-after-init-hook
            (lambda () (gcmh-mode) (diminish 'gcmh-mode))))
(use-package diminish :ensure t)
(use-package pulse
  :config
  (defun +pulse-line (&rest _)
    "Pulse-highlight the current line."
    (pulse-momentary-highlight-one-line (point)))
  ;;@TODO without advice
  (dolist (command '(scroll-up-command
                     scroll-down-command
                     recenter-top-bottom
                     other-window))
    (advice-add command :after #'pulse-line)))
;;; init.el ends here
