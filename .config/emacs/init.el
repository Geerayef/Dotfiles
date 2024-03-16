;;;; init.el --- Init -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ~  Load path
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'load-path core-dir)

;; ~  Performance
(setq read-process-output-max (* 4 1024 1024)
      process-adaptive-read-buffering nil)

;; ~  Custom file
(add-hook 'elpaca-after-init-hook (lambda ()
                                    (when (file-exists-p custom-file)
                                      (load custom-file))))

;; ~  --------------------------------------------------------------------------------  ~ ;;

;; ~  Elpaca
(require 'pacatim)
;; ~  GC
(use-package diminish :ensure t)
(use-package gcmh
             :ensure t
             :config
             (setq gcmh-high-cons-threshold (* 128 1024 1024))
             (add-hook 'elpaca-after-init-hook (lambda ()
                                                 (gcmh-mode)
                                                 (diminish 'gcmh-mode))))
(setq jit-lock-defer-time 0)

;; ~  --------------------------------------------------------------------------------  ~ ;;

(let
  ((debug-on-error t)
   (debug-on-quit t)
   (file-name-handler-alist nil))
  ;; Load everything
  (require 'core-init)
  (require 'elpaca)
  (elpaca-wait))

;;; init.el ends here
