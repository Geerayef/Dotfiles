;;;; init.el --- Init -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ~  Performance
(setq process-adaptive-read-buffering nil)
(setq read-process-output-max (* 8 1024 1024))
(add-hook 'elpaca-after-init-hook (lambda ()
                                    (setq gc-cons-threshold (* 16 1024 1024)
                                          gc-cons-percentage 0.1)))

;; ~  Load path
(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(add-to-list 'load-path core-dir)

;; ~  Elpaca
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))
(require 'pacatim)

;; ~  --------------------------------------------------------------------------------  ~ ;;

(require 'core-init)
;; (require 'completion)
;; (require 'lisp)
;; (require 'ui)
;; (require 'dev)
;; (require 'writing)
;; (require 'org)

;; ~  --------------------------------------------------------------------------------  ~ ;;

(provide 'init.el)
;;; init.el ends here
