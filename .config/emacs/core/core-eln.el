;;;; core-eln.el --- eln settings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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
                            (max 1 (/ (num-processors) (if noninteractive 1 4)))))))

(provide 'core-eln)
;;; core-eln.el ends here
