;;;; org.el --- ORG mode config  -*- lexical-binding: t; -*-
;;; Commentary
;;; Code:

;; Second brain/zettlekasten by Protesilaos Stavrou (also known as
;; Prot), similar features as Org-Roam, but keeps everything in a
;; single directory, does not use a database preferring filenameing
;; conventions and grep instead.
(add-to-list 'package-selected-packages 'denote)

;; Toggle the visibility of some Org elements.
(add-to-list 'package-selected-packages 'org-appear)

;; Return or left-click with mouse follows link
(customize-set-variable 'org-return-follows-link t)
(customize-set-variable 'org-mouse-1-follows-link t)

;; Display links as the description provided
(customize-set-variable 'org-link-descriptive t)

;; Visually indent org-mode files to a given header level
(add-hook 'org-mode-hook #'org-indent-mode)

;; Hide markup markers
(customize-set-variable 'org-hide-emphasis-markers t)
(when (locate-library "org-appear")
  (add-hook 'org-mode-hook 'org-appear-mode))

;; Disable auto-pairing of "<" in org-mode with electric-pair-mode
(defun crafted-org-enhance-electric-pair-inhibit-predicate ()
  "Disable auto-pairing of \"<\" in `org-mode' when using `electric-pair-mode'."
  (when (and electric-pair-mode (eql major-mode #'org-mode))
    (setq-local electric-pair-inhibit-predicate
                `(lambda (c)
                   (if (char-equal c ?<)
                       t
                     (,electric-pair-inhibit-predicate c))))))

;; Add hook to both electric-pair-mode-hook and org-mode-hook
;; This ensures org-mode buffers don't behave weirdly,
;; no matter when electric-pair-mode is activated.
(add-hook 'electric-pair-mode-hook #'crafted-org-enhance-electric-pair-inhibit-predicate)
(add-hook 'org-mode-hook #'crafted-org-enhance-electric-pair-inhibit-predicate)

(provide 'org)
;;; org.el ends here
