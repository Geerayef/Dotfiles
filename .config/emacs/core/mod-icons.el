;;; mod-icons.el --- Icons -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package all-the-icons
  :ensure t
  :after memoize
  :if window-system
  :custom
  (all-the-icons-scale-factor 1.0)
  (all-the-icons-default-adjust -0.2))

(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-scale-factor 0.7)
  (nerd-icons-default-adjust -0.1)
  :config
  (when (and (display-graphic-p nil)
             (not (member "Symbols Nerd Font Mono" (font-family-list))))
    (nerd-icons-install-fonts)))

(use-package nerd-icons-completion
  :ensure t
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (cl-defmethod nerd-icons-completion-get-icon (cand (_cat (eql project-buffer)))
    "Return the icon for the candidate CAND of completion category project-buffer."
    (nerd-icons-completion-get-icon cand 'buffer))
  (cl-defmethod nerd-icons-completion-get-icon (cand (_cat (eql tab)))
    "Display tab icon for nerd-icons-completion."
    (concat (nerd-icons-mdicon "nf-md-tab" :face 'nerd-icons-blue) " "))
  (cl-defmethod nerd-icons-completion-get-icon (cand (_cat (eql command)))
    "Display command icon for nerd-icons-completion."
    (concat (nerd-icons-mdicon "nf-md-cog" :face 'nerd-icons-blue) " "))
  (cl-defmethod nerd-icons-completion-get-icon (cand (_cat (eql mode)))
    "Display mode icon for nerd-icons-completion."
    (concat (nerd-icons-octicon "nf-oct-package" :face 'nerd-icons-blue) " "))
  (cl-defmethod nerd-icons-completion-get-icon (cand (_cat (eql function)))
    "Display function icon for nerd-icons-completion."
    (if (commandp (intern cand))
        (concat (nerd-icons-octicon "nf-oct-gear" :face 'nerd-icons-blue) " ")
      (concat (nerd-icons-octicon "nf-oct-package" :face 'nerd-icons-purple) " ")))
  (cl-defmethod nerd-icons-completion-get-icon (cand (_cat (eql variable)))
    "Display function icon for nerd-icons-completion."
    (if (custom-variable-p (intern cand))
        (concat (nerd-icons-octicon "nf-oct-tag" :face 'nerd-icons-lblue) " ")
      (concat (nerd-icons-mdicon "nf-md-tag" :face 'nerd-icons-lblue) " ")))
  (cl-defmethod nerd-icons-completion-get-icon (cand (_cat (eql face)))
    "Display face icon for nerd-icons-completion."
    (concat (nerd-icons-mdicon "nf-md-palette" :face 'nerd-icons-blue) " "))
  (cl-defmethod nerd-icons-completion-get-icon (cand (_cat (eql history)))
    "Display history icon for nerd-icons-completion."
    (concat (nerd-icons-octicon "nf-oct-history" :face 'nerd-icons-blue) " "))
  (cl-defmethod nerd-icons-completion-get-icon (cand (_cat (eql theme)))
    "Display theme icon for nerd-icons-completion."
    (concat (nerd-icons-mdicon "nf-md-palette" :face 'nerd-icons-lcyan) " "))
  (defun nerd-icons-completion--counsel-imenu-symbol (cand)
    "Return imenu symbol from CAND."
    (let ((str (split-string cand ": ")))
      (or (cadr str) (car str))))
  (cl-defmethod nerd-icons-completion-get-icon (cand (_cat (eql symbol)))
    "Display the symbol icon all-the-icons-completion."
    (let ((sym (intern (nerd-icons-completion--counsel-imenu-symbol cand))))
      (cond
       ((string-match-p "Packages?[:)]" cand)
        (all-the-icons-octicon "nf-oct-archive" :face 'nerd-icons-silver))
       ((or (functionp sym) (macrop sym))
        (nerd-icons-completion-get-icon cand 'function))
       ((facep sym)
        (nerd-icons-completion-get-icon cand 'face))
       ((symbolp sym)
        (nerd-icons-completion-get-icon cand 'variable))
       (t (nerd-icons-octicon "nf-oct-gear" :face 'all-the-icons-silver)))))
  (nerd-icons-completion-mode))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook
  (ibuffer-mode . nerd-icons-ibuffer-mode))

(provide 'mod-icons)
;;; mod-icons.el ends here
