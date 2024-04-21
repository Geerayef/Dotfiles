;;;; mod-write.el --- General editing enhancements -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ~  Org

(use-package org
  :ensure nil
  :custom
  (org-startup-indented t)
  (org-pretty-entities t)
  (org-use-sub-superscripts "{}")
  (org-hide-emphasis-markers t)
  (org-startup-with-inline-images t)
  (org-image-actual-width '(300)))

(use-package org-modern
  :ensure t
  :custom
  (org-modern-keyword nil)
  (org-modern-checkbox nil)
  (org-modern-table nil)
  :hook
  (org-mode . global-org-modern-mode))

(use-package org-fragtog
  :after org
  :custom
  (org-startup-with-latex-preview t)
  (org-format-latex-options
   (plist-put org-format-latex-options :scale 2)
   (plist-put org-format-latex-options :foreground 'auto)
   (plist-put org-format-latex-options :background 'auto))
  :hook
  (org-mode . org-fragtog-mode))

;; ~  PDF

(use-package pdf-tools
  :ensure t)

;; ~  Markdown

(use-package markdown-mode
  :ensure t
  :custom
  (markdown-command '("pandoc" "--from=markdown" "--to=html5"))
  (markdown-enable-math t)
  (markdown-hide-markup t)
  (markdown-gfm-uppercase-checkbox t)
  (markdown-indent-on-enter 'indent-and-new-item)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package markdown-preview-mode
  :ensure t
  :hook ((markdown-mode . markdown-preview-mode)
         (gfm-mode . markdown-preview-mode)))

(use-package markdown-ts-mode
  :ensure (:host github :repo "LionyxML/markdown-ts-mode")
  ;; :mode ("\\.md\\'" . markdown-ts-mode)
  :hook (markdown-mode . markdown-ts-mode))

;; ~  Latex

(use-package reftex
  :ensure nil
  :hook
  (LaTeX-mode . turn-on-reftex)
  (latex-mode . turn-on-reftex))

(use-package auctex
  :ensure (auctex
           :pre-build
           (("./autogen.sh")
            ("./configure"
             "--without-texmf-dir"
             "--with-lispdir=.")
            ("make"))
           :build (:not elpaca--compile-info)
           :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
           :version (lambda (_) (require 'tex-site) AUCTeX-version))
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-auto-untabify t
        TeX-PDF-mode t
        reftex-plug-into-AUCTeX t
        bib-cite-use-reftex-view-crossref t)
  (setq-default TeX-master nil)
  :mode (("\\tex.\\'" . LaTeX-mode)))

;; ~  Spelling

(use-package jinx
  :ensure t
  :hook ((text-mode prog-mode conf-mode) . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

(use-package zzz-to-char
  :ensure t
  :bind (("M-z" . zzz-to-char-up-to-char)))

(provide 'mod-write)
;;; mod-write.el ends here
