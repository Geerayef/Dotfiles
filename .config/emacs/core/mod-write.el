;;; mod-write.el --- Writing tools -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package jinx
  :ensure t
  :hook ((text-mode
          org-mode
          markdown-mode
          gfm-mode
          prog-mode
          conf-mode) . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

;; ~ Org ------------------------------------------------------------------- ~ ;;

;; (use-package org
;;   :ensure nil
;;   :custom
;;   (org-startup-indented t)
;;   (org-pretty-entities t)
;;   (org-use-sub-superscripts "{}")
;;   (org-hide-emphasis-markers t)
;;   (org-startup-with-inline-images t)
;;   (org-image-actual-width '(300)))

;; (use-package org-modern
;;   :ensure t
;;   :custom
;;   (org-modern-keyword nil)
;;   (org-modern-checkbox nil)
;;   (org-modern-table nil)
;;   :hook
;;   (org-mode . global-org-modern-mode))

;; (use-package org-fragtog
;;   :ensure t
;;   :after org
;;   :custom
;;   (org-startup-with-latex-preview t)
;;   (org-format-latex-options
;;    (plist-put org-format-latex-options :scale 2)
;;    (plist-put org-format-latex-options :foreground 'auto)
;;    (plist-put org-format-latex-options :background 'auto))
;;   :hook (org-mode . org-fragtog-mode))

;; (use-package org-appear
;;   :ensure t
;;   :hook (org-mode . org-appear-mode))

;; (use-package denote
;;   :ensure t
;;   :config
;;   (setq xref-search-program (cond (executable-find "rg") 'ripgrep) (t 'grep)
;;         denote-directory (expand-file-name "~/notes/")
;;         denote-save-buffer-after-creation nil
;;         denote-known-keywords '("philosophy" "programming" "note")
;;         denote-infer-keywords t
;;         denote-sort-keywords t
;;         denote-file-type '(org markdown markdown-toml text)
;;         denote-prompts '(title keywords)
;;         denote-excluded-directories-regexp nil
;;         denote-excluded-keywords-regexp nil
;; Set to t if you are familiar with `denote-rename-file'
;;         denote-rename-no-confirm nil
;;         denote-date-prompt-use-org-read-date t
;;         denote-backlinks-show-context t)
;;   :bind
;;   (:map global-map
;;         ("C-c d n" . denote)
;;         ("C-c d r" . denote-region) ; "contents" mnemonic
;;         ("C-c d t" . denote-type)
;;         ("C-c d d" . denote-date)
;;         ("C-c d s" . denote-signature) ; "zettelkasten" mnemonic
;;         ("C-c d s" . denote-subdirectory)
;;         ("C-c d t" . denote-template)
;;         ;; Otherwise follow the same pattern for `org-mode-map',
;;         ;; `markdown-mode-map', and/or `text-mode-map'.
;;         ("C-c d l" . denote-link) ; "insert" mnemonic
;;         ("C-c d L" . denote-add-links)
;;         ("C-c d b" . denote-backlinks)
;;         ("C-c d f l" . denote-find-link)
;;         ("C-c d f b" . denote-find-backlink)))
;; Note that `denote-rename-file' can work from any context, not just
;; Dired bufffers.  That is why we bind it here to the `global-map'.
;; ("C-c n r" . denote-rename-file)
;; ("C-c n R" . denote-rename-file-using-front-matter)

;; ~ Latex ----------------------------------------------------------------- ~ ;;

;; (use-package reftex
;;   :ensure nil
;;   :hook
;;   (LaTeX-mode . turn-on-reftex)
;;   (latex-mode . turn-on-reftex))

;; (use-package tex
;;   :ensure
;;   (auctex
;;    :pre-build
;;    (("./autogen.sh")
;;     ("./configure" "--without-texmf-dir" "--with-lispdir=.")
;;     ("make"))
;;    :build (:not elpaca--compile-info)
;;    :files ("*.el" "doc" "etc" "images" "latex" "style")
;;    :version (lambda (_) (require 'tex-site) AUCTeX-version))
;;   :config
;;   (setq TeX-auto-save t
;;         TeX-parse-self t
;;         TeX-auto-untabify t
;;         TeX-PDF-mode t
;;         reftex-plug-into-AUCTeX t
;;         bib-cite-use-reftex-view-crossref t)
;;   (setq-default TeX-master nil)
;;   :mode (("\\.tex\\'" . LaTeX-mode)))

;; ~ PDF ------------------------------------------------------------------- ~ ;;

;; (use-package pdf-tools
;;   :ensure t
;;   :init (pdf-loader-install))

;; ~ Markdown -------------------------------------------------------------- ~ ;;

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :hook (gfm-mode . (lambda () (visual-line-mode)))
  :mode (("README\\.md\\'" . gfm-mode))
  :custom
  (markdown-command
   '("pandoc" "--standalone" "--mathjax" "--from=markdown" "--to=html5"))
  (markdown-enable-math t)
  (markdown-hide-markup t)
  (markdown-gfm-uppercase-checkbox t)
  (markdown-indent-on-enter 'indent-and-new-item))

;; (use-package markdown-preview-mode
;;   :ensure t
;;   :hook
;;   ((markdown-mode . markdown-preview-mode) (gfm-mode . markdown-preview-mode))
;;  :custom (browse-url-browser-function 'browse-url-firefox))

(use-package obsidian
  :ensure t
  :hook (markdown-mode . obsidian-mode)
  :custom
  (obsidian-specify-path "~/notes")
  (obsidian-inbox-directory "inbox")
  (obsidian-wiki-link-create-file-in-inbox t)
  ;; Daily notes: file name is YYYY-MM-DD.md
  (obsidian-daily-notes-directory "daily")
  (obsidian-templates-directory "templates")
  (obsidian-daily-note-template "daily.md")
  :bind
  (:map
   obsidian-mode-map
   ("C-c o l f" . obsidian-follow-link-at-point)
   ("C-c o l b" . obsidian-backlink-jump)
   ("C-c o l i" . obsidian-insert-link)))

(provide 'mod-write)
;;; mod-write.el ends here
