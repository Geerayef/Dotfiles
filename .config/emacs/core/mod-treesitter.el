;;; mod-treesitter.el --- TreeSitter -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package treesit
  :ensure nil
  :config
  (setq major-mode-remap-alist
        '((ocaml-mode . ocaml-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (c-or-c++-mode . c-or-c++-ts-mode)
          (rust-mode . rust-ts-mode)
          (lisp-mode . lisp-ts-mode)
          (clojure-mode . clojure-ts-mode)
          (scheme-mode . scheme-ts-mode)
          (go-mode . go-ts-mode)
          (python-mode . python-ts-mode)
          (bash-mode . bash-ts-mode)
          (fish-mode . fish-ts-mode)
          (markdown-mode . markdown-ts-mode)
          (toml-mode . toml-ts-mode)
          (yaml-mode . yaml-ts-mode)
          (makefile-mode . make-ts-mode)
          (doctex-mode . docTeX-mode)
          (tex-mode . TeX-tex-mode)
          (latex-mode . LaTeX-mode)
          (texinfo-mode . Texinfo-mode)
          (plain-tex-mode . plain-TeX-mode)
          (json-mode . json-ts-mode)
          (js-mode . js-ts-mode)
          (js-base-mode . js-ts-mode)
          (javascript-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (html-mode . html-ts-mode)
          (css-mode . css-ts-mode)))
  (setq treesit-language-source-alist
        '((ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" "master" "grammars/ocaml/src"))
          (bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (fish . ("https://github.com/ram02z/tree-sitter-fish"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
          (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
          (go . ("https://github.com/tree-sitter/tree-sitter-go"))
          (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod"))
          (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
          (make . ("https://github.com/alemuller/tree-sitter-make"))
          (markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript")))))

(dolist (grammar treesit-language-source-alist)
  (unless (treesit-language-available-p (car grammar))
    (treesit-install-language-grammar (car grammar))))

(use-package treesit-auto
  :ensure t
  :init
  (setq treesit-auto-langs '(c cpp ocaml python elisp lua bash fish yaml toml markdown))
  :custom
  (treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'all)
  :config
  (global-treesit-auto-mode))

(provide 'mod-treesitter)
;;; mod-treesitter.el ends here
