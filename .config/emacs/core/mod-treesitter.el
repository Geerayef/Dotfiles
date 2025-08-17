;;; mod-treesitter.el --- TreeSitter -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4)
  :config
  (setq major-mode-remap-alist
        '((ocaml-mode . ocaml-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (c-or-c++-mode . c-or-c++-ts-mode)
          (rust-mode . rust-ts-mode)
          (go-mode . go-ts-mode)
          (haskell-mode . haskell-ts-mode)
          (python-mode . python-ts-mode)
          (lua-mode . lua-ts-mode)
          (bash-mode . bash-ts-mode)
          (fish-mode . fish-ts-mode)
          (toml-mode . toml-ts-mode)
          (yaml-mode . yaml-ts-mode)
          (json-mode . json-ts-mode)
          (makefile-mode . make-ts-mode)
          (lisp-mode . lisp-ts-mode)
          (clojure-mode . clojure-ts-mode)
          (scheme-mode . scheme-ts-mode)
          (doctex-mode . docTeX-mode)
          (tex-mode . TeX-tex-mode)
          (latex-mode . LaTeX-mode)
          (texinfo-mode . Texinfo-mode)
          (plain-tex-mode . plain-TeX-mode)
          (js-mode . js-ts-mode)
          (js-base-mode . js-ts-mode)
          (javascript-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (html-mode . html-ts-mode)
          (css-mode . css-ts-mode)))
  (setq treesit-language-source-alist
        '((ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" "master" "grammars/ocaml/src"))
          (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (make "https://github.com/alemuller/tree-sitter-make")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
          (fish "https://github.com/ram02z/tree-sitter-fish")
          (bash "https://github.com/tree-sitter/tree-sitter-bash")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript")
          (html "https://github.com/tree-sitter/tree-sitter-html"))))

(use-package treesit-auto
  :ensure t
  :init
  (setq treesit-auto-langs
        '(ocaml haskell rust c cpp go gomod python lua bash fish yaml toml markdown))
  :config
  (treesit-auto-install-all)
  (treesit-auto-add-to-auto-mode-alist)
  (global-treesit-auto-mode))

(provide 'mod-treesitter)
;;; mod-treesitter.el ends here
