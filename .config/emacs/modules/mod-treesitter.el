;;;; mod-treesitter.el --- Tree sitter -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq major-mode-remap-alist
      '((ocaml-mode . ocaml-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (c-or-c++-mode . c-or-c++-ts-mode)
        (rust-mode . rust-ts-mode)
        ;; (emacs-lisp-mode . elisp-ts-mode)
        (lisp-mode . lisp-ts-mode)
        (clojure-mode . clojure-ts-mode)
        (scheme-mode . scheme-ts-mode)
        (go-mode . go-ts-mode)
        (python-mode . python-ts-mode)
        (bash-mode . bash-ts-mode)
        (fish-mode . fish-ts-mode)
        (doctex-mode . docTeX-mode)
        (latex-mode . LaTeX-mode)
        (texinfo-mode . Texinfo-mode)
        (plain-tex-mode . plain-TeX-mode)
        (tex-mode . TeX-tex-mode)
        (json-mode . json-ts-mode)
        (js2-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (html-mode . html-ts-mode)
        (css-mode . css-ts-mode)
        (toml-mode . toml-ts-mode)
        (yaml-mode . yaml-ts-mode)
        (makefile-mode . make-ts-mode)))

(setq treesit-language-source-alist
      '((ocaml "https://github.com/tree-sitter/tree-sitter-ocaml" "master" "ocaml/src")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (lua "https://github.com/Azganoth/tree-sitter-lua")
        (bash "https://github.com/tree-sitter/tree-sitter-bash")
        (fish "https://github.com/ram02z/tree-sitter-fish")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp" "main" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")))

(dolist (lang treesit-language-source-alist)
  (unless (treesit-language-available-p (car lang))
    (treesit-install-language-grammar (car lang))))

(use-package treesit
  :ensure nil)

(use-package treesit-auto
  :ensure t
  :init
  (setq treesit-auto-langs '(c cpp ocaml python rust elisp commonlisp lua bash fish yaml toml markdown javascript))
  :custom (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(provide 'mod-treesitter)
;;; mod-treesitter.el ends here
