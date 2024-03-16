;;; mod-treesitter.el --- TreeSitter configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'treesit)

(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (elisp-mode . elisp-ts-mode)
        (go-mode . go-ts-mode)
        (python-mode . python-ts-mode)
        (json-mode . json-ts-mode)
        (js2-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (html-mode . html-ts-mode)
        (css-mode . css-ts-mode)
        (markdown-mode . markdown-ts-mode)
        (toml-mode . toml-ts-mode)
        (yaml-mode . yaml-ts-mode)
        (makefile-mode . make-ts-mode)))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")))

(provide 'mod-treesitter)
;;; mod-treesitter.el ends here
