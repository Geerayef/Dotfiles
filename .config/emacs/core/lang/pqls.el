;;;; pqls.el --- PQL script support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;; Error during redisplay: (jit-lock-function 1) signaled (treesit-query-error "Node type error at" 3 "[\"import\" \"method\" \"function\" \"procedure\" \"return\" \"break\" \"begin\" \"end\" \"if\" \"then\" \"endif\" \"elseif\" \"else\" \"foreach\" \"in\" \"next\" \"while\" \"select\" \"case\" \"default\" \"endselect\" \"as\" \"option\" \"referto\" \"of\" \"and\" \"or\" \"like\" \"unlike\" \"likeCs\" \"unlikeCs\" \"likeNch\" \"unlikeNch\" \"likeNchCs\" \"unlikeNchCs\" \"similar\" \"match\" \"top\" \"true\" \"false\"] @keyword (return) @keyword (break) @keyword (import) @keyword (block) @keyword (if) @keyword (select) @keyword (foreach) @keyword (while) @keyword" "Debug the query with `treesit-query-validate'")
;; Error during redisplay: (jit-lock-function 1501) signaled (treesit-query-error "Node type error at" 3 "[\"import\" \"method\" \"function\" \"procedure\" \"return\" \"break\" \"begin\" \"end\" \"if\" \"then\" \"endif\" \"elseif\" \"else\" \"foreach\" \"in\" \"next\" \"while\" \"select\" \"case\" \"default\" \"endselect\" \"as\" \"option\" \"referto\" \"of\" \"and\" \"or\" \"like\" \"unlike\" \"likeCs\" \"unlikeCs\" \"likeNch\" \"unlikeNch\" \"likeNchCs\" \"unlikeNchCs\" \"similar\" \"match\" \"top\" \"true\" \"false\"] @keyword (return) @keyword (break) @keyword (import) @keyword (block) @keyword (if) @keyword (select) @keyword (foreach) @keyword (while) @keyword" "Debug the query with `treesit-query-validate'")

(require 'treesit)

(defcustom pqls-ts-mode-indent-offset 2
  "Number of spaces used for indenting in `pqls-ts-mode'."
  :type 'integer
  :safe 'integerp
  :group 'pqls)

(defvar pqls-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\"" table)
    table)
  "Syntax table for `pqls-ts-mode'.")

(defvar pqls-ts-mode--indent-rules
  `((pqls
     ((node-is "}") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((parent-is "block") parent-bol pqls-ts-mode-indent-offset)
     ((parent-is "function") parent-bol pqls-ts-mode-indent-offset)
     ((parent-is "procedure") parent-bol pqls-ts-mode-indent-offset)
     ((parent-is "method") parent-bol pqls-ts-mode-indent-offset)
     ((parent-is "if") parent-bol pqls-ts-mode-indent-offset)
     ((parent-is "select") parent-bol pqls-ts-mode-indent-offset)
     ((parent-is "foreach") parent-bol pqls-ts-mode-indent-offset)
     ((parent-is "while") parent-bol pqls-ts-mode-indent-offset))))

(defvar pqls-ts-mode--keywords
  '("import" "method" "function" "procedure" "return" "break"
    "begin" "end" "if" "then" "endif" "elseif" "else"
    "foreach" "in" "next" "while" "select" "case" "default" "endselect"
    "as" "option" "referto" "of" "and" "or" "like" "unlike"
    "likeCs" "unlikeCs" "likeNch" "unlikeNch" "likeNchCs" "unlikeNchCs"
    "similar" "match" "top" "true" "false"))

(defvar pqls-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'pqls
   :feature 'comment
   '((comment) @comment)

   :language 'pqls
   :feature 'keyword
   `([,@pqls-ts-mode--keywords] @keyword
     (return) @keyword
     (break) @keyword
     (import) @keyword
     (block) @keyword
     (if) @keyword
     (select) @keyword
     (foreach) @keyword
     (while) @keyword)

   :language 'pqls
   :feature 'string
   '((string) @type.primitive)

   :language 'pqls
   :feature 'number
   '((number) @type.primitive)

   :language 'pqls
   :feature 'type
   '((type) @type
     (boolean) @type.primitive
     (oid) @type.primitive
     (guid) @type.primitive)

   :language 'pqls
   :feature 'function
   '((function name: (id) @function)
     (procedure name: (id) @function)
     (method name: (id) @function)
     (expr_call (id) @function.call))

   :language 'pqls
   :feature 'variable
   '((var) @variable
     (pattern) @variable.local
     (parameters) @variable.parameter)

   :language 'pqls
   :feature 'property
   '((expr_access) @property)

   :language 'pqls
   :feature 'operator
   '(["?" "!" ":" "=" "+" "+=" "-" "-=" "*" "*=" "/" "/=" "%" "%="
      ">>" ">>=" "<<" "<<=" "++" "--" "&&" "||" "<" "<=" ">=" ">"
      "==" "!=" "&" "&=" "^" "^=" "|" "|=" ".."] @operator)))

(define-derived-mode pqls-ts-mode prog-mode "pqls"
  "Major mode for PQL script based on Tree-Sitter."
  :syntax-table pqls-ts-mode--syntax-table
  (when (treesit-ready-p 'pqls)
    (treesit-parser-create 'pqls)
    (setq-local treesit-font-lock-settings pqls-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment keyword)
                  (string number type)
                  (function variable property operator)))
    (setq-local treesit-simple-indent-rules pqls-ts-mode--indent-rules)
    (treesit-major-mode-setup)))

(add-to-list 'auto-mode-alist '("\\.pqls\\'" . pqls-ts-mode))

(provide 'pqls)
