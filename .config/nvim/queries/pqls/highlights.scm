; ~ Punctuation
";" @punctuation
":" @punctuation
"," @punctuation
"." @punctuation
"(" @punctuation.paren
")" @punctuation.paren
; ~ Keywords
(return) @keyword
(break) @keyword
(option (id)) @identifier
((option) @option
          (#match? @option "[Oo][Pp][Tt][Ii][Oo][Nn]")) @keyword
(import) @keyword
; ~ Comments
(comment) @comment
; ~ Data types
(string) @type.builtin
(boolean) @type.builtin
(number) @type.builtin
(oid) @type.builtin
(guid) @type.builtin
(array) @type.builtin
(type) @type
; ~ Operators
[
 "?"
 "!"
 ":"
 "="
 "+"
 "+="
 "-"
 "-="
 "*"
 "*="
 "/"
 "/="
 "%"
 "%="
 ">>"
 ">>="
 "<<"
 "<<="
 "++"
 "--"
 (("top") @op (#match? @op "[Tt][Oo][Pp]"))
 "&&"
 (("and") @op (#match? @op "[Aa][Nn][Dd]"))
 "||"
 (("or") @op (#match? @op "[Oo][Rr]"))
 "<"
 "<="
 ">="
 ">"
 "=="
 "!="
 (("like") @op (#match? @op "[Ll][Ii][Kk][Ee]"))
 (("unlike") @op (#match? @op "[Uu][Nn][Ll][Ii][Kk][Ee]"))
 (("likeCs") @op (#match? @op "[Ll][Ii][Kk][Ee][Cc][Ss]"))
 (("unlikeCs") @op (#match? @op "[Uu][Nn][Ll][Ii][Kk][Ee][Cc][Ss]"))
 (("likeNch") @op (#match? @op "[Ll][Ii][Kk][Ee][Nn][Cc][Hh]"))
 (("unlikeNch") @op (#match? @op "[Uu][Nn][Ll][Ii][Kk][Ee][Nn][Cc][Hh]"))
 (("likeNchCs") @op (#match? @op "[Ll][Ii][Kk][Ee][Nn][Cc][Hh][Cc][Ss]"))
 (("unlikeNchCs") @op (#match? @op "[Uu][Nn][Ll][Ii][Kk][Ee][Nn][Cc][Hh][Cc][Ss]"))
 (("similar") @op (#match? @op "[Ss][Ii][Mm][Ii][Ll][Aa][Rr]"))
 (("match") @op (#match? @op "[Mm][Aa][Tt][Cc][Hh]"))
 "&"
 "&="
 "^"
 "^="
 "|"
 "|="
 ".."
 ] @operator
; ~ Constructs
(var) @variable
(pattern) @variable.local
(parameters) @variable.parameter
((block) @block
         (#match? @block "[Bb][Ee][Gg][Ii][Nn]")) @keyword
((block) @block
         (#match? @block "[Ee][Nn][Dd]")) @keyword
(function name: (id) @function)
((function) @function
            (#match? @function "[Ff][Uu][Nn][Cc][Tt][Ii][Oo][Nn]")) @keyword
(procedure name: (id) @function.procedure)
((procedure) @procedure
             (#match? @procedure "[Pp][Rr][Oo][Cc][Dd][Uu][Rr][Ee]")) @keyword
(method name: (id) @function.method)
((method) @method
          (#match? @method "[Mm][Ee][Tt][Hh][Oo][Dd]")) @keyword
(expr_call (id) @function.call)
(query root: (_) @variable (backslash) @keyword attributes: (fields) @property)
((query) @query
         (#match? @query "[Aa][Ss]")) @keyword
((parameters) @params
              (#match? @params "[Aa][Ss]")) @keyword
((fields) @fields
          (#match? @fields "[Aa][Ss]")) @keyword
; ~ Control flow
(if) @keyword
(select) @keyword
; ((select) @select
;           (#match? @select "[Cc][Aa][Ss][Ee]")) @keyword
; ((select) @select
;           (#match? @select "[Dd][Ee][Ff][Aa][Uu][Ll][Tt]")) @keyword
; ~ Iteration
(foreach) @keyword
((foreach) @foreach
         (#match? @foreach "[Ff][Oo][Rr][Ee][Aa][Cc][Hh]")) @keyword
((foreach) @foreach
         (#match? @foreach "[Ii][Nn]")) @keyword
((foreach) @foreach
         (#match? @foreach "[Nn][Ee][Xx][Tt]")) @keyword
((while) @while
         (#match? @while "[Ww][Hh][Ii][Ll][Ee]")) @keyword
((while) @while
         (#match? @while "[Nn][Ee][Xx][Tt]")) @keyword
; ~ Builtins
(expr_access) @property
(expr_access (var) @variable)
(node_exec) @variable.node
(builtin) @keyword
