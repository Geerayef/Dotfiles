; Punctuation
";" @punctuation
"," @punctuation
; Keywords
(return) @keyword
(break) @keyword
; Comments
(comment) @comment
; Data types
(string) @type.primitive
(boolean) @type.primitive
(number) @type.primitive
(oid) @type.primitive
(guid) @type.primitive
(type) @type
; Operators
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
 "top"
 "&&"
 "and"
 "||"
 "or"
 "<"
 "<="
 ">="
 ">"
 "=="
 "!="
 "like"
 "unlike"
 "likeCs"
 "unlikeCs"
 "likeNch"
 "unlikeNch"
 "likeNchCs"
 "unlikeNchCs"
 "similar"
 "match"
 "&"
 "&="
 "^"
 "^="
 "|"
 "|="
 ".."
 ] @operator
; Constructs
(var) @variable
(pattern) @variable.local
(parameters) @variable.parameter
(block) @scope
(function name: (id) @function)
(procedure name: (id) @function.procedure)
(method name: (id) @function.method)
; Control flow
(if) @conditional
(select) @conditional
; Iterations
(foreach) @loop
(while) @loop
; Builtins
(expr_access) @property
(expr_call) @function.call
(node_exec) @variable.node
