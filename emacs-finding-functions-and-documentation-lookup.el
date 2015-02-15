;; Inline Documentation Lookup
; Call describe-function [C-h f], then type the function name. If the word the cursor is on is a valid elisp function name, emacs will use that by default.
; Call describe-variable [C-h v] for inline doc of variables.
; You can use the asterisk ¡°*¡± as a wildcard when looking up function or variable doc.
; Once the function's inline doc string page comes up, you can jump to the function's location in source code by clicking on underlined file name (or press Tab to move your cursor to the link then press Enter).
;======================================================================


;; Searching for Functions
; To search command names, call apropos-command [C-h a].
; To search both function and command names, call apropos-command with a empty argument, like this: [C-u C-h a].
; To search variable names, call apropos-variable.
; To search variable values, call apropos-value.
; To search all symbols space (commands, functions, variables), call apropos.
;======================================================================


;; Finding a Function's Documentation in Elisp Manual
; Use elisp-index-search to find a function's documentation in the emacs lisp manual.
; Use emacs-index-search to find a function's documentation in the emacs manual.
;======================================================================
