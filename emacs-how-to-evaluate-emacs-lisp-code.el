; To evaluate a single lisp expression, move your cursor to the right of the last closing parenthesis, and call eval-last-sexp [Ctrl+x Ctrl+e].
; To evaluate all elisp code in a text selection, call eval-region. 
;
; | Command name    | Acting Area                                                                                       | Key                                   |
; |-----------------+---------------------------------------------------------------------------------------------------+---------------------------------------|
; | eval-last-sexp  | the complete lisp expression to the left of cursor                                                | [Ctrl+x Ctrl+e]                       |
; | eval-defun      | the function definition block (defun) the cursor is in. (your cursor needs to be near top level.) | [Ctrl+Alt+x](only when in lisp modes) |
; | eval-region     | text selection                                                                                    | null                                  |
; | eval-buffer     | whole file in current window                                                                      | null                                  |
; | load-file       | prompts you for a file name                                                                       | [L] in dired                          |
; | eval-expression | prompts you to type code                                                                          | [Alt+:] or [Esc :]                    |

