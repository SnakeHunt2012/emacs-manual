;; Navigating Nested Code
;
; | Key     | Command          | Purpose                                                                   |
; |---------+------------------+---------------------------------------------------------------------------|
; | C-left  | backward-sexp    | Move to previous sibling (move to the (beginning of) previous sexp unit)  |
; | C-right | forward-sexp     | Move to next sibling (move to the (end of) next sexp unit)                |
; | C-up    | backward-up-list | Move to parent (move to the (beginning of) outer paren pair)              |
; | C-down  | down-list        | Move to first child (move into the (beginning of) first inner paren pair) |
;
; The following is lisp source code laid out in a way to show its tree structure. 
; You should try the above commands on it. It is very helpful to understand how sexp corresponds to a tree, and how the commands move the cursor exactly.
(defun
  fold
  (f x li)
  "Applies (f x ele) recursively to the list li ..."
  (let
    (
      (li2 li)
      (ele)
      (x2 x)
    )
    (while
      (setq ele (pop li2))
      (setq x2 (funcall f x2 ele))
    )
    x2
  )
)

; Moving to Previous/Next Sibling that Has Children
; Use forward-list to jump to next sibling that has children. (i.e. skip siblings that does not have children.)
; Use backward-list to jump to next sibling that has children. (i.e. skip siblings that does not have children.)
; 
; The following is lisp source code laid out in a way to show its tree structure. 
; You should try the above commands on it. It is very helpful to understand how sexp corresponds to a tree, and how the commands move the cursor exactly.
(a (b) c d (e f))

;======================================================================


;; Selecting Lisp Code by Unit
; You can use the command mark-sexp [C-M-SPC] to select a complete sexp. Your cursor must be on the left bracket.
; To select a complete sexp, type [C-up] then [C-M-SPC].
;======================================================================
