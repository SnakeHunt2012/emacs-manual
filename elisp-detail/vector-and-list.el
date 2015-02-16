;; Vector
; Create Vector
(setq v (vector 1 2 3)) 
(setq v (vector 1 2 (1+ 3))) ; each element will be evaluated

(setq v [1 2 3])        
(setq v [1 2 (1+ 3)])        ; each element will NOT be evaluated

; Length
(length (vector 3 4 5)) ; -> 3

; Getting an Element
(elt (vector "a" "b" "c") 0) ; -> "a"

; Changing an Element
(setf v (vector "a" "b" "c")) ; -> ["a" "b" "c"]
(aset v 0 "d") ; -> "d"
v ; -> ["d" "b" "c"]

; Nested Vector
[[1 2] [3 4]] ; 2 by 2 matrix
[8 [3 [2 9] c] 7 [4 "b"]] ; (length (vector 3 4 5))

; Map: mapcar & mapc
; add 1 to each
(mapcar  '1+ [1 2 3]) ; -> (4 5 6)
; One simple way to go thru a vector is using mapcar. 
; Note that it returns a list, not vector.

; get first element of each row
(mapcar (lambda (x) (elt x 0)) [[1 2] [3 4]]) ; -> (1 3)
; If you want to map to list but don't need the return value, use mapc.

; you can also use while. Example:
(setq v [3 4 5])
(setq i 0)
(while (< i (length v))
  (insert (format "%d" (elt v i)))
  (setq i (1+ i))) ; insert 345

; Join Vectors
; (vconcat <sequence1> <sequence2> ...) join any sequence types and return a vector. (List and vector are both sequence types.)
(vconcat [3 4] ["a" "b"] [a b]) ; -> [3 4 "a" "b" a b]

; Convert Vector to List
; (append <sequence1> <sequence2> ... nil) join any sequence types and return a list. (List and vector are both sequence types.)
(append [3 4] ["a" "b"] [a b] nil) ; -> (3 4 "a" "b" a b)

(info "(elisp) Vector Functions")

;======================================================================


;; List
; Create List
; To create a list, write it like this (list a b ...).
; If you do not want the elements evaluated, write it like this: '(a b ...).
(list 1 2 3)       ; -> (1 2 3)
(list 1 2 (+ 1 2)) ; each element will be evaluated

'(1 2 3)           ; -> (1 2 3)
'(1 2 (+ 1 2))     ; each element will NOT be evaluated

(setq my-list '(a b c))
(message "%s" my-list)

(let ((x 1) (y 2) (z 3))
  (message "%s" (list x y z))) ; print "(1 2 3)"

; Length
; (length <l>) -> return number of elements.
(length (list "a" "b" "c"))

; Getting an Element
; (car <l>)        -> first element
; (car (last <l>)) -> last element
; (nth <n> <l>)    -> nth element (counts from 0)
(setq my-list (list "a" "b" "c"))
(car my-list)        ; -> "a"
(nth 1 my-list)      ; -> "b" (counts from 0)
(car (last my-list)) ; -> "c"

; (cdr <l>)         ; 2nd to last elements, as a list.
; (nthcdr <n> <l>)  ; nth to last elements, as a list.
; (butlast <l> <n>) ; without the last n elements, as a list.
(setq my-list (list "a" "b" "c"))
(cdr my-list)       ; -> ("b" "c")
(nthcdr 1 my-list)  ; -> ("b" "c")
(butlast my-list 1) ; -> ("a" "b")

; Changing an Element
; (ocns <x> <l>) add x to front. (prepend)
; (append <l1> <l2>) join two lists
(cons "a" (list "b" "c" "d"))              ; -> ("a" "b" "c" "d")
(cons (list "a" "b") (list "c" "d"))   ; -> (("a" "b") "c" "d")
(append (list "a" "b") (list "c" "d")) ; -> ("a" "b" "c" "c")

; (pop <l>) Remove first element from the variable. Returns the removed element.
; (nbutlast <l> <n>) Remove last <n> elements from the variable. Returns the new value of the variable.
; (setcar <l> <x>) replaces the first element in <l> with x. Returns <x>.
; (setcdr <l> <x>) replaces the rest of elements in <l> with x. Returns <x>.
(setq my-list '("a" "b" "c")) ; -> ("a" "b" "c")
(pop my-list)                 ; -> "a"
(print my-list)               ; -> ("b" "c")

(info "(elisp) Lists")

; Map: mapcar & mapc
; Here's a typical way of going thru a list. It is done with mapcar.
; Following examples use builtin functions:
; add one to each list member using the build in function 1+
(mapcar '1+ (list 1 2 3 4))              ; -> (2 3 4 5)
; take the 1st element of each
(mapcar 'car '((1 2) (3 4) (5 6) (7 8))) ; -> (1 3 5 7)

; Following examples use user-defined function ¡°lambda¡± created inline:
; add one to each list member
(mapcar (lambda (x) (+ x 1))
	(list 1 2 3 4)) ; -> (2 3 4 5)
; take the 2nd element of each
(mapcar (lambda (x) (nth 1 x))
	'((1 2) (3 4))) ; -> (1 3)

; Loop thru List with "while"
(let ((my-list '(1 2 3 4)))
  (while my-list
    (message "%s" (pop my-list))
    (sleep-for 1)))

; Number Sequence; Range
; (number-sequence <m> <n> <step>) -> return a list of a range of numbers, from <m> to <n>, in increment of <step>.
(number-sequence 5)     ; -> (5)
(number-sequence 2 8)   ; -> (2 3 4 5 6 7 8)
(number-sequence 0 9 3) ; -> (0 3 6 9)
;======================================================================

;; Vector vs List
;
;     _____________________________________________
;    |                                             |
;    |          Sequence                           |
;    |  ______   ________________________________  |
;    | |      | |                                | |
;    | | List | |             Array              | |
;    | |      | |    ________       ________     | |
;    | |______| |   |        |     |        |    | |
;    |          |   | Vector |     | String |    | |
;    |          |   |________|     |________|    | |
;    |          |  ____________   _____________  | |
;    |          | |            | |             | | |
;    |          | | Char-table | | Bool-vector | | |
;    |          | |____________| |_____________| | |
;    |          |________________________________| |
;    |_____________________________________________|
;
; Vector: Access time to any element is constant.
; List: Access time to an element is proportional to the distance of the element's position in the list.
; List's length can grow by prepending with cons, and can have the first element dropped by cdr. These operations has constant time.
; Vector's length cannot change. (if you create a new copy with extra item, the time is proportional to the length)

;Lisp culture is to almost always use list. I recommend using vector as much as possible. Use list ONLY IF you need to constantly grow the list. Even for that case, i recommend using vector by starting with large length, using the function make-vector.
; You can nest list £¦ vectors in any way. Example:
['(3 4) '(5 8) [4 2]]
(list [8 7] '(4 1))
;======================================================================
