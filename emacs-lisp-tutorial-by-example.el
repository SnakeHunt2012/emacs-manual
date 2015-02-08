;; Printing
(message "hi") ; printing
(message "Her age is: %d" 16)            ; %d is for number
(message "Her name is: %s" "Vicky")      ; %s is for string
(message "My list is: %S" (list 8 2 3))  ; %S is for any lisp expression
; You can see the output in the buffer named "*Messages*".
; You can switch to it by calling view-echo-area-messages [F1 e]
;======================================================================


;; Arithmetic Functions
(+ 4 5 1)     ;    -> 10
(- 9 2)       ;    ->  7
(- 9 2 3)     ;    ->  4
(* 2 3)       ;    ->  6
(* 2 3 2)     ;    -> 12
(/ 7 2)       ;    ->  3 (integer part of quotient)
(/ 7 2.0)     ;    ->  3.5
(% 7 4)       ;    ->  3 (Remainder)
(expt 2 3)    ;    ->  8 (power; exponential)

; WARNING3. is a integer, 3.0 is a float
(integerp 3.) ; returns t
(floatp 3.) ; returns nil
(floatp 3.0) ; returns t
; Function names that end with a "p" often means it return either true or false. 
; (The "p" stands for "predicate") t means true; nil means false.

; Converting String and Numbers
(string-to-number "3") ; -> 3
(number-to-string 3)   ; -> "3"

(info "(elisp) Numbers")
;======================================================================


;; True and False
; In elisp, the symbol nil is false, anything else is considered true. 
; So, 0 is true, and empty string "" is also true. 
; Also, nil is equivalent to the empty list (), so () is also false.

; All the following are false. They all evaluate to "nil"
(if nil "yes" "no")    ; -> "no"
(if () "yes" "no")     ; -> "no"
(if '() "yes" "no")    ; -> "no"
(if (list) "yes" "no") ; -> "no", because (list) eval to a empty list, same as ()

; By convention, the symbol t is used for true.
(if t "yes" "no")  ; -> "yes"
(if 0 "yes" "no")  ; -> "yes"
(if "" "yes" "no") ; -> "yes"
(if [] "yes" "no") ; -> "yes". The [] is vector of 0 elements

(and t nil) ; -> nil
(or t nil)  ; -> t
;======================================================================


;; Comparison Functions
; Comparing numbers:
(< 3 4)  ; less than
(> 3 4)  ; greater than
(<= 3 4) ; less or equal to
(>= 3 4) ; greater or equal to

(= 3 3)   ; -> t
(= 3 3.0) ; -> t

(/= 3 4) ; not equal. -> t

; Comparing strings:
(string-equal "this" "this") ; -> t. Case matters.

; For generic equality test, use equal. It tests if two variable/value/symbol have the same datatype and value.
; testing if two values have the same datatype and value.
(equal "abc" "abc") ; -> t
(equal 3 3) ; -> t
(equal 3.0 3.0) ; -> t
(equal 3 3.0) ; -> nil. Because datatype doesn't match.

; testing equality of lists
(equal '(3 4 5) '(3 4 5))  ; -> t
(equal '(3 4 5) '(3 4 "5")) ; -> nil

; testing equality of symbols
(equal 'abc 'abc) ; -> t
(equal 'abc 'cba) ; -> nil

; To test for inequality, the /= is for numbers only, and doesn't work for strings and other lisp data. 
; Use not to negate your equality test, like this:
(not (= 3 4)) ; -> t
(/= 3 4) ; -> t. ¡°/=¡± is for comparing numbers only

(not (equal 3 4)) ; -> t. General way to test inequality.
;======================================================================


;; Global and Local Variables
; setq is used to set variables. Variables need not be declared, and is global.
(setq x 1) ; assign 1 to x
(setq a 3 b 2 c 7) ; assign 3 to a, 2 to b, 7 to c

; To define local variables, use let. The form is: (let (var1 var2 ...) body) where body is (one or more) lisp expressions. 
; The body's last expression's value is returned.
(let (a b)
  (setq a 3)
  (setq b 4)
  (+ a b)) ; returns 7

; Another form of let is this: (let ((var1 val1) (var2 val2) ...) body). Example:
(let ((a 3) (b 4))
  (+ a b)) ; returns 7
; This form lets you set values to variable without using many setq in the body. 
; This form is convenient if you just have a few simple local vars with known values.

(info "(elisp) Variables")
;======================================================================


;; If Then Else
; The form for if statement is: (if test body).
; If you want a ¡°else¡± part, the form is (if test true body false body).
; Examples:
(if (< 3 2) (message "yes") )
(if (< 3 2) (message "yes") (message "no") )

(if nil (message "yes") (message "no") )  ; prints no

(info "(elisp) Control Structures")
; If you do not need a "else" part, you should use the function when instead, because it is more clear. 
; The form is this: (when test expr1 expr2 ...). Its meaning is the same as (if test (progn expr1 expr2 ...)).
;======================================================================


;; A Block of Expressions
; Sometimes you need to group several expressions together as one single expression. 
; This can be done with progn. For example, this code:
(progn (message "a") (message "b")); is equivalent to (message "a") (message "b")

; The purpose of (progn ...) is similar to a block of code {...} in C-like languages. 
; It is used to group together a bunch of expressions into one single parenthesized expression. 
; Most of the time it's used inside "if". For example:
; (if something
;     (progn ; true
;       ...
;     )
;     (progn ; else
;       ...
;     )
; )
; progn returns the last expression in its body.

(info "(elisp) Sequencing")
;======================================================================


;; Iteration
; The following code shows a loop using the while function.
; The form is: (while test body), where body is one or more lisp expressions.
(setq x 0)
(while (< x 4)
  (print (format "yay %d" x))
  (setq x (1+ x)))

(info "(elisp) Iteration")

; In the following sample code, it inserts Unicode chars 32 to 126. 
; First, it sets a local variable x to 32. 
; Then it starts a while loop, insert the corresponding Unicode char (in current buffer), then increase x by 1.
(let ((x 32))
  (while (< x 127)
    (ucs-insert x)
    (setq x (+ x 1))))

; Note: There is no ¡°for¡± loop construct.
;======================================================================


;; Defining a Function
; Basic function definition is of the form: (defun function name (param1 param2 ¡­) "doc string" body) . Example:
(defun myFunction () "testing" (message "Yay!"))
;======================================================================


;; Defining Commands
; To make a function available for interactive use, add (interactive) right after the doc string.
; The following is a basic function definition for interactive use. 
; The function takes no argument. Evaluate the following code. Then, you can call it by execute-extended-command [Alt+x]
(defun yay ()
  "Insert ¡°Yay!¡± at cursor position."
  (interactive)
  (insert "Yay!"))

; The following is a basic function definition, taking one argument from universal-argument [Ctrl+u]. 
; You can call it by typing [Ctrl+u 7 Alt+x myFunction].
(defun myFunction (myArg)
  "Prints the argument"
  (interactive "p")
  (message "Your argument is: %d" myArg))

; The following is a basic function definition taking region as arg. Note the (interactive "r"). 
; The "r" is a code that tells emacs that the function will receive the buffer's begin/end text selection positions as its argument.
(defun myFunction (myStart myEnd)
  "Prints region start and end positions"
  (interactive "r")
  (message "Region begin at: %d, end at: %d" myStart myEnd)
)

; In summary:
;
;    The (interactive ...) clause is a way to make your function interactively callable, and a way to fill out your function's parameters when used interactively.
;    A function with the (interactive ...) clause is called a command, and can be called by execute-extended-command (that is, pressing [Alt+x]).
;
; The (interactive "x...") form takes a single-letter code to indicate how the function gets its arguments from user. There are about 30 codes for interactive, but the most useful are the following:
;
;    (interactive), for commands that takes no argument.
;    (interactive "n"), prompt user for a number as argument. (prompt string can follow right after "n" as part of the string, like this: (interactive "nWhat is your age?").)
;    (interactive "s"), prompt user for a string as argument.
;    (interactive "r"), for commands that takes 2 arguments, the beginning and ending positions of the current region. This form is typically used for commands that act on a text selection.
;
; Here is a function definition template that majority of elisp commands follow:
; (defun myCommand ()
;   "One sentence summary of what this command do. More detailed documentation here."
;   (interactive)
;   (let (localVar1 localVar2 ...)
;     ; do something here ...
;     ; ...
;     ; last expression is returned
;   )
; )

(info "(elisp) Defining Functions")
(info "(elisp) Defining Commands")
;======================================================================


;; The Concept of Symbols in Lisp
