#lang pl 02

#|Assignment 2- this assignment was submitted by Ayala Bouhnik-Gelbord and Gilad Moalem|#

#|question 1-
Explenation: BNF for “LE”: a similarly simple language of “List Expressions”.
Valid ‘programs’ in this language are correspond to Racket expressions that evaluate to S-expressions holding
numbers and symbols.
The valid functions that can be used in these expressions are cons, list, and append and null.
The grammar allows only cons with an expression that represents a list, list can have any number of
arguments, and append can have any number of list arguments.
Plain values are either numbers or quoted symbols — no quoted lists, and only the quote character can be used.


**BNF:
The LE grammer

  <LE> ::=  <LEB>             (1)
           | {cons <LE> <ML>} (2)
           | {append <LEL>}   (3)
           | {append}         (4)
           | {list <LEL>}     (5)
           | {list}           (6)

  <LEB> :: = <num>            (7)
           |'<sym>            (8)
           | <null>           (9)

  <LEL> :: = <LEL> <ML>       (10)
             |<ML>            (11)

  <ML> :: = <ML> <LE>         (12)
            |<LE>             (13)


-------------------------------------------------------------------------------------------------------------------------
d. 3 different LE expressions, in which all plain values are sub-words of either our name or ID number.

1***{append {const 'Gil {list 'ad}} {list 'Moa 'lem 312}}

<LE> (3)=> {append <LEL>} (10)=> {append <LEL> <ML>} (11)=> {append <ML> <ML>}
	(13)=> {append <LE> <ML>} (2)=> {append {cons <LE> <ML>} <ML>} 
	(1)=> {append {cons <LEB> <ML>} <ML>} (8)=> {append {cons '<sym> <ML>} <ML>}
	(13)=> {append {cons '<sym> <LE>} <ML>} (5)=> {append {cons '<sym> {list <LEL>}} <ML>}
	(11)=> {append {cons '<sym> {list <ML>}} <ML>} (13)=> {append {cons '<sym> {list <LE>}} <ML>}
	(1)=> {append {cons '<sym> {list <LEB>}} <ML>} (8)=> {append {cons '<sym> {list '<sym>}} <ML>}
	(13)=> {append {cons '<sym> {list '<sym>}} <LE>} (5)=> {append {cons '<sym> {list '<sym>}} {list <LEL>}}
	(11)=>{append {cons '<sym> {list '<sym>}} {list <ML>}} (12)=> {append {cons '<sym> {list '<sym>}} {list <ML> <LE>}}
	(12)=>{append {cons '<sym> {list '<sym>}} {list <ML> <LE> <LE>}} (13)=> {append {cons '<sym> {list '<sym>}} {list <LE> <LE> <LE>}}
	(1*)=>{append {cons '<sym> {list '<sym>}} {list <LEB> <LEB> <LEB>}}
	(8)=>{append {cons '<sym> {list '<sym>}} {list '<sym> '<sym> <LEB>}}
	(7)=>{append {cons '<sym> {list '<sym>}} {list '<sym> '<sym> <num>}}
        ====>{append {cons 'Gil {list 'ad}} {list 'Moa 'lem 312}}


2***(cons 206 (cons (append (cons 'Aya null) (list 'Bou 'Gel)) (list 354 873)))

<LE>    (2)=> {cons <LE> <ML>} (1)=> {cons <LEB> <ML>} (7)=> {cons <num> <ML>} (13)=>{cons <num> <LE>} 
	(2)=>{cons <num> {cons <LE> <ML>}} (3)=> {cons <num> {cons {append <LEL>} <ML>}} 
	(10)=>{cons <num> {cons {append <LEL> <ML>} <ML>}} (11)=> {cons <num> {cons {append <ML> <ML>} <ML>}}
	(13*)=> {cons <num> {cons {append <LE> <LE>} <LE>}} (5*)=> {cons <num> {cons {append <LE> {list <LEL>}} {list <LEL>}}}
	(2)=> {cons <num> {cons {append {cons <LE> <ML>} {list <LEL>}} {list <LEL>}}}
	(1)=> {cons <num> {cons {append {cons <LEB> <ML>} {list <LEL>}} {list <LEL>}}}
	(8)=> {cons <num> {cons {append {cons '<sym> <ML>} {list <LEL>}} {list <LEL>}}}
	(13)=> {cons <num> {cons {append {cons '<sym> <LE>} {list <LEL>}} {list <LEL>}}}
	(1)=> {cons <num> {cons {append {cons '<sym> <LEB>} {list <LEL>}} {list <LEL>}}}
	(9)=> {cons <num> {cons {append {cons '<sym> <null>} {list <LEL>}} {list <LEL>}}}
	(11*)=> {cons <num> {cons {append {cons '<sym> <null>} {list <ML>}} {list <ML>}}}
	(12*)=> {cons <num> {cons {append {cons '<sym> <null>} {list <ML> <LE>}} {list <ML> <LE>}}}
	(13*)=> {cons <num> {cons {append {cons '<sym> <null>} {list <LE> <LE>}} {list <LE> <LE>}}}
	(1*)=> {cons <num> {cons {append {cons '<sym> <null>} {list <LEB> <LEB>}} {list <LEB> <LEB>}}}
	(8*)=> {cons <num> {cons {append {cons '<sym> <null>} {list '<sym> '<sym>}} {list <LEB> <LEB>}}}
	(7*)=> {cons <num> {cons {append {cons '<sym> <null>} {list '<sym> '<sym>}} {list <num> <num>}}}
         ====> {cons 206 {cons {append {cons 'Aya <null>} {list 'Bou 'Gel}} {list 654 873}}}


3*** {append {cons 206 {list 654}} {list null 873} {list 'Aya 'la}}

<LE>    (3)=> {append <LEL>} (10)=> {append <LEL> <ML>} (10)=> {append <LEL> <ML> <ML>} 
	(11)=> {append <ML> <ML> <ML>} (13*)=> {append <LE> <LE> <LE>} 
	(5*)=> {append <LE> {list <LEL>} {list <LEL>}} 
	(2)=> {append {cons <LE> <ML>} {list <LEL>} {list <LEL>}} 
	(11*)=> {append {cons <LE> <ML>} {list <ML>} {list <ML>}}
	(12*)=> {append {cons <LE> <ML>} {list <ML> <LE>} {list <ML> <LE>}}
        (13*)=> {append {cons <LE> <ML>} {list <LE> <LE>} {list <LE> <LE>}}
        (1*)=> {append {cons <LEB> <ML>} {list <LEB> <LEB>} {list <LEB> <LEB>}}
        (13)=> {append {cons <LEB> {list <LEL>}} {list <LEB> <LEB>} {list <LEB> <LEB>}}
        (11)=> {append {cons <LEB> {list <ML>}} {list <LEB> <LEB>} {list <LEB> <LEB>}}
        (13)=>{append {cons <LEB> {list <LE>}} {list <LEB> <LEB>} {list <LEB> <LEB>}}
        (1)=> {append {cons <LEB> {list <LEB>}} {list <LEB> <LEB>} {list <LEB> <LEB>}}
        (7*)=> {append {cons <num> {list <num>}} {list <LEB> <num>} {list <LEB> <LEB>}}
        (8*)=> {append {cons <num> {list <num>}} {list <LEB> <num>} {list '<sym> '<sym>}}
        (9)=> {append {cons <num> {list <num>}} {list null <num>} {list '<sym> '<sym>}}
        ===> {append {cons 206 {list 654}} {list null 873} {list 'Aya 'la}}

|#
;--------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------

#|question 2-
Make AE language use infix syntax and Allowing division by zero.|#
#|


The AE grammer

  <AE> ::= <num>
           | { + <AE> <AE> }
           | { - <AE> <AE> }
           | { * <AE> <AE> }
           | { / <AE> <AE> }
|#

(define-type AE
  [Num Number]
  [Add AE AE]
  [Sub AE AE]
  [Mul AE AE]
  [Div AE AE])


(: parse-sexpr : Sexpr -> AE)
(define (parse-sexpr sxp)
  (match sxp
    [(number: n) (Num n)]

    
    [(list l '+ r) (Add (parse-sexpr l)(parse-sexpr r))]
    [(list l '- r) (Sub (parse-sexpr l)(parse-sexpr r))]
    [(list l '* r) (Mul (parse-sexpr l)(parse-sexpr r))]
    [(list l '/ r) (Div (parse-sexpr l)(parse-sexpr r))]
    [else (error 'parse-sexpr "bad syntax in ~s" sxp)]))


(: parse : String -> AE)
(define (parse code)
  (parse-sexpr (string->sexpr code)))


;assuming we chose prefix form grammer with curly parentheses
(test (parse "{ 3 + 4 }") => (Add (Num 3)
                                   (Num 4)))
(test (parse "{ 3 / 0 }") => (Div (Num 3)
                                   (Num 0)))
(test (parse "{ 3 / 4 }") => (Div (Num 3)
                                   (Num 4)))
(test (parse "3") => (Num 3))
(test (parse "{4 + {3 - 2} }") =>  (Add (Num 4) (Sub (Num 3) (Num 2))))
(test (parse "{+ 1 2 3 4}") =error> "bad syntax")
(test (parse "{4 + {3 - 0} }") =>  (Add (Num 4) (Sub (Num 3) (Num 0))))
(test (parse "{* 1 4}") =error> "bad syntax")


#|
The goal of parse:
Input:  string describing the program
Output: Abstract Syntax Tree (or an exception if the string is not a valid program)

Two main phases:
1. Read -- turn the string into a simple data structure (we will use the Racket type Sexpr).
2. Actual Parsing -- turn an Sexpr into an AST


Definition of the pl type Sexpr:
Basis -- any Number/Symbol is an Sexpr
General -- any list of Sexpr is an Sexpr

|#



#|
;;; ====== EVAL  ==============
<AE> ::= <num>               a 
         | { + <AE> <AE> }   b
         | { - <AE> <AE> }   c

eval(<num>) = <num>
eval({+ E1 E2}) =  eval(E1) + eval(E2)
eval({- E1 E2}) =  eval(E1) - eval(E2)
|#



(: eval : AE -> Number)
(define (eval exp)
  (cases exp
    [(Num n) n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (if (eq? (eval r) 0) 999
               (/ (eval l) (eval r)))])) ;if you div by 0 you will get 999, else- a regular division.

(: run : String -> Number)
(define (run code)
  (eval (parse code)))


(test (eval (Num 3)) => 3)
(test (eval (Add (Num 3) (Num 4))) => 7)
(test (eval (Add (Num 3) (Num 0))) => 3)
(test (eval (Add (Sub (Num 3) (Num 2)) (Num 4))) => 5)

(test (eval (parse "{3 + 4 }")) => 7)
(test (eval (parse "3")) => 3)
(test (eval (parse "{4 + {3 - 2} }")) => 5)
(test (eval (parse "{+ 1 2 3 4}")) =error> "bad syntax")
(test (eval (parse "{+ 1 2}")) =error> "bad syntax")
(test (eval (parse "{* 1 2}")) =error> "bad syntax")

(test (eval (parse "{3 * {5 / 3} }")) => 5)
(test (run "{3 + 4 }") => 7)
(test (run "3") => 3)
(test (run "{4 + {3 - 2} }") => 5)
(test (run "{4 + {3 / 0} }") => 1003) 
(test (run "{{24 + 24} / {24 - 24}}") => 999)
(test (run "{+ 1 2 3 4}") =error> "bad syntax")

(test (eval (parse "{3 / 0 }")) => 999)
(test (eval (parse "{0 / 0 }")) => 999)
(test (eval (parse "{98 / 0 }")) => 999)

;--------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------

#|question 3-
sum-of-squares- takes a list of numbers as input, and produces a number which is the sum of the
squares of all of the numbers in the list. The function complexity is one-liner.
|#

;an helper function to calculate the square of a given number.
(: square : Number -> Number)
(define (square num)
  (* num num))

(test (square 3) => 9)
(test (square -3) => 9)
(test (square 0) => 0)
(test (square 3.5) => 12.25)

(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares lst)
  (foldl + 0 (map square lst))) ;0 is the initial value. foldl applies a procedure to the elements of one or more lists. map
;combines the return values into a list, foldl combines the return values in an arbitrary way that
;is determined by proc. 

(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(0 0)) => 0)
(test (sum-of-squares '(0 1 1 1)) => 3)
(test (sum-of-squares '(-1 -2 -3)) => 14)
(test (sum-of-squares '()) => 0) ;because of the initial value.

;--------------------------------------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------------------------------------
#|question 4-
binary tree-
*Leaf is holding a number.
*Node contains a binary tree on the left and one on the right. 
|#

;a-
(define-type BINTREE
  [Leaf Number] ;The Leaf is holding a number.
  [Node BINTREE BINTREE]) ;contains a binary tree on the left and one on the right.


#|b-
tree-map(higher-order function)-
takes in a numeric function f and a binary tree, and returns a tree with the
same shape but using f(n) for values in its leaves.
|#
(: tree-map : (Number -> Number) BINTREE -> BINTREE) ;takes a numeric function and a binary tree.
(define (tree-map num-fun binary-tree)
  (cases binary-tree 
    [(Leaf num) (Leaf(num-fun num))]
    [(Node right left) (Node(tree-map num-fun right) (tree-map num-fun left))]))
;we used cases and not match because it is a type that we defined. 

(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))
(test (tree-map sub1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => (Node (Leaf 0) (Node (Leaf 1) (Leaf 2))))
(test (tree-map add1 (Leaf 1)) => (Leaf 2))
(test (tree-map sub1 (Leaf -11)) => (Leaf -12))
(test (tree-map add1 (Node (Leaf 2) (Leaf 3))) => (Node (Leaf 3) (Leaf 4)))
