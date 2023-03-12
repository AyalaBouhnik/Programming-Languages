#lang pl

#|Assigment 3- this assigment was submitted by Ayala Bouhnik-Gelbord and Gilad Moalem|#

#|1. Extending the AE language-
We were asked to change the syntax to be in postfix form,
and in addition to add two operators: power and sqr.
power is evaluated to the binary (two argument) function, that takes the first argument and raises it
to the power of the second argument. Thus, the second argument must be an integer.
sqr is evaluated to the unary (a single argument) function, that take a number and raises it to the power of 2.
Note- part of the tests are tests we had in a second assignment with minor changes.
|#

#| BNF for the AE language:
 <AE> ::= <num>
 | { + <AE> <AE> }
 | { - <AE> <AE> }
 | { * <AE> <AE> }
 | { / <AE> <AE> }
 | { power <AE> <AE> }
 | { sqr <AE> }
|# ;; AE abstract syntax trees


 (define-type AE
 [Num Number]
 [Add AE AE]
 [Sub AE AE]
 [Mul AE AE]
 [Div AE AE]
 [Power AE AE]
 [Sqr AE])

 (: parse-sexpr : Sexpr -> AE); to convert s-expressions into AEs
 (define (parse-sexpr sexpr)
 (match sexpr
 [(number: n) (Num n)]
 [(list lhs rhs '+)
(Add (parse-sexpr lhs) (parse-sexpr rhs))]
 [(list lhs rhs '-)
(Sub (parse-sexpr lhs) (parse-sexpr rhs))]
 [(list lhs rhs '*)
(Mul (parse-sexpr lhs) (parse-sexpr rhs))]
 [(list lhs rhs '/)
(Div (parse-sexpr lhs) (parse-sexpr rhs))]
 [(list lhs rhs power)
(Power (parse-sexpr lhs) (parse-sexpr rhs))] ;Power takes two parse-sexprs.
 [(list lhs sqr)
(Sqr (parse-sexpr lhs))]  ;Sqr takes one parse-sexpr.
 [else
(error 'parse-sexpr"bad syntax in ~s" sexpr)]))

 (: parse : String -> AE); parses a string containing an AE expression to AE AST
 (define (parse str)
 (parse-sexpr (string->sexpr str)))

;assuming we chose postfix form grammer with curly parentheses-
;those tests are the same tests as the second assigment (with slightly changes).
(test (parse "{ 3 4 +}") => (Add (Num 3)
                                   (Num 4)))
(test (parse "{ 3 0 /}") => (Div (Num 3)
                                   (Num 0)))
(test (parse "{ 3 4 /}") => (Div (Num 3)
                                   (Num 4)))
(test (parse "3") => (Num 3))
(test (parse "{4 {3 2 -} + }") =>  (Add (Num 4) (Sub (Num 3) (Num 2))))
(test (parse "{1 2 3 4 +}") =error> "bad syntax")
(test (parse "{4 {3  0 - } + }") =>  (Add (Num 4) (Sub (Num 3) (Num 0))))
(test (parse "{+ 1 4}") =error> "bad syntax")

#|an helper function that takes two number arguments, and will return #f if the second one cannot be
cast as an integer, and the first to the power of the second otherwise. |#
 (: power : Number Number -> (U Number #f)) 
 (define (power num1 num2)
   (cond
     [(integer? num2)
      (if (and (eq? num1 0) (< num2 0)) +inf.0
      (expt num1 num2))]
     [else #f])) ;if the second arg is not an integer.

(test (power 3 4) => 81)
(test (power 3 -4) => 1/81)
(test (power 3 4.78) => #f)
(test (power 3.2 2) => 10.240000000000002)

#|In the begining We didn't succeed to use the expt function so We wrote this function.
  but then We understood how to use expt function so We decided to use Racket Built-in function.
(: power-num : Number Number -> Number)
(define (power-num num1 num2)
  (cond
    ((or (= num1 1) (= num2 0)) 1)
        (else (* num1 (power-num num2 (- num2 1))))))
|#

 (: eval : AE -> Number); consumes an AE and computes the corresponding number
 (define (eval expr)
 (cases expr
 [(Num n) n]
 [(Add l r) (+ (eval l) (eval r))]
 [(Sub l r) (- (eval l) (eval r))]
 [(Mul l r) (* (eval l) (eval r))]
 [(Div l r) (/ (eval l) (eval r))]
 ;Power- We first need to check if power on eval l and r return a number, if it is- 
 [(Power l r) (cond
                [(equal? (number? (power (eval l) (eval r))) #t)
                 (first (filter number?(list (power (eval l) (eval r)))))]
                 [else (error 'eval "eval: power expects an integer power,got ~s" expr)])]
 [(Sqr arg) (* (eval arg) (eval arg))] ;Sqr takes one arg and raises it to the power of 2.
   )) 

;eval tests-
(test (eval (Num 3)) => 3)
(test (eval (Add (Num 3) (Num 4))) => 7)
(test (eval (Add (Num 3) (Num 0))) => 3)
(test (eval (Add (Sub (Num 3) (Num 2)) (Num 4))) => 5)
(test (eval (parse "{3 4 +}")) => 7)
(test (eval (parse "3")) => 3)
(test (eval (parse "{4 {3 2 -} +}")) => 5)
(test (eval (parse "{1 2 + 3 4}")) =error> "bad syntax")
(test (eval (parse "{+ 1 2}")) =error> "bad syntax")
(test (eval (parse "{1 * 2}")) =error> "bad syntax")
(test (eval (parse "{3 {5 3 /} *}")) => 5)

 (: run : String -> Number) ;evaluate an AE program contained in a string
 (define (run str)
 (eval (parse str)))


; run function tests-
(test (run "3") => 3)
(test (run "{3 4 +}") => 7)
(test (run "{{3 4 -} 7 +}") => 6)
(test (run "{3 4 +}") => 7)
(test (run "3") => 3)
(test (run "{4 {3 2 -} +}") => 5)
(test (run "{1 + 2 3 4}") =error> "bad syntax")
(test (run "{3 sqr}") => 9)
(test (run "{{3 sqr} 9 -}") => 0)
(test (run "{3 4 power}") => 81)
(test (run "{{3 4 power} 7 +}") => 88)
(test (run "{{0 4 power} 7 +}") => 7)
(test (run "{{0 -4 power} 7 +}") => +inf.0)
(test (run "{{3 -4 power} 7 +}") => 568/81)
(test (run "{-3 -4 power}") => 1/81)
(test (run "{{-3 -4 power} 7 +}") => 568/81)
(test (run "{{2 4 power} {5 sqr} +}") => 41)
(test (run "{{2 4/5 power} {5 sqr} +}") =error> "eval: power expects an integer power,got")

;-----------------------------------------------------------------------------------------------------------------------
#|part two-
In the previous assigment, we have written a BNF for the LE language.
In this work, we completed the full interpreter for this language.|#

;LE abstract syntax trees
(define-type LE = (U LIST ATOM))

;LIST abstract syntax trees
(define-type LIST
  [consLE LE LIST]
  [listLE (Listof LE)]
  [appendLE (Listof LIST)]
  [nullLE Null])

;ATOM abstract syntax trees
(define-type ATOM
  [NumLE Number]
  [SymLE Symbol])

(: parse-sexpr->LEs : (Listof Sexpr) -> (Listof LE)); converts a list of s-expressions into a list of LEs
(define (parse-sexpr->LEs sexprs)
(map parse-sexprLE sexprs))

(: parse-sexpr->LISTs : (Listof Sexpr) -> (Listof LIST)); converts a list of s-exprs into a list of LISTs
(define (parse-sexpr->LISTs sexprs)
(map parse-sexpr->LIST sexprs))

(: parse-sexpr->LIST : Sexpr -> LIST)
(define (parse-sexpr->LIST sexpr)
(let ([ast (parse-sexprLE sexpr)]) ;define a idnetifier ast that is equal to parse-sexprLE sexpr
(if (LIST? ast) ast ;if ast is LIST type, we return ast
(error 'parsesexprLE "expected LIST; got ~s" ast)))) ;if ast is not LIST type -> error


(: parse-sexprLE : Sexpr -> LE) ; to convert s-expressions into LEs
(define (parse-sexprLE sexpr)
(match sexpr
[(number: n) (NumLE n)]
['null (nullLE null)]
[(symbol: s) (SymLE s)]
[(cons 'append rest) (appendLE (parse-sexpr->LISTs rest))] ;apply appendLE on a list of s-exprs that we convert into a list of LISTs
[(list 'cons left_arg right_arg) (consLE (parse-sexprLE left_arg) (parse-sexpr->LIST right_arg))] ;the second arg in cons is a list
[(cons 'list l)
 (listLE (parse-sexpr->LEs l))]
[else (error 'parsesexprLE "bad syntax in ~s" sexpr)])) ;if it's not one of the cases above -> error

(: parseLE : String -> LE) ; parses a string containing a LE expression to a LE AST
(define (parseLE str)
(parse-sexprLE (string->sexpr str)))

;tests for the parser-
(test (parseLE "{list a y a}") => (listLE (list (SymLE 'a) (SymLE 'y) (SymLE 'a))))
(test (parseLE "{cons s {list a y a}}") => (consLE (SymLE 's) (listLE(list(SymLE 'a) (SymLE 'y) (SymLE 'a)))))
(test (parseLE "{cons}") =error> "parsesexprLE: bad syntax in (cons)")


;------------------------------------------------------------------------------------------------------------------------
;eval part-

(: eval-append-args : (Listof LE) -> (Listof (Listof Any))); evaluates LE expressions by reducing them to lists
(define (eval-append-args exprs)
(if (null? exprs) null
(let ([fst-val (evalLE (first exprs))]) ;define new identifier fst-val which is equal to (evalLE (first exprs))
(if (list? fst-val)
(cons fst-val (eval-append-args (rest exprs))) ;if the first arg is list then we are doing a cons with the first arg,
;and we are calling this function (eval-append-args) with the rest.
(error 'evalLE "append argument: expected List got ~s" fst-val))))) ;if the first arg is not a list -> error.
;(In appendLE the forst arg must be a list).


(: evalLE : LE -> Any); evaluates LE expressions by reducing them to numbers
(define (evalLE expr)
(if (LIST? expr)
;LIST cases-
(cases expr
[(listLE lst) (map evalLE lst)] 
 #|I had a problem with this part(consLE), and instead of doing an evaluation it brought back
 to me what the object is made of. I consulted with a classmate and she told me to try to add let befor We Check if l is a list|#
[(consLE arg l)
(let ([ev (evalLE l)]) ;define a new identifier ev ehich is equal to (evalLE l)
(if (list? ev)
 (cons (evalLE arg) ev) ;if ev is a list then it is OK
(error 'evalLE "cons argument: expected List got ~s" l)))] ;there is no option to test this line, because the error is already catched in parsesexprLE: Dr. Said said it's okay). 
[(appendLE lst) (apply append (eval-append-args lst))]
[(nullLE n) null])
;ATOM cases-
(cases expr
[(NumLE n) n]
[(SymLE s) s])))

(: runLE : String -> Any); evaluate a WAE program contained in a string
(define (runLE str)
(evalLE (parseLE str)))

;------------------------------------------------------------------------------------------------------------------------
;tests-

(test (runLE "null") => null)
(test (runLE "12") => 12)
(test (runLE "boo") => 'boo)
(test (runLE "{cons 1 {cons two null}}") =>'(1 two))
(test (runLE "{append 1 {cons two null}}") =error> "parsesexprLE: expected LIST; got")
(test (runLE "{append {cons two null} {cons two null}}") => '(two two))
(test (runLE "{list 1 2 3}") => '(1 2 3))
(test (runLE "{list {cons}}") =error> "parsesexprLE: bad syntax in (cons)")
(test (runLE "{list {cons 2 1}}") =error>"parsesexprLE: expected LIST; got")

(test (eval-append-args (list (SymLE 'a) (SymLE 'y) (SymLE 'a))) =error> "evalLE: append argument: expected List got a")
(test (eval-append-args '()) => null)


