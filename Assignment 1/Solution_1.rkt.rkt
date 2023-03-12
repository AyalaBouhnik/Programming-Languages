#lang pl

;Written by Ayala Bouhnik-Gelbord, 31/03/2022
;Assignment 1- question 1:

#|We were asked to write in every question what was the challange-
 this question was'nt really difficult, The main challange was the syntax and the liitle "annoying" things in
 learning a new language.|#

;Aמ helper function that returns the minimum number between two numbers-
;NOTE- this function already exist (it called min), but I wrote it because I wanted to practice more.
(: min-between : Number Number -> Number)
 (define (min-between num1 num2)
   (if (<= num1 num2) num1 num2))

(test (min-between 3 5) => 3)
(test (min-between 3 3) => 3)
(test (min-between 8 5) => 5)
(test (min-between 3.4 5.7) => 3.4)
(test (min-between -8 -5) => -8)

;AN helper function that returns the maximum number between two numbers-
;NOTE- this function already exist (it called max), but I wrote it because I wanted to practice more.
(: max-between : Number Number -> Number)
 (define (max-between num1 num2)
   (if (<= num1 num2) num2 num1))

(test (max-between 3 5) => 5)
(test (max-between 3 3) => 3)
(test (max-between 8 5) => 8)
(test (max-between 3.4 5.7) => 5.7)
(test (max-between -8 -5) => -5)


;The following function (min&max),consumes 5 numbers and returns a list containing the minimum and the maximum of them.
(: min&max : Number Number Number Number Number -> (Listof Number))
(define (min&max num1 num2 num3 num4 num5)
  (list(min-between num1(min-between num2(min-between num3(min-between num4 num5))))
       (max-between num1(max-between num2(max-between num3(max-between num4 num5))))))

(test (min&max 3 5 5 7 9) => '(3 9))
(test (min&max 5 5 5 5 5) => '(5 5))
(test (min&max 9 8 7 6 5) => '(5 9))
(test (min&max 9.7 8 7 6.4 5) => '(5 9.7))

;--------------------------------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------------------------------

;Written by Ayala Bouhnik, 04/04/2022
;Assignment 1- question 2:

#|We were asked to write in every question what was the challange-
 the main challange was to understand how tail recursion works.|#

#|This function (sublist-numbers) consumes a list of Any and returns a list containing all elements from the original
 list that are numbers.|#

(: sublist-numbers : (Listof Any) -> (Listof Number))
(define (sublist-numbers lst)
  (helper-tail lst (list)))

(: helper-tail : (Listof Any) (Listof Number) -> (Listof Number))
(define (helper-tail givenlst acclst)
  (cond
    [(null? givenlst) acclst] ;stop condition - check if the list is null, if it is- return the accumulate list
    [(number? (first givenlst)) (helper-tail (rest givenlst) (cons (first givenlst) acclst))]
    [else (helper-tail (rest givenlst) acclst)]));if the first is not null and the list is not empty, do this function again on the rest of the list and the accumulator

(test (sublist-numbers '(2 "any" 4 7 "ff")) => '(7 4 2))
(test (sublist-numbers '(2 "any" 4 7 #t 8.7)) => '(8.7 7 4 2))
(test (sublist-numbers (list 'any "Benny" 10 'OP 8))=> '(8 10))
(test (sublist-numbers '(any "Benny" OP (2 3)))=> null)
(test (sublist-numbers '(8 8 8 (2 3)))=> '(8 8 8))
(test (sublist-numbers '(()))=> null)
(test (sublist-numbers '())=> null)
;-------------------------------------------------------------------------------------------------------------------
;question 2b-

#|We were asked to write in every question what was the challange-
 The main challange for was to write the helper function. at first I wrote it in a bad syntax and got errors,
 and it took me time to find the right sytax to solve this question.|#


#|This function consumes a list of lists(where the type of the elements in the inner list may be any type).
  The function returns a list of lists – such that for each inner list lst (in the original list) the following is done–
  1. If lst contains at least one number, then lst is replaced with a list of size two,
    containing the minimum and maximum in lst.
  2. Otherwise, lst is replaced with a null.|#

(: min&max-lists : (Listof(Listof Any)) -> (Listof(Listof Number)))
(define (min&max-lists lst)
 (cond
   [(null? lst) lst]
   [else (cons (min-max (first lst)) (min&max-lists (rest lst)))]))

#|Aמ helper function-
 gets list of any and return:
 *if the list is empty or does not contains any number- empty list
 *if the list contain only one number- it will return a list contain this number twice.
 *if the list contain two numbers or more, it will return a list of size two that contain the minimum and the maximum from
 the original list. |#

(: min-max : (Listof Any) -> (Listof Number))
(define (min-max lst)
  (cond
    [(null? (sublist-numbers lst)) (list)] ;if the sorted list does not contain any number. 
    [else (list (apply min (sublist-numbers lst)) (apply max (sublist-numbers lst)))])) ;happend when the length of (sublist-numbers lst) is >= 1.

(test (min&max-lists '((any "Benny" 10 OP 8) (any "Benny" OP (2 3)))) => '((8 10) ()))
(test (min&max-lists '((2 5 1 5 L) (4 5 6 7 3 2 1) ())) => '((1 5) (1 7) ()))
(test (min&max-lists '((7 8 4) (1 78.3 90.2))) => '((4 8) (1.0 90.2))) ;NOTE- the min/max function return the number as a decimal number, but in the instruction it says we can ust thosr functions.
(test (min&max-lists '((L) (A B C) ())) => '(() () ()))
(test (min&max-lists '((2))) => '((2 2)))
(test (min&max-lists '((55 6) (8) (J D E T 0))) => '((6 55) (8 8) (0 0)))
(test (min&max-lists '(())) => '(()))

;-------------------------------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------------------------------

;Written by Ayala Bouhnik-Gelbord, 31/03/2022
;Assignment 1- question 3:

(define-type KeyStack
  [EmptyKS] ;empty stack
  [Push Symbol String KeyStack])

;constractors tests – 
(test(EmptyKS) => (EmptyKS))
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "A" (EmptyKS)) => (Push 'a "A" (EmptyKS)))
;---------------------------------------------------------------------------------------------------------------

#|search-stack – 
takes input a symbol (key) and a keyed-stack and return the first (LIFO) value that is keyed accordingly.
If the key does not appear in the original stack,it return a #f value.
when we want to check a type that we define we use cases. |#
(: search-stack : Symbol KeyStack -> (U String #f)) ;should return string or false value.
(define (search-stack key keyed-stack)
  (cases keyed-stack
    [(Push sym str kyd)
    (if(equal? key sym)str
       (search-stack key kyd))] ;recursion
    [(EmptyKS) #f]))
  
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A"(EmptyKS))))) => "AAA")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A"(EmptyKS))))) => #f)
(test (search-stack 'o (Push 'a "A"(EmptyKS))) => #f)
(test (search-stack 'k (EmptyKS)) => #f)
;---------------------------------------------------------------------------------------------------------------

#|This question was a little bit difficult for me, I thought that I need to send also a symbol, and then I had problems with
  the test that the lecturer gave us. Just after a day (!) I undersoot what was my problem.
 (the problem was in the function definition.|#

(: pop-stack : KeyStack -> (U KeyStack #f))
(define (pop-stack kysk)
  (cases kysk
    [(Push symks str kyd) kyd]
    [(EmptyKS) #f]))

(test (pop-stack(Push 'a "AAA"(Push 'b "B" (Push 'a "A"(EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "A"(EmptyKS)) => (Push 'a "A" (EmptyKS)))
(test (pop-stack (EmptyKS)) => #f)
