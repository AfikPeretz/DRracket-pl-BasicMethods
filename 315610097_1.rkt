#lang pl

#|This function expects to get 5 variables and return a list of
the minimum and the maximum - I use the "min" and "max" methods
and by creating a list of both in the cons command

The main difficulty:
The main difficulty was to find out that max and min are reserved methods, then the question was easy
time:

40 min.
|#
(: min&max :  Number Number Number Number Number -> (Listof Number))
(define (min&max x y z j i)
 (cons (min x y z j i) (cons (max x y z j i) '()))
  )


;tests
(test (min&max 1 2 3 4 5) => '(1 5))
(test (min&max -1 0 0 0 0) => '(-1 0))
(test (min&max 0 0 0 0 0) => '(0 0))
(test (min&max 10 20 30 40 50) => '(10 50))
(test (min&max -2 -1 0 1 2) => '(-2 2))



#|In this section I use the tail recursion function, which is divided
into "ezer2.1" and "sublist-number".

In the function "ezer2.1" - I get a list of numbers and a list
of all types of "variables" - and recursively go through the variables
in the list I get, if the variable null, reach the stop condition.
if the variable is a number I put in the list, 
otherwise continue to the next variable In the list by "rest" method, So that in the end the
variables in the list are returned in the reverse order of the order in which
we received them.

In the "sublist-numbers" function - get a list of all types of "variables" and
by using the "ezer" function I return the requested list in question.

The main difficulty:
The main difficulty was to think logically what was the right way to approach the solution, since recursion is not a rival thing.
In addition, I had some difficulty with the syntax and commands so I worked with the presentations from the practice open.

time:
1.5 hours
|#


;main method
(: sublist-numbers : (Listof Any)->(Listof Number))
(define (sublist-numbers loa)
  (ezer2.1 '() loa))

;tail method
(: ezer2.1 : (Listof Number) (Listof Any)->(Listof Number))
(define (ezer2.1 n loa)       
  (cond [(null? loa) n]
        [(number? (first loa)) (ezer2.1 (cons (first loa) n) (rest loa))]
        [else (ezer2.1 n (rest loa))]))

;tests
(test (sublist-numbers (list 'any "Benny" 10 'OP 8))
 => '(8 10))
(test (sublist-numbers '(any "Benny" OP (2 3)))
 => null)
(test (sublist-numbers (list 1 2 3 4)) => (list 4 3 2 1))
(test (sublist-numbers (list "a" 1 "b" 2)) => (list 2 1))
(test (sublist-numbers (list "1" "2" "3" "4")) => '())
(test (sublist-numbers (list "a" "1" "b" "2")) => '())





#|In this question I used the tail recursion again, the main function that returns
the solution is "min & max-lists" which uses its tail function ezer2.2 as follows:
The function uses the function we wrote earlier "sublist-numbers" and by it isolates
from the list that holds different types of variables the variables that are of type
number and then by using the saved methods "min" and "max" return list
of numbers with the minimum and maximum number

The main difficulty:
The truth is that to my delight this time things were done quite simply, from the moment I realized that I should use the functions I had already written the design of the function did not take so long

time:
40 min
|#

;main method        
(: min&max-lists : (Listof (Listof Any))->(Listof (Listof Number)))
(define (min&max-lists lol)
  (map ezer2.2 lol))

;tail method
(: ezer2.2 : (Listof Any)->(Listof Number))
(define (ezer2.2 loa)
  (cond [(null? (sublist-numbers loa)) null]
        [else  (list (apply min (sublist-numbers loa)) (apply max (sublist-numbers loa)))]))


;tests
(test (min&max-lists '((any "Benny" 10 OP 8) (any "Benny" OP (2
3)))) => '((8 10) ()))

(test (min&max-lists '((2 5 1 5 L) (4 5 6 7 3 2 1) ()))
=> '((1 5) (1 7) ()))

(test (min&max-lists '((10 5) (5 5 5 5 10) (10 10 10 10 5) (5 6 7 8 9 10))) => '((5 10) (5 10) (5 10) (5 10)))

(test (min&max-lists '()) => '())






#|This question was on the most advanced topics in terms of material and yet somehow the easiest, thanks!
In the first 2 sections you just have to build builders and you showed in lecture 3 exactly how to do it, so that was just fine.
|#

;constructors as you show to do in lecture 3
(define-type KeyStack
  [EmptyKS]
  [Push Symbol String KeyStack])


              
#|         
In section c - the function is constructed so that it goes over all the options (if the stack is not empty,
return the value by the key, if not found the value, the function continues to progress, if the cartridge is empty, return false.
The main difficulty - lack of use of the language and its understanding to the end,
I would be happy if in the exercises there will be more practice and less theoretical material.
Time: 40 min
|#

(: search-stack  : (Symbol KeyStack ->(U String False)))         
  (define (search-stack key stack)
  (cases stack
    [(Push k sk ksk) 
     (if (eq? key k) sk 
         (search-stack key ksk))]
    [(EmptyKS) #f]))




#|         
In section d - we were asked to make a pop from the stack, after section c it was already really simple - I did a test there is something in the stack that will
return and another that will return false.
Time: 10-15 min
|#
(: pop-stack : (KeyStack ->(U KeyStack False)))
(define (pop-stack stack)
 (cases stack
 [(Push s sk ksk) ksk]
 [(EmptyKS) #f]))



;tests
(test (EmptyKS) => (EmptyKS))
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))) => (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "AAA")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)
(test (search-stack 'b (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "B")
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (pop-stack (EmptyKS)) => #f)
    
   
    
  
  
