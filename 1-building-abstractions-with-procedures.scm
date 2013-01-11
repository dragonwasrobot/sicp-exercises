;; 1-building-abstractions-with-procedures.scm

;; 1 Building Abstractions with Procedures

;; computational process
;; data
;; program
;; programming language
;; bugs/glitches
;; debug

;; Programming in Lisp

;; recursion equations
;; interpreter
;; procedures

;; 1.1 The Elements of Programming

;; * primitive expressions
;; * means of combination
;; * means of abstraction

;; 1.1.1 Expressions

;; expression
;; evaluating

486
;; 486
(+ 137 349)
;; 486
(- 1000 334)
;; 666
(* 5 99)
;; 495
(/ 10 5)
;; 2
(+ 2.7 10)
;; 12.7

;; combinations
;; operators
;; operands
;; arguments
;; prefix notation

(+ 21 35 12 7)
;; 75
(* 25 4 12)
;; 1200

;; nested

(+ (* 3 5) (- 10 6))
;; 19

(+ (* 3 (+  (* 2 4) (+ 3 5))) (+ (- 10 7) 6))
;; 57

(+ (* 3
			(+ (* 2 4)
				 (+ 3 5)))
	 (+ (- 10 7)
			6))
;; 57

;; pretty-printing
;; real-eval-print loop

;; 1.1.2 Naming and the Environment

;; variable
;; value

(define size 2)
size
;; 2
(* 5 size)
;; 10

(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))
;; 314.159
(define circumference (* 2 pi radius))
circumference
;; 62.8318

;; environment
;; global environment

;; 1.1.3 Evaluating Combinations

;; recursive

(* (+ 2 (* 4 6))
	 (+ 3 5 7))
;; 390

;; tree accumulation
;; special forms

;; 1.1.4 Compound Procedures

;; procedure definitions

(define (square x) (* x x))

;; compound procedure

(square 21)
;; 441
(square (+ 2 5))
;; 49
(square (square 3))
;; 81

;; (+ (square x) (square y))

(define (sum-of-squares x y)
	(+ (square x) (square y)))

(sum-of-squares 3 4)
;; 25

(define (f a)
	(sum-of-squares (+ a 1) (* a 2)))
(f 5)
;; 136

;; 1.1.5 The Substitution Model for Procedure Application

;; (f 5)
;; (sum-of-squares (+ a 1 (* a 2)))
;; (sum-of-squares (+ 5 1 (* 5 2)))
;; (+ (square 6) (square 10))
;; (+ (* 6 6) (* 10 10))
;; (+ 36 100)
;; 136

;; substitution model

;; Applicative order versus normal order

;; (f 5)
;; (sum-of-squares (+ 5 1) (* 5 2))
;; (+   (square (+ 5 1))      (square (* 5 2))  )
;; (+   (* (+ 5 1) (+ 5 1))   (* (* 5 2) (* 5 2)))
;; (+ (* 6 6) (* 10 10))
;; (+ 36 100)
;; 136

;; normal-order evaluation
;; applicative-order evaluation

;; 1.1.6 Conditional Expressions and Predicates

;; case analysis

(define (abs x)
	(cond ((> x 0) x)
				((= x 0) 0)
				((< x 0) (- x))))

;; (cond (<p_1> <e_1>)
;;       (<p_2> <e_2>)
;;       ...
;;       (<p_n> <e_n>))

;; (<p> <e>)
;; clauses
;; predicate
;; consequent expression

(define (abs x)
	(cond ((< x 0) (- x))
				(else x)))

(define (abs x)
	(if (< x 0)
			(- x)
			x))

;; (if <predicate> <consequent> <alternative>)

;; (and <e_1> ... <e_n>)
;; (or <e_1> ... <e_n>)
;; (not <e>)

;; (and (> x 5) (< x 10))

(define (>= x y) (or (> x y) (= x y)))

(define (>= x y) (not (< x y)))

;; Exercise 1.1

10
;; 10
(+ 5 3 4)
;; 12
(- 9 1)
;; 8
(/ 6 2)
;; 3
(+ (* 2 4) (- 4 6))
;; 6
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
;; 19
(= a b)
;; #f
(if (and (> b a) (< b (* a b)))
		b
		a)
;; 4
(cond ((= a 4) 6)
			((= b 4) (+ 6 7 a))
			(else 25))
;; 16
(+ 2 (if (> b a) b a))
;; 6
(* (cond ((> a b) a)
				 ((< a b) b)
				 (else -1))
	 (+ a 1))
;; 16

;; Exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6
											 (/ 4 5)))))
	 (* 3
			(- 6 2)
			(- 2 7)))
;; -37/150

;; Exercise 1.3

(define (sum-of-squares-3 a b c)
	(cond ((= (min a b c) a) (sum-of-squares b c))
				((= (min a b c) b) (sum-of-squares a c))
				(else (sum-of-squares a b))))

;; Exercise 1.4

(define (a-plus-abs-b a b)
	((if (> b 0) + -) a b))

;; If b > 0 then add a b, otherwise subtract.

;; Exercise 1.5

(define (p) (p))
(define (test x y)
	(if (= x 0) 0 y))

;; (test 0 (p))
;; Infinite loop for applicative-order evaluation.
;; Return the function p for normal-order evaluation.

;; If y is evaluated, it will loop forever.

;; 1.1.7 Example: Square Roots by Newton's Method

(define (sqrt-iter guess x)
	(if (good-enough? guess x)
			guess
			(sqrt-iter (improve guess x) x)))

(define (improve guess x)
	(average guess (/ x guess)))

(define (average x y)
	(/ (+ x y) 2))

(define (good-enough? guess x)
	(< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
	(sqrt-iter 1.0 x))

(sqrt 9)
;; 3.00009155413138
(sqrt (+ 100 37))
;; 11.704699917758145
(sqrt (+ (sqrt 2) (sqrt 3)))
;; 1.7739279023207892
(square (sqrt 1000))
;; 1000.000369924366

;; Exercise 1.6

;; Broken if
(define (new-if predicate then-clause else-clause)
	(cond (predicate then-clause)
				(else else-clause)))

(new-if (= 2 3) 0 5)
;; 5
(new-if (= 1 1) 0 5)
;; 0

;; (define (sqrt-iter guess x)
;; 	(new-if (good-enough? guess x)
;; 					guess
;; 					(sqrt-iter (improve guess x) x)))

;; The program will loop infinitely because the predicate expression will only
;; be evaluated once (to false) resulting in a infinite loop since it won't be
;; reevaluated for each iteration,

;; Exercise 1.7

;; TODO

;; Exercise 1.8

(define (cube x)
	(* x x x))

(define (cube-iter guess x)
	(if (cube-good-enough? guess x)
			guess
			(cube-iter (cube-improve guess x) x)))

(define (cube-improve guess x)
	(/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cube-good-enough? guess x)
	(< (abs (- (cube guess) x)) 0.001))

(define (cbrt x)
	(cube-iter 1.0 x))

(cbrt 27)
;; 3.0000005410641766

;; 1.1.8 Procedures as Black-Box Abstractions



;; end-of-1-building-abstractions-with-procedures.scm
