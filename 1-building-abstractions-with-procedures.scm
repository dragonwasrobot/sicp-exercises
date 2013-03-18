;; 1-building-abstractions-with-procedures.scm

;; # 1 Building Abstractions with Procedures

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

(+ (* 3 (+	(* 2 4) (+ 3 5))) (+ (- 10 7) 6))
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
;; (+		(square (+ 5 1))			(square (* 5 2))	)
;; (+		(* (+ 5 1) (+ 5 1))		(* (* 5 2) (* 5 2)))
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
;;			 (<p_2> <e_2>)
;;			 ...
;;			 (<p_n> <e_n>))

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
;;	(new-if (good-enough? guess x)
;;					guess
;;					(sqrt-iter (improve guess x) x)))

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

;; procedural abstraction

(define (square x) (* x x))
(define (square x) (exp (double (log x))))
(define (double x) (+ x x))

;; Local names

(define (square x) (* x x))
(define (square y) (* y y))

(define (good-enough? guess x)
	(< (abs (- (square guess) x))
		 0.001))

;; bound variable, binds, free, scope, capturing

;; Internal definitions and block structure

(define (sqrt x)
	(sqrt-iter 1.0 x))
(define (sqrt-iter guess x)
	(if (good-enough? guess x)
			guess
			(sqrt-iter (improve guess x))))
(define (good-enough? guess x)
	(< (abs (- (square guess) x)) 0.001))
(define (improve guess x)
	(average guess (/ x guess)))

(define (sqrt x)
	(define (good-enough? guess x)
		(< (abs (- (square guess) x)) 0.001))
	(define (improve guess x)
		(average guess (/ x guess)))
	(define (sqrt-iter guess x)
		(if (good-enough? guess x)
				guess
				(sqrt-iter (improve guess x) x)))
	(sqrt-iter 1.0 x))

;; block structure, lexical scoping

(define (sqrt x)
	(define (good-enough? guess)
		(< (abs (- (square guess) x)) 0.001))
	(define (improve guess)
		(average guess (/ x guess)))
	(define (sqrt-iter guess)
		(if (good-enough? guess)
				guess
				(sqrt-iter (improve guess))))
	(sqrt-iter 1.0))

;; 1.2 Procedures and the Processes They Generate

;; local evolution, global

;; 1.2.1 Linear Recursion and Iteration

;; n! = n * (n - 1) * (n - 2) * ... * 3 * 2 * 1

;; n! = n * [(n - 1) * (n - 2) * ... * 3 * 2 * 1] = n * (n - 1)!

(define (factorial n)
	(if (= n 1)
			1
			(* n (factorial (- n 1)))))

;; product <- counter * product
;; counter <- counter + 1

(define (factorial n)
	(fact-iter 1 1 n))
(define (fact-iter product counter max-count)
	(if (> counter max-count)
			product
			(fact-iter (* counter product)
								 (+ counter 1)
								 max-count)))

;; deferred operations, recursive process, linear recursive process, iterative
;; process, state variables, linear iterative process, stack, process,
;; procedure, tail-recursive

;; Exercise 1.9

;; (define (+ a b)
;;	(if (= a 0) b (inc (+ (dec a) b))))
;; recursive

;; (define (+ a b)
;;	(if (= a 0) b (+ (dec a) (inc b))))
;; iterative

;; TODO

;; Exercise 1.10

(define (A x y)
	(cond ((= y 0) 0)
				((= x 0) (* 2 y))
				((= y 1) 2)
				(else (A (- x 1) (A x (- y 1))))))

(A 1 10)
;; 1024
(A 2 4)
;; 65536
(A 3 3)
;; 65536

(define (f n) (A 0 n))
;; f compute 2*n
(define (g n) (A 1 n))
;; g computes 2^n
(h 1)
(define (h n) (A 2 n))
;; h computes 2^2^n

;; 1.2.2 Tree Recursion

;; tree recursion

;; 0,1,1,2,3,5,8,13,21,...

;;					/ 0										if n = 0,
;; Fib(n) = | 1										if n = 1,
;;					\ Fib(n-1) + Fib(n-2) otherwise.

(define (fib n)
	(cond ((= n 0) 0)
				((= n 1) 1)
				(else (+ (fib (- n 1))
								 (fib (- n 2))))))

(fib 8)
;; 21

;; a <- a + b
;; b <- a

(define (fib n)
	(fib-iter 1 0 n))
(define (fib-iter a b count)
	(if (= count 0)
			b
			(fib-iter (+ a b) a (- count 1))))

(fib 8)

;; Example: Counting change

(define (count-change amount)
	(cc amount 5))

(define (cc amount kinds-of-coins)
	(cond ((= amount 0) 1)
				 ((or (< amount 0) (= kinds-of-coins 0)) 0)
				 (else (+ (cc amount
											(- kinds-of-coins 1))
									(cc (- amount
												 (first-demonination
													kinds-of-coins))
											kinds-of-coins)))))

(define (first-demonination kinds-of-coins)
	(cond ((= kinds-of-coins 1) 1)
				((= kinds-of-coins 2) 5)
				((= kinds-of-coins 3) 10)
				((= kinds-of-coins 4) 25)
				((= kinds-of-coins 5) 50)))

(count-change 100)
;; 292

;; tabulation/memoization,

;; Exercise 1.11

(define (f n)
	(cond ((< n 3) n)
				(else (+ (f (- n 1))
								 (* 2 (f (- n 2)))
								 (* 3 (f (- n 3)))))))

;; ==> 0, 1, 2, 4, 11, 25, 59, 142, 335, 796, 892

;; TODO f-iter

;; Exercise 1.12

(define (binomial n k)
	(cond ((= k 0) 1)
				((= n 0) 0)
				(else
				 (+ (binomial (- n 1) (- k 1))
						(binomial (- n 1) k)))))

(binomial 4 2)
;; ==> 6

;; Exercise 1.13

;; TODO

;; 1.2.3 Orders of Growth

;; order of growth, Theta(f(n))

;; Exercise 1.14

;; TODO

;; Exercise 1.15

(define (cube x)
	(* x x x))

(define (p x)
	(- (* 3 x) (* 4 (cube x))))

(define (sine angle)
	(if (not (> (abs angle) 0.1))
			angle
			(p (sine (/ angle 3.0)))))

(sine 12.15)
;; a. 5 times: 12.15 -> 4.05 -> 1.34999 -> 0.44999 -> 0.14999 -> 0.04999

;; b. (ceiling (/ (log (/ a 0.1)) (log 3)))

;; 1.2.4 Exponentiation

;; b^n = b * b^(n-1)
;; b^0 = 1

(define (expt b n)
	(if (= n 0)
			1
			(* b (expt b (- n 1)))))

(expt 2 5)
;; 32

(define (expt b n)
	(expt-iter b n 1))
(define (expt-iter b counter product)
	(if (= counter 0)
			product
			(expt-iter b
								 (- counter 1)
								 (* b product))))

(expt 2 5)
;; 32

(define (fast-expt b n)
	(cond ((= n 0) 1)
				((even? n) (square (fast-expt b (/ n 2))))
				(else (* b (fast-expt b (- n 1))))))

(fast-expt 2 5)
;; 32

;; (define (even? n)
;;	(= (remainder n 2) 0))

;; Exercise 1.16

;; invariant quantity

(define (fast-expt-aux b n)
	(define (iter a b n)
		(cond ((= n 0) a)
					((even? n) (iter a (square b) (/ n 2)))
					(else (iter (* a b) b (- n 1)))))
	(iter 1 b n))

(fast-expt 2 5) ;; 32
(fast-expt-aux 2 5) ;; 32

;; Exercise 1.17

(define (mult a b)
	(if (= b 0)
			0
			(+ a (mult a (- b 1)))))

(define (double n)
	(if (= n 0)
			0
			(+ 2 (double (- n 1)))))

(double 17) ;; 34

(define (halve n)
	(define (iter a n)
		(cond ((= n 0) a)
					((= n 1) a)
					(else (iter (+ 1 a) (- n 2)))))
	(iter 0 n))

(halve 34) ; 17

(define (fast-mult b n)
	(cond ((= n 0) 0)
				((even? n) (double (fast-mult b (halve n))))
				(else (+ b (fast-mult b (- n 1))))))

(fast-mult 7 8) ; 56

;; Exercise 1.18

(define (russian-peasant b n)
	(define (iter a b n)
		(cond ((= n 0) a)
					((even? n) (iter a (double b) (/ n 2)))
					(else (iter (+ a b) b (- n 1)))))
	(iter 0 b n))

(russian-peasant 4 7) ; 28

;; Exercise 1.19

;; a' <- bq + aq + ap = (p + q)a + bq
;; b' <- bp + aq
;; a'' <- b'q + a'q + a'p = (bp + aq)q + ((p+q)a + bq)q + ((p+q)a + bq)p
;;												= bpq + aq^2 + apq aq^2 + bq^2 + ap^2 + apq + bpq
;;												= (2q^2 + 2pq + p^2)a + (2pq + q^2)b
;; b'' <- b'p + a'q = (bp + aq)p + ((p+q)a + bq)q
;;									= bp^2 + apq + apq + aq^2 + bq^2
;;									= (2pq + q^2)a (p^2 + q^2)b

(define (square x) (* x x))

(define (fib n)
	(fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
	(cond ((= count 0) b)
				((even? count)
				 (fib-iter a
									 b
									 (+ (square p) (square q))
									 (+ (* 2 p q) (square q))
									 (/ count 2)))
				(else (fib-iter (+ (* b q) (* a q) (* a p))
												(+ (* b p) (* a q))
												p
												q
												(- count 1)))))

(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)

;; 1.2.5 Greatest Common Divisor

;; GCD(a,b) = GCD(b,r)

;; GCD(206,40) = GCD(40,6)
;;						 = GCD(6,4)
;;						 = GCD(4,2)
;;						 = GCD(2,0)
;;						 = 2

;; Euclid's Algorithm

(define (gcd a b)
	(if (= b 0)
			a
			(gcd b (remainder a b))))

;; Lame's Theorem: If Euclid's Algorithm requires $k$ steps to compute the GCD
;; of some pair, then the smaller number in the pair must be greater than or
;; equal to the $k^{th}$ Fibonacci number.

;; Exercise 1.20

;; TODO

;; 1.2.6 Example: Testing for Primality

;; Searching for divisors

(define (smallest-divisor n)
	(find-divisor n 2))
(define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n)
				((divides? test-divisor n) test-divisor)
				(else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
	(= n (smallest-divisor n)))

(prime? 8)
(prime? 13)

;; The Fermat test

;; Fermat's Little Theorem: If $n$ is a prime number and $a$ is any positive
;; integer less than $n$, then $a$ raised to the $n^{th}$ power is congruent to
;; $a$ modulo $n$.

(define (expmod base exp m)
	(cond ((= exp 0)
				 1)
				((even? exp)
				 (remainder
					(square
					 (expmod base (/ exp 2) m))
					m))
				(else
				 (remainder
					(* base
						 (expmod base (- exp 1) m))
					m))))

(expmod 4 5 5)

;; our poorly implemented random function
(define (random n)
	(/ n 2))

(define (fermat-test n)
	(define (try-it a)
		(= (expmod a n n) a))
	(try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
	(cond ((= times 0) #t)
				((fermat-test n) (fast-prime? n (- times 1)))
				(else #f)))

(fast-prime? 13 1)

;; Probabilistic methods

;; Carmichael numbers: those that fool the Fermat test.

;; Probabilistic algorithms.

;; Exercise 1.21

(smallest-divisor 199) ;; 199
(smallest-divisor 1999) ;; 1999
(smallest-divisor 19999) ;; 7

;; Exercise 1.22

;; TODO

;; Exercise 1.23

;; TODO

;; Exercise 1.24

;; TODO

;; Exercise 1.25

;; TODO

;; Exercise 1.26

;; TODO

;; Exercise 1.27

;; TODO

;; Exercise 1.28

;; TODO

;; 1.3 Formulating Abstractions with Higher-Order Procedures

(define (cube x) (* x x x))

;; /higher order procedures/

;; 1.3.1 Procedures as Arguments

(define (sum-integers a b)
	(if (> a b)
			0
			(+ a
				 (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
	(if (> a b)
			0
			(+ (cube a)
				 (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
	(if (> a b)
			0
			(+ (/ 1.0 (* a (+ a 2)))
				 (pi-sum (+ a 4) b))))

;; (define (<name> a b)
;;		(if (> a b)
;;				0
;;				(+ (<term> a)
;;					 (<name> (<next> a) b))))

;; *summation of a series*

(define (sum term a next b)
	(if (> a b)
			0
			(+ (term a)
				 (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
	(sum cube a inc b))

(sum-cubes 1 10)
;; 3025

(define (identity x) x)

(define (sum-integers a b)
	(sum identity a inc b))

(sum-integers 1 10)
;; 55

(define (pi-sum a b)
	(define (pi-term x)
		(/ 1.0 (* x (+ x 2))))
	(define (pi-next x)
		(+ x 4))
	(sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000))
;; 3.139592655589783

(define (integral f a b dx)
	(define (add-dx x)
		(+ x dx))
	(* (sum f (+ a (/ dx 2.0)) add-dx b)
		 dx))

(integral cube 0 1 0.01)
;; 0.24998750000000042

(integral cube 0 1 0.001)
;; 0.249999875000001

;; Exercise 1.29

;; TODO

;; Exercise 1.30

;; TODO

;; Exercise 1.31

;; TODO

;; Exercise 1.32

;; TODO

;; Exercise 1.33

;; TODO

;; ### 1.3.2 Constructing Procedures Using Lambda

(lambda (x) (+ x 4))

(lambda (x) (/ 1.0 (* x (+ x 2))))

(define (pi-sum a b)
	(sum (lambda (x) (/ 1.0 (* x (+ x 2))))
			 a
			 (lambda (x) (+ x 4))
			 b))

(define (integral f a b dx)
	(* (sum f
					(+ a (/ dx 2.0))
					(lambda (x) (+ x dx))
					b)
		 dx))

;; (lambda (<formal-parameters>) <body>)

(define (plus4 x) (+ x 4))

(define plus4 (lambda (x) (+ x 4)))

((lambda (x y z) (+ x y (square z)))
 1 2 3)

;; #### Using let to create local variables

(define (f x y)
	(define (f-helper a b)
		(+ (* x (square a))
			 (* y b)
			 (* a b)))
	(f-helper (+ 1 (* x y))
						(- 1 y)))

(define (f x y)
	((lambda (a b)
		 (+ (* x (square a))
				(* y b)
				(* a b)))
	 (+ 1 (* x y))
	 (- 1 y)))

(define (f x y)
	(let ((a (+ 1 (* x y)))
				(b (- 1 y)))
		(+ (* x (square a))
			 (* y b)
			 (* a b))))

;; (let ((<var_1> <exp_1>)
;;			 (<var_2> <exp_2>)
;;			 ...
;;			 (<var_n> <exp_n>))
;;		<body>)

(define (f x y)
	(define a (+ 1 (* x y)))
	(define b (- 1 y))
	(+ (* x (square a))
		 (* y b)
		 (* a b)))

;; Exercise 1.34

;; When we evaluate (f f), we get (lambda (g) (g 2) 2) resulting in
;; (2 '(2)) which then results in an error as it tries to apply
;; the value 2 to the function 2.

;; ### 1.3.3 Procedures as General Methods

;; #### Finding roots of equations by the half-interval method

;; *half-interval method*.

(define (search f neg-point pos-point)
	(let ((midpoint (average neg-point pos-point)))
		(if (close-enough? neg-point pos-point)
				midpoint
				(let ((test-value (f midpoint)))
					(cond ((positive? test-value)
								 (search f neg-point midpoint))
								((negative? test-value)
								 (search f midpoint pos-point))
								(else midpoint))))))

(define (close-enough? x y) (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
	(let ((a-value (f a))
				(b-value (f b)))
		(cond ((and (negative? a-value) (positive? b-value))
					 (search f a b))
					((and (negative? b-value) (positive? a-value))
					 (search f b a))
					(else (error "Values are not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)
;; 3.14111328125

(half-interval-method (lambda (x) (- (* x x x) (+ (* 2 x) 3)))
											1.0
											2.0)
;; 1.89306640625

;; #### Finding fixed points of functions

;; *fixed point*

(define tolerance 0.00001)

(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2))
			 tolerance))
	(define (try guess)
		(let ((next (f guess)))
			(if (close-enough? guess next)
					next
					(try next))))
	(try first-guess))

(fixed-point cos 1.0)
;; 0.7390822985224023

(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)
;; 1.2587315962971173

(define (sqrt x) ; buggy
	(fixed-point (lambda (y) (/ x y)) 1.0))

(define (sqrt x)
	(fixed-point (lambda (y) (average y (/ x y)) 1.0)))

;; *average damping*

;; Exercise 1.35

;; Show that the golden ratio phi is a fixed point of the transformation
;; x |-> 1 + 1 / x.

;; phi = 1 + 1 / phi
;; phi ^ 2 = phi + 1
;; phi ^ 2 - phi - 1 = 0
;;		 1 + sqrt 5												 1 - sqrt 5
;; phi = ------- = 1.61803399887 and phi = ------- = -0.6180339897
;;				2																2

;;		 1 + sqrt 5
;; phi = ------- = 1.61803399887...
;;				2

;; Use this fact to compute phi by means of the `fixed-point` procedure.

;; golden ratio: phi = (/ (+ 1 (sqrt 5)) 2) = 1.6180339887...

(define golden-ratio (fixed-point (lambda (y) (+ 1 (/ 1 y))) 1.0))
;; 1.6180327868852458

;; Exercise 1.36

;; Modify `fixed-point` so that it prints the sequence of approximations it
;; generates, using the `newline` and `display` primitives.

(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2))
			 tolerance))
	(define (try guess)
		(display guess)
		(newline)
		(let ((next (f guess)))
			(if (close-enough? guess next)
					next
					(try next))))
	(try first-guess))

((lambda () (fixed-point (lambda (y) (+ 1 (/ 1 y))) 1.0))) ; phi again

(define x-power-x (fixed-point (lambda (y) (/ (log 1000) (log y))) 2.0))

x-power-x
;; 4.555532270803653

;; Exercise 1.37

;; a.

;; *continued fraction*
;; *k-term finite continued fraction*

;; (/ N_1 (+ D_1 (/ N_2 (+ D_2 (...)))))

(define (cont-frac-rec n d k)
	(define (cont-rec i)
		(if (= i k)
				0
				(/ (n i) (+ (d i) (cont-rec (+ i 1))))))
	(cont-rec 0))

;; golden ratio: 0.61803398875

(cont-frac-rec (lambda (i) 1.0)
							 (lambda (i) 1.0)
							 11)
;; 0.6180555555555556

;; Accuracy of 4 decimals: k = 11.

;; b.

(define (cont-frac-iter n d k)
	(define (cont-iter i a)
		(if (= 0 i)
				a
				(cont-iter (- i 1) (/ (n i) (+ (d i) a)))))
		(cont-iter k 0))

(cont-frac-iter (lambda (i) 1.0)
								(lambda (i) 1.0)
								11)
;; 0.6180555555555556

;; Exercise 1.38

(define (nat-log-series i)
	(cond ((= i 0) 1)
				((= i 1) 2)
				(else (if (not (= (remainder (- i 1) 3) 0))
									1
									(+ (* (/ 2 3)
												(- i 2))
										 (/ 8 3))))))

(define euler-e-frac
	(+ 2 (cont-frac-rec (lambda (i) 1.0)
											nat-log-series
											10)))

euler-e-frac
;; 2.7182817182817183

;; Exercise 1.39

;; tan x = (/ x (- 1 (/ (square x) (- 3 (square x) (/ -5 ...)))))

(define (tan-cf x k)
	(cont-frac-rec (lambda (i) (if (= i 0)
														(+ x 0.0)
														(- (square (+ x 0.0)))))
								 (lambda (i) (+ 1.0 (* 2 i)))
								 k))

(tan-cf 1 10)





;; end-of-1-building-abstractions-with-procedures.scm
