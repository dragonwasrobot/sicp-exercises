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
;; 	(if (= a 0) b (inc (+ (dec a) b))))
;; recursive

;; (define (+ a b)
;; 	(if (= a 0) b (+ (dec a) (inc b))))
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

;;          / 0                   if n = 0,
;; Fib(n) = | 1                   if n = 1,
;;          \ Fib(n-1) + Fib(n-2) otherwise.

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
;; 	(= (remainder n 2) 0))

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
;;                        = bpq + aq^2 + apq aq^2 + bq^2 + ap^2 + apq + bpq
;;                        = (2q^2 + 2pq + p^2)a + (2pq + q^2)b
;; b'' <- b'p + a'q = (bp + aq)p + ((p+q)a + bq)q
;;                  = bp^2 + apq + apq + aq^2 + bq^2
;;                  = (2pq + q^2)a (p^2 + q^2)b

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
;;             = GCD(6,4)
;;             = GCD(4,2)
;;             = GCD(2,0)
;;             = 2

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



;; end-of-1-building-abstractions-with-procedures.scm
