;; # 2 Building Abstractions with Data

(define (linear-combination a b x y)
  (+ (* a x) (* b y)))

(define (linear-combination a b x y)
  (add (mul a x) (mul b y)))

;; ## 2.1 Introduction to Data Abstraction

;; ### 2.1.1 Example: Arithmetic Operations for Rational Numbers

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;; #### Pairs

(define x (cons 1 2))
(car x) ;; 1
(cdr x) ;; 2

(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))

(car (car z)) ;; 1
(car (cdr z)) ;; 3

;; #### Representing rational numbers

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(print-rat one-half) ;; 1/2
(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third)) ;; 5/6
(print-rat (mul-rat one-half one-third)) ;; 1/6
(print-rat (add-rat one-third one-third)) ;; 6/9

;; gcd from Section 1.2.5
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(print-rat (add-rat one-third one-third)) ;; 2/3

;; ##### Exercise 2.1

;; Define a better version of `make-rat` that handles both positive and negative
;; arguments. `Make-rat` should normalize the sign so that if the rational
;; number is positive, both the numerator and denominator are positive, and if
;; the rational number is negative, only the numerator is negative.

;; We observe that all we have to do is negate n and d whenever d is negative.
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< d 0)
        (cons (/ (- n) g) (/ (- d) g))
        (cons (/ n g) (/ d g)))))

;; ### 2.1.2 Abstraction Barriers

(define (make-rat n d) (cons n d))
(define (numer x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))
(define (denom x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))

;; ##### Exercise 2.2

;; Consider the problem of representing line segments in a plane. Each segment
;; is represented as a pair of points: a starting point and an ending
;; point. Define a constructor `make-segment` and selectors `start-segment` and
;; `end-segment` that define the representation of segments in terms of points.
;; Furthermore, a point can be represented as a pair of numbers: the *x*
;; coordinate and the *y* coordinate. Accordingly, specify a constructor
;; `make-point` and selectors `x-point` and `y-point` that define this
;; representation. Finally, using your selectors and constructors, define a
;; procedure `midpoint-segment` that takes a line segment as argument and returns
;; its midpoint (the point whose coordinates are the average of the coordinates
;; of the endpoints). To try your procedures, you’ll need a way to print points:

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment s e) (cons s e))
(define (start-segment l) (car l))
(define (end-segment l) (cdr l))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (midpoint-segment l)
  (let ((x (/ (+ (x-point (start-segment l))
                 (x-point (end-segment l)))
              2))
        (y (/ (+ (y-point (start-segment l))
                 (y-point (end-segment l)))
              2)))
    (make-point x y)))

(define p1 (make-point 1 1))
(define p2 (make-point 2 3))
(define l1 (make-segment p1 p2))
(midpoint-segment l1) ;; (3/2 . 2)

;; ##### Exercise 2.3

;; Implement a representation for rectangles in a plane.  (Hint: You may want to
;; make use of Exercise 2.2.) In terms of your constructors and selectors,
;; create procedures that compute the perimeter and the area of a given
;; rectangle. Now implement a different representation for rectangles. Can you
;; design your system with suitable abstraction barriers, so that the same
;; perimeter and area procedures will work using either representation?

(define (make-rectangle s e) (cons s e))
(define (upper-left r) (car r))
(define (lower-right r) (cdr r))

(define (perimeter r)
  (let ((a (upper-left r))
        (d (lower-right r)))
    (* 2
       (+ (- (x-point d) (x-point a))
          (- (y-point a) (y-point d))))))

(define (area r)
  (let ((a (upper-left r))
        (d (lower-right r)))
    (* (- (x-point d) (x-point a))
       (- (y-point a) (y-point d)))))

(define p3 (make-point 2 3))
(define p4 (make-point 4 0))
(define r1 (make-rectangle p3 p4))
(perimeter r1) ;; 10
(area r1) ;; 6

(define (make-rectangle-alt upper-left width height)
  (cons upper-left (cons width height)))
(define (upper-left r) (car r))
(define (lower-right r) (make-point (+ (x-point (car r)) (cddr r))
                                    (- (y-point (car r)) (cdar r))))

(define point1 (make-point 2 3))
(define height1 3)
(define width1 2)
(define r2 (make-rectangle p3 p4))
(perimeter r2) ;; 10
(area r2) ;; 6

;; ### What Is Meant by Data?

(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1: CONS" m))))
  dispatch)
(define (car z) (z 0))
(define (cdr z) (z 1))

;; ##### Exercise 2.4

;; Here is an alternative procedural representation of pairs. For this
;; representation, verify that `(car (cons x y))` yields `x` for any object `x`
;; and `y`.

(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))

;; What is the corresponding definition of cdr? (Hint: To verify that this
;; works, make use of the substitution model of Section 1.1.5.)

(define (cdr z)
  (z (lambda (p q) q)))

(cdr (cons 5 3)) ;; 3

;; ##### Exercise 2.5

;; Show that we can represent pairs of nonnegative integers using only numbers
;; and arithmetic operations if we represent the pair *a* and *b* as the integer
;; that is the product *2^a 3^b*. Give the corresponding definitions of the
;; procedures `cons`, `car, and `cdr`.

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car c)
  (if (not (= (remainder c 2) 0))
      0
      (+ 1 (car (/ c 2)))))

(define (cdr c)
  (if (not (= (remainder c 3) 0))
      0
      (+ 1 (cdr (/ c 3)))))

(cons 7 3) ;; 3456
(car 3456) ;; 7
(cdr 3456) ;; 3

;; ##### Exercise 2.6

;; In case representing pairs as procedures wasn’t mindboggling enough, consider
;; that, in a language that can manipulate procedures, we can get by without
;; numbers (at least insofar as nonnegative integers are concerned) by
;; implementing 0 and the operation of adding 1 as

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; This representation is known as Church numerals, after its inventor, Alonzo
;; Church, the logician who invented the lambda-calculus.

;; Define one and two directly (not in terms of zero and add-1). (Hint: Use
;; substitution to evaluate (add-1 zero)). Give a direct definition of the
;; addition procedure + (not in terms of repeated application of add-1).

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (plus m n)
  (lambda (m) (lambda (n) (lambda (f) (lambda (x) (m f (n f x)))))))

;; ### 2.1.4 Extended Exercise: Interval Arithmetic

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

;; ##### exercise 2.7

;; Alyssa’s program is incomplete because she has not specified the
;; implementation of the interval abstraction. Here is a definition of the
;; interval constructor:

(define (make-interval a b) (cons a b))

;; Define selectors `upper-bound` and `lower-bound` to complete the
;; implementation.

(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

;; ##### Exercise 2.8

;; Using reasoning analogous to Alyssa's, describe how the difference of two
;; intervals may be computed. Define a corresponding subtraction procedure,
;; called sub-interval.

;; If we subtract two intervals we expect the resulting interval to span the
;; smallest and greatest difference between the bounds of the two intervals.

(define (sub-interval x y)
  (let ((p1 (abs (- (lower-bound x) (lower-bound y))))
        (p2 (abs (- (lower-bound x) (upper-bound y))))
        (p3 (abs (- (upper-bound x) (lower-bound y))))
        (p4 (abs (- (upper-bound x) (upper-bound y)))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;; ##### Exercise 2.9

;; The `width` of an interval is half of the difference between its upper and
;; lower bounds. The width is a measure of the uncertainty of the number
;; speciﬁed by the interval. For some arithmetic operations the width of the
;; result of combining two intervals is a function only of the widths of the
;; argument intervals, whereas for others the width of the combination is not a
;; function of the widths of the argument intervals. Show that the width of the
;; sum (or difference) of two intervals is a function only of the widths of the
;; intervals being added (or subtracted). Give examples to show that this is not
;; true for multiplication or division.

(define (width interval)
  (/ (- (upper-bound interval)
        (lower-bound interval))
        2))

;; TODO

;; ##### Exercise 2.10

;; Ben Bitdiddle, an expert systems programmer, looks over Alyssa’s shoulder and
;; comments that it is not clear what it means to divide by an interval that
;; spans zero. Modify Alyssa’s code to check for this condition and to signal an
;; error if it occurs.

(define (div-interval x y)
  (if (or (and (>= 0 (lower-bound x))
               (<= 0 (upper-bound x)))
          (and (>= 0 (lower-bound y))
               (<= 0 (upper-bound y))))
      (error "One of the intervals spanned zero.")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

;; ##### Exercise 2.11

;; In passing, Ben also cryptically comments: "By testing the signs of the
;; endpoints of the intervals, it is possible to break mul-interval into nine
;; cases, only one of which requires more than two multiplications." Rewrite this
;; procedure using Ben’s suggestion.

;; TODO

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; ##### Exercise 2.12

;; Define a constructor `make-center-percent` that takes a center and a
;; percentage tolerance and produces the desired interval. You must also define
;; a selector `percent` that produces the percentage tolerance for a given
;; interval. The `center` selector is the same as the one shown above.

(define (make-center-percent c p)
  (make-interval (- c (* c (/ p 100)))
                 (+ c (* c (/ p 100)))))

(make-center-percent 5 10) ;; '(9/2 . 11/2)

(define (percent i)
  (* (- (upper-bound i) (lower-bound i)) 10))

(percent (make-center-percent 5 3)) ;; 3

;; ##### Exercise 2.13

;; Show that under the assumption of small percentage tolerances there is a
;; simple formula for the approximate percentage tolerance of the product of two
;; intervals in terms of the tolerances of the factors. You may simplify the
;; problem by assuming that all numbers are positive.

;; TODO

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))

;; ##### Exercise 2.14

;; Demonstrate that Lem is right. Investigate the behavior of the system on a
;; variety of arithmetic expressions. Make some intervals *A* and *B*, and use
;; them in computing the expressions *A/A* and *A/B*. You will get the most
;; insight by using intervals whose width is a small percentage of the center
;; value. Examine the results of the computation in center-percent form (see
;; Ex. 2.12).

(define i1 (make-center-percent 3 0.003))
(define i2 (make-center-percent 5 0.0073))
(div-interval i1 i1) ;; '(0.9999400017999459 . 1.000060001800054)
(div-interval i2 i2) ;; '(0.9998540106572219 . 1.0001460106587783)
(par1 i1 i2) ;; '(1.8747002555292165 . 1.8752997867816237)
(par2 i1 i2) ;; '(1.874913514812405 . 1.8750864835624976)

;; ##### Exercise 2.15

;; Eva Lu Ator, another user, has also noticed the different intervals computed
;; by different but algebraically equivalent expressions. She says that a
;; formula to compute with intervals using Alyssa’s system will produce tighter
;; error bounds if it can be written in such a form that no variable that
;; represents an uncertain number is repeated. Thus, she says, `par2` is a
;; “better” program for parallel resistances than `par1`. Is she right? Why?

;; TODO

;; ##### Exercise 2.16

;; Explain, in general, why equivalent algebraic expressions may lead to
;; different answers. Can you devise an interval-arithmetic package that does
;; not have this shortcoming, or is this task impossible? (Warning: This problem
;; is very difficult).

;; ## 2.2 Hierarchical Data and the Closure Property

;; ### 2.2.1 Representing Sequences

(cons 1 (cons 2 (cons 3 (cons 4 null)))) ;; '(1 2 3 4)
(list 1 2 3 4) ;; '(1 2 3 4)

(define one-through-four (list 1 2 3 4))
one-through-four ;; '(1 2 3 4)

(car one-through-four) ;; 1
(cdr one-through-four) ;; '(2 3 4)
(car (cdr one-through-four)) ;; 2
(cons 10 one-through-four) ;; '(10 1 2 3 4)
(cons 5 one-through-four) ;; '(5 1 2 3 4)

;; #### List operations

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25))
(list-ref squares 3) ;; 16

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(define odds (list 1 3 5 7))
(length odds) ;; 4

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(append squares odds) ;; '(1 4 9 16 25 1 3 5 7)
(append odds squares) ;; '(1 3 5 7 1 4 9 16 25)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;; ##### Exercise 2.17

;; Define a procedure `last-pair` that returns the list that contains only the
;; last element of a given (nonempty) list:

(define (last-pair lst)
  (if (null? (cdr lst))
      (list (car lst))
      (last-pair (cdr lst))))

(last-pair (list 23 72 149 34)) ;; '(34)

;; ##### Exercise 2.18

;; Define a procedure reverse that takes a list as argument and returns a list
;; of the same elements in reverse order:

(define (reverse lst)
  (if (null? lst)
      null
      (append (reverse (cdr lst)) (list (car lst)))))

(reverse (list 1 4 9 16 25)) ;; '(25 16 9 4 1)

;; ##### Exercise 2.19

;; Consider the change-counting program of Section 1.2.2. It would be nice to be
;; able to easily change the currency used by the program, so that we could
;; compute the number of ways to change a British pound, for example. As the
;; program is written, the knowledge of the currency is distributed partly into
;; the procedure `first-denomination` and partly into the procedure
;; `count-change` (which knows that there are five kinds of U.S. coins). It
;; would be nicer to be able to supply a list of coins to be used for making
;; change.

;; We want to rewrite the procedure `cc` so that its second argument is a list
;; of the values of the coins to use rather than an integer specifying which
;; coins to use. We could then have lists that defined each kind of currency:

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

;; We could then call `cc` as follows:
;; (cc 100 us-coins) ;; 292

;; To do this will require changing the program `cc` somewhat. It will still
;; have the same form, but it will access its second argument differently, as
;; follows:

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

;; Define the procedures `first-denomination`, `except-first-denomination`, and
;; `no-more?` in terms of primitive operations on list structures. Does the
;; order of the list `coin-values` affect the answer produced by `cc`? Why or
;; why not?

;; TODO

;; ##### Exercise 2.20

;; The procedures `+`, `*`, and `list` take arbitrary numbers of arguments. One
;; way to define such procedures is to use define with *dotted-tail
;; notation*. In a procedure definition, a parameter list that has a dot before
;; the last parameter name indicates that, when the procedure is called, the
;; initial parameters (if any) will have as values the initial arguments, as
;; usual, but the final parameter's value will be a *list* of any remaining
;; arguments. For instance, given the definition

;;    (define (f x y . z) <body>)

;; the procedure `f` can be called with two or more arguments. If we evaluate

;;    (f 1 2 3 4 5 6)

;; then in the body of `f`, `x` will be 1, `y` will be 2, and `z` will be the
;; list `(3 4 5 6)`. Given the definition

;;    (define (g . w) <body>)

;; the procedure `g` can be called with zero or more arguments. If we evaluate

;;    (g 1 2 3 4 5 6)

;; then in the body of `g`, w will be the list (1 2 3 4 5 6).

;; Use this notation to write a procedure `same-parity` that takes one or more
;; integers and returns a list of all the arguments that have the same even-odd
;; parity as the first argument.

(define (same-parity decider . numbers)
  (letrec ((parity (modulo decider 2))
           (visit (lambda (numbers)
                    (if (null? numbers)
                        null
                        (if (= parity (modulo (car numbers) 2))
                            (cons (car numbers) (visit (cdr numbers)))
                            (visit (cdr numbers)))))))
    (append (list decider) (visit numbers))))

(same-parity 1 2 3 4 5 6 7) ;; '(1 3 5 7)
(same-parity 2 3 4 5 6 7) ;; '(2 4 6)

;; #### Mapping over lists

(define (scale-list items factor)
  (if (null? items)
      null
      (cons (* (car items) factor)
            (scale-list (cdr items)
                        factor))))
(scale-list (list 1 2 3 4 5) 10) ;; '(10 20 30 40 50)

(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))
(map abs (list -10 2.5 -11.6 17)) ;; '(10 2.5 11.6 17)
(map (lambda (x) (* x x)) (list 1 2 3 4)) ;; '(1 4 9 16)

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

(scale-list (list 1 2 3 4 5) 10) ;; '(10 20 30 40 50)

;; ##### Exercise 2.21

;; The procedure `square-list` takes a list of numbers as argument and returns a
;; list of the squares of those numbers.

;;    (square-list (list 1 2 3 4)) ;; '(1 4 9 16)

;; Here are two different definitions of `square-list`. Complete both of them by
;; filling in the missing expressions:

(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      null
      (cons (square (car items))
            (square-list (cdr items)))))
(square-list (list 1 2 3 4)) ;; '(1 4 9 16)

(define (square-list items)
  (map (lambda (x) (square x)) items))
(square-list (list 1 2 3 4)) ;; '(1 4 9 16)

;; ##### Exercise 2.22

;; Louis Reasoner tries to rewrite the first `square-list` procedure of Exercise
;; 2.21 so that it evolves an iterative process:

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items null))

(square-list (list 1 2 3 4)) ;; '(16 9 4 1)

;; Unfortunately, defining square-list this way produces the answer list in the
;; reverse order of the one desired. Why?

;; It generates the reverse list because it builds the resulting list as it goes
;; through the items while the recursive first builds the result when it hits
;; the last item. In other words, in the iterative approach we start by
;; `cons`'ing `1` and `null`, then 4 and `(1)` and so on.

;; Louis then tries to fix his bug by interchanging the arguments to `cons`:

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items null))

(square-list (list 1 2 3 4)) ;; '((((() . 1) . 4) . 9) . 16)

;; This doesn’t work either. Explain.

;; This approach `cons`es the empty list, `null`, to each item in `items`
;; resulting in the above unwanted nested list structure.

;; ##### Exercise 2.23

;; The procedure `for-each` is similar to `map`. It takes as arguments a
;; procedure and a list of elements. However, rather than forming a list of the
;; results, `for-each` just applies the procedure to each of the elements in
;; turn, from left to right. The values returned by applying the procedure to
;; the elements are not used at all — `for-each` is used with procedures that
;; perform an action, such as printing. For example,

(for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))
;; 57
;; 321
;; 88

;; The value returned by the call to `for-each` (not illustrated above) can be
;; something arbitrary, such as true. Give an implementation of `for-each`.

(define (for-each proc items)
  (if (null? items)
      #t
      (begin (proc (car items))
             (for-each proc (cdr items)))))

(for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))
;; 57
;; 321
;; 88
;; #t

;; ### 2.2.2 Hierarchical Structures

;; TODO
