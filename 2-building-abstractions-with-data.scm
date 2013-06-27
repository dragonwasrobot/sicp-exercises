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
  (let ((parity (modulo decider 2)))
    (define (visit numbers)
      (if (null? numbers)
          null
          (if (= parity (modulo (car numbers) 2))
              (cons (car numbers) (visit (cdr numbers)))
              (visit (cdr numbers)))))
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

;; Borrowed function - start (`1-building-abstractions-with-procedures.scm`)

(define (square x) (* x x))

;; Borrowed function - stop

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

(cons (list 1 2) (list 3 4)) ;; '((1 2) 3 4)

(define x (cons (list 1 2) (list 3 4)))
(length x) ;; 3
(count-leaves x) ;; 4
(list x x) ;; '(((1 2) 3 4) ((1 2) 3 4))
(length (list x x)) ;; 2
(count-leaves (list x x)) ;; 8

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;; ##### Exercise 2.24

;; Suppose we evaluate the expression `(list 1 (list 2 (list 3 4)))`. Give the
;; result printed by the interpreter, the corresponding box-and-pointer
;; structure, and the interpretation of this as a tree (as in Figure 2.6).

(list 1 (list 2 (list 3 4))) ;; '(1 (2 (3 4)))

;; TODO box-and-pointer + tree.

;; ##### Exercise 2.25

;; Give combinations of `cars` and `cdrs` that will pick 7 from each of the
;; following lists:

;; `(1 3 (5 7) 9)`

(cadr (caddr '(1 3 (5 7) 9))) ;; 7

;; `((7))`

(caar '((7))) ;; 7

;; `(1 (2 (3 (4 (5 (6 7))))))`

(cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7))))))))) ;; 7

;; ##### Exercise 2.26

;; Suppose we define `x` and `y` to be two lists:

(define x (list 1 2 3))
(define y (list 4 5 6))

;; What result is printed by the interpreter in response to evaluating each of
;; the following expressions:

(append x y) ;; '(1 2 3 4 5 6)
(cons x y) ;; '((1 2 3) 4 5 6)
(list x y) ;; '((1 2 3) (4 5 6))

;; ##### Exercise 2.27

;; Modify your `reverse` procedure of Exercise 2.18 to produce a `deep-reverse`
;; procedure that takes a list as argument and returns as its value the list
;; with its elements reversed and with all sublists deep-reversed as well.

(define (deep-reverse lst)
  (if (null? lst)
      null
      (append (deep-reverse (cdr lst)) (if (pair? (car lst))
                                      (list (reverse (car lst)))
                                      (list (car lst))))))

(define x (list (list 1 2) (list 3 4)))
x ;; '((1 2) (3 4))
(reverse x) ;; '((3 4) (1 2))
(deep-reverse x) ;; '((4 3) (2 1))

;; ##### Exercise 2.28

;; Write a procedure `fringe` that takes as argument a tree (represented as a
;; list) and returns a list whose elements are all the leaves of the tree
;; arranged in left-to-right order.

;; This guy presumes its a binary tree.
(define (fringe tree)
  (cond ((null? tree) null)
        ((pair? tree) (append (fringe (car tree)) (fringe (cdr tree))))
        (else (list tree))))

(define x (list (list 1 2) (list 3 4)))
(fringe x) ;; '(1 2 3 4)
(fringe (list x x)) ;; '(1 2 3 4 1 2 3 4)

;; ##### Exercise 2.29

;; A binary mobile consists of two branches, a left branch and a right branch.
;; Each branch is a rod of a certain length, from which hangs either a weight or
;; another binary mobile. We can represent a binary mobile using compound data
;; by constructing it from two branches (for example, using `list`):

(define (make-mobile left right)
  (list left right))

;; A branch is constructed from a `length` (which must be a number) together
;; with a `structure`, which may be either a number (representing a simple
;; weight) or another mobile:

(define (make-branch length structure)
  (list length structure))

;; a. Write the corresponding selectors `left-branch` and `right-branch`, which
;; return the branches of a mobile, and `branch-length` and `branch-structure`,
;; which return the components of a branch.

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (mobile? structure)
  (pair? structure))

(define my-mobile (make-mobile (make-branch 1 2) (make-branch 3 4)))
(define fancy-mobile (make-mobile
                      (make-branch 2 (make-mobile (make-branch 1 2)
                                                  (make-branch 3 4)))
                      (make-branch 3 (make-mobile (make-branch 4 5)
                                                  (make-branch 5 6)))))

(left-branch my-mobile) ;; '(1 2)
(right-branch my-mobile) ;; '(3 4)
(branch-length (left-branch my-mobile)) ;; 1
(branch-structure (right-branch my-mobile)) ;; 4
(branch-structure (right-branch fancy-mobile)) ;; '((4 5) (5 6))

;; b. Using your selectors, define a procedure `total-weight` that returns the
;; total weight of a mobile.

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (mobile? structure)
        (total-weight structure)
        structure)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(total-weight my-mobile) ;; 6
(total-weight fancy-mobile) ;; 17

;; c. A mobile is said to be *balanced* if the torque applied by its top-left
;; branch is equal to that applied by its top-right branch (that is, if the
;; length of the left rod multiplied by the weight hanging from that rod is
;; equal to the corresponding product for the right side) and if each of the
;; submobiles hanging off its branches is balanced. Design a predicate that
;; tests whether a binary mobile is balanced.

(define (branch-balanced? branch)
  (let ((structure (branch-structure branch)))
    (if (mobile? structure)
        (balanced? structure)
        #t)))

(branch-balanced? (make-branch 2 3)) ;; #t

(define (branch-torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(branch-torque (make-branch 2 3)) ;; 6

(define (balanced? mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (= (branch-torque left)
            (branch-torque right))
         (branch-balanced? left)
         (branch-balanced? right))))

(define my-mobile (make-mobile (make-branch 2 6) (make-branch 3 4)))
(define fancy-mobile (make-mobile
                      (make-branch 2 (make-mobile (make-branch 1 2)
                                                  (make-branch 3 4)))
                      (make-branch 3 (make-mobile (make-branch 4 5)
                                                  (make-branch 5 6)))))

(balanced? my-mobile) ;; #t
(balanced? fancy-mobile) ;; #f

;; d. Suppose we change the representation of mobiles so that the constructors
;; are

(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

;; How much do you need to change your programs to convert to the new
;; representation?

;; Only have to change the definitions of `left-branch` and `right-branch`, now
;; that we can just use `cdr` instead of `cadr`.

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

(define (mobile? structure)
  (pair? structure))

(left-branch my-mobile) ;; '(2 . 6)
(right-branch my-mobile) ;; '(3 . 4)
(branch-length (left-branch my-mobile)) ;; 1
(branch-structure (right-branch my-mobile)) ;; 4
(branch-structure (right-branch fancy-mobile)) ;; '((4 . 5) (5 . 6))

(total-weight my-mobile) ;; 6
(total-weight fancy-mobile) ;; 17

(balanced? my-mobile) ;; #t
(balanced? fancy-mobile) ;; #f

;; #### Mapping over trees

(define (scale-tree tree factor)
  (cond ((null? tree) null)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
;; '(10 (20 (30 40) 50) (60 70))

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
;; '(10 (20 (30 40) 50) (60 70))

;; ##### Exercise 2.30

;; Define a procedure `square-tree` analogous to the square-list procedure of
;; Exercise 2.21. That is, square-tree should behave as follows:

;;    (square-tree
;;      (list 1
;;        (list 2 (list 3 4) 5)
;;        (list 6 7)))
;;    '(1 (4 (9 16) 25) (36 49))

;; Define `square-tree` both directly (i.e., without using any higher-order
;; procedures) and also by using map and recursion.

(define (square-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;; '(1 (4 (9 16) 25) (36 49))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;; '(1 (4 (9 16) 25) (36 49))

;; ##### Exercise 2.31

;; Abstract your answer to Exercise 2.30 to produce a procedure `tree-map` with
;; the property that square-tree could be defined as

;;    (define (square-tree tree) (tree-map square tree))

(define (tree-map fun tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map fun sub-tree)
             (fun sub-tree)))
       tree))

(define (square-tree tree) (tree-map square tree))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
;; '(1 (4 (9 16) 25) (36 49))

;; ##### Exercise 2.32

;; We can represent a set as a list of distinct elements, and we can represent
;; the set of all subsets of the set as a list of lists. For example, if the set
;; is (1 2 3), then the set of all subsets is (() (3) (2) (2 3) (1) (1 3) (1 2)
;; (1 2 3)). Complete the following definition of a procedure that generates the
;; set of subsets of a set and give a clear explanation of why it works:

(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map
                      (lambda (element)
                        (cons (car s) element))
                      rest)))))

(subsets (list 1 2 3))
;; '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

;; ### 2.2.3 Sequences as Conventional Interfaces

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree)
             (square tree)
             0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define a-tree (list (list (list 1 2) (list 3 4)) (list (list 5 6) (list 7 8))))
(sum-odd-squares a-tree) ;; 84 (+ 1 9 25 49)

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        null
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
    (next 0))

(even-fibs 10) ;; '(0 2 8 34)

;; #### Sequence Operations

(map square (list 1 2 3 4 5 )) ;; '(1 4 9 16 25)

(define (filter predicate sequence)
  (cond ((null? sequence)
         null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(filter odd? (list 1 2 3 4 5)) ;; '(1 3 5)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(accumulate + 0 (list 1 2 3 4 5)) ;; 15
(accumulate * 1 (list 1 2 3 4 5)) ;; 120
(accumulate cons null (list 1 2 3 4 5)) ;; '(1 2 3 4 5)

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 2 7) ;; '(2 3 4 5 6 7)

(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5)) ;; '(1 2 3 4 5)

(define (sum-odd-squares tree)
  (accumulate
   + 0 (map square (filter odd? (enumerate-tree tree)))))

(define a-tree (list (list (list 1 2) (list 3 4)) (list (list 5 6) (list 7 8))))
(sum-odd-squares a-tree) ;; 84

(define (even-fibs n)
  (accumulate
   cons null (filter even? (map fib (enumerate-interval 0 n)))))

(even-fibs 10) ;; '(0 2 8 34)

(define (list-fib-squares n)
  (accumulate
   cons null (map square (map fib (enumerate-interval 0 n)))))

(list-fib-squares 10) ;; '(0 1 1 4 9 25 64 169 441 1156 3025)

(define (product-of-squares-of-odd-elements sequence)
  (accumulate * 1 (map square (filter odd? sequence))))

(product-of-squares-of-odd-elements (list 1 2 3 4 5)) ;; 225 (* 1 9 25)

;; (define (salary-of-highest-paid-programmer records)
;;  (accumulate
;;   max 0 (map salary (filter programmer? records))))

;; ##### Exercise 2.33

;; Fill in the missing expressions to complete the following definitions of
;; some basic list-manipulation operations as accumulations:

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))

(map square (list 1 2 3 4 5)) ;; '(1 4 9 16 25)

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 1 2 3 4) (list 5 6 7)) ;; '(1 2 3 4 5 6 7)

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(length (list 1 3 5 7 9)) ;; 5

;; ##### Exercise 2.34

;; Evaluating a polynomial in *x* at a given value of *x* can be formulated as
;; an accumulation. We evaluate the polynomial

;;    a_n*x^n + a_{n-1}*x^{n-1} + ... + a_1*x + a_0

;; using a well-known algorithm called *Horner's rule*, which structures the
;; computation as

;;    (...(a_n*x + a_{n-1})*x + ... + a_1)*x + a_0.

;; In other words, we start with *a_n*, multiply by *x*, add *a_{n−1}*, multiply
;; by *x*, and so on, until we reach *a_0*.

;; Fill in the following template to produce a procedure that evaluates a
;; polynomial using Horner's rule. Assume that the coefficients of the
;; polynomial are arranged in a sequence, from *a_0* through *a_n*.

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))

;; For example, to compute *1 + 3x + 5x3 + x5 at x = 2* you would evaluate
;; `(horner-eval 2 (list 1 3 0 5 0 1))`

;; (+ (* (+ (* (+ (* (+ (* (+ (* 1 x) (* 0 x)) x) 5) x) 0) x) 3) x) 1)
(horner-eval 3 (list 1 0 5 0 3 1))
;; (horner-eval 2 (list 1 3 0 5 0 1)) ;; 79

(horner-eval 0 (list 3 2 1)) ;; 3
(horner-eval 1 (list 3 2 1)) ;; 6
(horner-eval 2 (list 3 2 1)) ;; 11
(horner-eval 3 (list 3 2 1)) ;; 18
(horner-eval 4 (list 3 2 1)) ;; 27
(horner-eval 5 (list 3 2 1)) ;; 38


;; ##### Exercise 2.35

;; Redefine `count-leaves` from Section 2.2.2 as an accumulation:

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (node)
                     (if (not (pair? node))
                         1
                         (count-leaves node)))
                   t)))

(count-leaves a-tree) ;; 8

;; ##### Exercise 2.36

;; The procedure `accumulate-n` is similar to accumulate except that it takes as
;; its third argument a sequence of sequences, which are all assumed to have the
;; same number of elements. It applies the designated accumulation procedure to
;; combine all the first elements of the sequences, all the second elements of
;; the sequences, and so on, and returns a sequence of the results. For
;; instance, if `s` is a sequence containing four sequences, `((1 2 3) (4 5 6)
;; (7 8 9) (10 11 12))`, then the value of `(accumulate-n + 0 s)` should be the
;; sequence `(22 26 30)`. Fill in the missing expressions in the following
;; definition of `accumulate-n`:

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define some-seqs '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
(accumulate-n + 0 some-seqs) ;; '(22 26 30)

;; ##### Exercise 2.37

;; Suppose we represent vectors *v = (v_i)* as sequences of numbers, and
;; matrices *m = (m_{ij})* as sequences of vectors (the rows of the matrix). For
;; example, the matrix

;; *(1 2 3 4)*
;; *(4 5 6 6)*
;; *(6 7 8 9)*

;; is represented as the sequence *((1 2 3 4) (4 5 6 6) (6 7 8 9))*. With this
;; representation, we can use sequence operations to concisely express the basic
;; matrix and vector operations. These operations (which are described in any
;; book on matrix algebra) are the following:

;; `(dot-product v w)` returns the sum *\sum_i v_i w_i*,
;; `(matrix-*-vector m w)` returns the vector *t*,
;; where *t_i = \sum_j m_{ij} v_j
;; `(matrix-*-matrix m n)` returns the matrix *p*,
;; where *p_{ij} = \sum_k m_{ik} n_{kj}*,
;; `(transpose m)` returns the matrix *n*, where *n_{ij} = m_{ji}*

;; We can define the dot product as

;; This guy uses the built in version of `map` which takes an arbitrary number
;; of lists.
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product '(1 2 3 4) '(5 6 7 8)) ;; 70
;; (+ (* 1 5) (* 2 6) (* 3 7) (* 4 8))

;; Fill in the missing expressions in the following procedures for computing
;; the other matrix operations. (The procedure `accumulate-n` is defined in
;; Exercise 2.36).

(define a-matrix '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
(define a-vector '(4 3 2 1))

;; A = (a b) , B = (x)
;;     (c d)       (y)
;;
;; AB = (a b)(x) = (ax + by)
;;      (c d)(y)   (cx + dy)

(define (matrix-*-vector m v)
  (map (lambda (w)
         (accumulate + 0 (map * w v)))
       m))

;; (+ (* 1 4) (* 2 3) (* 3 2) (* 4 1)) = (20)
;; (+ (* 4 4) (* 5 3) (* 6 2) (* 6 1)) = (49)
;; (+ (* 6 4) (* 7 3) (* 8 2) (* 9 1)) = (70)
(matrix-*-vector a-matrix a-vector) ;; '(20 49 70)

;; (1 2 3 4)^T   (1 4 6)
;; (4 5 6 6)   = (2 5 7)
;; (6 7 8 9)     (3 6 8)
;;               (4 6 9)

(define (transpose mat)
  (accumulate-n
   cons
   null
   mat))

(transpose a-matrix) ;; '((1 4 6) (2 5 7) (3 6 8) (4 6 9))

;; (2 3 4) (0 1000)   (3 2340)
;; (1 0 0) (1  100) = (0 1000)
;;         (0   10)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map
     (lambda (row)
       (map (lambda (col) (accumulate +  0 (map * row col)))
            cols))
     m)))

(define m1 '((2 3 4) (1 0 0)))
(define m2 '((0 1000) (1 100) (0 10)))
(transpose m2) ;; '((0 1 0) (1000 100 10))

;; ( (+ (* 2 0) (* 3 1) (* 4 0)) (+ (* 2 1000) (* 3 100) (* 4 10)) )
;; ( (+ (* 1 0) (* 0 1) (* 0 0)) (+ (* 1 1000) (* 0 100) (* 0 10)) )
(matrix-*-matrix m1 m2) ;; '((3 2340) (0 1000))

;; ##### Exercise 2.38

;; The `accumulate` procedure is also known as `fold-right`, because it combines
;; the first element of the sequence with the result of combining all the
;; elements to the right. There is also a `fold-left`, which is similar to
;; `fold-right`, except that it combines elements working in the opposite
;; direction:

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; What are the values of

(fold-right / 1 (list 1 2 3)) ;; 3/2
(fold-left / 1 (list 1 2 3)) ;; 1/6
(fold-right list null (list 1 2 3)) ;; '(1 (2 (3 ())))
(fold-left list null (list 1 2 3)) ;; '(((() 1) 2) 3)

;; Give a property that `op` should satisfy to guarantee that `fold-right` and
;; `fold-left` will produce the same values for any sequence.

;; `op` should be commutative, i.e. (= (op a b) (op b a))

(fold-right + 0 (list 1 2 3)) ;; 6
(fold-left + 0 (list 1 2 3)) ;; 6
(= (fold-right + 0 (list 1 2 3)) (fold-left + 0 (list 1 2 3))) ;; #t

;; ##### Exercise 2.39

;; Complete the following definitions of `reverse` (Exercise 2.18) in terms of
;; `fold-right` and `fold-left` from Exercise 2.38:

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) null sequence))

(reverse '(1 2 3 4 5)) ;; '(5 4 3 2 1)

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) null sequence))

(reverse '(1 2 3 4 5)) ;; '(5 4 3 2 1)

;; #### Nested Mappings

;;    (accumulate
;;     append null (map (lambda (i)
;;                      (map (lambda (j) (list i j))
;;                           (enumerate-interval 1 (- i 1))))
;;                    (enumerate-interval 1 n)))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

;; Borrowed functions - start (`1-building-abstractions-with-procedures.scm`)

(define (divides? a b) (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

;; Borrowed functions - stop

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(prime-sum-pairs 6)
;; '((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))

(define (permutations s)
  (if (null? s)        ; empty set?
      (list null)      ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(permutations '(1 2 3))
;; '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))

;; ##### Exercise 2.40

;; Define a procedure `unique-pairs` that, given an integer *n*, generates the
;; sequence of pairs *(i, j)* with *1 <= j < i <= n*. Use `unique-pairs` to
;; simplify the definition of `prime-sum-pairs` given above.

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(unique-pairs 5)
;; '((2 1) (3 1) (3 2) (4 1) (4 2) (4 3) (5 1) (5 2) (5 3) (5 4))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(prime-sum-pairs 6)
;; '((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))

;; ##### Exercise 2.41

;; Write a procedure to find all ordered triples of distinct positive integers
;; *i*, *j*, and *k* less than or equal to a given integer *n* that sum to a
;; given integer *s*.

(define (sums-to? s lst)
  (= s (accumulate + 0 lst)))

(sums-to? '(1 2 3) 6) ;; #t
(sums-to? '(1 42 3) 20) ;; #f

(define (unique-triples n)
  (flatmap
   (lambda (i)
     (flatmap (lambda (j)
            (map (lambda (k) (list i j k))
                 (enumerate-interval 1 (- j 1))))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(unique-triples 5)

;; '((3 2 1) (4 2 1) (4 3 1) (4 3 2) (5 2 1)
;;   (5 3 1) (5 3 2) (5 4 1) (5 4 2) (5 4 3))

(define (triples-sum-to? n s)
  (filter (lambda (lst) (sums-to? s lst))
          (unique-pairs n)))

(triples-sum-to? 5 7) ;; '((4 3) (5 2))
(triples-sum-to? 5 3) ;; '((2 1))

;; ##### Exercise 2.42

;; The "eight-queens puzzle" asks how to place eight queens on a chessboard so
;; that no queen is in check from any other (i.e., no two queens are in the same
;; row, column, or diagonal). One possible solution is shown in Figure 2.8. One
;; way to solve the puzzle is to work across the board, placing a queen in each
;; column. Once we have placed *k − 1* queens, we must place the *k^{th}* queen
;; in a position where it does not check any of the queens already on the
;; board. We can formulate this approach recursively: Assume that we have
;; already generated the sequence of all possible ways to place *k − 1* queens
;; in the first *k − 1* columns of the board. For each of these ways, generate
;; an extended set of positions by placing a queen in each row of the *k^{th}*
;; column. Now filter these, keeping only the positions for which the queen in
;; the *k^{th}* column is safe with respect to the other queens. This produces
;; the sequence of all ways to place *k* queens in the first *k* columns. By
;; continuing this process, we will produce not only one solution, but all
;; solutions to the puzzle.

;; We implement this solution as a procedure `queens`, which returns a sequence
;; of all solutions to the problem of placing *n* queens on an *n * n*
;; chessboard. Queens has an internal procedure `queen-cols` that returns the
;; sequence of all ways to place queens in the first *k* columns of the board.

(define (queens board-size)
  (define (queen-cols k)
      (if (= k 0)
          (list empty-board)
          (filter
           (lambda (positions) (safe? k positions))
           (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row
                                      k
                                      rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
    (queen-cols board-size))

;; In this procedure `rest-of-queens` is a way to place *k − 1* queens in the
;; first *k − 1* columns, and `new-row` is a proposed row in which to place the
;; queen for the *k^{th}* column. Complete the program by implementing the
;; representation for sets of board positions, including the procedure
;; `adjoin-position`, which adjoins a new row-column position to a set of
;; positions, and `empty-board`, which represents an empty set of positions. You
;; must also write the procedure `safe?`, which determines for a set of
;; positions, whether the queen in the *k^{th}* column is safe with respect to
;; the others. (Note that we need only check whether the new queen is safe - the
;; other queens are already guaranteed safe with respect to each other).

(define (make-position row column)
  (cons row column))

(define (position-row position)
  (car position))

(define (position-column position)
  (cdr position))

(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list (make-position new-row k))))

(define (safe? k positions) #t)

(define (safe? k positions)
  (print k)
  (print positions)

  (define (rows-safe? k positions)
    (cond ((null? positions) #t)
          ((= (position-row k) (position-row (car positions))) #f)
          (else (rows-safe? k (cdr positions)))))

  (define (diagonal-safe? k positions)
    (cond ((null? positions) #t)
          ((= (abs (- (position-row k)
                      (position-row (car positions))))
              (abs (- (position-column k)
                      (position-column (car positions)))))
           #f)
          (else (diagonal-safe? k (cdr positions)))))

  (and (rows-safe? k positions)
       (diagonal-safe? k positions)))

;; TODO

;; ##### Exercise 2.43

;; Louis Reasoner is having a terrible time doing Exercise 2.42. His queens
;; procedure seems to work, but it runs extremely slowly. (Louis never does
;; manage to wait long enough for it to solve even the 6 x 6 case.) When Louis
;; asks Eva Lu Ator for help, she points out that he has interchanged the order
;; of the nested mappings in the flatmap, writing it as

;;    (flatmap (lambda (new-row)
;;      (map (lambda (rest-of-queens) (adjoin-position new-row k rest-of-queens))
;;           (queen-cols (- k 1)))) (enumerate-interval 1 board-size))

;; Explain why this interchange makes the program run slowly. Estimate how long
;; it will take Louis' program to solve the eight-queens puzzle, assuming that
;; the program in Exercise 2.42 solves the puzzle in time T.

;; ### 2.2.4 Example: A Picture Language
