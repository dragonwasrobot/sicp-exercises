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
