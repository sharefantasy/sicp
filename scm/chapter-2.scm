#lang scheme

;; chapter 2

;; ---------------------------------------- 2.1 ----------------------------------------
(define (minus x) (- 0 x))
(define (abs x) (if (>= x 0) x (minus x)))
(define (xor x y) (or (and x (not y)) (and y (not x))))
(define (xorsig x y) (if (xor (> x 0) (> y 0)) (minus (abs x)) (abs x)))
(define (gcd x y) 
  (if (= x 0)
      y
      (gcd (remainder y x) x)))

(define (make-rat n d) 
  (let* ((g (gcd (abs n) (abs d)))
         (nn (/ n g))
         (nd (/ d g)))
    (cons (xorsig nn nd) (abs nd))))


;;(define (make-rat n d) 
;;  (cons (/ n (gcd n d))
;; (/ d (gcd n d))))

(define numer car)
(define denom cdr)
(define (add-rat x y) 
  (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y) 
  (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y) (make-rat (* (numer x) (numer y)) (* (denom x) (denom y))))
(define (div-rat x y) (make-rat (* (numer x) (denom y)) (* (denom x) (numer y))))

(define (equal-rat? x y) 
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

(define (print-rat x) 
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))


(define (test-2.1)
  (and 
    (equal-rat? (make-rat -3 6)  (cons -1 2))
    (equal-rat? (make-rat -3 -6) (cons 1 2))
    (equal-rat? (make-rat 3 6) (cons 1 2))
    (equal-rat? (make-rat 3 -6) (cons -1 2))))

;; (test-2.1)
;; --------------------------------------------------------------------------------------------

;; ---------------------------------------- 2.2 ----------------------------------------

(define (make-point x y) (cons x y))
(define x-point car)
(define y-point cdr)
(define start-point car)
(define end-point cdr)
(define (make-segment x1 y1 x2 y2) (cons (make-point x1 y1) (make-point x2 y2)))
(define (mid-point x y) (make-point (/ (+ (x-point x) (x-point y)) 2) (/ (+ (y-point x) (y-point y)) 2)))
(define (mid-segment x) (mid-point (start-point x) (end-point x)))

;; works
(mid-segment (make-segment 1 2 3 4))

;; --------------------------------------------------------------------------------------------


;; ---------------------------------------- 2.4 ------------------------------------------------

(define (acons x y) 
  (lambda (m) (m x y)))

(define (acar z)
  (z (lambda (p q) p)))

(define (acdr z)
  (z (lambda (p q) q)))

(define (test-2.4)
  (display (and 
             (= (acar (acons 1 2)) 1)
             (= (acdr (acons 1 2)) 2))))

;; works
;; (test-2.4)

;; ---------------------------------------------------------------------------------------------


;; ---------------------------------------- 2.5 ------------------------------------------------

(define (bcons x y) 
  (* (expt 2 x) (expt 3 y)))

(define (eliminate x c) 
  (if (= (remainder x c) 0) 
      (eliminate (/ x c) c)
      x))

(define (count-pow x b count)
  (if (= x 1)
      count
      (count-pow (/ x b) b (+ count 1))))

(define (bcar x) (count-pow (eliminate x 3) 2 0))
(define (bcdr x) (count-pow (eliminate x 2) 3 0))


(define (test-2.5)
  (display (and 
             (= (bcar (bcons 1 2)) 1)
             (= (bcdr (bcons 1 2)) 2))))

;; works
;; (test-2.5)

;; --------------------------------------------------------------------------------------------


;; --------------------------------------- 2.6 ------------------------------------------------

;; show 'f' call times by counting
(define (show f) 
  ((f (lambda (x) (+ 1 x))) 0))

(define (add-1 n) 
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

;; works
(define (test-2.6)
  (display (show one))
  (display (show two)))

(test-2.6)

;; --------------------------------------------------------------------------------------------


;; --------------------------------------- 2.7 ------------------------------------------------

(define (make-interval a b) (cons a b))

(define lower-bound car)
(define upper-bound cdr)

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
  (mul-interval x 
    (make-interval (/ 1.0 (lower-bound x))
       (/ 1.0 (upper-bound y)))))

(define (test-2.7) 
  (mul-interval (make-interval 1 2) (make-interval 2 5))
  (mul-interval (make-interval 1 2) (make-interval 2 5))
  (div-interval (make-interval 1 2) (make-interval 2 5)))

;; works
;; (test-2.7)

;; --------------------------------------------------------------------------------------------


;; --------------------------------------- 2.8 ------------------------------------------------

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
     (- (upper-bound x) (lower-bound y))))


(sub-interval (make-interval 2 7) (make-interval 2 5))
;; --------------------------------------------------------------------------------------------
