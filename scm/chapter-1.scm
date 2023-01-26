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

(test-2.1)
;; --------------------------------------------------------------------------------------------



