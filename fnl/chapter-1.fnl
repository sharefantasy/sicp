;; newton-iteration
(fn abs [x] (if (> x 0) x (- 0 x)))

(fn average [x y] (/ (+ x y) 2))
(fn improve [guess x] (average guess (/ x guess)))

(fn good-enough? [x y] 
  (< (abs (- (* x x) y)) 0.0001))


(fn sqrt-iter [guess x]
  (if (good-enough? guess x) 
    guess
    (sqrt-iter (improve guess x) x)))

(fn sqrt [x] (sqrt-iter 1 x))

(sqrt 2)

