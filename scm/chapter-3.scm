#lang scheme

; --------------------prequisite--------------------
(use srfi-18)

;; Exports: parallel-execute, random-wait, ramda, rset!

(define (random-wait) (thread-sleep! (/ 1 (+ 1 (random 99)))))

;; A lambda with a random execution delay
(define-syntax rambda
  (syntax-rules ()
    ((rambda () . body)
     (lambda () (random-wait) . body))))

;; A set! with delayed assignment
(define-syntax rset!
  (syntax-rules ()
    ((rset! a b)
     (let ((tmp #f))
       (random-wait) (set! tmp b)
       (random-wait) (set! a tmp)))))

;; Parallel execution of threads
(define (parallel-execute . thunks)
  (define (ensure-end ts)
    (cond ((null? ts) 'done)
          ((memq (thread-state (car ts)) '(terminated dead))
           (begin (thread-join! (car ts)) (ensure-end (cdr ts))))
          (else
           (ensure-end (append (cdr ts) (list (car ts))))))

    (let ((threads (map make-thread thunks)))
      (for-each thread-start! threads)
      (ensure-end threads))))

; --------------------------------------------------

; (RESTART 1)
(define x 10)
(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (+ x 1))))
