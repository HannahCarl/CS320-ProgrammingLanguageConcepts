#lang racket

;Hannah Carl
;Assignment 1

(define (make_list_of_size n e)
  (cond
  ((zero? n) '())
  (else (cons e (make_list_of_size (- n 1) e)))
  )
)

(make_list_of_size 4 2)
(make_list_of_size 3 '(1 a))
(make_list_of_size 3 (+ 3 2))

(define (zeros lst)
  (cond
    ((null? lst) 0)
    ((= (car lst) 0) (+ 1 (zeros (cdr lst))))
    (else (zeros (cdr lst)))
    )
  )
(zeros '(0))
(zeros '(1 2 0 0))
(zeros '(0 0 0))
(zeros '())

(define (power a b)
  (cond
    ((not(= b 0)) (* a (power a (- b 1))))
    ((= b 0) 1)
    )
  )
(power 5 2)
(power 2 3)
(power 2 1)

(define (remove lst atm)
  (cond
    ((null? lst) '())
    ((not (equal? atm (car lst))) (cons (car lst) (remove (cdr lst) atm)))
    ((equal? atm (car lst)) (remove (cdr lst) atm))
    )
  )
(remove '(1 2 a (a b) a 3) 'a)

(define (largest lst)
  (cond
    ((null? lst) '())
    ((= (length lst) 1) (car lst))
    ((not(null? lst)) (max (car lst) (largest (cdr lst))))
    )
  )
(largest '(1 4 5 7 99 0))
(largest '())
(largest '(1 0 1))












