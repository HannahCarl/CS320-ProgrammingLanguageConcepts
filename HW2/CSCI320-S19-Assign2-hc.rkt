#lang racket

;Hannah Carl
;Assignment 2

;Define complement
(define (compl num)
  (append (list(car num)) (list(* -1 (car (cdr num)))))
)

;Define absolute value
(define (abs num)
  (sqrt (+ (* (car num) (car num)) (* (car (cdr num)) (car (cdr num)))))
  )


;Define equal
(define (equal? num1 num2)
    (and (= (car num1) (car num2)) (= (car (cdr num1)) (car (cdr num2))))
  )

;Define plus
(define (plus num1 num2)
  (list (+ (car num1) (car num2)) (+ (car (cdr num1)) (car (cdr num2))))
  )

;Define minus
(define (minus num1 num2)
  (list (- (car num1) (car num2)) (- (car (cdr num1)) (car (cdr num2))))
  )

;Define prod
(define (prod num1 num2)
  (list (- (* (car num1) (car num2)) (* (car (cdr num1)) (car (cdr num2)))) (+ (* (car num1) (car (cdr num2))) (* (car num2) (car (cdr num1)))))
  )

;Define quotient
(define (quotient num1 num2)
  (list (/ (+ (* (car num1) (car num2)) (* (car (cdr num1)) (car (cdr num2)))) (+ (* (car num2) (car num2)) (* (car(cdr num2)) (car(cdr num2))))) (/ (- (* (car num2) (car (cdr num1))) (* (car num1) (car (cdr num2)))) (+ (* (car num2) (car num2)) (* (car(cdr num2)) (car(cdr num2))))))
  )


;Define member? - for use in permutation
(define (member? atm lst)
  (cond
    ((null? lst) #f)
    ((eqv? atm (car lst)) #t)
    (else (member? atm (cdr lst)))
    )
  )

;Define subset? - for use in permutation
(define (subset? lst1 lst2)
  (cond
    ((null? lst1) #t)
    (else(and (member? (car lst1) lst2) (subset? (cdr lst1) lst2)))
    )
  )

;Define permutation?
(define (permutation? lst1 lst2)
  (cond
    ((and (null? lst1) (null? lst2)) #t)
    ((and (= (length lst1) (length lst2))(subset? lst1 lst2) (subset? lst2 lst1) ) #t )
    (else #f)
  
  )
  )


;Define tree 
(define (tree? treelst)
  (cond
    ((null? treelst) #t)
    (else (and (not(list? (car treelst))) (list? (cadr treelst)) (list? (caddr treelst))))
    )
  )

;Define preorder
(define (preorder treelst)
  (cond
    ((null? treelst) '())
    (else (append (list (car treelst)) (preorder (cadr treelst)) (preorder (caddr treelst))))
    )
  )


;Define inorder
(define (inorder treelst)
  (cond
    ((null? treelst) '())
    (else (append (inorder (cadr treelst)) (list (car treelst)) (inorder (caddr treelst))))
    )
  )

;Define postorder
(define (postorder treelst)
  (cond
    ((null? treelst) '())
    (else (append (postorder (cadr treelst)) (postorder (caddr treelst)) (list (car treelst))))
    )
  )

(compl '(2.5 3.7))
(abs '(2.5 3.7))
(equal? '(3.5 2.0) '(3.5 2.0))
(equal? '(3.4 2.1) '(0.0 1.0))
(plus '(3.4 2.1) '(0.0 1.0))
(minus '(3.4 2.1) '(0.0 1.0))
(prod '(3.4 2.1) '(0.0 1.0))
(quotient '(3.4 2.1) '(0.0 1.0))
(permutation? '(1 3 2) '(3 1 2))
(permutation? '(1 3 2) '(1 2))
(permutation? '(1 3 2) '(1 2 3 a))
(permutation? '(1 3 3) '(1 3 2))
(permutation? '(5 1 4) '(7 6 2))
(permutation? '() '())
(permutation? '(1 2 3) '(1 2 3))
'(end of permutation)
(tree? '(a (b () (c () ())) (d () (e (f () ()) ()))))
(tree? '(a b))
(tree? '())
(preorder '(a (b () (c () ())) (d () (e (f () ()) ()))))
(inorder '(a (b () (c () ())) (d () (e (f () ()) ()))))
(postorder '(a (b () (c () ())) (d () (e (f () ()) ()))))



