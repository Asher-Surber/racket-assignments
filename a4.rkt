#lang racket

;; Asher Surber
;; Assignment 4

(define value-of-ds
  (λ (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`#t #t]
      [`#f #f]
      [`,y #:when (symbol? y) (apply-env-ds env y)]
      [`(λ (,x) ,body) #:when (symbol? x)
                       (make-closure-ds x body env)]
      [`(lambda (,x) ,body) #:when (symbol? x)
                       (make-closure-ds x body env)]
      [`(sub1 ,z) #:when (number? (value-of-ds z env)) (- (value-of-ds z env) 1)]
      [`(* ,a ,b) #:when (and (number? (value-of-ds a env)) (number? (value-of-ds b env)))
                  (* (value-of-ds a env) (value-of-ds b env))]
      [`(,rator ,rand)
       (apply-closure-ds
        (value-of-ds rator env)
        (value-of-ds rand env))]
      [`(zero? ,m) #:when (number? (value-of-ds m env))
                   (cond
                     [(eqv? (value-of-ds m) 0) #t]
                     [else #f])]
      [`(if ,cnd ,then ,els)
       (λ (cnd then els)
         (cond
           [(value-of-ds cnd env) (apply-closure-ds (value-of-ds then env) env)]
           [else (apply-closure-ds (value-of-ds els env))]
           ))]
      [`(let ((,v ,w)) ,body)
       (value-of-ds `((λ (,v) ,body) ,w) env)])))


(define empty-env-ds
  (λ ()
    '()))

(define extend-env-ds
  (λ (var val env)
    (cons (cons var val) env)))

(define apply-env-ds
  (λ (env var)
    (cond
      [(assv var env) (cdr (assv var env))]
      [else error 'val-of "unbound ~a" var])))

(define make-closure-ds
  (λ (x body env)
    `(make-closure-ds ,x ,body ,env)))
    

(define apply-closure-ds
  (λ (closure arg)
    (match closure
      [`(make-closure-ds ,x ,body ,env)
       (value-of-ds body (extend-env-ds x arg env))]
      #;[_ (clos arg)])))
  


;; Tests

(value-of-ds
   '((lambda (x) (if (zero? x)
                     #t
                     #f))
     0)
   (empty-env-ds))

(value-of-ds
   '((lambda (x) (if (zero? x) 
                     12 
                     47)) 
     0) 
   (empty-env-ds))

(value-of-ds
   '(let ([y (* 3 4)])
      ((lambda (x) (* x y)) (sub1 6)))
   (empty-env-ds))

(value-of-ds
   '(let ([x (* 2 3)])
      (let ([y (sub1 x)])
        (* x y)))
   (empty-env-ds))

(value-of-ds
   '(let ([x (* 2 3)])
      (let ([x (sub1 x)])
        (* x x)))
   (empty-env-ds))

(value-of-ds
   '(((lambda (f)
        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
      (lambda (f)
        (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
     5)
   (empty-env-ds))