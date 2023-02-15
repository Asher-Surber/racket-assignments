#lang racket

;; Asher Surber
;; Assignment 5


(define value-of-ds
  (λ (e env)
    (match e
      [`,n #:when (number? n) n]
      [`(+ ,nexpr1 ,nexpr2)
       (+ (value-of-ds nexpr1 env)
          (value-of-ds nexpr2 env))]
      [`(if-null ,l ,null-expr ,not-null-expr)
        (cond
          [(null? (value-of-ds l env))
           (value-of-ds null-expr env)]
          [else (value-of-ds not-null-expr env)])]
      [`(cons ,car-expr ,cdr-expr)
       (cons (value-of-ds car-expr env)
             (value-of-ds cdr-expr env))]
      [`(car ,l-expr)
       (car (value-of-ds l-expr env))]
      [`(cdr ,l-expr)
       (cdr (value-of-ds l-expr env))]
      ['empty '()]
      ; Lookup the symbol y in environment
      [`,y #:when (symbol? y) (env y)]
       [`(let ((,x ,v)) ,body)
       (value-of-ds `((λ (,x) ,body) ,v) env)]
      ; Return function
      [`(λ (,x) ,body)
       #:when (symbol? x)
       ;; Replacing the formal env with env^ turns this into
       ;; a lexically scoped interpreter
       (λ (arg env)
         ; We also need to extend the environment but the
         ; env is not bound from the usual one
         (value-of-ds body (λ (y)
                          (cond
                            [(eqv? y x) arg]
                            [else (env y)]))))]
      [`(,rator ,rand)
       ((value-of-ds rator env)
        (value-of-ds rand env)
        env)])))



(define empty-env-ds
  ('("env")))


(define extend-env-ds
  (λ (env var val)
    (cons env (cons var val))))


(define apply-env-ds)


(define make-closure-ds)


(define apply-closure-ds)