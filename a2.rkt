#lang racket
(require racket/trace)

;;Assignment 2
;;Asher Surber

;;1 list-ref

(define list-ref
  (lambda (ls n)
    (letrec
        ([nth-cdr (lambda (n)
                    (cond
                      [(eqv? n 0) ls]
                      [else (list-ref (cdr ls) (sub1 n))]))])
      (car (nth-cdr n)))))

(list-ref '(a b c) 2)
(list-ref '(a b c) 0)