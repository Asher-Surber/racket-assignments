#lang racket

;Asher Surber Assignment 1
;NOTE: I've never used Racket before, and struggled with the weird syntax structure
;for most of the time we had this assignment


;;1: Countdown
(define countdown
  (λ (num)
    (cond
      [(< num 0) null]
      [else (cons num (countdown (sub1 num)))]
      )))

(countdown 5)

;;2: insertL
(define insertL
  (λ (sym1 sym2 list)
    (cond
      [(null? (cdr list)) list]
      [(eqv? (car list) sym1) (cons (cons sym2 (car list)) (insertL sym1 sym2 (cdr list)))]
     )))
;;PROBLEM

(insertL 'x 'y '(x z z x y x))

;;3: remv-1st
(define remv-1st
  (λ (sym list)
    (cond
      [(null? list) list]
      [(eqv? (car list) sym) (cdr list)]
      [else (cons (car list) (remv-1st sym (cdr list)))]
      )))

(remv-1st 'x '(x y z x))
(remv-1st 'y '(x y z y x))
(remv-1st 'z '(a b c))

;;4: filter-out
(define filter-out
  (λ (pred list)
    (cond
      [(null?  list) list]
      [(pred (car list)) (filter-out pred (cdr list))]
      [else (cons (car list) (filter-out pred (cdr list)))]
      )))

(filter-out even? '(1 2 3 4 5 6))

;;5: map
(define map
  (λ (p list)
    (cond
      [(null? list) list]
      [else (cons (p (car list)) (map p (cdr list)))]
      )))

(map sub1 '(1 2 3 4))

;;6: zip
(define zip
  (λ (list1 list2)
    (cond
      [(or (null? list1) (null? list2)) null]
      [else (cons (cons (car list1) (car list2)) (zip (cdr list1) (cdr list2)))]
      )))

(zip '(1 2 3) '(a b c))
(zip '(1 2 3 4 5 6) '(a b c))
(zip '(1 2 3) '(a b c d e f))

;;7: list-index-ofv
(define ctr 0)
(define list-index-ofv
  (λ (elem list)
      (cond
      [(eqv? elem (car list)) ctr]
      [else (add1 ctr)(list-index-ofv elem (cdr list))]
      )))
;;can't update ctr this way
;;don't know how to implement counters in racket, esp with these limitations

(list-index-ofv 'x '(x y z x x))
(list-index-ofv 'x '(y z x x))

;;8: append
#|(define append
  (λ (ls1 ls2)
    (cond
      [(and (null? ls1) (null? ls2)) null]
      [(null? ls1) |#

;;9: reverse
#|(define reverse
  (λ (list)
    (cond
      [(null? (cdr list)) (cons (car list) (reverse (cdr list)))]
      [(null? (car list)) null]
      [else (reverse (cdr list))]
      )))


(reverse '(a 3 x))|#

;;10: repeat
#|(define repeat
  (λ (list num)
    (cond
      []
      []
      ))) |#
;;don't know how to implement counters in racket, esp with these limitations

;;11: same-lists*
(define same-lists*
  (λ (list1 list2)
    (cond
      [(and (null? list1) (null? list2) #t)]
      [(or (null? list1) (null? list2) #f)]
      [(eqv? (car list1) (car list2)) (same-lists* (cdr list1) (cdr list2))]
      [else #f]
      )))

(same-lists* '() '())
(same-lists* '(1 2 3 4 5) '(1 2 3 4 5))
(same-lists* '(1 2 3 4) '(1 2 3 4 5))
(same-lists* '(a (b c) d) '(a (b) c d))
(same-lists* '((a) b (c d) d) '((a) b (c d) d))

;;12: (((w . (x . ())) . (y . ())) . ((z . ()) . ()))

#|(let ([w "w"] [x "x"] [y "y"] [z "z"])
(eqv? ('('(w x) y '(z))) ('('('(w . (x . '())) . '(y . '())) . '('(z . '()) . '())))))|#

;;13: binary->natural
#|(define binary->natural
  (λ (binList)
    (cond
      [(null? binList) null]
      [
      |#