#lang scheme
;Exercise 1.3
(define (square x) (* x x))
(define (sum-of-squars a b c) (+ (square a) (square b) (square c)))
;Exercise 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
;Es wird eine procedure "a-plus-abs-b" definiert mit 2 parametern a und b
;Im body der procedure wird der Ausdruck (if(> b 0) + - ) als Operator
;auf a b angewendet. Dafuer muss dieser erst ausgewertet werden
;Ist b>0 wird der Operator zu + d.h (+ a b) = a + b
;Sonst wird der Operator zu - d.h (- a b) = a - b = a + (-b)
; Dh erhalten wir das Verhalten des Absolutbetrags. Fuer b = 0
; ==> -b = b

;Exercise 1.5
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))