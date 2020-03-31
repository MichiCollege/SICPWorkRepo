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

;Excercise 1.6
;Newton method taken from SICP
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess)x)) 0.001))
(define (abs x)
  (if (< x 0)
      (* x -1)
      x))
(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;Using cond based if for sqrt-iter
(define (sqrt-iter2 guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter2 (improve guess x)
                     x)))
(define (sqrt2 x)
  (sqrt-iter2 1.0 x))
;The program does't terminate. Because new-if is not a special form all operands are expanded
;before starting evaluation. Thus the else clause of new-if is also evaluated even if the
; predicate is #t. In this case this causes an endless recursion.

;Exercise 1.7
(sqrt 8100000000)
(sqrt (/ 1 10000))
(* (/ 0.001 8100000000) 100) ;off by 1.2 * 10^(-11) % Uses additional iterations for mostly unnecessary precise approximation
(* (/ 0.001 (/ 1 10000)) 100) ; off by 1000% Stop way before any precie approximation is reached

(define (pcent-change x y)
  (cond ((> x y) (/ y x))
        (else (/ x y))))

(define (better-good-enough? guess x)
   (< (abs (- (square guess) x)) (* 0.001 x)))

(define (sqrt-iter3 guess x)
  (if (better-good-enough? guess x)
      guess
      (sqrt-iter3 (improve guess x) x)))

(define (sqrt3 x)
  (sqrt-iter3 1.0 x))
5
(sqrt3 8100000000)
(sqrt3 (/ 1 10000))
      