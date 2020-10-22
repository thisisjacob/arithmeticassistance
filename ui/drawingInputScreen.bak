#lang racket
(require racket/gui/base)
(require math/base)
(require racket/gui)
(require k-infix)
(require "../constants/userInterfaceConstants.rkt")
(require "../drawing/circle.rkt")
(require "../drawing/textProblem.rkt")

; Randomly creates variables and operations for expression

;============================================================;
;Middle-school algorithm
(define a (random-natural 25))
(define b (random-natural 25))
(define c (random-natural 25))
(define d (random-natural 25))

(define op1 (random-natural 4))
(define op2 (random-natural 4))
(define op3 (random-natural 4))
(define o (open-output-string))

(define answer 0)

; Writes out expression in the infix format

(write a o)

(define sym1
  (case (+ op1)
    [(0) (write '+ o)
         (write b o)]
    [(1) (write '- o)
         (write b o)]
    [(2) (write '* o)
         (write b o)]
    [(3) (write '/ o)
         (write b o)]))

(define sym2
  (case (+ op2)
    [(0) (write '+ o)
         (write c o)
         (+ answer c)]
    [(1) (write '- o)
         (write c o)
         (- answer c)]
    [(2) (write '* o)
         (write c o)
         (* answer c)]
    [(3) (write '/ o)
         (write c o)]))

(define sym3
  (case (+ op3)
    [(0) (write '+ o)
         (write d o)
         (+ answer d)]
    [(1) (write '- o)
         (write d o)
         (- answer d)]
    [(2) (write '* o)
         (write d o)
         (* answer d)]
    [(3) (write '/ o)
         (write d o)]))

;(print "Solve the following equation: ")
;(get-output-string o)

; Calculates the answer based on the variables and operations

(define equation
  (case (+ op1)
    [(0) (case (+ op2)
           [(0) (case (+ op3)
                  [(0) (set! answer ($ a + b + c + d))]
                  [(1) (set! answer ($ a + b + c - d))]
                  [(2) (set! answer ($ a + b + c * d))]
                  [(3) (set! answer ($ a + b + (floor (c / d))))])]
           [(1) (case (+ op3)
                  [(0) (set! answer ($ a + b - c + d))]
                  [(1) (set! answer ($ a + b - c - d))]
                  [(2) (set! answer ($ a + b - c * d))]
                  [(3) (set! answer ($ a + b - (floor (c / d))))])]
           [(2) (case (+ op3)
                  [(0) (set! answer ($ a + b * c + d))]
                  [(1) (set! answer ($ a + b * c - d))]
                  [(2) (set! answer ($ a + b * c * d))]
                  [(3) (set! answer ($ a + b * (floor (c / d))))])]
           [(3) (case (+ op3)
                  [(0) (set! answer ($ a + (floor (b / c)) + d))]
                  [(1) (set! answer ($ a + (floor (b / c)) - d))]
                  [(2) (set! answer ($ a + (floor (b / c)) * d))]
                  [(3) (set! answer ($ a + (floor (floor (b / c) / d))))])])]
    [(1) (case (+ op2)
           [(0) (case (+ op3)
                  [(0) (set! answer ($ a - b + c + d))]
                  [(1) (set! answer ($ a - b + c - d))]
                  [(2) (set! answer ($ a - b + c * d))]
                  [(3) (set! answer ($ a - b + (floor (c / d))))])]
           [(1) (case (+ op3)
                  [(0) (set! answer ($ a - b - c + d))]
                  [(1) (set! answer ($ a - b - c - d))]
                  [(2) (set! answer ($ a - b - c * d))]
                  [(3) (set! answer ($ a - b - (floor (c / d))))])]
           [(2) (case (+ op3)
                  [(0) (set! answer ($ a - b * c + d))]
                  [(1) (set! answer ($ a - b * c - d))]
                  [(2) (set! answer ($ a - b * c * d))]
                  [(3) (set! answer ($ a - b * (floor (c / d))))])]
           [(3) (case (+ op3)
                  [(0) (set! answer ($ a - (floor (b / c)) + d))]
                  [(1) (set! answer ($ a - (floor (b / c)) - d))]
                  [(2) (set! answer ($ a - (floor (b / c)) * d))]
                  [(3) (set! answer ($ a - (floor (floor (b / c)) / d)))])])]
    [(2) (case (+ op2)
           [(0) (case (+ op3)
                  [(0) (set! answer ($ a * b + c + d))]
                  [(1) (set! answer ($ a * b + c - d))]
                  [(2) (set! answer ($ a * b + c * d))]
                  [(3) (set! answer ($ a * b + (floor (c / d))))])]
           [(1) (case (+ op3)
                  [(0) (set! answer ($ a * b - c + d))]
                  [(1) (set! answer ($ a * b - c - d))]
                  [(2) (set! answer ($ a * b - c * d))]
                  [(3) (set! answer ($ a * b - (floor (c / d))))])]
           [(2) (case (+ op3)
                  [(0) (set! answer ($ a * b * c + d))]
                  [(1) (set! answer ($ a * b * c - d))]
                  [(2) (set! answer ($ a * b * c * d))]
                  [(3) (set! answer ($ a * b * (floor (c / d))))])]
           [(3) (case (+ op3)
                  [(0) (set! answer ($ a * (floor (b / c)) + d))]
                  [(1) (set! answer ($ a * (floor (b / c)) - d))]
                  [(2) (set! answer ($ a * (floor (b / c)) * d))]
                  [(3) (set! answer ($ a * (floor (floor (b / c)) / d)))])])]
    [(3) (case (+ op2)
           [(0) (case (+ op3)
                  [(0) (set! answer ($ (floor (a / b)) + c + d))]
                  [(1) (set! answer ($ (floor (a / b)) + c - d))]
                  [(2) (set! answer ($ (floor (a / b)) + c * d))]
                  [(3) (set! answer ($ (floor (a / b)) + (floor (c / d))))])]
           [(1) (case (+ op3)
                  [(0) (set! answer ($ (floor (a / b)) - c + d))]
                  [(1) (set! answer ($ (floor (a / b)) - c - d))]
                  [(2) (set! answer ($ (floor (a / b)) - c * d))]
                  [(3) (set! answer ($ (floor (a / b)) - (floor (c / d))))])]
           [(2) (case (+ op3)
                  [(0) (set! answer ($ (floor (a / b)) * c + d))]
                  [(1) (set! answer ($ (floor (a / b)) * c - d))]
                  [(2) (set! answer ($ (floor (a / b)) * c * d))]
                  [(3) (set! answer ($ (floor (a / b)) * (floor (c / d))))])]
           [(3) (case (+ op3)
                  [(0) (set! answer ($ (floor (floor (a / b)) / c) + d))]
                  [(1) (set! answer ($ (floor (floor (a / b)) / c) - d))]
                  [(2) (set! answer ($ (floor (floor (a / b)) / c) * d))]
                  [(3) (set! answer ($ (floor (floor (floor (a / b) / c) / d))))])])]))

; Makes the value of the answer variable a string for later comparisons

(define string_answer
  (set! answer (number->string answer)))

;===============================================================================;



#|
;===============================================================================;
;Elementary-school algorithm
(define a (random-natural 10))
(define b (random-natural 10))

(define op1 (random-natural 4))
(define o (open-output-string))

(define answer 0)

; Writes out expression in the infix format

(write a o)

(define sym1
  (case (+ op1)
    [(0) (write '+ o)
         (write b o)]
    [(1) (write '- o)
         (write b o)]
    [(2) (write '* o)
         (write b o)]
    [(3) (write '/ o)
         (write b o)]))

;(print "Solve the following equation: ")
;(get-output-string o)

; Calculates the answer based on the variables and operations

(define equation
  (case (+ op1)
    [(0) (set! answer ($ a + b))]
    [(1) (set! answer ($ a - b))]
    [(2) (set! answer ($ a * b))]
    [(3) (set! answer (floor ($ a / b)))]))

; Makes the value of the answer variable a string for later comparisons

(define string_answer
  (set! answer (number->string answer)))

;==============================================================================;
|#


;===============================================================================;
;High-school algorithm

#|
(define b (random-natural 10))
(define c (random-natural 10))
(define d (random-natural 10))
(define e (random-natural 10))
(define f (random-natural 10))
(define g (random-natural 10))

(define op2 (random-natural 4))
(define op3 (random-natural 4))
(define op4 (random-natural 4))
(define op5 (random-natural 4))
(define op6 (random-natural 4))

;(define sq1 (random-natural 4))
;(define sq1 (random-natural 4))
;(define sq1 (random-natural 4))
;(define sq1 (random-natural 4))
;(define sq1 (random-natural 4))
;(define sq1 (random-natural 4))
;(define sq1 (random-natural 4))

(define o (open-output-string))

(define answer 0)

; Writes out expression in the infix format

(write b o)

(define sym1
  (case (+ op2)
    [(0) (write '+ o)
         (write c o)]
    [(1) (write '- o)
         (write c o)]
    [(2) (write '* o)
         (write c o)]
    [(3) (write '/ o)
         (write c o)]))

(define sym2
  (case (+ op3)
    [(0) (write '+ o)
         (write d o)]
    [(1) (write '- o)
         (write d o)]
    [(2) (write '* o)
         (write d o)]
    [(3) (write '/ o)
         (write d o)]))

(define sym3
  (case (+ op4)
    [(0) (write '+ o)
         (write e o)]
    [(1) (write '- o)
         (write e o)]
    [(2) (write '* o)
         (write e o)]
    [(3) (write '/ o)
         (write e o)]))

(define sym4
  (case (+ op5)
    [(0) (write '+ o)
         (write f o)]
    [(1) (write '- o)
         (write f o)]
    [(2) (write '* o)
         (write f o)]
    [(3) (write '/ o)
         (write f o)]))

(define sym5
  (case (+ op6)
    [(0) (write '+ o)
         (write g o)]
    [(1) (write '- o)
         (write g o)]
    [(2) (write '* o)
         (write g o)]
    [(3) (write '/ o)
         (write g o)]))


;(print "Solve the following equation: ")
;(get-output-string o)

; Calculates the answer based on the variables and operations

(define equation
  (case (+ op2)
           [(0) (case (+ op3)
                  [(0) (case (+ op4)
                         [(0) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b + c + d + e + f + g))]
                                       [(1) (set! answer ($ b + c + d + e + f - g))]
                                       [(2) (set! answer ($ b + c + d + e + f * g))]
                                       [(3) (set! answer ($ b + c + d + e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b + c + d + e - f + g))]
                                       [(1) (set! answer ($ b + c + d + e - f - g))]
                                       [(2) (set! answer ($ b + c + d + e - f * g))]
                                       [(3) (set! answer ($ b + c + d + e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b + c + d + e * f + g))]
                                       [(1) (set! answer ($ b + c + d + e * f - g))]
                                       [(2) (set! answer ($ b + c + d + e * f * g))]
                                       [(3) (set! answer ($ b + c + d + e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b + c + d + (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b + c + d + (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b + c + d + (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b + c + d + (floor (floor (e / f)) / g)))])])]
                         [(1) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b + c + d - e + f + g))]
                                       [(1) (set! answer ($ b + c + d - e + f - g))]
                                       [(2) (set! answer ($ b + c + d - e + f * g))]
                                       [(3) (set! answer ($ b + c + d - e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b + c + d - e - f + g))]
                                       [(1) (set! answer ($ b + c + d - e - f - g))]
                                       [(2) (set! answer ($ b + c + d - e - f * g))]
                                       [(3) (set! answer ($ b + c + d - e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b + c + d - e * f + g))]
                                       [(1) (set! answer ($ b + c + d - e * f - g))]
                                       [(2) (set! answer ($ b + c + d - e * f * g))]
                                       [(3) (set! answer ($ b + c + d - e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b + c + d - (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b + c + d - (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b + c + d - (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b + c + d - (floor (floor (e / f)) / g)))])])]
                         [(2) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b + c + d * e + f + g))]
                                       [(1) (set! answer ($ b + c + d * e + f - g))]
                                       [(2) (set! answer ($ b + c + d * e + f * g))]
                                       [(3) (set! answer ($ b + c + d * e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b + c + d * e - f + g))]
                                       [(1) (set! answer ($ b + c + d * e - f - g))]
                                       [(2) (set! answer ($ b + c + d * e - f * g))]
                                       [(3) (set! answer ($ b + c + d * e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b + c + d * e * f + g))]
                                       [(1) (set! answer ($ b + c + d * e * f - g))]
                                       [(2) (set! answer ($ b + c + d * e * f * g))]
                                       [(3) (set! answer ($ b + c + d * e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b + c + d * (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b + c + d * (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b + c + d * (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b + c + d * (floor (floor (e / f)) / g)))])])]
                         [(3) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b + c + (floor (d / e)) + f + g))]
                                       [(1) (set! answer ($ b + c + (floor (d / e)) + f - g))]
                                       [(2) (set! answer ($ b + c + (floor (d / e)) + f * g))]
                                       [(3) (set! answer ($ b + c + (floor (d / e)) + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b + c + (floor (d / e)) - f + g))]
                                       [(1) (set! answer ($ b + c + (floor (d / e)) - f - g))]
                                       [(2) (set! answer ($ b + c + (floor (d / e)) - f * g))]
                                       [(3) (set! answer ($ b + c + (floor (d / e)) - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b + c + (floor (d / e)) * f + g))]
                                       [(1) (set! answer ($ b + c + (floor (d / e)) * f - g))]
                                       [(2) (set! answer ($ b + c + (floor (d / e)) * f * g))]
                                       [(3) (set! answer ($ b + c + (floor (d / e)) * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b + c + (floor (floor (d / e)) / f) + g))]
                                       [(1) (set! answer ($ b + c + (floor (floor (d / e)) / f) - g))]
                                       [(2) (set! answer ($ b + c + (floor (floor (d / e)) / f) * g))]
                                       [(3) (set! answer ($ b + c + (floor (floor (floor (d / e)) / f) / g)))])])])]
                  [(1) (case (+ op4)
                         [(0) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b + c - d + e + f + g))]
                                       [(1) (set! answer ($ b + c - d + e + f - g))]
                                       [(2) (set! answer ($ b + c - d + e + f * g))]
                                       [(3) (set! answer ($ b + c - d + e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b + c - d + e - f + g))]
                                       [(1) (set! answer ($ b + c - d + e - f - g))]
                                       [(2) (set! answer ($ b + c - d + e - f * g))]
                                       [(3) (set! answer ($ b + c - d + e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b + c - d + e * f + g))]
                                       [(1) (set! answer ($ b + c - d + e * f - g))]
                                       [(2) (set! answer ($ b + c - d + e * f * g))]
                                       [(3) (set! answer ($ b + c - d + e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b + c - d + (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b + c - d + (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b + c - d + (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b + c - d + (floor (floor (e / f)) / g)))])])]
                         [(1) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b + c - d - e + f + g))]
                                       [(1) (set! answer ($ b + c - d - e + f - g))]
                                       [(2) (set! answer ($ b + c - d - e + f * g))]
                                       [(3) (set! answer ($ b + c - d - e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b + c - d - e - f + g))]
                                       [(1) (set! answer ($ b + c - d - e - f - g))]
                                       [(2) (set! answer ($ b + c - d - e - f * g))]
                                       [(3) (set! answer ($ b + c - d - e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b + c - d - e * f + g))]
                                       [(1) (set! answer ($ b + c - d - e * f - g))]
                                       [(2) (set! answer ($ b + c - d - e * f * g))]
                                       [(3) (set! answer ($ b + c - d - e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b + c - d - (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b + c - d - (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b + c - d - (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b + c - d - (floor (floor (e / f)) / g)))])])]
                         [(2) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b + c - d * e + f + g))]
                                       [(1) (set! answer ($ b + c - d * e + f - g))]
                                       [(2) (set! answer ($ b + c - d * e + f * g))]
                                       [(3) (set! answer ($ b + c - d * e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b + c - d * e - f + g))]
                                       [(1) (set! answer ($ b + c - d * e - f - g))]
                                       [(2) (set! answer ($ b + c - d * e - f * g))]
                                       [(3) (set! answer ($ b + c - d * e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b + c - d * e * f + g))]
                                       [(1) (set! answer ($ b + c - d * e * f - g))]
                                       [(2) (set! answer ($ b + c - d * e * f * g))]
                                       [(3) (set! answer ($ b + c - d * e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b + c - d * (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b + c - d * (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b + c - d * (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b + c - d * (floor (floor (e / f)) / g)))])])]
                         [(3) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b + c - (floor (d / e)) + f + g))]
                                       [(1) (set! answer ($ b + c - (floor (d / e)) + f - g))]
                                       [(2) (set! answer ($ b + c - (floor (d / e)) + f * g))]
                                       [(3) (set! answer ($ b + c - (floor (d / e)) + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b + c - (floor (d / e)) - f + g))]
                                       [(1) (set! answer ($ b + c - (floor (d / e)) - f - g))]
                                       [(2) (set! answer ($ b + c - (floor (d / e)) - f * g))]
                                       [(3) (set! answer ($ b + c - (floor (d / e)) - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b + c - (floor (d / e)) * f + g))]
                                       [(1) (set! answer ($ b + c - (floor (d / e)) * f - g))]
                                       [(2) (set! answer ($ b + c - (floor (d / e)) * f * g))]
                                       [(3) (set! answer ($ b + c - (floor (d / e)) * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b + c - (floor (floor (d / e)) / f) + g))]
                                       [(1) (set! answer ($ b + c - (floor (floor (d / e)) / f) - g))]
                                       [(2) (set! answer ($ b + c - (floor (floor (d / e)) / f) * g))]
                                       [(3) (set! answer ($ b + c - (floor (floor (floor (d / e)) / f) / g)))])])])]
                  [(2) (case (+ op4)
                         [(0) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b + c * d + e + f + g))]
                                       [(1) (set! answer ($ b + c * d + e + f - g))]
                                       [(2) (set! answer ($ b + c * d + e + f * g))]
                                       [(3) (set! answer ($ b + c * d + e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b + c * d + e - f + g))]
                                       [(1) (set! answer ($ b + c * d + e - f - g))]
                                       [(2) (set! answer ($ b + c * d + e - f * g))]
                                       [(3) (set! answer ($ b + c * d + e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b + c * d + e * f + g))]
                                       [(1) (set! answer ($ b + c * d + e * f - g))]
                                       [(2) (set! answer ($ b + c * d + e * f * g))]
                                       [(3) (set! answer ($ b + c * d + e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b + c * d + (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b + c * d + (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b + c * d + (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b + c * d + (floor (floor (e / f)) / g)))])])]
                         [(1) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b + c * d - e + f + g))]
                                       [(1) (set! answer ($ b + c * d - e + f - g))]
                                       [(2) (set! answer ($ b + c * d - e + f * g))]
                                       [(3) (set! answer ($ b + c * d - e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b + c * d - e - f + g))]
                                       [(1) (set! answer ($ b + c * d - e - f - g))]
                                       [(2) (set! answer ($ b + c * d - e - f * g))]
                                       [(3) (set! answer ($ b + c * d - e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b + c * d - e * f + g))]
                                       [(1) (set! answer ($ b + c * d - e * f - g))]
                                       [(2) (set! answer ($ b + c * d - e * f * g))]
                                       [(3) (set! answer ($ b + c * d - e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b + c * d - (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b + c * d - (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b + c * d - (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b + c * d - (floor (floor (e / f)) / g)))])])]
                         [(2) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b + c * d * e + f + g))]
                                       [(1) (set! answer ($ b + c * d * e + f - g))]
                                       [(2) (set! answer ($ b + c * d * e + f * g))]
                                       [(3) (set! answer ($ b + c * d * e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b + c * d * e - f + g))]
                                       [(1) (set! answer ($ b + c * d * e - f - g))]
                                       [(2) (set! answer ($ b + c * d * e - f * g))]
                                       [(3) (set! answer ($ b + c * d * e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b + c * d * e * f + g))]
                                       [(1) (set! answer ($ b + c * d * e * f - g))]
                                       [(2) (set! answer ($ b + c * d * e * f * g))]
                                       [(3) (set! answer ($ b + c * d * e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b + c * d * (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b + c * d * (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b + c * d * (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b + c * d * (floor (floor (e / f)) / g)))])])]
                         [(3) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b + c * (floor (d / e)) + f + g))]
                                       [(1) (set! answer ($ b + c * (floor (d / e)) + f - g))]
                                       [(2) (set! answer ($ b + c * (floor (d / e)) + f * g))]
                                       [(3) (set! answer ($ b + c * (floor (d / e)) + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b + c * (floor (d / e)) - f + g))]
                                       [(1) (set! answer ($ b + c * (floor (d / e)) - f - g))]
                                       [(2) (set! answer ($ b + c * (floor (d / e)) - f * g))]
                                       [(3) (set! answer ($ b + c * (floor (d / e)) - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b + c * (floor (d / e)) * f + g))]
                                       [(1) (set! answer ($ b + c * (floor (d / e)) * f - g))]
                                       [(2) (set! answer ($ b + c * (floor (d / e)) * f * g))]
                                       [(3) (set! answer ($ b + c * (floor (d / e)) * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b + c * (floor (floor (d / e)) / f) + g))]
                                       [(1) (set! answer ($ b + c * (floor (floor (d / e)) / f) - g))]
                                       [(2) (set! answer ($ b + c * (floor (floor (d / e)) / f) * g))]
                                       [(3) (set! answer ($ b + c * (floor (floor (floor (d / e)) / f) / g)))])])])]
                  [(3) (case (+ op4)
                         [(0) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b + (floor (c / d)) + e + f + g))]
                                       [(1) (set! answer ($ b + (floor (c / d)) + e + f - g))]
                                       [(2) (set! answer ($ b + (floor (c / d)) + e + f * g))]
                                       [(3) (set! answer ($ b + (floor (c / d)) + e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b + (floor (c / d)) + e - f + g))]
                                       [(1) (set! answer ($ b + (floor (c / d)) + e - f - g))]
                                       [(2) (set! answer ($ b + (floor (c / d)) + e - f * g))]
                                       [(3) (set! answer ($ b + (floor (c / d)) + e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b + (floor (c / d)) + e * f + g))]
                                       [(1) (set! answer ($ b + (floor (c / d)) + e * f - g))]
                                       [(2) (set! answer ($ b + (floor (c / d)) + e * f * g))]
                                       [(3) (set! answer ($ b + (floor (c / d)) + e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b + (floor (c / d)) + (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b + (floor (c / d)) + (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b + (floor (c / d)) + (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b + (floor (c / d)) + (floor (floor (e / f)) / g)))])])]
                         [(1) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b + (floor (c / d)) - e + f + g))]
                                       [(1) (set! answer ($ b + (floor (c / d)) - e + f - g))]
                                       [(2) (set! answer ($ b + (floor (c / d)) - e + f * g))]
                                       [(3) (set! answer ($ b + (floor (c / d)) - e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b + (floor (c / d)) - e - f + g))]
                                       [(1) (set! answer ($ b + (floor (c / d)) - e - f - g))]
                                       [(2) (set! answer ($ b + (floor (c / d)) - e - f * g))]
                                       [(3) (set! answer ($ b + (floor (c / d)) - e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b + (floor (c / d)) - e * f + g))]
                                       [(1) (set! answer ($ b + (floor (c / d)) - e * f - g))]
                                       [(2) (set! answer ($ b + (floor (c / d)) - e * f * g))]
                                       [(3) (set! answer ($ b + (floor (c / d)) - e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b + (floor (c / d)) - (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b + (floor (c / d)) - (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b + (floor (c / d)) - (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b + (floor (c / d)) - (floor (floor (e / f)) / g)))])])]
                         [(2) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b + (floor (c / d)) * e + f + g))]
                                       [(1) (set! answer ($ b + (floor (c / d)) * e + f - g))]
                                       [(2) (set! answer ($ b + (floor (c / d)) * e + f * g))]
                                       [(3) (set! answer ($ b + (floor (c / d)) * e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b + (floor (c / d)) * e - f + g))]
                                       [(1) (set! answer ($ b + (floor (c / d)) * e - f - g))]
                                       [(2) (set! answer ($ b + (floor (c / d)) * e - f * g))]
                                       [(3) (set! answer ($ b + (floor (c / d)) * e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b + (floor (c / d)) * e * f + g))]
                                       [(1) (set! answer ($ b + (floor (c / d)) * e * f - g))]
                                       [(2) (set! answer ($ b + (floor (c / d)) * e * f * g))]
                                       [(3) (set! answer ($ b + (floor (c / d)) * e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b + (floor (c / d)) * (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b + (floor (c / d)) * (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b + (floor (c / d)) * (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b + (floor (c / d)) * (floor (floor (e / f)) / g)))])])]
                         [(3) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b + (floor (floor (c / d) / e)) + f + g))]
                                       [(1) (set! answer ($ b + (floor (floor (c / d) / e)) + f - g))]
                                       [(2) (set! answer ($ b + (floor (floor (c / d) / e)) + f * g))]
                                       [(3) (set! answer ($ b + (floor (floor (c / d) / e)) + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b + (floor (floor (c / d) / e)) - f + g))]
                                       [(1) (set! answer ($ b + (floor (floor (c / d) / e)) - f - g))]
                                       [(2) (set! answer ($ b + (floor (floor (c / d) / e)) - f * g))]
                                       [(3) (set! answer ($ b + (floor (floor (c / d) / e)) - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b + (floor (floor (c / d) / e)) * f + g))]
                                       [(1) (set! answer ($ b + (floor (floor (c / d) / e)) * f - g))]
                                       [(2) (set! answer ($ b + (floor (floor (c / d) / e)) * f * g))]
                                       [(3) (set! answer ($ b + (floor (floor (c / d) / e)) * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b + (floor (floor (floor (c / d)) / e) / f) + g))]
                                       [(1) (set! answer ($ b + (floor (floor (floor (c / d)) / e) / f) - g))]
                                       [(2) (set! answer ($ b + (floor (floor (floor (c / d)) / e) / f) * g))]
                                       [(3) (set! answer ($ b + (floor (floor (floor (floor (c / d) / e) / f) / g))))])])])])]
           [(1) (case (+ op3)
                  [(0) (case (+ op4)
                         [(0) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b - c + d + e + f + g))]
                                       [(1) (set! answer ($ b - c + d + e + f - g))]
                                       [(2) (set! answer ($ b - c + d + e + f * g))]
                                       [(3) (set! answer ($ b - c + d + e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b - c + d + e - f + g))]
                                       [(1) (set! answer ($ b - c + d + e - f - g))]
                                       [(2) (set! answer ($ b - c + d + e - f * g))]
                                       [(3) (set! answer ($ b - c + d + e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b - c + d + e * f + g))]
                                       [(1) (set! answer ($ b - c + d + e * f - g))]
                                       [(2) (set! answer ($ b - c + d + e * f * g))]
                                       [(3) (set! answer ($ b - c + d + e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b - c + d + (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b - c + d + (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b - c + d + (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b - c + d + (floor (floor (e / f)) / g)))])])]
                         [(1) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b - c + d - e + f + g))]
                                       [(1) (set! answer ($ b - c + d - e + f - g))]
                                       [(2) (set! answer ($ b - c + d - e + f * g))]
                                       [(3) (set! answer ($ b - c + d - e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b - c + d - e - f + g))]
                                       [(1) (set! answer ($ b - c + d - e - f - g))]
                                       [(2) (set! answer ($ b - c + d - e - f * g))]
                                       [(3) (set! answer ($ b - c + d - e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b - c + d - e * f + g))]
                                       [(1) (set! answer ($ b - c + d - e * f - g))]
                                       [(2) (set! answer ($ b - c + d - e * f * g))]
                                       [(3) (set! answer ($ b - c + d - e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b - c + d - (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b - c + d - (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b - c + d - (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b - c + d - (floor (floor (e / f)) / g)))])])]
                         [(2) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b - c + d * e + f + g))]
                                       [(1) (set! answer ($ b - c + d * e + f - g))]
                                       [(2) (set! answer ($ b - c + d * e + f * g))]
                                       [(3) (set! answer ($ b - c + d * e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b - c + d * e - f + g))]
                                       [(1) (set! answer ($ b - c + d * e - f - g))]
                                       [(2) (set! answer ($ b - c + d * e - f * g))]
                                       [(3) (set! answer ($ b - c + d * e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b - c + d * e * f + g))]
                                       [(1) (set! answer ($ b - c + d * e * f - g))]
                                       [(2) (set! answer ($ b - c + d * e * f * g))]
                                       [(3) (set! answer ($ b - c + d * e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b - c + d * (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b - c + d * (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b - c + d * (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b - c + d * (floor (floor (e / f)) / g)))])])]
                         [(3) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b - c + (floor (d / e)) + f + g))]
                                       [(1) (set! answer ($ b - c + (floor (d / e)) + f - g))]
                                       [(2) (set! answer ($ b - c + (floor (d / e)) + f * g))]
                                       [(3) (set! answer ($ b - c + (floor (d / e)) + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b - c + (floor (d / e)) - f + g))]
                                       [(1) (set! answer ($ b - c + (floor (d / e)) - f - g))]
                                       [(2) (set! answer ($ b - c + (floor (d / e)) - f * g))]
                                       [(3) (set! answer ($ b - c + (floor (d / e)) - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b - c + (floor (d / e)) * f + g))]
                                       [(1) (set! answer ($ b - c + (floor (d / e)) * f - g))]
                                       [(2) (set! answer ($ b - c + (floor (d / e)) * f * g))]
                                       [(3) (set! answer ($ b - c + (floor (d / e)) * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b - c + (floor (floor (d / e)) / f) + g))]
                                       [(1) (set! answer ($ b - c + (floor (floor (d / e)) / f) - g))]
                                       [(2) (set! answer ($ b - c + (floor (floor (d / e)) / f) * g))]
                                       [(3) (set! answer ($ b - c + (floor (floor (floor (d / e)) / f) / g)))])])])]
                  [(1) (case (+ op4)
                         [(0) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b - c - d + e + f + g))]
                                       [(1) (set! answer ($ b - c - d + e + f - g))]
                                       [(2) (set! answer ($ b - c - d + e + f * g))]
                                       [(3) (set! answer ($ b - c - d + e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b - c - d + e - f + g))]
                                       [(1) (set! answer ($ b - c - d + e - f - g))]
                                       [(2) (set! answer ($ b - c - d + e - f * g))]
                                       [(3) (set! answer ($ b - c - d + e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b - c - d + e * f + g))]
                                       [(1) (set! answer ($ b - c - d + e * f - g))]
                                       [(2) (set! answer ($ b - c - d + e * f * g))]
                                       [(3) (set! answer ($ b - c - d + e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b - c - d + (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b - c - d + (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b - c - d + (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b - c - d + (floor (floor (e / f)) / g)))])])]
                         [(1) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b - c - d - e + f + g))]
                                       [(1) (set! answer ($ b - c - d - e + f - g))]
                                       [(2) (set! answer ($ b - c - d - e + f * g))]
                                       [(3) (set! answer ($ b - c - d - e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b - c - d - e - f + g))]
                                       [(1) (set! answer ($ b - c - d - e - f - g))]
                                       [(2) (set! answer ($ b - c - d - e - f * g))]
                                       [(3) (set! answer ($ b - c - d - e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b - c - d - e * f + g))]
                                       [(1) (set! answer ($ b - c - d - e * f - g))]
                                       [(2) (set! answer ($ b - c - d - e * f * g))]
                                       [(3) (set! answer ($ b - c - d - e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b - c - d - (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b - c - d - (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b - c - d - (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b - c - d - (floor (floor (e / f)) / g)))])])]
                         [(2) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b - c - d * e + f + g))]
                                       [(1) (set! answer ($ b - c - d * e + f - g))]
                                       [(2) (set! answer ($ b - c - d * e + f * g))]
                                       [(3) (set! answer ($ b - c - d * e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b - c - d * e - f + g))]
                                       [(1) (set! answer ($ b - c - d * e - f - g))]
                                       [(2) (set! answer ($ b - c - d * e - f * g))]
                                       [(3) (set! answer ($ b - c - d * e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b - c - d * e * f + g))]
                                       [(1) (set! answer ($ b - c - d * e * f - g))]
                                       [(2) (set! answer ($ b - c - d * e * f * g))]
                                       [(3) (set! answer ($ b - c - d * e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b - c - d * (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b - c - d * (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b - c - d * (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b - c - d * (floor (floor (e / f)) / g)))])])]
                         [(3) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b - c - (floor (d / e)) + f + g))]
                                       [(1) (set! answer ($ b - c - (floor (d / e)) + f - g))]
                                       [(2) (set! answer ($ b - c - (floor (d / e)) + f * g))]
                                       [(3) (set! answer ($ b - c - (floor (d / e)) + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b - c - (floor (d / e)) - f + g))]
                                       [(1) (set! answer ($ b - c - (floor (d / e)) - f - g))]
                                       [(2) (set! answer ($ b - c - (floor (d / e)) - f * g))]
                                       [(3) (set! answer ($ b - c - (floor (d / e)) - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b - c - (floor (d / e)) * f + g))]
                                       [(1) (set! answer ($ b - c - (floor (d / e)) * f - g))]
                                       [(2) (set! answer ($ b - c - (floor (d / e)) * f * g))]
                                       [(3) (set! answer ($ b - c - (floor (d / e)) * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b - c - (floor (floor (d / e)) / f) + g))]
                                       [(1) (set! answer ($ b - c - (floor (floor (d / e)) / f) - g))]
                                       [(2) (set! answer ($ b - c - (floor (floor (d / e)) / f) * g))]
                                       [(3) (set! answer ($ b - c - (floor (floor (floor (d / e)) / f) / g)))])])])]
                  [(2) (case (+ op4)
                         [(0) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b - c * d + e + f + g))]
                                       [(1) (set! answer ($ b - c * d + e + f - g))]
                                       [(2) (set! answer ($ b - c * d + e + f * g))]
                                       [(3) (set! answer ($ b - c * d + e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b - c * d + e - f + g))]
                                       [(1) (set! answer ($ b - c * d + e - f - g))]
                                       [(2) (set! answer ($ b - c * d + e - f * g))]
                                       [(3) (set! answer ($ b - c * d + e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b - c * d + e * f + g))]
                                       [(1) (set! answer ($ b - c * d + e * f - g))]
                                       [(2) (set! answer ($ b - c * d + e * f * g))]
                                       [(3) (set! answer ($ b - c * d + e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b - c * d + (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b - c * d + (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b - c * d + (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b - c * d + (floor (floor (e / f)) / g)))])])]
                         [(1) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b - c * d - e + f + g))]
                                       [(1) (set! answer ($ b - c * d - e + f - g))]
                                       [(2) (set! answer ($ b - c * d - e + f * g))]
                                       [(3) (set! answer ($ b - c * d - e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b - c * d - e - f + g))]
                                       [(1) (set! answer ($ b - c * d - e - f - g))]
                                       [(2) (set! answer ($ b - c * d - e - f * g))]
                                       [(3) (set! answer ($ b - c * d - e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b - c * d - e * f + g))]
                                       [(1) (set! answer ($ b - c * d - e * f - g))]
                                       [(2) (set! answer ($ b - c * d - e * f * g))]
                                       [(3) (set! answer ($ b - c * d - e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b - c * d - (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b - c * d - (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b - c * d - (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b - c * d - (floor (floor (e / f)) / g)))])])]
                         [(2) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b - c * d * e + f + g))]
                                       [(1) (set! answer ($ b - c * d * e + f - g))]
                                       [(2) (set! answer ($ b - c * d * e + f * g))]
                                       [(3) (set! answer ($ b - c * d * e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b - c * d * e - f + g))]
                                       [(1) (set! answer ($ b - c * d * e - f - g))]
                                       [(2) (set! answer ($ b - c * d * e - f * g))]
                                       [(3) (set! answer ($ b - c * d * e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b - c * d * e * f + g))]
                                       [(1) (set! answer ($ b - c * d * e * f - g))]
                                       [(2) (set! answer ($ b - c * d * e * f * g))]
                                       [(3) (set! answer ($ b - c * d * e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b - c * d * (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b - c * d * (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b - c * d * (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b - c * d * (floor (floor (e / f)) / g)))])])]
                         [(3) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b - c * (floor (d / e)) + f + g))]
                                       [(1) (set! answer ($ b - c * (floor (d / e)) + f - g))]
                                       [(2) (set! answer ($ b - c * (floor (d / e)) + f * g))]
                                       [(3) (set! answer ($ b - c * (floor (d / e)) + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b - c * (floor (d / e)) - f + g))]
                                       [(1) (set! answer ($ b - c * (floor (d / e)) - f - g))]
                                       [(2) (set! answer ($ b - c * (floor (d / e)) - f * g))]
                                       [(3) (set! answer ($ b - c * (floor (d / e)) - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b - c * (floor (d / e)) * f + g))]
                                       [(1) (set! answer ($ b - c * (floor (d / e)) * f - g))]
                                       [(2) (set! answer ($ b - c * (floor (d / e)) * f * g))]
                                       [(3) (set! answer ($ b - c * (floor (d / e)) * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b - c * (floor (floor (d / e)) / f) + g))]
                                       [(1) (set! answer ($ b - c * (floor (floor (d / e)) / f) - g))]
                                       [(2) (set! answer ($ b - c * (floor (floor (d / e)) / f) * g))]
                                       [(3) (set! answer ($ b - c * (floor (floor (floor (d / e)) / f) / g)))])])])]
                  [(3) (case (+ op4)
                         [(0) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b - (floor (c / d)) + e + f + g))]
                                       [(1) (set! answer ($ b - (floor (c / d)) + e + f - g))]
                                       [(2) (set! answer ($ b - (floor (c / d)) + e + f * g))]
                                       [(3) (set! answer ($ b - (floor (c / d)) + e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b - (floor (c / d)) + e - f + g))]
                                       [(1) (set! answer ($ b - (floor (c / d)) + e - f - g))]
                                       [(2) (set! answer ($ b - (floor (c / d)) + e - f * g))]
                                       [(3) (set! answer ($ b - (floor (c / d)) + e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b - (floor (c / d)) + e * f + g))]
                                       [(1) (set! answer ($ b - (floor (c / d)) + e * f - g))]
                                       [(2) (set! answer ($ b - (floor (c / d)) + e * f * g))]
                                       [(3) (set! answer ($ b - (floor (c / d)) + e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b - (floor (c / d)) + (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b - (floor (c / d)) + (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b - (floor (c / d)) + (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b - (floor (c / d)) + (floor (floor (e / f)) / g)))])])]
                         [(1) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b - (floor (c / d)) - e + f + g))]
                                       [(1) (set! answer ($ b - (floor (c / d)) - e + f - g))]
                                       [(2) (set! answer ($ b - (floor (c / d)) - e + f * g))]
                                       [(3) (set! answer ($ b - (floor (c / d)) - e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b - (floor (c / d)) - e - f + g))]
                                       [(1) (set! answer ($ b - (floor (c / d)) - e - f - g))]
                                       [(2) (set! answer ($ b - (floor (c / d)) - e - f * g))]
                                       [(3) (set! answer ($ b - (floor (c / d)) - e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b - (floor (c / d)) - e * f + g))]
                                       [(1) (set! answer ($ b - (floor (c / d)) - e * f - g))]
                                       [(2) (set! answer ($ b - (floor (c / d)) - e * f * g))]
                                       [(3) (set! answer ($ b - (floor (c / d)) - e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b - (floor (c / d)) - (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b - (floor (c / d)) - (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b - (floor (c / d)) - (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b - (floor (c / d)) - (floor (floor (e / f)) / g)))])])]
                         [(2) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b - (floor (c / d)) * e + f + g))]
                                       [(1) (set! answer ($ b - (floor (c / d)) * e + f - g))]
                                       [(2) (set! answer ($ b - (floor (c / d)) * e + f * g))]
                                       [(3) (set! answer ($ b - (floor (c / d)) * e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b - (floor (c / d)) * e - f + g))]
                                       [(1) (set! answer ($ b - (floor (c / d)) * e - f - g))]
                                       [(2) (set! answer ($ b - (floor (c / d)) * e - f * g))]
                                       [(3) (set! answer ($ b - (floor (c / d)) * e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b - (floor (c / d)) * e * f + g))]
                                       [(1) (set! answer ($ b - (floor (c / d)) * e * f - g))]
                                       [(2) (set! answer ($ b - (floor (c / d)) * e * f * g))]
                                       [(3) (set! answer ($ b - (floor (c / d)) * e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b - (floor (c / d)) * (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b - (floor (c / d)) * (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b - (floor (c / d)) * (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b - (floor (c / d)) * (floor (floor (e / f)) / g)))])])]
                         [(3) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b - (floor (floor (c / d) / e)) + f + g))]
                                       [(1) (set! answer ($ b - (floor (floor (c / d) / e)) + f - g))]
                                       [(2) (set! answer ($ b - (floor (floor (c / d) / e)) + f * g))]
                                       [(3) (set! answer ($ b - (floor (floor (c / d) / e)) + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b - (floor (floor (c / d) / e)) - f + g))]
                                       [(1) (set! answer ($ b - (floor (floor (c / d) / e)) - f - g))]
                                       [(2) (set! answer ($ b - (floor (floor (c / d) / e)) - f * g))]
                                       [(3) (set! answer ($ b - (floor (floor (c / d) / e)) - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b - (floor (floor (c / d) / e)) * f + g))]
                                       [(1) (set! answer ($ b - (floor (floor (c / d) / e)) * f - g))]
                                       [(2) (set! answer ($ b - (floor (floor (c / d) / e)) * f * g))]
                                       [(3) (set! answer ($ b - (floor (floor (c / d) / e)) * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b - (floor (floor (floor (c / d)) / e) / f) + g))]
                                       [(1) (set! answer ($ b - (floor (floor (floor (c / d)) / e) / f) - g))]
                                       [(2) (set! answer ($ b - (floor (floor (floor (c / d)) / e) / f) * g))]
                                       [(3) (set! answer ($ b - (floor (floor (floor (floor (c / d) / e) / f) / g))))])])])])]
           [(2) (case (+ op3)
                  [(0) (case (+ op4)
                         [(0) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b * c + d + e + f + g))]
                                       [(1) (set! answer ($ b * c + d + e + f - g))]
                                       [(2) (set! answer ($ b * c + d + e + f * g))]
                                       [(3) (set! answer ($ b * c + d + e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b * c + d + e - f + g))]
                                       [(1) (set! answer ($ b * c + d + e - f - g))]
                                       [(2) (set! answer ($ b * c + d + e - f * g))]
                                       [(3) (set! answer ($ b * c + d + e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b * c + d + e * f + g))]
                                       [(1) (set! answer ($ b * c + d + e * f - g))]
                                       [(2) (set! answer ($ b * c + d + e * f * g))]
                                       [(3) (set! answer ($ b * c + d + e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b * c + d + (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b * c + d + (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b * c + d + (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b * c + d + (floor (floor (e / f)) / g)))])])]
                         [(1) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b * c + d - e + f + g))]
                                       [(1) (set! answer ($ b * c + d - e + f - g))]
                                       [(2) (set! answer ($ b * c + d - e + f * g))]
                                       [(3) (set! answer ($ b * c + d - e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b * c + d - e - f + g))]
                                       [(1) (set! answer ($ b * c + d - e - f - g))]
                                       [(2) (set! answer ($ b * c + d - e - f * g))]
                                       [(3) (set! answer ($ b * c + d - e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b * c + d - e * f + g))]
                                       [(1) (set! answer ($ b * c + d - e * f - g))]
                                       [(2) (set! answer ($ b * c + d - e * f * g))]
                                       [(3) (set! answer ($ b * c + d - e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b * c + d - (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b * c + d - (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b * c + d - (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b * c + d - (floor (floor (e / f)) / g)))])])]
                         [(2) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b * c + d * e + f + g))]
                                       [(1) (set! answer ($ b * c + d * e + f - g))]
                                       [(2) (set! answer ($ b * c + d * e + f * g))]
                                       [(3) (set! answer ($ b * c + d * e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b * c + d * e - f + g))]
                                       [(1) (set! answer ($ b * c + d * e - f - g))]
                                       [(2) (set! answer ($ b * c + d * e - f * g))]
                                       [(3) (set! answer ($ b * c + d * e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b * c + d * e * f + g))]
                                       [(1) (set! answer ($ b * c + d * e * f - g))]
                                       [(2) (set! answer ($ b * c + d * e * f * g))]
                                       [(3) (set! answer ($ b * c + d * e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b * c + d * (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b * c + d * (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b * c + d * (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b * c + d * (floor (floor (e / f)) / g)))])])]
                         [(3) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b * c + (floor (d / e)) + f + g))]
                                       [(1) (set! answer ($ b * c + (floor (d / e)) + f - g))]
                                       [(2) (set! answer ($ b * c + (floor (d / e)) + f * g))]
                                       [(3) (set! answer ($ b * c + (floor (d / e)) + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b * c + (floor (d / e)) - f + g))]
                                       [(1) (set! answer ($ b * c + (floor (d / e)) - f - g))]
                                       [(2) (set! answer ($ b * c + (floor (d / e)) - f * g))]
                                       [(3) (set! answer ($ b * c + (floor (d / e)) - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b * c + (floor (d / e)) * f + g))]
                                       [(1) (set! answer ($ b * c + (floor (d / e)) * f - g))]
                                       [(2) (set! answer ($ b * c + (floor (d / e)) * f * g))]
                                       [(3) (set! answer ($ b * c + (floor (d / e)) * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b * c + (floor (floor (d / e)) / f) + g))]
                                       [(1) (set! answer ($ b * c + (floor (floor (d / e)) / f) - g))]
                                       [(2) (set! answer ($ b * c + (floor (floor (d / e)) / f) * g))]
                                       [(3) (set! answer ($ b * c + (floor (floor (floor (d / e)) / f) / g)))])])])]
                  [(1) (case (+ op4)
                         [(0) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b * c - d + e + f + g))]
                                       [(1) (set! answer ($ b * c - d + e + f - g))]
                                       [(2) (set! answer ($ b * c - d + e + f * g))]
                                       [(3) (set! answer ($ b * c - d + e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b * c - d + e - f + g))]
                                       [(1) (set! answer ($ b * c - d + e - f - g))]
                                       [(2) (set! answer ($ b * c - d + e - f * g))]
                                       [(3) (set! answer ($ b * c - d + e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b * c - d + e * f + g))]
                                       [(1) (set! answer ($ b * c - d + e * f - g))]
                                       [(2) (set! answer ($ b * c - d + e * f * g))]
                                       [(3) (set! answer ($ b * c - d + e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b * c - d + (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b * c - d + (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b * c - d + (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b * c - d + (floor (floor (e / f)) / g)))])])]
                         [(1) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b * c - d - e + f + g))]
                                       [(1) (set! answer ($ b * c - d - e + f - g))]
                                       [(2) (set! answer ($ b * c - d - e + f * g))]
                                       [(3) (set! answer ($ b * c - d - e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b * c - d - e - f + g))]
                                       [(1) (set! answer ($ b * c - d - e - f - g))]
                                       [(2) (set! answer ($ b * c - d - e - f * g))]
                                       [(3) (set! answer ($ b * c - d - e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b * c - d - e * f + g))]
                                       [(1) (set! answer ($ b * c - d - e * f - g))]
                                       [(2) (set! answer ($ b * c - d - e * f * g))]
                                       [(3) (set! answer ($ b * c - d - e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b * c - d - (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b * c - d - (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b * c - d - (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b * c - d - (floor (floor (e / f)) / g)))])])]
                         [(2) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b * c - d * e + f + g))]
                                       [(1) (set! answer ($ b * c - d * e + f - g))]
                                       [(2) (set! answer ($ b * c - d * e + f * g))]
                                       [(3) (set! answer ($ b * c - d * e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b * c - d * e - f + g))]
                                       [(1) (set! answer ($ b * c - d * e - f - g))]
                                       [(2) (set! answer ($ b * c - d * e - f * g))]
                                       [(3) (set! answer ($ b * c - d * e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b * c - d * e * f + g))]
                                       [(1) (set! answer ($ b * c - d * e * f - g))]
                                       [(2) (set! answer ($ b * c - d * e * f * g))]
                                       [(3) (set! answer ($ b * c - d * e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b * c - d * (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b * c - d * (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b * c - d * (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b * c - d * (floor (floor (e / f)) / g)))])])]
                         [(3) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b * c - (floor (d / e)) + f + g))]
                                       [(1) (set! answer ($ b * c - (floor (d / e)) + f - g))]
                                       [(2) (set! answer ($ b * c - (floor (d / e)) + f * g))]
                                       [(3) (set! answer ($ b * c - (floor (d / e)) + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b * c - (floor (d / e)) - f + g))]
                                       [(1) (set! answer ($ b * c - (floor (d / e)) - f - g))]
                                       [(2) (set! answer ($ b * c - (floor (d / e)) - f * g))]
                                       [(3) (set! answer ($ b * c - (floor (d / e)) - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b * c - (floor (d / e)) * f + g))]
                                       [(1) (set! answer ($ b * c - (floor (d / e)) * f - g))]
                                       [(2) (set! answer ($ b * c - (floor (d / e)) * f * g))]
                                       [(3) (set! answer ($ b * c - (floor (d / e)) * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b * c - (floor (floor (d / e)) / f) + g))]
                                       [(1) (set! answer ($ b * c - (floor (floor (d / e)) / f) - g))]
                                       [(2) (set! answer ($ b * c - (floor (floor (d / e)) / f) * g))]
                                       [(3) (set! answer ($ b * c - (floor (floor (floor (d / e)) / f) / g)))])])])]
                  [(2) (case (+ op4)
                         [(0) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b * c * d + e + f + g))]
                                       [(1) (set! answer ($ b * c * d + e + f - g))]
                                       [(2) (set! answer ($ b * c * d + e + f * g))]
                                       [(3) (set! answer ($ b * c * d + e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b * c * d + e - f + g))]
                                       [(1) (set! answer ($ b * c * d + e - f - g))]
                                       [(2) (set! answer ($ b * c * d + e - f * g))]
                                       [(3) (set! answer ($ b * c * d + e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b * c * d + e * f + g))]
                                       [(1) (set! answer ($ b * c * d + e * f - g))]
                                       [(2) (set! answer ($ b * c * d + e * f * g))]
                                       [(3) (set! answer ($ b * c * d + e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b * c * d + (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b * c * d + (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b * c * d + (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b * c * d + (floor (floor (e / f)) / g)))])])]
                         [(1) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b * c * d - e + f + g))]
                                       [(1) (set! answer ($ b * c * d - e + f - g))]
                                       [(2) (set! answer ($ b * c * d - e + f * g))]
                                       [(3) (set! answer ($ b * c * d - e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b * c * d - e - f + g))]
                                       [(1) (set! answer ($ b * c * d - e - f - g))]
                                       [(2) (set! answer ($ b * c * d - e - f * g))]
                                       [(3) (set! answer ($ b * c * d - e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b * c * d - e * f + g))]
                                       [(1) (set! answer ($ b * c * d - e * f - g))]
                                       [(2) (set! answer ($ b * c * d - e * f * g))]
                                       [(3) (set! answer ($ b * c * d - e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b * c * d - (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b * c * d - (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b * c * d - (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b * c * d - (floor (floor (e / f)) / g)))])])]
                         [(2) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b * c * d * e + f + g))]
                                       [(1) (set! answer ($ b * c * d * e + f - g))]
                                       [(2) (set! answer ($ b * c * d * e + f * g))]
                                       [(3) (set! answer ($ b * c * d * e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b * c * d * e - f + g))]
                                       [(1) (set! answer ($ b * c * d * e - f - g))]
                                       [(2) (set! answer ($ b * c * d * e - f * g))]
                                       [(3) (set! answer ($ b * c * d * e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b * c * d * e * f + g))]
                                       [(1) (set! answer ($ b * c * d * e * f - g))]
                                       [(2) (set! answer ($ b * c * d * e * f * g))]
                                       [(3) (set! answer ($ b * c * d * e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b * c * d * (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b * c * d * (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b * c * d * (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b * c * d * (floor (floor (e / f)) / g)))])])]
                         [(3) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b * c * (floor (d / e)) + f + g))]
                                       [(1) (set! answer ($ b * c * (floor (d / e)) + f - g))]
                                       [(2) (set! answer ($ b * c * (floor (d / e)) + f * g))]
                                       [(3) (set! answer ($ b * c * (floor (d / e)) + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b * c * (floor (d / e)) - f + g))]
                                       [(1) (set! answer ($ b * c * (floor (d / e)) - f - g))]
                                       [(2) (set! answer ($ b * c * (floor (d / e)) - f * g))]
                                       [(3) (set! answer ($ b * c * (floor (d / e)) - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b * c * (floor (d / e)) * f + g))]
                                       [(1) (set! answer ($ b * c * (floor (d / e)) * f - g))]
                                       [(2) (set! answer ($ b * c * (floor (d / e)) * f * g))]
                                       [(3) (set! answer ($ b * c * (floor (d / e)) * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b * c * (floor (floor (d / e)) / f) + g))]
                                       [(1) (set! answer ($ b * c * (floor (floor (d / e)) / f) - g))]
                                       [(2) (set! answer ($ b * c * (floor (floor (d / e)) / f) * g))]
                                       [(3) (set! answer ($ b * c * (floor (floor (floor (d / e)) / f) / g)))])])])]
                  [(3) (case (+ op4)
                         [(0) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b * (floor (c / d)) + e + f + g))]
                                       [(1) (set! answer ($ b * (floor (c / d)) + e + f - g))]
                                       [(2) (set! answer ($ b * (floor (c / d)) + e + f * g))]
                                       [(3) (set! answer ($ b * (floor (c / d)) + e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b * (floor (c / d)) + e - f + g))]
                                       [(1) (set! answer ($ b * (floor (c / d)) + e - f - g))]
                                       [(2) (set! answer ($ b * (floor (c / d)) + e - f * g))]
                                       [(3) (set! answer ($ b * (floor (c / d)) + e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b * (floor (c / d)) + e * f + g))]
                                       [(1) (set! answer ($ b * (floor (c / d)) + e * f - g))]
                                       [(2) (set! answer ($ b * (floor (c / d)) + e * f * g))]
                                       [(3) (set! answer ($ b * (floor (c / d)) + e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b * (floor (c / d)) + (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b * (floor (c / d)) + (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b * (floor (c / d)) + (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b * (floor (c / d)) + (floor (floor (e / f)) / g)))])])]
                         [(1) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b * (floor (c / d)) - e + f + g))]
                                       [(1) (set! answer ($ b * (floor (c / d)) - e + f - g))]
                                       [(2) (set! answer ($ b * (floor (c / d)) - e + f * g))]
                                       [(3) (set! answer ($ b * (floor (c / d)) - e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b * (floor (c / d)) - e - f + g))]
                                       [(1) (set! answer ($ b * (floor (c / d)) - e - f - g))]
                                       [(2) (set! answer ($ b * (floor (c / d)) - e - f * g))]
                                       [(3) (set! answer ($ b * (floor (c / d)) - e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b * (floor (c / d)) - e * f + g))]
                                       [(1) (set! answer ($ b * (floor (c / d)) - e * f - g))]
                                       [(2) (set! answer ($ b * (floor (c / d)) - e * f * g))]
                                       [(3) (set! answer ($ b * (floor (c / d)) - e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b * (floor (c / d)) - (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b * (floor (c / d)) - (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b * (floor (c / d)) - (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b * (floor (c / d)) - (floor (floor (e / f)) / g)))])])]
                         [(2) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b * (floor (c / d)) * e + f + g))]
                                       [(1) (set! answer ($ b * (floor (c / d)) * e + f - g))]
                                       [(2) (set! answer ($ b * (floor (c / d)) * e + f * g))]
                                       [(3) (set! answer ($ b * (floor (c / d)) * e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b * (floor (c / d)) * e - f + g))]
                                       [(1) (set! answer ($ b * (floor (c / d)) * e - f - g))]
                                       [(2) (set! answer ($ b * (floor (c / d)) * e - f * g))]
                                       [(3) (set! answer ($ b * (floor (c / d)) * e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b * (floor (c / d)) * e * f + g))]
                                       [(1) (set! answer ($ b * (floor (c / d)) * e * f - g))]
                                       [(2) (set! answer ($ b * (floor (c / d)) * e * f * g))]
                                       [(3) (set! answer ($ b * (floor (c / d)) * e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b * (floor (c / d)) * (floor (e / f)) + g))]
                                       [(1) (set! answer ($ b * (floor (c / d)) * (floor (e / f)) - g))]
                                       [(2) (set! answer ($ b * (floor (c / d)) * (floor (e / f)) * g))]
                                       [(3) (set! answer ($ b * (floor (c / d)) * (floor (floor (e / f)) / g)))])])]
                         [(3) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ b * (floor (floor (c / d) / e)) + f + g))]
                                       [(1) (set! answer ($ b * (floor (floor (c / d) / e)) + f - g))]
                                       [(2) (set! answer ($ b * (floor (floor (c / d) / e)) + f * g))]
                                       [(3) (set! answer ($ b * (floor (floor (c / d) / e)) + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ b * (floor (floor (c / d) / e)) - f + g))]
                                       [(1) (set! answer ($ b * (floor (floor (c / d) / e)) - f - g))]
                                       [(2) (set! answer ($ b * (floor (floor (c / d) / e)) - f * g))]
                                       [(3) (set! answer ($ b * (floor (floor (c / d) / e)) - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ b * (floor (floor (c / d) / e)) * f + g))]
                                       [(1) (set! answer ($ b * (floor (floor (c / d) / e)) * f - g))]
                                       [(2) (set! answer ($ b * (floor (floor (c / d) / e)) * f * g))]
                                       [(3) (set! answer ($ b * (floor (floor (c / d) / e)) * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ b * (floor (floor (floor (c / d)) / e) / f) + g))]
                                       [(1) (set! answer ($ b * (floor (floor (floor (c / d)) / e) / f) - g))]
                                       [(2) (set! answer ($ b * (floor (floor (floor (c / d)) / e) / f) * g))]
                                       [(3) (set! answer ($ b * (floor (floor (floor (floor (c / d) / e) / f) / g))))])])])])]
           [(3) (case (+ op3)
                  [(0) (case (+ op4)
                         [(0) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) + d + e + f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) + d + e + f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) + d + e + f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) + d + e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) + d + e - f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) + d + e - f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) + d + e - f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) + d + e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) + d + e * f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) + d + e * f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) + d + e * f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) + d + e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) + d + (floor (e / f)) + g))]
                                       [(1) (set! answer ($ (floor (b / c)) + d + (floor (e / f)) - g))]
                                       [(2) (set! answer ($ (floor (b / c)) + d + (floor (e / f)) * g))]
                                       [(3) (set! answer ($ (floor (b / c)) + d + (floor (floor (e / f)) / g)))])])]
                         [(1) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) + d - e + f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) + d - e + f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) + d - e + f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) + d - e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) + d - e - f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) + d - e - f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) + d - e - f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) + d - e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) + d - e * f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) + d - e * f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) + d - e * f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) + d - e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) + d - (floor (e / f)) + g))]
                                       [(1) (set! answer ($ (floor (b / c)) + d - (floor (e / f)) - g))]
                                       [(2) (set! answer ($ (floor (b / c)) + d - (floor (e / f)) * g))]
                                       [(3) (set! answer ($ (floor (b / c)) + d - (floor (floor (e / f)) / g)))])])]
                         [(2) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) + d * e + f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) + d * e + f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) + d * e + f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) + d * e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) + d * e - f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) + d * e - f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) + d * e - f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) + d * e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) + d * e * f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) + d * e * f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) + d * e * f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) + d * e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) + d * (floor (e / f)) + g))]
                                       [(1) (set! answer ($ (floor (b / c)) + d * (floor (e / f)) - g))]
                                       [(2) (set! answer ($ (floor (b / c)) + d * (floor (e / f)) * g))]
                                       [(3) (set! answer ($ (floor (b / c)) + d * (floor (floor (e / f)) / g)))])])]
                         [(3) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) + (floor (d / e)) + f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) + (floor (d / e)) + f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) + (floor (d / e)) + f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) + (floor (d / e)) + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) + (floor (d / e)) - f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) + (floor (d / e)) - f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) + (floor (d / e)) - f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) + (floor (d / e)) - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) + (floor (d / e)) * f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) + (floor (d / e)) * f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) + (floor (d / e)) * f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) + (floor (d / e)) * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) + (floor (floor (d / e)) / f) + g))]
                                       [(1) (set! answer ($ (floor (b / c)) + (floor (floor (d / e)) / f) - g))]
                                       [(2) (set! answer ($ (floor (b / c)) + (floor (floor (d / e)) / f) * g))]
                                       [(3) (set! answer ($ (floor (b / c)) + (floor (floor (floor (d / e)) / f) / g)))])])])]
                  [(1) (case (+ op4)
                         [(0) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) - d + e + f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) - d + e + f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) - d + e + f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) - d + e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) - d + e - f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) - d + e - f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) - d + e - f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) - d + e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) - d + e * f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) - d + e * f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) - d + e * f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) - d + e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) - d + (floor (e / f)) + g))]
                                       [(1) (set! answer ($ (floor (b / c)) - d + (floor (e / f)) - g))]
                                       [(2) (set! answer ($ (floor (b / c)) - d + (floor (e / f)) * g))]
                                       [(3) (set! answer ($ (floor (b / c)) - d + (floor (floor (e / f)) / g)))])])]
                         [(1) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) - d - e + f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) - d - e + f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) - d - e + f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) - d - e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) - d - e - f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) - d - e - f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) - d - e - f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) - d - e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) - d - e * f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) - d - e * f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) - d - e * f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) - d - e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) - d - (floor (e / f)) + g))]
                                       [(1) (set! answer ($ (floor (b / c)) - d - (floor (e / f)) - g))]
                                       [(2) (set! answer ($ (floor (b / c)) - d - (floor (e / f)) * g))]
                                       [(3) (set! answer ($ (floor (b / c)) - d - (floor (floor (e / f)) / g)))])])]
                         [(2) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) - d * e + f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) - d * e + f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) - d * e + f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) - d * e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) - d * e - f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) - d * e - f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) - d * e - f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) - d * e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) - d * e * f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) - d * e * f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) - d * e * f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) - d * e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) - d * (floor (e / f)) + g))]
                                       [(1) (set! answer ($ (floor (b / c)) - d * (floor (e / f)) - g))]
                                       [(2) (set! answer ($ (floor (b / c)) - d * (floor (e / f)) * g))]
                                       [(3) (set! answer ($ (floor (b / c)) - d * (floor (floor (e / f)) / g)))])])]
                         [(3) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) - (floor (d / e)) + f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) - (floor (d / e)) + f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) - (floor (d / e)) + f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) - (floor (d / e)) + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) - (floor (d / e)) - f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) - (floor (d / e)) - f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) - (floor (d / e)) - f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) - (floor (d / e)) - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) - (floor (d / e)) * f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) - (floor (d / e)) * f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) - (floor (d / e)) * f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) - (floor (d / e)) * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) - (floor (floor (d / e)) / f) + g))]
                                       [(1) (set! answer ($ (floor (b / c)) - (floor (floor (d / e)) / f) - g))]
                                       [(2) (set! answer ($ (floor (b / c)) - (floor (floor (d / e)) / f) * g))]
                                       [(3) (set! answer ($ (floor (b / c)) - (floor (floor (floor (d / e)) / f) / g)))])])])]
                  [(2) (case (+ op4)
                         [(0) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) * d + e + f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) * d + e + f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) * d + e + f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) * d + e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) * d + e - f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) * d + e - f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) * d + e - f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) * d + e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) * d + e * f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) * d + e * f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) * d + e * f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) * d + e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) * d + (floor (e / f)) + g))]
                                       [(1) (set! answer ($ (floor (b / c)) * d + (floor (e / f)) - g))]
                                       [(2) (set! answer ($ (floor (b / c)) * d + (floor (e / f)) * g))]
                                       [(3) (set! answer ($ (floor (b / c)) * d + (floor (floor (e / f)) / g)))])])]
                         [(1) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) * d - e + f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) * d - e + f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) * d - e + f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) * d - e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) * d - e - f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) * d - e - f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) * d - e - f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) * d - e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) * d - e * f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) * d - e * f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) * d - e * f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) * d - e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) * d - (floor (e / f)) + g))]
                                       [(1) (set! answer ($ (floor (b / c)) * d - (floor (e / f)) - g))]
                                       [(2) (set! answer ($ (floor (b / c)) * d - (floor (e / f)) * g))]
                                       [(3) (set! answer ($ (floor (b / c)) * d - (floor (floor (e / f)) / g)))])])]
                         [(2) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) * d * e + f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) * d * e + f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) * d * e + f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) * d * e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) * d * e - f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) * d * e - f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) * d * e - f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) * d * e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) * d * e * f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) * d * e * f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) * d * e * f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) * d * e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) * d * (floor (e / f)) + g))]
                                       [(1) (set! answer ($ (floor (b / c)) * d * (floor (e / f)) - g))]
                                       [(2) (set! answer ($ (floor (b / c)) * d * (floor (e / f)) * g))]
                                       [(3) (set! answer ($ (floor (b / c)) * d * (floor (floor (e / f)) / g)))])])]
                         [(3) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) * (floor (d / e)) + f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) * (floor (d / e)) + f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) * (floor (d / e)) + f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) * (floor (d / e)) + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) * (floor (d / e)) - f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) * (floor (d / e)) - f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) * (floor (d / e)) - f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) * (floor (d / e)) - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) * (floor (d / e)) * f + g))]
                                       [(1) (set! answer ($ (floor (b / c)) * (floor (d / e)) * f - g))]
                                       [(2) (set! answer ($ (floor (b / c)) * (floor (d / e)) * f * g))]
                                       [(3) (set! answer ($ (floor (b / c)) * (floor (d / e)) * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ (floor (b / c)) * (floor (floor (d / e)) / f) + g))]
                                       [(1) (set! answer ($ (floor (b / c)) * (floor (floor (d / e)) / f) - g))]
                                       [(2) (set! answer ($ (floor (b / c)) * (floor (floor (d / e)) / f) * g))]
                                       [(3) (set! answer ($ (floor (b / c)) * (floor (floor (floor (d / e)) / f) / g)))])])])]
                  [(3) (case (+ op4)
                         [(0) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ (floor (floor (b / c) / d)) + e + f + g))]
                                       [(1) (set! answer ($ (floor (floor (b / c) / d)) + e + f - g))]
                                       [(2) (set! answer ($ (floor (floor (b / c) / d)) + e + f * g))]
                                       [(3) (set! answer ($ (floor (floor (b / c) / d)) + e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ (floor (floor (b / c) / d)) + e - f + g))]
                                       [(1) (set! answer ($ (floor (floor (b / c) / d)) + e - f - g))]
                                       [(2) (set! answer ($ (floor (floor (b / c) / d)) + e - f * g))]
                                       [(3) (set! answer ($ (floor (floor (b / c) / d)) + e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ (floor (floor (b / c) / d)) + e * f + g))]
                                       [(1) (set! answer ($ (floor (floor (b / c) / d)) + e * f - g))]
                                       [(2) (set! answer ($ (floor (floor (b / c) / d)) + e * f * g))]
                                       [(3) (set! answer ($ (floor (floor (b / c) / d)) + e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ (floor (floor (b / c) / d)) + (floor (e / f)) + g))]
                                       [(1) (set! answer ($ (floor (floor (b / c) / d)) + (floor (e / f)) - g))]
                                       [(2) (set! answer ($ (floor (floor (b / c) / d)) + (floor (e / f)) * g))]
                                       [(3) (set! answer ($ (floor (floor (b / c) / d)) + (floor (floor (e / f)) / g)))])])]
                         [(1) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ (floor (floor (b / c) / d)) - e + f + g))]
                                       [(1) (set! answer ($ (floor (floor (b / c) / d)) - e + f - g))]
                                       [(2) (set! answer ($ (floor (floor (b / c) / d)) - e + f * g))]
                                       [(3) (set! answer ($ (floor (floor (b / c) / d)) - e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ (floor (floor (b / c) / d)) - e - f + g))]
                                       [(1) (set! answer ($ (floor (floor (b / c) / d)) - e - f - g))]
                                       [(2) (set! answer ($ (floor (floor (b / c) / d)) - e - f * g))]
                                       [(3) (set! answer ($ (floor (floor (b / c) / d)) - e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ (floor (floor (b / c) / d)) - e * f + g))]
                                       [(1) (set! answer ($ (floor (floor (b / c) / d)) - e * f - g))]
                                       [(2) (set! answer ($ (floor (floor (b / c) / d)) - e * f * g))]
                                       [(3) (set! answer ($ (floor (floor (b / c) / d)) - e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ (floor (floor (b / c) / d)) - (floor (e / f)) + g))]
                                       [(1) (set! answer ($ (floor (floor (b / c) / d)) - (floor (e / f)) - g))]
                                       [(2) (set! answer ($ (floor (floor (b / c) / d)) - (floor (e / f)) * g))]
                                       [(3) (set! answer ($ (floor (floor (b / c) / d)) - (floor (floor (e / f)) / g)))])])]
                         [(2) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ (floor (floor (b / c) / d)) * e + f + g))]
                                       [(1) (set! answer ($ (floor (floor (b / c) / d)) * e + f - g))]
                                       [(2) (set! answer ($ (floor (floor (b / c) / d)) * e + f * g))]
                                       [(3) (set! answer ($ (floor (floor (b / c) / d)) * e + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ (floor (floor (b / c) / d)) * e - f + g))]
                                       [(1) (set! answer ($ (floor (floor (b / c) / d)) * e - f - g))]
                                       [(2) (set! answer ($ (floor (floor (b / c) / d)) * e - f * g))]
                                       [(3) (set! answer ($ (floor (floor (b / c) / d)) * e - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ (floor (floor (b / c) / d)) * e * f + g))]
                                       [(1) (set! answer ($ (floor (floor (b / c) / d)) * e * f - g))]
                                       [(2) (set! answer ($ (floor (floor (b / c) / d)) * e * f * g))]
                                       [(3) (set! answer ($ (floor (floor (b / c) / d)) * e * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ (floor (floor (b / c) / d)) * (floor (e / f)) + g))]
                                       [(1) (set! answer ($ (floor (floor (b / c) / d)) * (floor (e / f)) - g))]
                                       [(2) (set! answer ($ (floor (floor (b / c) / d)) * (floor (e / f)) * g))]
                                       [(3) (set! answer ($ (floor (floor (b / c) / d)) * (floor (floor (e / f)) / g)))])])]
                         [(3) (case (+ op5)
                                [(0) (case (+ op6)
                                       [(0) (set! answer ($ (floor (floor (floor (b / c) / d) / e)) + f + g))]
                                       [(1) (set! answer ($ (floor (floor (floor (b / c) / d) / e)) + f - g))]
                                       [(2) (set! answer ($ (floor (floor (floor (b / c) / d) / e)) + f * g))]
                                       [(3) (set! answer ($ (floor (floor (floor (b / c) / d) / e)) + (floor (f / g))))])]
                                [(1) (case (+ op6)
                                       [(0) (set! answer ($ (floor (floor (floor (b / c) / d) / e)) - f + g))]
                                       [(1) (set! answer ($ (floor (floor (floor (b / c) / d) / e)) - f - g))]
                                       [(2) (set! answer ($ (floor (floor (floor (b / c) / d) / e)) - f * g))]
                                       [(3) (set! answer ($ (floor (floor (floor (b / c) / d) / e)) - (floor (f / g))))])]
                                [(2) (case (+ op6)
                                       [(0) (set! answer ($ (floor (floor (floor (b / c) / d) / e)) * f + g))]
                                       [(1) (set! answer ($ (floor (floor (floor (b / c) / d) / e)) * f - g))]
                                       [(2) (set! answer ($ (floor (floor (floor (b / c) / d) / e)) * f * g))]
                                       [(3) (set! answer ($ (floor (floor (floor (b / c) / d) / e)) * (floor (f / g))))])]
                                [(3) (case (+ op6)
                                       [(0) (set! answer ($ (floor (floor (floor (floor (b / c) / d) / e) / f)) + g))]
                                       [(1) (set! answer ($ (floor (floor (floor (floor (b / c) / d) / e) / f)) - g))]
                                       [(2) (set! answer ($ (floor (floor (floor (floor (b / c) / d) / e) / f)) * g))]
                                       [(3) (set! answer ($ (floor (floor (floor (floor (floor (b / c) / d) / e) / f) / g))))])])])])]))
; Makes the value of the answer variable a string for later comparisons

(define string_answer
  (set! answer (number->string answer)))
|#
;==============================================================================;




; GUI Class:

; The screen for drawing problems and entering responses
; Initialization Arguments:
; givenParent : the container element of this instance
; menuReturnFunction: a function for returning from this page and to a given return page
; mode: the mode class construct (as in free practice vs linked practice) NOTE: currently under construction, this will be required in the future
; category: the category of problems calss construct NOTE: currently under construction, this will be required in the future
; Public Functions:
; enable : enables visibility of this object instance
; disable: disables visibility of this object instance

(define drawInputMenuUI%
  (class object%
    (init-field
     givenParent
     menuReturnFunction
     )
    (super-new)

    (define currentGameMode 0)
    (define currentProblemCategory 0)

    ; A callback function for rendering problems to the canvas
    ; Currently needs: information provided that will tell the program which
    ; shape to draw
    (define (canvasPaintingCallbackFunction canvas dc)
      (send dc set-scale 3 3)
      (send dc set-text-foreground "blue")
      (draw-text-problem dc (get-output-string o) 0 0)
      )

     ; Callback definitions
    (define (submit-callback b e)
      (let ((text (send textEnter get-value)))
        (if (string=? text answer)
            (message-box "Good job" (format "That is correct!") givenParent '(no-icon ok))
            (message-box "Go to the gazebo" (format "That is incorrect.") givenParent '(stop ok)))))
    (define (return-callback button event)
      (menuReturnFunction)
      )
    
    (define drawingInputMenu (new frame%
                                  [label "Problem Screen"]
                                  [width frameWidthAndHeight]
                                  [height frameWidthAndHeight]
                                  [style frameStyle]
                                  )
      )
    (define drawingCanvas (new canvas%
                               [parent drawingInputMenu]
                               [style containerStyle]
                               [min-width 600]
                               [min-height 400]
                               [paint-callback canvasPaintingCallbackFunction]
                               )
      )
    
    (define inputPanel (new horizontal-panel%
                            [parent drawingInputMenu]
                            [style containerStyle]
                            [min-width 600]
                            [min-height 200]))


    (define returnButton (new button%
                              [parent inputPanel]
                              [label "Return to Main Menu"]
                              [callback return-callback]
                              )
      )
    
    (define textEnter (new text-field%
                           [parent inputPanel]
                           [label "Enter Answer:"]))


    
    (define submit (new button%
                        [parent inputPanel]
                        [label "Submit"]
                        [callback submit-callback]))

    (define/public (enable)
      (send drawingInputMenu show #t)
      )
    (define/public (pass-settings game-mode problem-category)
      (set! currentGameMode game-mode)
      (set! currentProblemCategory problem-category)
      )
    (define/public (disable)
      (send drawingInputMenu show #f))

    )
 )

(provide drawInputMenuUI%)
