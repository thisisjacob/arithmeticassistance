#lang racket
(require racket/gui/base)
(require math/base)
(require racket/gui)
(require k-infix)
(require "../constants/userInterfaceConstants.rkt")
(require "../constants/gameModes.rkt")
(require "../drawing/canvasShapeDrawingFunctions.rkt")
(require "../logic/equationGenerator.rkt")

; Randomly creates variables and operations for expression

#|
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
|#


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

    (define test (new equation-generator%)
      )

    ; Internal constants
    (define playerOne "Player 1")
    (define playerTwo "Player 2")

    ; Gameplay state
    ; currentGameMode holds the current gameMode construct of the drawingInputScreen
    ; These constructs are stored in constants/gameModes.rkt
    ; This should be used to decide the behavior of the problems screen
    (define currentGameMode null)
    ; currentProblemCategory holds the difficulty-class construct of the drawingInputScreen
    ; These constructs are stored in constants/difficultiesAndCategories.rkt
    ; This construct should be used to decide which problems to generate
    (define currentProblemCategory null)

    (define playerOneScore 0)
    (define playerTwoScore 0)
    (define currentPlayer playerOne)

    (define problems (new equation-generator%)
      )


    ; A callback function for rendering problems to the canvas
    ; Erases the canvas at the start of each call to allow for updated screens
    ; Currently needs: information provided that will tell the program which
    ; shape to draw
    (define (canvasPaintingCallbackFunction canvas dc)
      (send dc clear)
      ; Draws score if set to multiplayer
      (cond
        [(eq? currentGameMode (first (rest (rest game-modes))))
         (draw-score dc currentPlayer playerOneScore playerTwoScore)
         ]
        )
      (send problems generateProblem (send drawingCanvas get-dc) currentProblemCategory)
      (print (send problems getAnswer))
      )

    ; Callback definitions
    ; Fired when the user submits an answer
    ; Updates the score of each player and switches the current player (only relevant if in multiplayer)
    ; Tells the user with a textbox whether the answer was correct or not
    (define (submit-callback b e)
      (let ((text (send textEnter get-value)))
        ; increases score of player who successfully answers the question
        (cond [(string=? text (number->string (send problems getAnswer)))
               (cond [(eq? currentPlayer playerOne)
                      (set! playerOneScore (+ playerOneScore 1))
                      ]
                     [else
                      (set! playerTwoScore (+ playerTwoScore 1))
                      ]
                     )
               ]
              )
        ; switches current player
        (cond [(eq? currentPlayer playerOne)
               (set! currentPlayer playerTwo)]
              [else
               (set! currentPlayer playerOne)
               ])
        ; tells user whether their answer was correct
        (if (string=? text (number->string (send problems getAnswer)))
            (message-box "Good job" (format "That is correct!") givenParent '(no-icon ok))
            (message-box "Go to the gazebo" (format "That is incorrect.") givenParent '(stop ok))))
      ; redraws screen
      (canvasPaintingCallbackFunction drawingCanvas (send drawingCanvas get-dc))
      )
    
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
                               [min-height canvasHeight]
                               [paint-callback canvasPaintingCallbackFunction]
                               )
      )
    ; Initialized canvas appearance
    (send (send drawingCanvas get-dc) set-background problemCanvasBackground)
    (send (send drawingCanvas get-dc) set-text-foreground "blue")
    
    (define inputPanel (new horizontal-panel%
                            [parent drawingInputMenu]
                            [style containerStyle]
                            [min-width 600]
                            [min-height inputSectionHeight]))


    (define returnButton (new button%
                              [parent inputPanel]
                              [label "Return to Main Menu"]
                              [callback return-callback]
                              )
      )
    
    (define textEnter (new text-field%
                           [parent inputPanel]
                           [label "Enter Answer:"]
                           [min-width textBoxWidth]))


    
    (define submit (new button%
                        [parent inputPanel]
                        [label "Submit"]
                        [callback submit-callback]))

    (define/public (enable)
      (send drawingInputMenu show #t)
      ;(print currentGameMode)
      ;(print (send currentProblemCategory getName))
      )
    ; This sets the game mode and problem category of the current problemScreen instance to the passed values
    (define/public (pass-information game-mode problem-category)
      (set! currentGameMode game-mode)
      (set! currentProblemCategory problem-category)
      (print (send game-mode getName))
      (print "|")
      (print (send problem-category getName))
      (set! playerOneScore 0)
      (set! playerTwoScore 0)
      (set! currentPlayer playerOne)
      )
    (define/public (disable)
      (send drawingInputMenu show #f))



    )
 )

(provide drawInputMenuUI%)
