#lang racket
(require math/base)
(require k-infix)
(require "../drawing/canvasShapeDrawingFunctions.rkt")

(define equation-generator%
  (class object%
    (init-field)
    (super-new)

    (define answer null)
    (define device-context null)

    ; Use this function to retrieve the numeric answer for the problems screen
    (define/public (getAnswer)
      answer)

    ; Modify this function so it calls an available problem for the given problem category.
    ; The problems screen will call this function to create new problems
    (define/public (generateProblem deviceContext problemCategory)
      (trapezoid-area deviceContext)
      )

    (define (trapezoid-area deviceContext)
      (define a (random-integer 5 14))
      (set! a ($ (a * 1.0)))
      (define b (random-integer 15 25))
      (set! b ($ (b * 1.0)))
      (define h (random-integer 1 25))
      (set! h ($ (h * 1.0)))
      (define answer 0)

      (set! answer ($ (((a + b) * h) / 2)))

      (set! a (inexact->exact a))
      (set! b (inexact->exact b))
      (set! h (inexact->exact h))

      (define o (open-output-string))
      (define A (open-output-string))
      (define B (open-output-string))
      (define H (open-output-string))

      (display "Find the area of the trapezoid." o)
      (display "a is: " A)
      (write a A)
      (display "b is: " B)
      (write b B)
      (display "The height is: " H)
      (write h H)

      (draw-trapezoid deviceContext b a h (list "Find the area of the trapezoid"
                                                (string-append "a is: " (number->string a))
                                                (string-append "b is: " (number->string b))
                                                (string-append "The height is: " (number->string h))))

      )

    
    )
  )

(provide equation-generator%)
         

