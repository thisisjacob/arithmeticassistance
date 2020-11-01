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

      (rectangle-area deviceContext)
      (triangle-area deviceContext)
      (circle-area deviceContext)
      (parallelogram-area deviceContext)

      )

    (define (trapezoid-area deviceContext)
      (define a (random-integer 5 14))
      (set! a ($ (a * 1.0)))
      (define b (random-integer 15 25))
      (set! b ($ (b * 1.0)))
      (define h (random-integer 1 25))
      (set! h ($ (h * 1.0)))
      (set! answer 0)

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


    (define (rectangle-area deviceContext)
      (define length (random-integer 1 20))
      (define width (random-integer 1 20))
      (set! answer ($ (length * width)))

      (define o (open-output-string))
      (define l (open-output-string))
      (define w (open-output-string))
      
      (display "Find the area of the rectangle." o)
      (display "The length is: " l)
      (write length l)
      (display "The width is: " w)
      (write width w)

      (draw-rectangle deviceContext width length (list "Find the area of the rectangle"
                                                       (string-append "Length is: " (number->string length))
                                                       (string-append "Width is: " (number->string width))))
      )

    (define (triangle-area deviceContext)
      (define base (random-integer 1 10))
      (set! base ($ (base * 1.0)))
      (define height (random-integer 1 10))
      (set! height ($ (height * 1.0)))
      (set! answer ($ (base * height) / 2))

      (define o (open-output-string))
      (define b (open-output-string))
      (define h (open-output-string))
      
      (display "Find the area of the triangle." o)
      (display "The base is: " b)
      (write base b)
      (display "The height is: " h)
      (write height h)

      (draw-regular-triangle deviceContext base height (list "Find the area of the triangle"
                                                (string-append "Base is: " (number->string base))
                                                (string-append "Height is: " (number->string height))))
      )

    (define (circle-area deviceContext)
      (define radius (random-integer 1 25))
      (set! radius ($ (radius * 1.0)))
      (define p 3.14)
      (define rs (expt radius 2))
      (set! answer ($ (p * rs)))
      (set! answer (round answer))
      (set! answer (inexact->exact answer))

      (define o (open-output-string))
      (define r (open-output-string))
      (define q (open-output-string))
      
      (display "Find the area of the circle." o)
      (display "The radius is: " r)
      (write radius r)
      (display "Round to the nearest whole number." q)

      (draw-circle deviceContext radius (list "Find the area of the circle"
                                                (string-append "Radius is: " (number->string radius))))
      )

    (define (parallelogram-area deviceContext)
      (define base (random-integer 1 10))
      (define height (random-integer 1 10))

      (set! answer ($ (base * height)))

      (define o (open-output-string))
      (define b (open-output-string))
      (define h (open-output-string))
      
      (display "Find the area of the parallelogram." o)
      (display "The base is: " b)
      (write base b)
      (display "The height is: " h)
      (write height h)

      (draw-parallelogram deviceContext base height (list "Find the area of the parallelogram"
                                                          (string-append "Base is: " (number->string base))
                                                          (string-append "Height is: " (number->string height))))
      )
    )
  )



(provide equation-generator%)
         

