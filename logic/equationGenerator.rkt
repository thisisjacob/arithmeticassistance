#lang racket
(require math/base)
(require k-infix)
(require "../drawing/canvasShapeDrawingFunctions.rkt")
(require "../constants/difficultiesAndCategories.rkt")
(require "../constants/userInterfaceConstants.rkt")

; This class is used for generating problems and calling drawing functions
; No initialization parameters are needed
; Available problems defined in difficultiesAndCategories.rkt constants file

(define equation-generator%
  (class object%
    (init-field)
    (super-new)

    (define answer null)
    (define answer2 null)

    ; Use this function to retrieve the numeric answer for the problems screen
    (define/public (getAnswer)
      answer)

    (define/public (getRound)
      answer2)

    ; Generates a problem based on the given given problemCategory
    ; Projects it to the given deviceContext
    ; Answer of the problem can be retrieved with the getAnswer function
    (define/public (generateProblem deviceContext problemCategory)
      (define newProblemNum (random-integer 0 5))
      ; If geometric problem, randomly call one of the geometric problem generators
      (send deviceContext set-brush geometricShapeBackground geometricShapeBackgroundStyle)
      (send deviceContext set-text-foreground geometricShapeTextColor)
      (cond
        [(eq? (send problemCategory getId) (send GEOMETRY getId))
         (cond 
           [(eq? 0 newProblemNum)       (trapezoid-area deviceContext)]
           [(eq? 1 newProblemNum)       (rectangle-area deviceContext)]
           [(eq? 2 newProblemNum)       (triangle-area deviceContext)]
           [(eq? 3 newProblemNum)       (circle-area deviceContext)]
           [(eq? 4 newProblemNum)       (parallelogram-area deviceContext)]
           )
         ]
        ; calls a random arithmetic problem
        ; weighted towards generating high school level problems
        [(eq? (send problemCategory getId) (send ARITHMETIC getId))
              (cond
                [(eq? 0 newProblemNum) (elementary-level-arithmetic-problem deviceContext)]
                [(eq? 1 newProblemNum) (middle-school-level-arithmetic-problem deviceContext)]
                [else (high-level-arithmetic-problem deviceContext)]
                )
              ]
        [(eq? (send problemCategory getId) (send ELEMENTARY-SCHOOL getId))
         (elementary-level-arithmetic-problem deviceContext)]
        [(eq? (send problemCategory getId) (send MIDDLE-SCHOOL getId))
         (middle-school-level-arithmetic-problem deviceContext)
         ]
        [(eq? (send problemCategory getId) (send HIGH-SCHOOL getId))
         (high-level-arithmetic-problem deviceContext)
         ]

        )
      )

    ; Generates and renders a trapezoid problem rendered onto the given deviceContext
    ; Updates answer
    (define (trapezoid-area deviceContext)
      (define a (random-integer 5 14))
      (set! a ($ (a * 1.0)))
      (define b (random-integer 15 25))
      (set! b ($ (b * 1.0)))
      (define h (random-integer 1 25))
      (set! h ($ (h * 1.0)))
      (set! answer 0)

      (set! answer ($ (((a + b) * h) / 2)))

      (set! answer2 (inexact->exact answer))

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

      (draw-trapezoid deviceContext b a h (list "Find the area of the trapezoid."
                                                (string-append "a is: " (number->string a))
                                                (string-append "b is: " (number->string b))
                                                (string-append "The height is: " (number->string h))))

      )

    ; Generates and renders a rectangle area problem onto the given device context
    ; Updates answer
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

      (draw-rectangle deviceContext width length (list "Find the area of the rectangle."
                                                       (string-append "Length is: " (number->string length))
                                                       (string-append "Width is: " (number->string width))))
      )

    ; Generates and renders a triangle area problem onto the given device context
    ; Updates answer
    (define (triangle-area deviceContext)
      (define base (random-integer 4 10))
      (set! base ($ (base * 1.0)))
      (define height (random-integer 4 10))
      (set! height ($ (height * 1.0)))
      (set! answer ($ (base * height) / 2))

      (set! answer2 (inexact->exact answer))

      (define o (open-output-string))
      (define b (open-output-string))
      (define h (open-output-string))
      
      (display "Find the area of the triangle." o)
      (display "The base is: " b)
      (write base b)
      (display "The height is: " h)
      (write height h)

      (draw-regular-triangle deviceContext base height (list "Find the area of the triangle."
                                                (string-append "Base is: " (number->string base))
                                                (string-append "Height is: " (number->string height))))
      )

    ; Generates and renders a circle area problem onto the given device context
    ; Updates answer
    (define (circle-area deviceContext)
      (define radius (random-integer 5 15))
      (set! radius ($ (radius * 1.0)))
      (define p 3.14)
      (define rs (expt radius 2))
      (set! answer ($ (p * rs)))
      (set! answer (round answer))
      (set! answer (inexact->exact answer))

      (set! answer2 (inexact->exact answer))

      (define o (open-output-string))
      (define r (open-output-string))
      (define q (open-output-string))
      
      (display "Find the area of the circle." o)
      (display "The radius is: " r)
      (write radius r)
      (display "Round to the nearest whole number." q)

      (draw-circle deviceContext radius (list "Find the area of the circle, round down answer."
                                                (string-append "Radius is: " (number->string radius))))
      )

    ; Generates and renders a parallelogram area problem onto the given device context
    ; Updates answer
    (define (parallelogram-area deviceContext)
      (define base (random-integer 1 10))
      (define height (random-integer 1 10))

      (set! answer ($ (base * height)))

      (set! answer2 (inexact->exact answer))

      (define o (open-output-string))
      (define b (open-output-string))
      (define h (open-output-string))
      
      (display "Find the area of the parallelogram." o)
      (display "The base is: " b)
      (write base b)
      (display "The height is: " h)
      (write height h)

      (draw-parallelogram deviceContext base height (list "Find the area of the parallelogram."
                                                          (string-append "Base is: " (number->string base))
                                                          (string-append "Height is: " (number->string height))))
      )

    ; Generates a random number, which can be either in the range of [- range, 1] or [1, range]
    ; Range must be a positive integer
    (define (arithmeticProblemNumGenerator range)
      (define isPositive (random-integer 0 2))
      (cond
        [(eq? 0 isPositive) (random-integer 1 (+ range 1))]
        ;[else (random-integer (- 0 range) 0)]
        [else (random-integer 1 (+ range 1))]
        )
      )
    
    ; Generates and renders a "middle school arithmetic problem"
    ; Updates answer
    (define (middle-school-level-arithmetic-problem device-context)
      (define a (arithmeticProblemNumGenerator 25))
      (define b (arithmeticProblemNumGenerator 25))
      (define c (arithmeticProblemNumGenerator 25))
      (define d (arithmeticProblemNumGenerator 25))

      (define a2 (arithmeticProblemNumGenerator 25))
      (define b2 (arithmeticProblemNumGenerator 25))
      (define c2 (arithmeticProblemNumGenerator 25))
      (define d2 (arithmeticProblemNumGenerator 25))

      (define op1 (random-natural 4))
      (define op2 (random-natural 4))
      (define op3 (random-natural 4))

      (define Op1 (random-natural 4))
      (define Op2 (random-natural 4))
      (define Op3 (random-natural 4))
      (define Op4 (random-natural 4))

      (define par1 (random-natural 5))
      (define par2 (random-natural 5))
      (define par3 (random-natural 5))
      (define par4 (random-natural 5))
      
      (define o (open-output-string))

      (set! answer 0)

      ; Writes out expression in the infix format

      (cond [(eq? par1 3)
             (write '< o)
             (write a o)
             

             (case (+ Op1)
               [(0) (write '+ o)
                    (write a2 o)
                    (set! a ($ (a + a2)))
                    ]
               [(1) (write '- o)
                    (write a2 o)
                    (set! a ($ (a - a2)))
                    ]
               [(2) (write '* o)
                    (write a2 o)
                    (set! a ($ (a * a2)))
                    ]
               [(3) (write '/ o)
                    (write a2 o)
                    (set! a (floor ($ (a / a2))))
                    ])
           
             (write '> o)
             ]
            [else
             (write a o)])

      (case (+ op1)
               [(0) (write '+ o)]
               [(1) (write '- o)]
               [(2) (write '* o)]
               [(3) (write '/ o)])
             
    
      (cond [(eq? par2 3)
             (write '< o)
             (write b o)
             

             (case (+ Op2)
               [(0) (write '+ o)
                    (write b2 o)
                    (set! b ($ (b + b2)))
                    ]
               [(1) (write '- o)
                    (write b2 o)
                    (set! b ($ (b - b2)))
                    ]
               [(2) (write '* o)
                    (write b2 o)
                    (set! b ($ (b * b2)))
                    ]
               [(3) (write '/ o)
                    (write b2 o)
                    (set! b (floor ($ (b / b2))))
                    ])
           
             (write '> o)
             ]

            [else
             (write b o)]
            )

      (case (+ op2)
        [(0) (write '+ o)]
        [(1) (write '- o)]
        [(2) (write '* o)]
        [(3) (write '/ o)])

      (cond [(eq? par3 3)
             (write '< o)
             (write c o)
             

             (case (+ Op3)
               [(0) (write '+ o)
                    (write c2 o)
                    (set! c ($ (c + c2)))
                    ]
               [(1) (write '- o)
                    (write c2 o)
                    (set! c ($ (c - c2)))
                    ]
               [(2) (write '* o)
                    (write c2 o)
                    (set! c ($ (c * c2)))
                    ]
               [(3) (write '/ o)
                    (write c2 o)
                    (set! c (floor ($ (c / c2))))
                    ])
           
             (write '> o)
             ]

            [else
             (write c o)]
            )

      (case (+ op3)
        [(0) (write '+ o)]
        [(1) (write '- o)]
        [(2) (write '* o)]
        [(3) (write '/ o)])

      (cond [(eq? par4 3)
             (write '< o)
             (write d o)
             
             (case (+ Op4)
               [(0) (write '+ o)
                    (write d2 o)
                    (set! d ($ (d + d2)))
                    ]
               [(1) (write '- o)
                    (write d2 o)
                    (set! d ($ (d - d2)))
                    ]
               [(2) (write '* o)
                    (write d2 o)
                    (set! d ($ (d * d2)))
                    ]
               [(3) (write '/ o)
                    (write d2 o)
                    (set! d (floor ($ (d / d2))))
                    ])
           
             (write '> o)
             ]

            [else
             (write d o)]
            )



      ; Calculates the answer based on the variables and operations

      (define equation               
          (case (+ op1)
            [(0) (case (+ op2)
                   [(0) (case (+ op3)
                          [(0) (set! answer ($ a + b + c + d))
                               (set! answer2 (inexact->exact answer))]
                          [(1) (set! answer ($ a + b + c - d))
                               (set! answer2 (inexact->exact answer))]
                          [(2) (set! answer ($ a + b + c * d))
                               (set! answer2 (inexact->exact answer))]
                          [(3) (set! answer ($ a + b + (floor (c / d))))
                               (set! answer2 (inexact->exact answer))])]
                   [(1) (case (+ op3)
                          [(0) (set! answer ($ a + b - c + d))
                               (set! answer2 (inexact->exact answer))]
                          [(1) (set! answer ($ a + b - c - d))
                               (set! answer2 (inexact->exact answer))]
                          [(2) (set! answer ($ a + b - c * d))
                               (set! answer2 (inexact->exact answer))]
                          [(3) (set! answer ($ a + b - (floor (c / d))))
                               (set! answer2 (inexact->exact answer))])]
                   [(2) (case (+ op3)
                          [(0) (set! answer ($ a + b * c + d))
                               (set! answer2 (inexact->exact answer))]
                          [(1) (set! answer ($ a + b * c - d))
                               (set! answer2 (inexact->exact answer))]
                          [(2) (set! answer ($ a + b * c * d))
                               (set! answer2 (inexact->exact answer))]
                          [(3) (set! answer ($ a + b * (floor (c / d))))
                               (set! answer2 (inexact->exact answer))])]
                   [(3) (case (+ op3)
                          [(0) (set! answer ($ a + (floor (b / c)) + d))
                               (set! answer2 (inexact->exact answer))]
                          [(1) (set! answer ($ a + (floor (b / c)) - d))
                               (set! answer2 (inexact->exact answer))]
                          [(2) (set! answer ($ a + (floor (b / c)) * d))
                               (set! answer2 (inexact->exact answer))]
                          [(3) (set! answer ($ a + (floor (floor (b / c) / d))))
                               (set! answer2 (inexact->exact answer))])])]
            [(1) (case (+ op2)
                   [(0) (case (+ op3)
                          [(0) (set! answer ($ a - b + c + d))
                               (set! answer2 (inexact->exact answer))]
                          [(1) (set! answer ($ a - b + c - d))
                               (set! answer2 (inexact->exact answer))]
                          [(2) (set! answer ($ a - b + c * d))
                               (set! answer2 (inexact->exact answer))]
                          [(3) (set! answer ($ a - b + (floor (c / d))))
                               (set! answer2 (inexact->exact answer))])]
                   [(1) (case (+ op3)
                          [(0) (set! answer ($ a - b - c + d))
                               (set! answer2 (inexact->exact answer))]
                          [(1) (set! answer ($ a - b - c - d))
                               (set! answer2 (inexact->exact answer))]
                          [(2) (set! answer ($ a - b - c * d))
                               (set! answer2 (inexact->exact answer))]
                          [(3) (set! answer ($ a - b - (floor (c / d))))
                               (set! answer2 (inexact->exact answer))])]
                   [(2) (case (+ op3)
                          [(0) (set! answer ($ a - b * c + d))
                               (set! answer2 (inexact->exact answer))]
                          [(1) (set! answer ($ a - b * c - d))
                               (set! answer2 (inexact->exact answer))]
                          [(2) (set! answer ($ a - b * c * d))
                               (set! answer2 (inexact->exact answer))]
                          [(3) (set! answer ($ a - b * (floor (c / d))))
                               (set! answer2 (inexact->exact answer))])]
                   [(3) (case (+ op3)
                          [(0) (set! answer ($ a - (floor (b / c)) + d))
                               (set! answer2 (inexact->exact answer))]
                          [(1) (set! answer ($ a - (floor (b / c)) - d))
                               (set! answer2 (inexact->exact answer))]
                          [(2) (set! answer ($ a - (floor (b / c)) * d))
                               (set! answer2 (inexact->exact answer))]
                          [(3) (set! answer ($ a - (floor (floor (b / c)) / d)))
                               (set! answer2 (inexact->exact answer))])])]
            [(2) (case (+ op2)
                   [(0) (case (+ op3)
                          [(0) (set! answer ($ a * b + c + d))
                               (set! answer2 (inexact->exact answer))]
                          [(1) (set! answer ($ a * b + c - d))
                               (set! answer2 (inexact->exact answer))]
                          [(2) (set! answer ($ a * b + c * d))
                               (set! answer2 (inexact->exact answer))]
                          [(3) (set! answer ($ a * b + (floor (c / d))))
                               (set! answer2 (inexact->exact answer))])]
                   [(1) (case (+ op3)
                          [(0) (set! answer ($ a * b - c + d))
                               (set! answer2 (inexact->exact answer))]
                          [(1) (set! answer ($ a * b - c - d))
                               (set! answer2 (inexact->exact answer))]
                          [(2) (set! answer ($ a * b - c * d))
                               (set! answer2 (inexact->exact answer))]
                          [(3) (set! answer ($ a * b - (floor (c / d))))
                               (set! answer2 (inexact->exact answer))])]
                   [(2) (case (+ op3)
                          [(0) (set! answer ($ a * b * c + d))
                               (set! answer2 (inexact->exact answer))]
                          [(1) (set! answer ($ a * b * c - d))
                               (set! answer2 (inexact->exact answer))]
                          [(2) (set! answer ($ a * b * c * d))
                               (set! answer2 (inexact->exact answer))]
                          [(3) (set! answer ($ a * b * (floor (c / d))))
                               (set! answer2 (inexact->exact answer))])]
                   [(3) (case (+ op3)
                          [(0) (set! answer ($ a * (floor (b / c)) + d))
                               (set! answer2 (inexact->exact answer))]
                          [(1) (set! answer ($ a * (floor (b / c)) - d))
                               (set! answer2 (inexact->exact answer))]
                          [(2) (set! answer ($ a * (floor (b / c)) * d))
                               (set! answer2 (inexact->exact answer))]
                          [(3) (set! answer ($ a * (floor (floor (b / c)) / d)))
                               (set! answer2 (inexact->exact answer))])])]
            [(3) (case (+ op2)
                   [(0) (case (+ op3)
                          [(0) (set! answer ($ (floor (a / b)) + c + d))
                               (set! answer2 (inexact->exact answer))]
                          [(1) (set! answer ($ (floor (a / b)) + c - d))
                               (set! answer2 (inexact->exact answer))]
                          [(2) (set! answer ($ (floor (a / b)) + c * d))
                               (set! answer2 (inexact->exact answer))]
                          [(3) (set! answer ($ (floor (a / b)) + (floor (c / d))))
                               (set! answer2 (inexact->exact answer))])]
                   [(1) (case (+ op3)
                          [(0) (set! answer ($ (floor (a / b)) - c + d))
                               (set! answer2 (inexact->exact answer))]
                          [(1) (set! answer ($ (floor (a / b)) - c - d))
                               (set! answer2 (inexact->exact answer))]
                          [(2) (set! answer ($ (floor (a / b)) - c * d))
                               (set! answer2 (inexact->exact answer))]
                          [(3) (set! answer ($ (floor (a / b)) - (floor (c / d))))
                               (set! answer2 (inexact->exact answer))])]
                   [(2) (case (+ op3)
                          [(0) (set! answer ($ (floor (a / b)) * c + d))
                               (set! answer2 (inexact->exact answer))]
                          [(1) (set! answer ($ (floor (a / b)) * c - d))
                               (set! answer2 (inexact->exact answer))]
                          [(2) (set! answer ($ (floor (a / b)) * c * d))
                               (set! answer2 (inexact->exact answer))]
                          [(3) (set! answer ($ (floor (a / b)) * (floor (c / d))))
                               (set! answer2 (inexact->exact answer))])]
                   [(3) (case (+ op3)
                          [(0) (set! answer ($ (floor (floor (a / b)) / c) + d))
                               (set! answer2 (inexact->exact answer))]
                          [(1) (set! answer ($ (floor (floor (a / b)) / c) - d))
                               (set! answer2 (inexact->exact answer))]
                          [(2) (set! answer ($ (floor (floor (a / b)) / c) * d))
                               (set! answer2 (inexact->exact answer))]
                          [(3) (set! answer ($ (floor (floor (floor (a / b) / c) / d))))
                               (set! answer2 (inexact->exact answer))])])]))

      
      ; Makes the value of the answer variable a string for later comparisons
      ;(print "Solve the following equation: ")
      ;(get-output-string o)
      (print answer)
      (draw-text-problem-with-multiple-lines device-context (list "Solve the following equation: " (get-output-string o)))

    
      )

    ; Generates and renders a "elementary arithmetic problem"
    ; Updates answer
    (define (elementary-level-arithmetic-problem device-context)
      ;Elementary-school algorithm
      (define a (random-natural 10))
      (define b (random-natural 10))

      (define a2 (random-natural 10))
      (define b2 (random-natural 10))

      (define op1 (random-natural 4))
      (define op2 (random-natural 4))
      (define op3 (random-natural 4))
      (define o (open-output-string))

      (define par1 (random-natural 5))
      (define par2 (random-natural 5))
      
      (set! answer 0)

      ; Writes out expression in the infix format

      (cond [(eq? par1 3)
             (write '< o)
             (write a o)
             
             (with-handlers
                 ([exn:fail:contract:divide-by-zero? elementary-level-arithmetic-problem])
               (case (+ op2)
                 [(0) (write '+ o)
                      (write a2 o)
                      (set! a ($ (a + a2)))
                      ]
                 [(1) (write '- o)
                      (write a2 o)
                      (set! a ($ (a - a2)))
                      ]
                 [(2) (write '* o)
                      (write a2 o)
                      (set! a ($ (a * a2)))
                      ]
                 [(3) (write '/ o)
                      (write a2 o)
                      (set! a (floor ($ (a / a2))))
                      ]))
           
             (write '> o)
             ]
            [else
             (write a o)])

      (case (+ op1)
               [(0) (write '+ o)]
               [(1) (write '- o)]
               [(2) (write '* o)]
               [(3) (write '/ o)])
             
    
      (cond [(eq? par2 3)
             (write '< o)
             (write b o)
             
             (with-handlers
               ([exn:fail:contract:divide-by-zero? elementary-level-arithmetic-problem])
               (case (+ op2)
                 [(0) (write '+ o)
                      (write b2 o)
                      (set! b ($ (b + b2)))
                      ]
                 [(1) (write '- o)
                      (write b2 o)
                      (set! b ($ (b - b2)))
                      ]
                 [(2) (write '* o)
                      (write b2 o)
                      (set! b ($ (b * b2)))
                      ]
                 [(3) (write '/ o)
                      (write b2 o)
                      (set! b (floor ($ (b / b2))))
                      ]))
           
             (write '> o)
             ]

            [else
             (write b o)]
            )

      ; Calculates the answer based on the variables and operations

      (define equation
         (with-handlers
               ([exn:fail:contract:divide-by-zero? elementary-level-arithmetic-problem])
           (case (+ op1)
             [(0) (set! answer ($ a + b))
                  (set! answer2 (inexact->exact answer))]
             [(1) (set! answer ($ a - b))
                  (set! answer2 (inexact->exact answer))]
             [(2) (set! answer ($ a * b))
                  (set! answer2 (inexact->exact answer))]
             [(3) (set! answer (floor ($ a / b)))
                  (set! answer2 (inexact->exact answer))])))

      

      (print answer)
      (draw-text-problem-with-multiple-lines device-context (list "Solve the following equation: " (get-output-string o)))
      
      )

    (define (high-level-arithmetic-problem device-context)

      (define b (arithmeticProblemNumGenerator 10))
      (define c (arithmeticProblemNumGenerator 10))
      (define d (arithmeticProblemNumGenerator 10))
      (define e (arithmeticProblemNumGenerator 10))
      (define f (arithmeticProblemNumGenerator 10))
      (define g (arithmeticProblemNumGenerator 10))

      (define b2 (arithmeticProblemNumGenerator 10))
      (define c2 (arithmeticProblemNumGenerator 10))
      (define d2 (arithmeticProblemNumGenerator 10))
      (define e2 (arithmeticProblemNumGenerator 10))
      (define f2 (arithmeticProblemNumGenerator 10))
      (define g2 (arithmeticProblemNumGenerator 10))

      (define op2 (random-natural 4))
      (define op3 (random-natural 4))
      (define op4 (random-natural 4))
      (define op5 (random-natural 4))
      (define op6 (random-natural 4))

      (define Op1 (random-natural 4))
      (define Op2 (random-natural 4))
      (define Op3 (random-natural 4))
      (define Op4 (random-natural 4))
      (define Op5 (random-natural 4))
      (define Op6 (random-natural 4))

      (define par1 (random-natural 5))
      (define par2 (random-natural 5))
      (define par3 (random-natural 5))
      (define par4 (random-natural 5))
      (define par5 (random-natural 5))
      (define par6 (random-natural 5))

      ;(define sq1 (random-natural 4))
      ;(define sq1 (random-natural 4))
      ;(define sq1 (random-natural 4))
      ;(define sq1 (random-natural 4))
      ;(define sq1 (random-natural 4))
      ;(define sq1 (random-natural 4))
      ;(define sq1 (random-natural 4))

      (define o (open-output-string))

      (set! answer 0)

      ; Writes out expression in the infix format

      (cond [(eq? par1 3)
             (write '< o)
             (write b o)
             

             (case (+ Op1)
               [(0) (write '+ o)
                    (write b2 o)
                    (set! b ($ (b + b2)))
                    ]
               [(1) (write '- o)
                    (write b2 o)
                    (set! b ($ (b - b2)))
                    ]
               [(2) (write '* o)
                    (write b2 o)
                    (set! b ($ (b * b2)))
                    ]
               [(3) (write '/ o)
                    (write b2 o)
                    (set! b (floor ($ (b / b2))))
                    ])
           
             (write '> o)
             ]
            [else
             (write b o)])

      (case (+ op2)
               [(0) (write '+ o)]
               [(1) (write '- o)]
               [(2) (write '* o)]
               [(3) (write '/ o)])
             
    
      (cond [(eq? par2 3)
             (write '< o)
             (write c o)
             

             (case (+ Op2)
               [(0) (write '+ o)
                    (write c2 o)
                    (set! c ($ (c + c2)))
                    ]
               [(1) (write '- o)
                    (write c2 o)
                    (set! c ($ (c - c2)))
                    ]
               [(2) (write '* o)
                    (write c2 o)
                    (set! c ($ (c * c2)))
                    ]
               [(3) (write '/ o)
                    (write c2 o)
                    (set! c (floor ($ (c / c2))))
                    ])
           
             (write '> o)
             ]

            [else
             (write c o)]
            )

      (case (+ op3)
               [(0) (write '+ o)]
               [(1) (write '- o)]
               [(2) (write '* o)]
               [(3) (write '/ o)])

      (cond [(eq? par3 3)
             (write '< o)
             (write d o)
             

             (case (+ Op3)
               [(0) (write '+ o)
                    (write d2 o)
                    (set! d ($ (d + d2)))
                    ]
               [(1) (write '- o)
                    (write d2 o)
                    (set! d ($ (d - d2)))
                    ]
               [(2) (write '* o)
                    (write d2 o)
                    (set! d ($ (d * d2)))
                    ]
               [(3) (write '/ o)
                    (write d2 o)
                    (set! d (floor ($ (d / d2))))
                    ])
           
             (write '> o)
             ]

            [else
             (write d o)]
            )

      (case (+ op4)
               [(0) (write '+ o)]
               [(1) (write '- o)]
               [(2) (write '* o)]
               [(3) (write '/ o)])

      (cond [(eq? par4 3)
             (write '< o)
             (write e o)
             

             (case (+ Op4)
               [(0) (write '+ o)
                    (write e2 o)
                    (set! e ($ (e + e2)))
                    ]
               [(1) (write '- o)
                    (write e2 o)
                    (set! e ($ (e - e2)))
                    ]
               [(2) (write '* o)
                    (write e2 o)
                    (set! e ($ (e * e2)))
                    ]
               [(3) (write '/ o)
                    (write e2 o)
                    (set! e (floor ($ (e / e2))))
                    ])
           
             (write '> o)
             ]

            [else
             (write e o)]
            )

      (case (+ op5)
               [(0) (write '+ o)]
               [(1) (write '- o)]
               [(2) (write '* o)]
               [(3) (write '/ o)])

      (cond [(eq? par5 3)
             (write '< o)
             (write f o)
             

             (case (+ Op5)
               [(0) (write '+ o)
                    (write f2 o)
                    (set! f ($ (f + f2)))
                    ]
               [(1) (write '- o)
                    (write f2 o)
                    (set! f ($ (f - f2)))
                    ]
               [(2) (write '* o)
                    (write f2 o)
                    (set! f ($ (f * f2)))
                    ]
               [(3) (write '/ o)
                    (write f2 o)
                    (set! f (floor ($ (f / f2))))
                    ])
           
             (write '> o)
             ]

            [else
             (write f o)]
            )

      (case (+ op6)
               [(0) (write '+ o)]
               [(1) (write '- o)]
               [(2) (write '* o)]
               [(3) (write '/ o)])

      (cond [(eq? par6 3)
             (write '< o)
             (write g o)
             

             (case (+ Op6)
               [(0) (write '+ o)
                    (write g2 o)
                    (set! g ($ (g + g2)))
                    ]
               [(1) (write '- o)
                    (write g2 o)
                    (set! g ($ (g - g2)))
                    ]
               [(2) (write '* o)
                    (write g2 o)
                    (set! g ($ (g * g2)))
                    ]
               [(3) (write '/ o)
                    (write g2 o)
                    (set! g (floor ($ (g / g2))))
                    ])
           
             (write '> o)
             ]

            [else
             (write g o)]
            )


      ;(print "Solve the following equation: ")
      ;(get-output-string o)

      ; Calculates the answer based on the variables and operations

      (define equation
          (case (+ op2)
            [(0) (case (+ op3)
                   [(0) (case (+ op4)
                          [(0) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b + c + d + e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c + d + e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c + d + e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c + d + e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b + c + d + e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c + d + e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c + d + e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c + d + e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b + c + d + e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c + d + e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c + d + e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c + d + e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b + c + d + (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c + d + (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c + d + (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c + d + (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(1) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b + c + d - e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c + d - e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c + d - e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c + d - e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b + c + d - e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c + d - e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c + d - e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c + d - e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b + c + d - e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c + d - e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c + d - e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c + d - e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b + c + d - (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c + d - (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c + d - (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c + d - (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(2) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b + c + d * e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c + d * e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c + d * e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c + d * e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b + c + d * e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c + d * e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c + d * e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c + d * e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b + c + d * e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c + d * e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c + d * e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c + d * e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b + c + d * (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c + d * (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c + d * (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c + d * (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(3) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b + c + (floor (d / e)) + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c + (floor (d / e)) + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c + (floor (d / e)) + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c + (floor (d / e)) + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b + c + (floor (d / e)) - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c + (floor (d / e)) - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c + (floor (d / e)) - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c + (floor (d / e)) - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b + c + (floor (d / e)) * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c + (floor (d / e)) * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c + (floor (d / e)) * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c + (floor (d / e)) * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b + c + (floor (floor (d / e)) / f) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c + (floor (floor (d / e)) / f) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c + (floor (floor (d / e)) / f) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c + (floor (floor (floor (d / e)) / f) / g)))
                                             (set! answer2 (inexact->exact answer))])])])]
                   [(1) (case (+ op4)
                          [(0) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b + c - d + e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c - d + e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c - d + e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c - d + e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b + c - d + e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c - d + e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c - d + e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c - d + e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b + c - d + e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c - d + e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c - d + e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c - d + e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b + c - d + (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c - d + (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c - d + (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c - d + (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(1) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b + c - d - e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c - d - e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c - d - e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c - d - e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b + c - d - e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c - d - e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c - d - e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c - d - e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b + c - d - e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c - d - e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c - d - e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c - d - e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b + c - d - (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c - d - (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c - d - (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c - d - (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(2) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b + c - d * e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c - d * e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c - d * e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c - d * e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b + c - d * e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c - d * e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c - d * e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c - d * e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b + c - d * e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c - d * e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c - d * e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c - d * e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b + c - d * (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c - d * (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c - d * (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c - d * (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(3) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b + c - (floor (d / e)) + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c - (floor (d / e)) + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c - (floor (d / e)) + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c - (floor (d / e)) + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b + c - (floor (d / e)) - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c - (floor (d / e)) - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c - (floor (d / e)) - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c - (floor (d / e)) - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b + c - (floor (d / e)) * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c - (floor (d / e)) * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c - (floor (d / e)) * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c - (floor (d / e)) * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b + c - (floor (floor (d / e)) / f) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c - (floor (floor (d / e)) / f) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c - (floor (floor (d / e)) / f) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c - (floor (floor (floor (d / e)) / f) / g)))
                                             (set! answer2 (inexact->exact answer))])])])]
                   [(2) (case (+ op4)
                          [(0) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b + c * d + e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c * d + e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c * d + e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c * d + e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b + c * d + e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c * d + e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c * d + e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c * d + e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b + c * d + e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c * d + e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c * d + e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c * d + e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b + c * d + (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c * d + (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c * d + (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c * d + (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(1) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b + c * d - e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c * d - e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c * d - e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c * d - e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b + c * d - e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c * d - e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c * d - e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c * d - e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b + c * d - e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c * d - e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c * d - e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c * d - e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b + c * d - (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c * d - (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c * d - (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c * d - (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(2) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b + c * d * e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c * d * e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c * d * e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c * d * e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b + c * d * e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c * d * e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c * d * e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c * d * e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b + c * d * e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c * d * e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c * d * e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c * d * e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b + c * d * (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c * d * (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c * d * (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c * d * (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(3) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b + c * (floor (d / e)) + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c * (floor (d / e)) + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c * (floor (d / e)) + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c * (floor (d / e)) + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b + c * (floor (d / e)) - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c * (floor (d / e)) - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c * (floor (d / e)) - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c * (floor (d / e)) - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b + c * (floor (d / e)) * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c * (floor (d / e)) * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c * (floor (d / e)) * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c * (floor (d / e)) * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b + c * (floor (floor (d / e)) / f) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + c * (floor (floor (d / e)) / f) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + c * (floor (floor (d / e)) / f) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + c * (floor (floor (floor (d / e)) / f) / g)))
                                             (set! answer2 (inexact->exact answer))])])])]
                   [(3) (case (+ op4)
                          [(0) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b + (floor (c / d)) + e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + (floor (c / d)) + e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + (floor (c / d)) + e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + (floor (c / d)) + e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b + (floor (c / d)) + e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + (floor (c / d)) + e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + (floor (c / d)) + e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + (floor (c / d)) + e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b + (floor (c / d)) + e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + (floor (c / d)) + e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + (floor (c / d)) + e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + (floor (c / d)) + e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b + (floor (c / d)) + (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + (floor (c / d)) + (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + (floor (c / d)) + (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + (floor (c / d)) + (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(1) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b + (floor (c / d)) - e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + (floor (c / d)) - e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + (floor (c / d)) - e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + (floor (c / d)) - e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b + (floor (c / d)) - e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + (floor (c / d)) - e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + (floor (c / d)) - e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + (floor (c / d)) - e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b + (floor (c / d)) - e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + (floor (c / d)) - e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + (floor (c / d)) - e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + (floor (c / d)) - e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b + (floor (c / d)) - (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + (floor (c / d)) - (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + (floor (c / d)) - (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + (floor (c / d)) - (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(2) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b + (floor (c / d)) * e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + (floor (c / d)) * e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + (floor (c / d)) * e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + (floor (c / d)) * e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b + (floor (c / d)) * e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + (floor (c / d)) * e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + (floor (c / d)) * e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + (floor (c / d)) * e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b + (floor (c / d)) * e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + (floor (c / d)) * e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + (floor (c / d)) * e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + (floor (c / d)) * e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b + (floor (c / d)) * (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + (floor (c / d)) * (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + (floor (c / d)) * (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + (floor (c / d)) * (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(3) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b + (floor (floor (c / d) / e)) + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + (floor (floor (c / d) / e)) + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + (floor (floor (c / d) / e)) + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + (floor (floor (c / d) / e)) + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b + (floor (floor (c / d) / e)) - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + (floor (floor (c / d) / e)) - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + (floor (floor (c / d) / e)) - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + (floor (floor (c / d) / e)) - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b + (floor (floor (c / d) / e)) * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + (floor (floor (c / d) / e)) * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + (floor (floor (c / d) / e)) * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + (floor (floor (c / d) / e)) * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b + (floor (floor (floor (c / d)) / e) / f) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b + (floor (floor (floor (c / d)) / e) / f) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b + (floor (floor (floor (c / d)) / e) / f) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b + (floor (floor (floor (floor (c / d) / e) / f) / g))))
                                             (set! answer2 (inexact->exact answer))])])])])]
            [(1) (case (+ op3)
                   [(0) (case (+ op4)
                          [(0) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b - c + d + e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c + d + e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c + d + e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c + d + e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b - c + d + e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c + d + e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c + d + e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c + d + e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b - c + d + e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c + d + e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c + d + e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c + d + e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b - c + d + (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c + d + (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c + d + (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c + d + (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(1) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b - c + d - e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c + d - e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c + d - e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c + d - e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b - c + d - e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c + d - e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c + d - e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c + d - e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b - c + d - e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c + d - e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c + d - e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c + d - e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b - c + d - (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c + d - (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c + d - (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c + d - (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(2) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b - c + d * e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c + d * e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c + d * e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c + d * e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b - c + d * e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c + d * e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c + d * e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c + d * e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b - c + d * e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c + d * e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c + d * e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c + d * e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b - c + d * (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c + d * (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c + d * (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c + d * (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(3) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b - c + (floor (d / e)) + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c + (floor (d / e)) + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c + (floor (d / e)) + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c + (floor (d / e)) + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b - c + (floor (d / e)) - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c + (floor (d / e)) - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c + (floor (d / e)) - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c + (floor (d / e)) - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b - c + (floor (d / e)) * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c + (floor (d / e)) * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c + (floor (d / e)) * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c + (floor (d / e)) * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b - c + (floor (floor (d / e)) / f) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c + (floor (floor (d / e)) / f) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c + (floor (floor (d / e)) / f) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c + (floor (floor (floor (d / e)) / f) / g)))
                                             (set! answer2 (inexact->exact answer))])])])]
                   [(1) (case (+ op4)
                          [(0) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b - c - d + e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c - d + e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c - d + e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c - d + e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b - c - d + e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c - d + e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c - d + e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c - d + e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b - c - d + e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c - d + e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c - d + e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c - d + e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b - c - d + (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c - d + (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c - d + (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c - d + (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(1) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b - c - d - e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c - d - e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c - d - e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c - d - e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b - c - d - e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c - d - e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c - d - e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c - d - e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b - c - d - e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c - d - e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c - d - e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c - d - e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b - c - d - (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c - d - (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c - d - (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c - d - (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(2) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b - c - d * e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c - d * e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c - d * e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c - d * e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b - c - d * e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c - d * e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c - d * e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c - d * e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b - c - d * e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c - d * e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c - d * e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c - d * e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b - c - d * (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c - d * (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c - d * (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c - d * (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(3) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b - c - (floor (d / e)) + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c - (floor (d / e)) + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c - (floor (d / e)) + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c - (floor (d / e)) + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b - c - (floor (d / e)) - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c - (floor (d / e)) - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c - (floor (d / e)) - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c - (floor (d / e)) - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b - c - (floor (d / e)) * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c - (floor (d / e)) * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c - (floor (d / e)) * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c - (floor (d / e)) * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b - c - (floor (floor (d / e)) / f) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c - (floor (floor (d / e)) / f) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c - (floor (floor (d / e)) / f) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c - (floor (floor (floor (d / e)) / f) / g)))
                                             (set! answer2 (inexact->exact answer))])])])]
                   [(2) (case (+ op4)
                          [(0) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b - c * d + e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c * d + e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c * d + e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c * d + e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b - c * d + e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c * d + e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c * d + e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c * d + e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b - c * d + e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c * d + e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c * d + e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c * d + e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b - c * d + (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c * d + (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c * d + (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c * d + (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(1) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b - c * d - e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c * d - e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c * d - e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c * d - e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b - c * d - e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c * d - e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c * d - e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c * d - e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b - c * d - e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c * d - e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c * d - e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c * d - e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b - c * d - (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c * d - (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c * d - (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c * d - (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(2) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b - c * d * e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c * d * e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c * d * e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c * d * e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b - c * d * e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c * d * e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c * d * e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c * d * e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b - c * d * e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c * d * e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c * d * e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c * d * e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b - c * d * (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c * d * (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c * d * (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c * d * (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(3) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b - c * (floor (d / e)) + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c * (floor (d / e)) + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c * (floor (d / e)) + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c * (floor (d / e)) + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b - c * (floor (d / e)) - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c * (floor (d / e)) - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c * (floor (d / e)) - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c * (floor (d / e)) - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b - c * (floor (d / e)) * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c * (floor (d / e)) * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c * (floor (d / e)) * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c * (floor (d / e)) * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b - c * (floor (floor (d / e)) / f) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - c * (floor (floor (d / e)) / f) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - c * (floor (floor (d / e)) / f) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - c * (floor (floor (floor (d / e)) / f) / g)))
                                             (set! answer2 (inexact->exact answer))])])])]
                   [(3) (case (+ op4)
                          [(0) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b - (floor (c / d)) + e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - (floor (c / d)) + e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - (floor (c / d)) + e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - (floor (c / d)) + e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b - (floor (c / d)) + e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - (floor (c / d)) + e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - (floor (c / d)) + e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - (floor (c / d)) + e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b - (floor (c / d)) + e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - (floor (c / d)) + e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - (floor (c / d)) + e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - (floor (c / d)) + e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b - (floor (c / d)) + (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - (floor (c / d)) + (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - (floor (c / d)) + (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - (floor (c / d)) + (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(1) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b - (floor (c / d)) - e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - (floor (c / d)) - e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - (floor (c / d)) - e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - (floor (c / d)) - e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b - (floor (c / d)) - e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - (floor (c / d)) - e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - (floor (c / d)) - e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - (floor (c / d)) - e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b - (floor (c / d)) - e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - (floor (c / d)) - e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - (floor (c / d)) - e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - (floor (c / d)) - e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b - (floor (c / d)) - (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - (floor (c / d)) - (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - (floor (c / d)) - (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - (floor (c / d)) - (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(2) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b - (floor (c / d)) * e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - (floor (c / d)) * e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - (floor (c / d)) * e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - (floor (c / d)) * e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b - (floor (c / d)) * e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - (floor (c / d)) * e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - (floor (c / d)) * e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - (floor (c / d)) * e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b - (floor (c / d)) * e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - (floor (c / d)) * e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - (floor (c / d)) * e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - (floor (c / d)) * e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b - (floor (c / d)) * (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - (floor (c / d)) * (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - (floor (c / d)) * (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - (floor (c / d)) * (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(3) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b - (floor (floor (c / d) / e)) + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - (floor (floor (c / d) / e)) + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - (floor (floor (c / d) / e)) + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - (floor (floor (c / d) / e)) + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b - (floor (floor (c / d) / e)) - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - (floor (floor (c / d) / e)) - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - (floor (floor (c / d) / e)) - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - (floor (floor (c / d) / e)) - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b - (floor (floor (c / d) / e)) * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - (floor (floor (c / d) / e)) * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - (floor (floor (c / d) / e)) * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - (floor (floor (c / d) / e)) * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b - (floor (floor (floor (c / d)) / e) / f) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b - (floor (floor (floor (c / d)) / e) / f) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b - (floor (floor (floor (c / d)) / e) / f) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b - (floor (floor (floor (floor (c / d) / e) / f) / g))))
                                             (set! answer2 (inexact->exact answer))])])])])]
            [(2) (case (+ op3)
                   [(0) (case (+ op4)
                          [(0) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b * c + d + e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c + d + e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c + d + e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c + d + e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b * c + d + e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c + d + e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c + d + e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c + d + e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b * c + d + e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c + d + e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c + d + e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c + d + e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b * c + d + (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c + d + (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c + d + (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c + d + (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(1) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b * c + d - e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c + d - e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c + d - e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c + d - e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b * c + d - e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c + d - e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c + d - e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c + d - e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b * c + d - e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c + d - e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c + d - e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c + d - e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b * c + d - (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c + d - (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c + d - (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c + d - (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(2) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b * c + d * e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c + d * e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c + d * e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c + d * e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b * c + d * e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c + d * e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c + d * e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c + d * e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b * c + d * e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c + d * e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c + d * e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c + d * e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b * c + d * (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c + d * (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c + d * (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c + d * (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(3) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b * c + (floor (d / e)) + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c + (floor (d / e)) + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c + (floor (d / e)) + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c + (floor (d / e)) + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b * c + (floor (d / e)) - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c + (floor (d / e)) - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c + (floor (d / e)) - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c + (floor (d / e)) - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b * c + (floor (d / e)) * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c + (floor (d / e)) * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c + (floor (d / e)) * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c + (floor (d / e)) * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b * c + (floor (floor (d / e)) / f) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c + (floor (floor (d / e)) / f) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c + (floor (floor (d / e)) / f) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c + (floor (floor (floor (d / e)) / f) / g)))
                                             (set! answer2 (inexact->exact answer))])])])]
                   [(1) (case (+ op4)
                          [(0) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b * c - d + e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c - d + e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c - d + e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c - d + e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b * c - d + e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c - d + e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c - d + e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c - d + e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b * c - d + e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c - d + e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c - d + e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c - d + e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b * c - d + (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c - d + (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c - d + (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c - d + (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(1) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b * c - d - e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c - d - e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c - d - e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c - d - e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b * c - d - e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c - d - e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c - d - e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c - d - e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b * c - d - e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c - d - e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c - d - e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c - d - e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b * c - d - (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c - d - (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c - d - (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c - d - (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(2) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b * c - d * e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c - d * e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c - d * e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c - d * e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b * c - d * e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c - d * e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c - d * e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c - d * e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b * c - d * e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c - d * e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c - d * e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c - d * e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b * c - d * (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c - d * (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c - d * (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c - d * (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(3) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b * c - (floor (d / e)) + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c - (floor (d / e)) + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c - (floor (d / e)) + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c - (floor (d / e)) + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b * c - (floor (d / e)) - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c - (floor (d / e)) - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c - (floor (d / e)) - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c - (floor (d / e)) - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b * c - (floor (d / e)) * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c - (floor (d / e)) * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c - (floor (d / e)) * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c - (floor (d / e)) * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b * c - (floor (floor (d / e)) / f) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c - (floor (floor (d / e)) / f) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c - (floor (floor (d / e)) / f) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c - (floor (floor (floor (d / e)) / f) / g)))
                                             (set! answer2 (inexact->exact answer))])])])]
                   [(2) (case (+ op4)
                          [(0) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b * c * d + e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c * d + e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c * d + e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c * d + e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b * c * d + e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c * d + e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c * d + e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c * d + e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b * c * d + e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c * d + e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c * d + e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c * d + e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b * c * d + (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c * d + (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c * d + (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c * d + (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(1) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b * c * d - e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c * d - e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c * d - e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c * d - e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b * c * d - e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c * d - e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c * d - e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c * d - e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b * c * d - e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c * d - e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c * d - e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c * d - e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b * c * d - (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c * d - (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c * d - (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c * d - (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(2) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b * c * d * e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c * d * e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c * d * e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c * d * e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b * c * d * e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c * d * e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c * d * e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c * d * e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b * c * d * e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c * d * e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c * d * e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c * d * e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b * c * d * (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c * d * (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c * d * (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c * d * (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(3) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b * c * (floor (d / e)) + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c * (floor (d / e)) + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c * (floor (d / e)) + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c * (floor (d / e)) + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b * c * (floor (d / e)) - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c * (floor (d / e)) - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c * (floor (d / e)) - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c * (floor (d / e)) - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b * c * (floor (d / e)) * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c * (floor (d / e)) * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c * (floor (d / e)) * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c * (floor (d / e)) * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b * c * (floor (floor (d / e)) / f) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * c * (floor (floor (d / e)) / f) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * c * (floor (floor (d / e)) / f) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * c * (floor (floor (floor (d / e)) / f) / g)))
                                             (set! answer2 (inexact->exact answer))])])])]
                   [(3) (case (+ op4)
                          [(0) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b * (floor (c / d)) + e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * (floor (c / d)) + e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * (floor (c / d)) + e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * (floor (c / d)) + e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b * (floor (c / d)) + e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * (floor (c / d)) + e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * (floor (c / d)) + e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * (floor (c / d)) + e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b * (floor (c / d)) + e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * (floor (c / d)) + e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * (floor (c / d)) + e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * (floor (c / d)) + e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b * (floor (c / d)) + (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * (floor (c / d)) + (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * (floor (c / d)) + (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * (floor (c / d)) + (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(1) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b * (floor (c / d)) - e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * (floor (c / d)) - e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * (floor (c / d)) - e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * (floor (c / d)) - e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b * (floor (c / d)) - e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * (floor (c / d)) - e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * (floor (c / d)) - e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * (floor (c / d)) - e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b * (floor (c / d)) - e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * (floor (c / d)) - e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * (floor (c / d)) - e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * (floor (c / d)) - e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b * (floor (c / d)) - (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * (floor (c / d)) - (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * (floor (c / d)) - (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * (floor (c / d)) - (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(2) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b * (floor (c / d)) * e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * (floor (c / d)) * e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * (floor (c / d)) * e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * (floor (c / d)) * e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b * (floor (c / d)) * e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * (floor (c / d)) * e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * (floor (c / d)) * e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * (floor (c / d)) * e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b * (floor (c / d)) * e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * (floor (c / d)) * e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * (floor (c / d)) * e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * (floor (c / d)) * e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b * (floor (c / d)) * (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * (floor (c / d)) * (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * (floor (c / d)) * (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * (floor (c / d)) * (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(3) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ b * (floor (floor (c / d) / e)) + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * (floor (floor (c / d) / e)) + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * (floor (floor (c / d) / e)) + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * (floor (floor (c / d) / e)) + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ b * (floor (floor (c / d) / e)) - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * (floor (floor (c / d) / e)) - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * (floor (floor (c / d) / e)) - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * (floor (floor (c / d) / e)) - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ b * (floor (floor (c / d) / e)) * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * (floor (floor (c / d) / e)) * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * (floor (floor (c / d) / e)) * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * (floor (floor (c / d) / e)) * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ b * (floor (floor (floor (c / d)) / e) / f) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ b * (floor (floor (floor (c / d)) / e) / f) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ b * (floor (floor (floor (c / d)) / e) / f) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ b * (floor (floor (floor (floor (c / d) / e) / f) / g))))
                                             (set! answer2 (inexact->exact answer))])])])])]
            [(3) (case (+ op3)
                   [(0) (case (+ op4)
                          [(0) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) + d + e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) + d + e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) + d + e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) + d + e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) + d + e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) + d + e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) + d + e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) + d + e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) + d + e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) + d + e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) + d + e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) + d + e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) + d + (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) + d + (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) + d + (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) + d + (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(1) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) + d - e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) + d - e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) + d - e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) + d - e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) + d - e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) + d - e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) + d - e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) + d - e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) + d - e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) + d - e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) + d - e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) + d - e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) + d - (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) + d - (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) + d - (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) + d - (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(2) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) + d * e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) + d * e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) + d * e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) + d * e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) + d * e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) + d * e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) + d * e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) + d * e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) + d * e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) + d * e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) + d * e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) + d * e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) + d * (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) + d * (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) + d * (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) + d * (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(3) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) + (floor (d / e)) + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) + (floor (d / e)) + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) + (floor (d / e)) + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) + (floor (d / e)) + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) + (floor (d / e)) - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) + (floor (d / e)) - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) + (floor (d / e)) - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) + (floor (d / e)) - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) + (floor (d / e)) * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) + (floor (d / e)) * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) + (floor (d / e)) * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) + (floor (d / e)) * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) + (floor (floor (d / e)) / f) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) + (floor (floor (d / e)) / f) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) + (floor (floor (d / e)) / f) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) + (floor (floor (floor (d / e)) / f) / g)))
                                             (set! answer2 (inexact->exact answer))])])])]
                   [(1) (case (+ op4)
                          [(0) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) - d + e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) - d + e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) - d + e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) - d + e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) - d + e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) - d + e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) - d + e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) - d + e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) - d + e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) - d + e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) - d + e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) - d + e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) - d + (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) - d + (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) - d + (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) - d + (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(1) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) - d - e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) - d - e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) - d - e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) - d - e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) - d - e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) - d - e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) - d - e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) - d - e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) - d - e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) - d - e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) - d - e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) - d - e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) - d - (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) - d - (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) - d - (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) - d - (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(2) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) - d * e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) - d * e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) - d * e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) - d * e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) - d * e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) - d * e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) - d * e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) - d * e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) - d * e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) - d * e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) - d * e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) - d * e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) - d * (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) - d * (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) - d * (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) - d * (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(3) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) - (floor (d / e)) + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) - (floor (d / e)) + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) - (floor (d / e)) + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) - (floor (d / e)) + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) - (floor (d / e)) - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) - (floor (d / e)) - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) - (floor (d / e)) - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) - (floor (d / e)) - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) - (floor (d / e)) * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) - (floor (d / e)) * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) - (floor (d / e)) * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) - (floor (d / e)) * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) - (floor (floor (d / e)) / f) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) - (floor (floor (d / e)) / f) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) - (floor (floor (d / e)) / f) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) - (floor (floor (floor (d / e)) / f) / g)))
                                             (set! answer2 (inexact->exact answer))])])])]
                   [(2) (case (+ op4)
                          [(0) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) * d + e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) * d + e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) * d + e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) * d + e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) * d + e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) * d + e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) * d + e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) * d + e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) * d + e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) * d + e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) * d + e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) * d + e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) * d + (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) * d + (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) * d + (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) * d + (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(1) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) * d - e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) * d - e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) * d - e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) * d - e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) * d - e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) * d - e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) * d - e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) * d - e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) * d - e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) * d - e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) * d - e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) * d - e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) * d - (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) * d - (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) * d - (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) * d - (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(2) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) * d * e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) * d * e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) * d * e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) * d * e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) * d * e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) * d * e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) * d * e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) * d * e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) * d * e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) * d * e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) * d * e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) * d * e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) * d * (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) * d * (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) * d * (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) * d * (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(3) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) * (floor (d / e)) + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) * (floor (d / e)) + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) * (floor (d / e)) + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) * (floor (d / e)) + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) * (floor (d / e)) - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) * (floor (d / e)) - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) * (floor (d / e)) - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) * (floor (d / e)) - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) * (floor (d / e)) * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) * (floor (d / e)) * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) * (floor (d / e)) * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) * (floor (d / e)) * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ (floor (b / c)) * (floor (floor (d / e)) / f) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (b / c)) * (floor (floor (d / e)) / f) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (b / c)) * (floor (floor (d / e)) / f) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (b / c)) * (floor (floor (floor (d / e)) / f) / g)))
                                             (set! answer2 (inexact->exact answer))])])])]
                   [(3) (case (+ op4)
                          [(0) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ (floor (floor (b / c) / d)) + e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (floor (b / c) / d)) + e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (floor (b / c) / d)) + e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (floor (b / c) / d)) + e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ (floor (floor (b / c) / d)) + e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (floor (b / c) / d)) + e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (floor (b / c) / d)) + e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (floor (b / c) / d)) + e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ (floor (floor (b / c) / d)) + e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (floor (b / c) / d)) + e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (floor (b / c) / d)) + e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (floor (b / c) / d)) + e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ (floor (floor (b / c) / d)) + (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (floor (b / c) / d)) + (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (floor (b / c) / d)) + (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (floor (b / c) / d)) + (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(1) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ (floor (floor (b / c) / d)) - e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (floor (b / c) / d)) - e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (floor (b / c) / d)) - e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (floor (b / c) / d)) - e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ (floor (floor (b / c) / d)) - e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (floor (b / c) / d)) - e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (floor (b / c) / d)) - e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (floor (b / c) / d)) - e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ (floor (floor (b / c) / d)) - e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (floor (b / c) / d)) - e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (floor (b / c) / d)) - e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (floor (b / c) / d)) - e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ (floor (floor (b / c) / d)) - (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (floor (b / c) / d)) - (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (floor (b / c) / d)) - (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (floor (b / c) / d)) - (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(2) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ (floor (floor (b / c) / d)) * e + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (floor (b / c) / d)) * e + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (floor (b / c) / d)) * e + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (floor (b / c) / d)) * e + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ (floor (floor (b / c) / d)) * e - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (floor (b / c) / d)) * e - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (floor (b / c) / d)) * e - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (floor (b / c) / d)) * e - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ (floor (floor (b / c) / d)) * e * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (floor (b / c) / d)) * e * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (floor (b / c) / d)) * e * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (floor (b / c) / d)) * e * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ (floor (floor (b / c) / d)) * (floor (e / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (floor (b / c) / d)) * (floor (e / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (floor (b / c) / d)) * (floor (e / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (floor (b / c) / d)) * (floor (floor (e / f)) / g)))
                                             (set! answer2 (inexact->exact answer))])])]
                          [(3) (case (+ op5)
                                 [(0) (case (+ op6)
                                        [(0) (set! answer ($ (floor (floor (floor (b / c) / d) / e)) + f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (floor (floor (b / c) / d) / e)) + f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (floor (floor (b / c) / d) / e)) + f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (floor (floor (b / c) / d) / e)) + (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(1) (case (+ op6)
                                        [(0) (set! answer ($ (floor (floor (floor (b / c) / d) / e)) - f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (floor (floor (b / c) / d) / e)) - f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (floor (floor (b / c) / d) / e)) - f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (floor (floor (b / c) / d) / e)) - (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(2) (case (+ op6)
                                        [(0) (set! answer ($ (floor (floor (floor (b / c) / d) / e)) * f + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (floor (floor (b / c) / d) / e)) * f - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (floor (floor (b / c) / d) / e)) * f * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (floor (floor (b / c) / d) / e)) * (floor (f / g))))
                                             (set! answer2 (inexact->exact answer))])]
                                 [(3) (case (+ op6)
                                        [(0) (set! answer ($ (floor (floor (floor (floor (b / c) / d) / e) / f)) + g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(1) (set! answer ($ (floor (floor (floor (floor (b / c) / d) / e) / f)) - g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(2) (set! answer ($ (floor (floor (floor (floor (b / c) / d) / e) / f)) * g))
                                             (set! answer2 (inexact->exact answer))]
                                        [(3) (set! answer ($ (floor (floor (floor (floor (floor (b / c) / d) / e) / f) / g))))
                                             (set! answer2 (inexact->exact answer))])])])])]))

      (draw-text-problem-with-multiple-lines device-context (list "Solve the following equation: " (get-output-string o)))

      )


    
    )
  )



(provide equation-generator%)
         





