#lang racket
(require racket/gui/base)
(require math/base)
(require racket/gui)
(require k-infix)

(define equation-generator%
  (class object%
    (init-field)
    (super-new)

    (define answer null)
    
    (define/public (getAnswer)
      answer)

    (define/public (middleSchoolArithmeticProblem)
      (define a (random-natural 25))
      (define b (random-natural 25))
      (define c (random-natural 25))
      (define d (random-natural 25))

      (define op1 (random-natural 4))
      (define op2 (random-natural 4))
      (define op3 (random-natural 4))
      (define o (open-output-string))

      (set! answer 0)

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
      
      o
      )
    
    )
  )

(provide equation-generator%)
         

