#lang racket

(define difficulty-class%
         (class object%
           (super-new)
           (init-field
            id
            name
            )
           )
  )

(define difficulty-manager%
  (class object%
    (super-new)
    (init-field
     list-of-difficulty-class
     )
    
    )
  )

(define ELEMENTARY-SCHOOL (0 "Elementary"))
(define MIDDLE-SCHOOL (1 "Middle"))
(define HIGH-SCHOOL (2 "High"))
(define ARITHMETIC (3 "Arithmetic"))
(define ALGEBRA (4 "Algebra"))
(define GEOMETRY (5 "Geometry"))



