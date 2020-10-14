#lang racket

; A class representing the selection or existence of a category or difficulty
; Initialization Parameters:
; id: a unique numeric id for the class
; name: name of the problem (should be human readable, first letter capitalized)
(define difficulty-class%
         (class object%
           (super-new)
           (init-field
            id
            name
            )
           (define/public (getId)
             id
             )
           )
  )

; A class for managing and interacting with multiple difficulty-class%
; Initialization Parameters:
; list-of-difficulty-class: the list to be stored in the manager

; Public Functions
; list-of-categories: returns the list of saved categories
; get-category-with-id: enter an id of a valid class, returns a difficulty-class if a difficulty class with that id is found. returns 0 if no such class is found
(define difficulty-manager%
  (class object%
    (super-new)
    (init-field
     list-of-difficulty-class
     )
    (define/public (list-of-categories)
      list-of-difficulty-class)
    (define/public (get-category-with-id id)
      (category-helper list-of-difficulty-class id)
      )

    (define (category-helper list id)
      (cond
        [(empty? list) 0]
        [(equal? (id) (send (first list) getId)) (send (first list) getId)]
        [else (category-helper (rest list) id)]
        )
      )
        

    )
  )

; pre defined difficulty classes
; Modify these to change the available categories throughout the entire program
(define ELEMENTARY-SCHOOL (new difficulty-class%
                               [id 0]
                               [name "Elementary"]
                               )
  )
(define MIDDLE-SCHOOL (new difficulty-class%
                               [id 1]
                               [name "Middle"]
                               )
  )
(define HIGH-SCHOOL (new difficulty-class%
                               [id 2]
                               [name "High School"]
                               )
  )
(define ARITHMETIC (new difficulty-class%
                               [id 3]
                               [name "Arithmetic"]
                               )
  )
(define ALGEBRA (new difficulty-class%
                               [id 4]
                               [name "Algebra"]
                               )
  )
(define GEOMETRY (new difficulty-class%
                               [id 5]
                               [name "Geometry"]
                               )
  )

; The predefined manager to be used throughout the program
; Use this to pass information on difficulties and categories throughout the program
(define difficulties (new difficulty-manager%
                          [list-of-difficulty-class (list
                                                     ELEMENTARY-SCHOOL
                                                     MIDDLE-SCHOOL
                                                     HIGH-SCHOOL
                                                     ARITHMETIC
                                                     ALGEBRA
                                                     GEOMETRY
                                                     )
                                                    ]
                          )
  )

(provide difficulties)



