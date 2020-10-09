#lang racket

(define game-mode%
         (class object%
           (super-new)
           (init-field
            id
            name
            )
           )
  )

(define game-mode-manager%
  (class object%
    (super-new)
    (init-field
     mode-list
     )
    )
  )

(define free (new game-mode%
                  





