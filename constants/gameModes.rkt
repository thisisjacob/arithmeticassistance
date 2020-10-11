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
                  

(define game-modes (
                    (new game-mode%
                         [id 0]
                         [name "Free Practice"]
                         )
                    (new game-mode%
                         [id 1]
                         [name "Linked Mode"]
                         )
                    (new game-mode%
                         [id 2]
                         [name "Multiplayer"]
                         )
                    )
  )


(provide game-modes)
