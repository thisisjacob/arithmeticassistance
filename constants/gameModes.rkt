#lang racket
; Defines a small class for holding information on a game mode
; Defines a list named game-modes that is is just a list of available gamemodes for the program
; This class should be used to hold information on what is available - it will not implement these gamemodes

(define game-mode%
         (class object%
           (super-new)
           (init-field
            id
            name
            )
           (define/public (getId)
             id
             )
           (define/public (getName)
             name
             )
           )
  )
                  
; A list holding all of the available game modes
(define game-modes (list
                    (new game-mode%
                         [id 0]
                         [name "Free Practice"]
                         )
                    (new game-mode%
                         [id 1]
                         [name "Multiplayer"]
                         )
                    )
  )


(provide game-modes)
