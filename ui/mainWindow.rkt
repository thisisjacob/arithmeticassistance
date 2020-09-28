#lang racket
(require racket/gui/base)
(require "mainMenu.rkt")

(define mainWindow%
  (class object%
    (init-field
     title
     )
    (super-new)
    (define mainFrame (new frame% [label title]))
    (define mainMenu (new mainMenuUI% [givenParent mainFrame]))
    (send mainMenu switchTo)

    ;Functions
    (define/public (startUI)
      (send mainFrame show #t)
    )
  )
)

(provide mainWindow%)



