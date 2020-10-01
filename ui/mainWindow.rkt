; Initializes the window and pages for the program's user interface
; call the function startUI to activate the user interface

#lang racket
(require racket/gui/base)
(require "mainMenu.rkt")

(define mainWindow%
  (class object%
    (init-field
     title
     )
    (super-new)
    (define mainFrame (new frame%
                           [label title]
                           [width 800]
                           [height 800]
                           [style '(no-resize-border)]))
    (define mainMenu (new mainMenuUI% [givenParent mainFrame]))
    (send mainMenu switchTo)

    ;Functions
    (define/public (startUI)
      (send mainFrame show #t)
    )
  )
)

(provide mainWindow%)



