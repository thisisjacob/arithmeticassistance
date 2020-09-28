#lang racket
(require racket/gui/base)

(define mainMenuUI%
  (class object%
    (init-field
     givenParent
     )
    (super-new)
    (define mainMenu (new panel%
      [parent givenParent]
     ))
    (define menuWrapper (new panel%
                             [parent mainMenu]
                             [style '(border)]
                             [alignment '(center center)]
                             [enabled #t]))
    (define menu (new vertical-panel%
                      [parent menuWrapper]
                      [alignment '(center center)]
                      [style '(border)]
                      [vert-margin 100]
                      [horiz-margin 100]
                      [enabled #t])
      )
    (define message (new message%
                         [parent menu]
                         [label "Testing Menu"]))
    ; test buttons - remove these buttons later in development
    (define testButton1 (new button%
                             [label "Test"]
                             [parent menu]
                             [stretchable-width #t]
                             [stretchable-height #t])) 
    (define testButton2 (new button%
                             [label "Test"]
                             [parent menu]
                             [stretchable-width #t]
                             [stretchable-height #t])) 
    (define testButton3 (new button%
                             [label "Test"]
                             [parent menu]
                             [stretchable-width #t]
                             [stretchable-height #t]))

    ; Functions
    (define/public (switchTo)
      (send mainMenu show #t)
      )
   ))

(provide mainMenuUI%)