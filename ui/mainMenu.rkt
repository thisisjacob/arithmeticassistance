#lang racket
(require racket/gui/base)
(require "widgets/buttonMenu.rkt")

; A class for creating the main menu of the program
; 
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
                             [enabled #t]))
    (define menu (new buttonMenu%
                      [givenParent menuWrapper]
                      [givenHorizontalMargin 200]
                      [givenVerticalMargin 100]
                      [givenButtonSpacing 5]
                      [givenMenuLabelText "Main Menu"]))


    ; Functions
    (define/public (switchTo)
      (send mainMenu show #t)
      )
    (define/public (switchFrom)
      (send mainMenu show #f)
      )
   ))

(provide mainMenuUI%)