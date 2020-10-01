; For creating a labelled menu holding buttons
; Instantiation parameters:
; givenParent - the container for this instance
; givenHorizontalMargin - the horizontal margin between the container and menu
; givenVerticalMargin - the vertical margin between the containe rand menu
; givenButtonSpacing - the spacing between the buttons and labels of this menu
; givenMenuLabelText - the text of the label

; Functions:
; activate: enables the menu
; deactivate: disables the menu

#lang racket
(require racket/gui/base)

(define buttonMenu%
  (class object%
    (init-field
     givenParent
     givenHorizontalMargin
     givenVerticalMargin
     givenButtonSpacing
     givenMenuLabelText
     )
    (super-new)
    (define menu (new vertical-panel%
                      [parent givenParent]
                      [style '(border)]
                      [alignment '(center center)]
                      [horiz-margin givenHorizontalMargin]
                      [vert-margin givenVerticalMargin]
                      [spacing givenButtonSpacing]))
    (define message (new message%
                      [parent menu]
                      [label givenMenuLabelText]))
     ; test buttons - remove these buttons later in development, replace with a method for passing object functions
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
    (define/public (activate)
      (send menu show #t)
      )
    (define/public (deactivate)
      (send menu show #f)
      )
    )
  )

(provide buttonMenu%)