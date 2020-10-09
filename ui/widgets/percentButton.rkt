#lang racket
(require racket/gui/base)


; A button widget that has a label next to it intended for storing percentage values
; Inputs:
; givenParent : the parent container of the button
; buttonLabel: the text on the button
; percentage: equivalent to the input for buttonLabel - placed next to the button
; buttonFunction: a function in the form of (function button event) - assigned as a callback to the button

(define percentButton%
  (class object%
    (init-field
     givenParent
     buttonLabel
     percentage
     buttonFunction
     )
    (super-new)

    (define (callback-function button event)
      (buttonFunction button event)
      )
    
    (define button-wrapper (new horizontal-panel%
                                [parent givenParent]
                                [alignment '(center center)]
                                )
      )

    (define button (new button%
                        [parent button-wrapper]
                        [callback callback-function]
                        [label buttonLabel]
                        )
      )

    (define percetangeMessage (new message%
                                   [parent button-wrapper]
                                   [label percentage]
                                   )
      )
    )
  )

(provide percentButton%)