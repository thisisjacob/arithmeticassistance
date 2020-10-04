#lang racket
(require racket/gui/base)

; The screen for drawing problems and entering responses
; Initialization Arguments:
; givenParent : the container element of this instance
; Public Functions:
; enable : enables visibility of this object instance
; disable: disables visibility of this object instance


(define drawInputMenuUI%
  (class object%
    (init-field
     givenParent
     )
    (super-new)
    (define drawingInputMenu (new vertical-panel%
                                  [parent givenParent]
                                  ))
    (define drawingCanvas (new canvas%
                               [parent drawingInputMenu]
                               [style '(border)]
                               [min-width 600]
                               [min-height 400]
                               ))
    (define inputPanel (new horizontal-panel%
                            [parent drawingInputMenu]
                            [style '(border)]
                            [min-width 600]
                            [min-height 200]))
    (define message (new message%
                         [parent inputPanel]
                         [label "ENTER HERE TEST 123 TEST TEST TEST"]))
    (define textEnter (new text-field%
                           [parent inputPanel]
                           [label "Enter Answer:"]))
    (define submit (new button%
                        [parent inputPanel]
                        [label "Submit"]))

    (define/public (enable)
      (send drawingInputMenu show #t))
    (define/public (disable)
      (send drawingInputMenu show #f))

    )
 )

(provide drawInputMenuUI%)