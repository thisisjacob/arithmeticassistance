#lang racket
(require racket/gui/base)

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