#lang racket
(require racket/gui/base)

(define gradeAndDifficultySelectScreen%
         (class object%
           (init-field
            givenParent
            menuReturnFunction
            )
           (super-new)

           (define pageWrapper (new frame%
                                    [label "Difficulty Selection"]
                                    [width 800]
                                    [height 800]
                                    [style '(no-resize-border)]
                                    )
             )
           (define menuWrapper (new panel%
                                 [parent pageWrapper]
                                 )
             )

           (define menu (new vertical-panel%
                             [style '(border)]
                             [parent menuWrapper]
                             [spacing 5]))


           
           (define (return-callback button event)
             (menuReturnFunction)
             )

           (define testButtonOne (new button%
                                      [parent menu]
                                      [label "TEST BUTTON"]
                                      [callback return-callback]
                                      )
             )

           ; Public Functions
           (define/public (enable)
             (send pageWrapper show #t)
             )
           (define/public (disable)
             (send pageWrapper show #f)
             )

             
           )
  )


(provide gradeAndDifficultySelectScreen%)