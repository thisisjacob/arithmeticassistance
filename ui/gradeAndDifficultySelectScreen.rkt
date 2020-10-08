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
                                 [horiz-margin 200]
                                 [vert-margin 200]
                                 )
             )

           (define menu (new vertical-panel%
                             [style '(border)]
                             [parent menuWrapper]
                             [spacing 5]))


           
           (define (return-callback button event)
             (menuReturnFunction)
             )

           (define gradesHeader (new message%
                                     [parent menu]
                                     [label "Grades"]
                                     )
             )

           (define testButtonOne (new button%
                                      [parent menu]
                                      [label "TEST Elementary"]
                                      [callback return-callback]
                                      )
             )
           (define testButtonTwo (new button%
                                      [parent menu]
                                      [label "TEST Middle"]
                                      [callback return-callback]
                                      )
             )
           (define testButtonThree (new button%
                                        [parent menu]
                                        [label "TEST High"]
                                        [callback return-callback]
                                        )
             )

           (define categoryHeader (new message%
                                       [parent menu]
                                       [label "Categories of Problems"]
                                       )
             )
           (define testButtonFour (new button%
                                       [parent menu]
                                       [label "TEST Arithmetic"]
                                       [callback return-callback]
                                       )
             )
           (define testButtonFive (new button%
                                       [parent menu]
                                       [label "TEST Algebra"]
                                       [callback return-callback]
                                       )
             )
           (define testButtonSix (new button%
                                      [parent menu]
                                      [label "TEST Geometry"]
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