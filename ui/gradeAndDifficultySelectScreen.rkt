#lang racket
(require racket/gui/base)
(require "widgets/percentButton.rkt")
(require "../constants/userInterfaceConstants.rkt")

; The screeen for selecting a grade level or category/difficulty

; Initialization Arguments:
; menuReturnFunction: a function for returning from this page and to a given return page
; mode: a class representing the current mode of play NOTE: give a placeholder value for now because
; this feature is still under construction

; Public Functions:
; enable : enables visibility of this object instance
; disable: disables visibility of this object instance

(define gradeAndDifficultySelectScreen%
         (class object%
           (init-field
            menuReturnFunction
            mode
            )
           (super-new)

           (define pageWrapper (new frame%
                                    [label "Difficulty Selection"]
                                    [width frameWidthAndHeight]
                                    [height frameWidthAndHeight]
                                    [style frameStyle]
                                    )
             )
           (define menuWrapper (new panel%
                                 [parent pageWrapper]
                                 [horiz-margin 200]
                                 [vert-margin 200]
                                 )
             )

           (define menu (new vertical-panel%
                             [style containerStyle]
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

           (define testButtonOne (new percentButton%
                                      [givenParent menu]
                                      [buttonLabel "TEST Elementary"]
                                      [percentage "0.2%"]
                                      [buttonFunction return-callback]
                                      )
             )
                      (define testButtonTwo (new percentButton%
                                      [givenParent menu]
                                      [buttonLabel "TEST Middle"]
                                      [percentage "0.2%"]
                                      [buttonFunction return-callback]
                                      )
             )
                      (define testButtonThree (new percentButton%
                                      [givenParent menu]
                                      [buttonLabel "TEST High"]
                                      [percentage "0.2%"]
                                      [buttonFunction return-callback]
                                      )
             )
           
           (define categoryHeader (new message%
                                       [parent menu]
                                       [label "Categories of Problems"]
                                       )
             )
           (define testButtonFour (new percentButton%
                                      [givenParent menu]
                                      [buttonLabel "TEST Arithmetic"]
                                      [percentage "0.2%"]
                                      [buttonFunction return-callback]
                                      )
             )
           (define testButtonFive (new percentButton%
                                      [givenParent menu]
                                      [buttonLabel "TEST Algebra"]
                                      [percentage "0.2%"]
                                      [buttonFunction return-callback]
                                      )
             )
           (define testButtonSix (new percentButton%
                                      [givenParent menu]
                                      [buttonLabel "TEST Geometry"]
                                      [percentage "0.2%"]
                                      [buttonFunction return-callback]
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