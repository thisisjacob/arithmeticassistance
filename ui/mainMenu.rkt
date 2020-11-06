#lang racket
(require racket/gui/base)
(require "../utilities/buttonFunctionsAndGenerators.rkt")
(require "../constants/userInterfaceConstants.rkt")

; A class for creating the main menu of the program
; Instance Parameters:
; givenParent: the container that will hold the main menu
; Public Functions:
; enable: enables visbility of the mainMenu
; disable: disables visibility of the mainMenu

(define mainMenuUI%
  (class object%
    (init-field
     givenParent
     string-function-pair-list
     )
    (super-new)
    
    (define mainMenu (new panel%
      [parent givenParent]
     ))
    
    (define menuWrapper (new panel%
                             [parent mainMenu]))
      
    (define menu (new vertical-panel%
                      [style '(border)]
                      [parent menuWrapper]
                      [horiz-margin containerHorizMargin]
                      [vert-margin containerVerticalMargin]
                      [spacing 5]))

     (define header (new message%
                 [font menuTitleFont]
                 [parent menu]
                 [label "Main Menu"]))

    ; Public Functions
    (define/public (enable)
      (send mainMenu show #t)
      )
    (define/public (disable)
      (send mainMenu show #f)
      )
    ;the button manager for managing menu's buttons
    (define buttons (new buttonGeneratorAndManager%
                         [givenParent menu]
                         [functionList string-function-pair-list]
                         )
      )
    (send buttons initialize-buttons-args)
    


   )
  )



(provide mainMenuUI%)