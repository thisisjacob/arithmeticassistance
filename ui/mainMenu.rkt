#lang racket
(require racket/gui/base)
(require "../utilities/buttonFunctionsAndGenerators.rkt")
(require "../constants/userInterfaceConstants.rkt")

; An interface class for the main menu of the program
; Instance Parameters:
; givenParent: the container that will hold the main menu
; string-function-pair-list: a list of pairs of strings and functions for generating buttons,
;     where the string is the text of the button, and the function is the called function
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

    ; RacketGUI definitions
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
    ; Enables visibility of the window
    (define/public (enable)
      (send mainMenu show #t)
      )
    ; Disables visibility of the window
    (define/public (disable)
      (send mainMenu show #f)
      )
    
    ; Creates a list of buttons attached to menu
    (define buttons (new buttonGeneratorAndManager%
                         [givenParent menu]
                         [functionList string-function-pair-list]
                         )
      )
    (send buttons initialize-buttons-args)
    


   )
  )



(provide mainMenuUI%)