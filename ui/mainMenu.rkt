#lang racket
(require racket/gui/base)

; A class for creating the main menu of the program
; Instance Parameters:
; givenParent: the container that will hold the main menu
; Functions:
; enable: enables visbility of the mainMenu
; disable: disables visibility of the mainMenu

(define mainMenuUI%
  (class object%
    (init-field
     givenParent
     function
     string-function-pair-list
     )
    (super-new)

    ; Syntax definitions
    (define-syntax create-button
      (syntax-rules ()
              [(create-button name function parent)
               (new button%
                                 [callback button-handler]
                                 [label name]
                                 [parent parent])]))

    ; Private Functions
    (define (initialize-buttons)
      (map (lambda (i)
             (create-button (car i) (cdr i) menu))
           (string-function-pair-list))
      )
    
    (define mainMenu (new panel%
      [parent givenParent]
     ))
    (define menuWrapper (new panel%
                             [parent mainMenu]
                             [enabled #t]))

     ; Event handler
    (define (button-handler button event)
      (function)
      )
      
    (define menu (new vertical-panel%
                      [style '(border)]
                      [parent menuWrapper]
                      [horiz-margin 200]
                      [vert-margin 100]
                      [spacing 5]))

     (define header (new message%
                 [parent menu]
                 [label "Main Menu"]))

    initialize-buttons
    
     ; testing buttons
    ; TODO: CREATE A FUNCTION FOR HANDLING A LIST OF PAIRS OF STRINGS AND FUNCTIONS
    ; AND GENERATING THE BUTTONS FROM THAT LIST
    (define enableMainMenu (new button%
                                [callback button-handler]
                                [label "Enable"]
                                [parent menu]))
    (define disableMainMenu (new button%
                                 [callback button-handler]
                                 [label "Disable"]
                                 [parent menu]))


    ; Public Functions
    (define/public (enable)
      (send mainMenu show #t)
      )
    (define/public (disable)
      (send mainMenu show #f)
      )


    

    
   )
  )



(provide mainMenuUI%)