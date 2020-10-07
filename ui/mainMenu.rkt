#lang racket
(require racket/gui/base)

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
     function
     string-function-pair-list
     )
    (super-new)
    
    (define mainMenu (new panel%
      [parent givenParent]
     ))
    (define menuWrapper (new panel%
                             [parent mainMenu]
                             [enabled #t]))

    ; Event handlers
    
    ; Calls the function that is paired with the name of the button that fired the event
    ; (using the string-function-pair-list field)
    (define (button-handler button event)
      (button-handler-helper button string-function-pair-list)
      )

    ; button-handler helper
    ; Iterates through the given-string-function-pair-list, fires the function
    ; that is paired with the label of the button that called button-handler in the list
    ; First argument is the button that fired the event
    ; Second argument is the string-function-pair-list of the current mainMenu instance
    
    (define (button-handler-helper button given-string-function-pair-list)
      (cond
        [(empty? given-string-function-pair-list) 0]
        [(equal? (send button get-label) (first (first given-string-function-pair-list)))
         ((first (rest (first given-string-function-pair-list))))]
        [else (button-handler-helper button (rest given-string-function-pair-list))]
        )
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
    
    ; Private syntax definitions

    ; Creates an anonymous button%
    ; name is the label of the button
    ; Function is the function for the button
    ; Parent is the parent container to attach the button to
    (define-syntax create-button
      (syntax-rules ()
              [(create-button name function parent)
               (new button%
                                 [callback button-handler]
                                 [label name]
                                 [parent parent])]))

    ; Private Functions

    ; Initializes buttons, attaches them to a container
    ; First input is a list of pairs of strings (for the label of the button) and functions
    ; Attaches the buttons to the given parent
    (define (initialize-buttons given-string-function-pair-list parent)
      (cond
        [(empty? given-string-function-pair-list) 0]
        [else (create-button (first (first given-string-function-pair-list))
                             (rest (first given-string-function-pair-list))
                             parent)
              (initialize-buttons (rest given-string-function-pair-list) parent)]))
    


    (initialize-buttons string-function-pair-list menu)


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