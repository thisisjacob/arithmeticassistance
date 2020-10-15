#lang racket
(require racket/gui/base)

; This class is for generating, storing and managing multiple buttons without requiring the programmer to manually generate
; each button
; Initialization Paramenters
; givenParent: a parent container to hold the buttons
; string-function-pair-list: a list of lists, each holding a string in the first item, and a function to be called by the button
; in the second
; Public Functions
; getButtons: returns a list of all the buttons
(define buttonGeneratorAndManager%
  (class object%
    (init-field
     givenParent
     string-function-pair-list
     )
    (super-new)

    ; Calls the function that is paired with the name of the button that was fired
    ; (using the string-function-pair-list field)
    (define (button-handler button event)
      (button-handler-helper button string-function-pair-list)
      )

    ; Helper for the button-handler function
    (define (button-handler-helper button given-string-function-pair-list)
      (cond
        [(empty? given-string-function-pair-list) 0]
        [(equal? (send button get-label) (first (first given-string-function-pair-list)))
         ((first (rest (first given-string-function-pair-list))))]
        [else (button-handler-helper button (rest given-string-function-pair-list))]
        )
      )

    ; Syntax for creating a button
    ; syntax is (create-button "name" "parent")
    (define-syntax create-button
      (syntax-rules ()
        [(create-button name parent)
         (new button%
              [callback button-handler]
              [label name]
              [parent parent])]))

    ; Initializes buttons, attaches them to a container
    ; First input is a list of pairs of strings (for the label of the button) and functions
    ; Attaches the buttons to the given parent
    (define (initialize-buttons given-string-function-pair-list parent)
      (cond
        [(empty? given-string-function-pair-list) 0]
        [else (create-button (first (first given-string-function-pair-list))
                             parent)
              (initialize-buttons (rest given-string-function-pair-list) parent)]))

    ; holds all the buttons
    (define buttons
      (initialize-buttons string-function-pair-list givenParent)
      )

    ; returns the list of buttons
    (define/public (getButtons)
      buttons)

    )
  )

(provide buttonGeneratorAndManager%)