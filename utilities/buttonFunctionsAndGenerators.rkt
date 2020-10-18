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
     functionList
     )
    (super-new)

    ; Calls the function that is paired with the name of the button that was fired
    ; (using the string-function-pair-list field)
    (define (button-handler button event)
      (button-handler-helper button functionList)
      )


    (define (button-handler-with-args button event)
      (button-handler-helper-with-args button functionList)
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

    (define (button-handler-helper-with-args button given-string-function-args-pair-list)
      (print "testing")
      (cond
        [(empty? given-string-function-args-pair-list) 0]
        [(print (send button get-label)) (print (first (first given-string-function-args-pair-list)))
         (equal? (send button get-label) (first (first given-string-function-args-pair-list)))
         ((first (rest (first given-string-function-args-pair-list))))]
        [else (button-handler-helper button (rest given-string-function-args-pair-list))]
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

    (define-syntax create-button-with-list-args
      (syntax-rules ()
        [(create-button-with-list-args name parent list-args)
         (new button%
              [callback button-handler-with-list-args]
              [label name]
              [parent parent])]))

    ; Initializes buttons, attaches them to a container
    ; First input is a list of pairs of strings (for the label of the button) and functions
    ; Attaches the buttons to the given parent
    ; NOTE: Should only be used if the given functions do not need arguments passed
    (define (initialize-buttons given-string-function-pair-list parent)
      (cond
        [(empty? given-string-function-pair-list) 0]
        [else (create-button (first (first given-string-function-pair-list))
                             parent)
              (initialize-buttons (rest given-string-function-pair-list) parent)]))
    
    (define (initialize-buttons-with-args given-string-function-arg-list parent)
      (cond
        [(empty? given-string-function-arg-list) 0]
        [else (create-button (first (first (first given-string-function-arg-list)))
                             parent)
              (initialize-buttons-with-args (rest given-string-function-arg-list) parent)]))
    

    ; holds all the buttons
    ; must be initialized by the initialize function
    (define buttons null)

    (define/public (initialize-buttons-no-arg)
      (set! buttons (initialize-buttons functionList givenParent))
      )
    (define/public (initialize-buttons-args)
      (set! buttons (initialize-buttons-with-args functionList givenParent))
      )
    ; returns the list of buttons
    (define/public (getButtons)
      buttons)
    )
  )

(provide buttonGeneratorAndManager%)