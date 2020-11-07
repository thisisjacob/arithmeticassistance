#lang racket
(require racket/gui/base)
(require math/base)
(require racket/gui)
(require k-infix)
(require "../constants/userInterfaceConstants.rkt")
(require "../constants/gameModes.rkt")
(require "../drawing/canvasShapeDrawingFunctions.rkt")
(require "../logic/equationGenerator.rkt")

<<<<<<< HEAD
; GUI Class:

=======
>>>>>>> eef93b409c68fc836db4b2b91297de757d791934
; The screen for drawing problems and entering responses
; Initialization Arguments:
; givenParent : the container element of this instance
; menuReturnFunction: a function for returning from this page and to a given return page
; mode: the mode class construct (as in free practice vs linked practice) NOTE: currently under construction, this will be required in the future
; category: the category of problems calss construct NOTE: currently under construction, this will be required in the future
; Public Functions:
; enable : enables visibility of this object instance
; disable: disables visibility of this object instance

(define drawInputMenuUI%
  (class object%
    (init-field
     givenParent
     menuReturnFunction
     )
    (super-new)

    (define test (new equation-generator%)
      )

    ; Internal constants
    (define playerOne "Player 1")
    (define playerTwo "Player 2")

    ; Gameplay state
    ; currentGameMode holds the current gameMode construct of the drawingInputScreen
    ; These constructs are stored in constants/gameModes.rkt
    ; This should be used to decide the behavior of the problems screen
    (define currentGameMode null)
    ; currentProblemCategory holds the difficulty-class construct of the drawingInputScreen
    ; These constructs are stored in constants/difficultiesAndCategories.rkt
    ; This construct should be used to decide which problems to generate
    (define currentProblemCategory null)

    (define playerOneScore 0)
    (define playerTwoScore 0)
    (define currentPlayer playerOne)

    (define problems (new equation-generator%)
      )


    ; A callback function for rendering problems to the canvas
    ; Erases the canvas at the start of each call to allow for updated screens
    ; Currently needs: information provided that will tell the program which
    ; shape to draw
    (define (canvasPaintingCallbackFunction canvas dc)
      ; set-background not needed here - kept to keep appearance contained into one location
      ; problem description menu appearance is changed in the drawing class
      (send (send drawingCanvas get-dc) set-background problemCanvasBackground)
      (send (send drawingCanvas get-dc) set-pen penBackgroundColor penWidth penStyle)
      (send dc clear)
      ; Draws score if set to multiplayer
      (cond
        [(eq? currentGameMode (first (rest game-modes)))
         (draw-score dc currentPlayer playerOneScore playerTwoScore)
         ]
        )
      (send problems generateProblem (send drawingCanvas get-dc) currentProblemCategory)
      )

    ; Callback definitions
    ; Fired when the user submits an answer
    ; Updates the score of each player and switches the current player (only relevant if in multiplayer)
    ; Tells the user with a textbox whether the answer was correct or not
    (define (submit-callback b e)
      (let ((text (send textEnter get-value)))
        ; increases score of player who successfully answers the question
        (cond [(string=? text (number->string (send problems getAnswer)))
               (cond [(eq? currentPlayer playerOne)
                      (set! playerOneScore (+ playerOneScore 1))
                      ]
                     [else
                      (set! playerTwoScore (+ playerTwoScore 1))
                      ]
                     )
               ]

              [(string=? text (number->string (send problems getRound)))
               (cond [(eq? currentPlayer playerOne)
                      (set! playerOneScore (+ playerOneScore 1))
                      ]
                     [else
                      (set! playerTwoScore (+ playerTwoScore 1))
                      ]
                     )
               ]
              )
        ; switches current player
        (cond [(eq? currentPlayer playerOne)
               (set! currentPlayer playerTwo)]
              [else
               (set! currentPlayer playerOne)
               ])
        ; tells user whether their answer was correct

        
        
        (if (or (string=? text (number->string (send problems getAnswer)))
                (string=? text (number->string (send problems getRound))))
            (message-box "Good job" (format "That is correct!") givenParent '(no-icon ok))
            (message-box "Go to the gazebo" (format
                                             (string-append "That is incorrect. \n The correct answer is: "
                                                            (number->string (send problems getAnswer))
                                                            "\n or: "
                                                            (number->string (send problems getRound)))) givenParent '(stop ok))))
      ; redraws screen
      (canvasPaintingCallbackFunction drawingCanvas (send drawingCanvas get-dc))
      )
    
    (define (return-callback button event)
      (menuReturnFunction)
      )
    
    (define drawingInputMenu (new frame%
                                  [label "Problem Screen"]
                                  [width frameWidthAndHeight]
                                  [height frameWidthAndHeight]
                                  [style frameStyle]
                                  )
      )
    (define drawingCanvas (new canvas%
                               [parent drawingInputMenu]
                               [style containerStyle]
                               [min-width 600]
                               [min-height canvasHeight]
                               [paint-callback canvasPaintingCallbackFunction]
                               )
      )

    
    (define inputPanel (new horizontal-panel%
                            [parent drawingInputMenu]
                            [style containerStyle]
                            [min-width 600]
                            [min-height inputSectionHeight]))


    (define returnButton (new button%
                              [parent inputPanel]
                              [label "Return to Main Menu"]
                              [callback return-callback]
                              )
      )
    
    (define textEnter (new text-field%
                           [parent inputPanel]
                           [label "Enter Answer:"]
                           [min-width textBoxWidth]))


    
    (define submit (new button%
                        [parent inputPanel]
                        [label "Submit"]
                        [callback submit-callback]))

    (define/public (enable)
      (send drawingInputMenu show #t)
      )
    ; This sets the game mode and problem category of the current problemScreen instance to the passed values
    (define/public (pass-information game-mode problem-category)
      (set! currentGameMode game-mode)
      (set! currentProblemCategory problem-category)
      (set! playerOneScore 0)
      (set! playerTwoScore 0)
      (set! currentPlayer playerOne)
      )
    (define/public (disable)
      (send drawingInputMenu show #f))



    )
 )

(provide drawInputMenuUI%)
