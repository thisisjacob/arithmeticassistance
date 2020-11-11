#lang racket
(require racket/gui/base)
(require math/base)
(require racket/gui)
(require k-infix)
(require "../constants/userInterfaceConstants.rkt")
(require "../constants/gameModes.rkt")
(require "../drawing/canvasShapeDrawingFunctions.rkt")
(require "../logic/equationGenerator.rkt")


; GUI Class:

; The screen for drawing problems and entering answers
; Initialization Arguments:
; givenParent : the container element of this instance
; menuReturnFunction: a function for returning from this page and to a given return page
; Public Functions:
; pass-information: Requires a game-mode and problem-category construct as arguments. Should be called before the enable function,
; sets the current game mode and problem category to the passed arguments
; enable : enables visibility of this object instance
; disable: disables visibility of this object instance

(define drawInputMenuUI%
  (class object%
    (init-field
     givenParent
     menuReturnFunction
     )
    (super-new)

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

    ; Multiplayer information
    (define playerOneScore 0)
    (define playerTwoScore 0)
    (define currentPlayer playerOne)

    ; For generating problems
    (define problems (new equation-generator%)
      )


    ; A callback function for rendering the user interface and problems to the canvas
    ; Also refreshes the screen between each call
    ; Arguments
    ; canvas: which is a canvas UI class
    ; dc: the device context of the passed canvas
    (define (canvasPaintingCallbackFunction canvas dc)
      ; set-background and set-pen not needed here - kept to keep appearance contained into one location
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
      ; Generates and draws problem
      (send problems generateProblem (send drawingCanvas get-dc) currentProblemCategory)
      )

    ; Callback definitions
    ; Fired when the user submits an answer
    ; Tells the user whether the answer is correct, returns the correct answer if they are wrong
    ; Updates the score of each player and switches the current player (only relevant if in multiplayer)
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
        ; Opens a textbox informing the user of the result
        (if (or (string=? text (number->string (send problems getAnswer)))
                (string=? text (number->string (send problems getRound))))
            (message-box "Good job" (format "That is correct!") givenParent '(no-icon ok))
            (message-box "Go to the gazebo" (format
                                             (string-append "That is incorrect. \n The correct answer is: "
                                                            (number->string (send problems getAnswer))
                                                            "\n or: "
                                                            (number->string (send problems getRound)))) givenParent '(stop ok))))
      ; Generates problem, redraws screen
      (canvasPaintingCallbackFunction drawingCanvas (send drawingCanvas get-dc))
      )

    ; Called when pressing the return button - calls the fuction passed as menuReturnFunction in initialization
    (define (return-callback button event)
      (menuReturnFunction)
      )

    ; RacketGUI definitions
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

    ; Enables visibility of this window
    (define/public (enable)
      (send drawingInputMenu show #t)
      )
    
    ; Updates state of the drawingInputScreen to the passed gameMode and problemCategory constructs
    ; Should be called before the enable function
    (define/public (pass-information game-mode problem-category)
      (set! currentGameMode game-mode)
      (set! currentProblemCategory problem-category)
      (set! playerOneScore 0)
      (set! playerTwoScore 0)
      (set! currentPlayer playerOne)
      )

    ; Disables visibility of this window
    (define/public (disable)
      (send drawingInputMenu show #f))



    )
 )

(provide drawInputMenuUI%)
