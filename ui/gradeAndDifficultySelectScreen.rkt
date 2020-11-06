#lang racket
(require racket/gui/base)
(require "widgets/percentButton.rkt")
(require "../constants/userInterfaceConstants.rkt")
(require "../constants/difficultiesAndCategories.rkt")
(require "../utilities/buttonFunctionsAndGenerators.rkt")

; The screeen for selecting a grade level or category/difficulty

; Initialization Arguments:
; menuReturnFunction: a function for returning from this page and to a given return page
; problemScreenFunction: a function for switching to the problems screen
; Public Functions:
; enable : enables visibility of this object instance
; pass-information: requires a game mode, sets the current game mode to it
; disable: disables visibility of this object instance

(define gradeAndDifficultySelectScreen%
         (class object%
           (init-field
            menuReturnFunction
            problemScreenFunction
            )
           (super-new)

           ; Holds the selected game mode, passed into the problems screen
           (define currentGameMode 0)

           ; Setups structural menu objects
           (define pageWrapper (new frame%
                                    [label "Difficulty Selection"]
                                    [width frameWidthAndHeight]
                                    [height frameWidthAndHeight]
                                    [style frameStyle]
                                    )
             )
           (define menuWrapper (new panel%
                                 [parent pageWrapper]
                                 [horiz-margin containerHorizMargin]
                                 [vert-margin containerVerticalMargin]
                                 )
             )

           (define menu (new vertical-panel%
                             [style containerStyle]
                             [parent menuWrapper]
                             [spacing 5]))

           ; A button callback function for returning to the main menu
           (define (return-callback button event)
             (menuReturnFunction)
             )
           ; A button callback function for switching to the problems screen
           (define (open-problems button event)
             (problemScreenFunction)
             )

           ; Creates a list of button text and button functions
           ; Names come from the given difficulty list
           ; List of functions are a list of the problemScreenFunction, with the current difficulty construct passed as an argument
           ; given-list should be null, it is used so the function can recursively call itself to build the list
           (define (buttonFunctionGenerator difficulty-list given-list)
             (cond
               [(empty? difficulty-list) given-list]
               [else
                (define new-list (append given-list (list (list (send (first difficulty-list) getName)
                                                                problemScreenFunction
                                                                 (list (first currentGameMode) (first difficulty-list))
                                                                )
                                                          )
                                         )
                  )
                (buttonFunctionGenerator (rest difficulty-list) new-list)
                ]
               )
             )


           ; Public Functions
           ; Enable switches to this menu, enables its visibility, initializes its buttons
           (define/public (enable)
             (define current-buttons (send menu get-children))
             (for-each (lambda (arg)
                         (send menu delete-child arg)
                         )
                       current-buttons)
             (define header (new message%
                                 [font menuTitleFont]
                                 [parent menu]
                                 [label "Problem Categories"]
                                 )
               )
             (define buttons (new buttonGeneratorAndManager%
                                  [givenParent menu]
                                  [functionList (buttonFunctionGenerator (send difficulties list-of-categories) '()) ]
                                  )
               )
             (send buttons initialize-buttons-args)
             (send pageWrapper show #t)
             )
           ; Sets the current game-mode of the class. Game-mode must come from the difficulty constructs under constants
           (define/public (pass-information game-mode)
             (set! currentGameMode game-mode)
             )
           ; Disables the visibility
           (define/public (disable)
             (send pageWrapper show #f)
             )

             
           )
  )


(provide gradeAndDifficultySelectScreen%)