#lang racket
(require racket/gui/base)
(require "widgets/percentButton.rkt")
(require "../constants/userInterfaceConstants.rkt")
(require "../constants/difficultiesAndCategories.rkt")
(require "../utilities/buttonFunctionsAndGenerators.rkt")

; Window for selecting a problem category

; Initialization Arguments:
; menuReturnFunction: a function for switching to a separate window for "going back"
; problemScreenFunction: a function for switching to the problems screen
; Public Functions:
; enable : enables visibility of this object instance
; pass-information: One argument, a game-mode construct
; Sets the internal state of this window to store the current game-mode
; Should be called before the enable function
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

           ; Setups RacketGUI objects
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

           ; A button callback function for calling the menuReturnFunction
           (define (return-callback button event)
             (menuReturnFunction)
             )

           ; Creates a list of button text and button functions, used with the buttonGenerator class to dynamically generate
           ; the buttons of this window.
           ; Arguments:
           ; difficulty-list: the imported list of difficulty constructs
           ; given-list: must be null
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
           ; Sets the current game-mode of the class. game-mode must come from the game made constants import
           ; This should be called before enable
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