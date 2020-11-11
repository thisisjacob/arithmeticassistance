; Initializes the windows of the program, creates functions for managing them
; Instance Parameters:
; title: a string for the title of the window
; Public Functions:
; startUI: makes the UI visible, 

#lang racket
(require racket/gui/base)
(require "mainMenu.rkt")
(require "drawingInputScreen.rkt")
(require "gradeAndDifficultySelectScreen.rkt")
(require "../constants/userInterfaceConstants.rkt")
(require "../constants/gameModes.rkt")

(define mainWindow%
  (class object%
    (init-field
     title
     )
    (super-new)

    ;Functions
    ; Starts the program
    (define/public (startUI)
      (send mainFrame show #t)
      (enableMainMenu))
    ; Enables main menu, disables other screens
    (define (enableMainMenu)
      (send problemsScreen disable)
      (send difficultyScreen disable)
      (send mainFrame show #t)
      (send mainMenu enable)
      )
    ; Enables problems screen, disables other screens
    ; Not the primary switching function - only a helper.
    ; Use pass/switchToProblemsScreen
    (define (enableProblemsScreen)
      (send mainMenu disable)
      (send difficultyScreen disable)
      (send problemsScreen enable)
      (send mainFrame show #f)
      )
    ; Enables difficulty screen, disables other screens
    ; Not the primary switching function - only a helper.
    ; Use pass/switchToDifficultyScreen
    (define (enableDifficultyScreen)
      (send problemsScreen disable)
      (send mainMenu disable)
      (send difficultyScreen enable)
      (send mainFrame show #f)
      )
    ; Disables the main menu
    (define (disableMainMenu)
      (send mainMenu disable)
      )
    
    ; The primary function for switching to the problems screen
    ; Requires a pair holding game mode and problem category constructs
    ; Using this function passes these constructs to the drawingInputScreen (problems screen)
    (define (pass/switchToProblemsScreen game-mode-and-problem-category-pair)
      (send problemsScreen pass-information (first game-mode-and-problem-category-pair)

            (first (rest game-mode-and-problem-category-pair)
                   ))

      (enableProblemsScreen)
      )

    ; RacketGUI definition
    (define difficultyScreen (new gradeAndDifficultySelectScreen%
                                  [menuReturnFunction enableMainMenu]
                                  [problemScreenFunction pass/switchToProblemsScreen]
                                  )
      )

    ; The primary function for switching to the difficulty screen
    ; requires a gameMode construct from constants
    ; This is used to pass the selected game mode down the chain of menus
    (define (pass/switchToDifficultyScreen game-mode)
      (send difficultyScreen pass-information game-mode)
      (enableDifficultyScreen)
      )



    ; Generates a list of (list (name pass/switchToDifficultyScreen game-mode))
    ; This is used to create a list of names, functions and arguments used to generate buttons
    ; for switching to the gradeAndDifficultySelectScreen in the mainMenu
    ; mode-list is a list of mode constructs that are used to create the function procedure arguments
    ; given-list should be an empty list/null, or an already existing list of the returned construct
    (define (mainMenuFunctionGenerator mode-list given-list)
       (cond
         [(empty? mode-list) given-list]
         [else
          (define new-list (append given-list (list (list (send (first mode-list) getName) pass/switchToDifficultyScreen (list (first mode-list))))))
                                   (mainMenuFunctionGenerator (rest mode-list) new-list)
                                   ]
         )
      )
      
    ; RacketGUI definitions
    (define mainFrame (new frame%
                           [label title]
                           [width frameWidthAndHeight]
                           [height frameWidthAndHeight]
                           [style frameStyle]
                           )
      )
    (define problemsScreen (new drawInputMenuUI%
                                [givenParent mainFrame]
                                [menuReturnFunction enableMainMenu]
                                )
      )
    (define mainMenu (new mainMenuUI%
                          [givenParent mainFrame]
                          [string-function-pair-list  (mainMenuFunctionGenerator game-modes null)]))
  )
)

(provide mainWindow%)



