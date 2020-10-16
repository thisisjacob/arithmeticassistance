; Initializes the window and "pages" for the program's user interface, attaches the pages to this class
; Functions are called to switch between the "pages"
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
    (define/public (startUI)
      (send mainFrame show #t)
      (enableMainMenu))
    (define (enableMainMenu)
      (send problemsScreen disable)
      (send difficultyScreen disable)
      (send mainFrame show #t)
      (send mainMenu enable)
      )
    (define (enableProblemsScreen)
      (send mainMenu disable)
      (send difficultyScreen disable)
      (send problemsScreen enable)
      (send mainFrame show #f)
      )
    (define (enableDifficultyScreen)
      (send problemsScreen disable)
      (send mainMenu disable)
      (send difficultyScreen enable)
      (send mainFrame show #f)
      )
    (define (disableMainMenu)
      (send mainMenu disable)
      )

    (define difficultyScreen (new gradeAndDifficultySelectScreen%
                                  [menuReturnFunction enableMainMenu]
                                  [problemScreenFunction enableProblemsScreen]
                                  )
      )

    (define (pass/switchToDifficultyScreen game-mode)
      (send difficultyScreen pass-information game-mode)
      
      )
    
    (define (mainMenuFunctionGenerator mode-list given-list)
       (cond
         [(empty? mode-list) given-list]
         [else
          (define new-list (append given-list (list (send (first mode-list) getName) (pass/switchToDifficultyScreen (first mode-list)))))
                                   (mainMenuFunctionGenerator (rest mode-list) new-list)
                                   ]
         )
      )
      
    
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



    (define test '())
    (define mainMenu (new mainMenuUI%
                          [givenParent mainFrame]
                          [string-function-pair-list (list (mainMenuFunctionGenerator game-modes test))]))


    









  )
)

(provide mainWindow%)



