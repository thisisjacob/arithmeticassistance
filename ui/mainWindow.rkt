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

(define mainWindow%
  (class object%
    (init-field
     title
     )
    (super-new)
    (define mainFrame (new frame%
                           [label title]
                           [width 800]
                           [height 800]
                           [style '(no-resize-border)]
                           )
      )


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


    (define mainMenu (new mainMenuUI%
                          [givenParent mainFrame]
                          [function enableMainMenu]
                          [string-function-pair-list (list (list "Test Main Menu" enableMainMenu)
                                                           (list "Test Problems Screen" enableProblemsScreen)
                                                           (list "Test Difficulty Screen" enableDifficultyScreen))]))
    (define problemsScreen (new drawInputMenuUI%
                                [givenParent mainFrame]
                                [menuReturnFunction enableMainMenu]
                                )
      )

    (define difficultyScreen (new gradeAndDifficultySelectScreen%
                                  [givenParent mainFrame]
                                  [menuReturnFunction enableMainMenu]
                                  )
      )


  )
)

(provide mainWindow%)



