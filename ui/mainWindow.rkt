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
                           [style '(no-resize-border)]))


    ;Functions
    (define/public (startUI)
      (send mainFrame show #t)
      (enableMainMenu))
    (define (enableMainMenu)
      (send mainMenu enable)
      (send problemsScreen disable))
    (define (enableProblemsScreen)
      (send problemsScreen enable)
      (send mainMenu disable))
    (define (disableMainMenu)
      (send mainMenu disable))

    (define mainMenu (new mainMenuUI%
                          [givenParent mainFrame]
                          [function enableProblemsScreen]
                          [string-function-pair-list (list (list "Test Main Menu" enableMainMenu)
                                                           (list "Test Problems Screen" enableProblemsScreen))]))
    (define problemsScreen (new drawInputMenuUI%
                                [givenParent mainFrame]))
  )
)

(provide mainWindow%)



