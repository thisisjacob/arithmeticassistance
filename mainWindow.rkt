#lang racket
(require racket/gui/base)
(require "menuBar.rkt")

(define mainWindow%
  (class object%
    (init-field
     title
     )
    (super-new)
    (define mainFrame (new frame% [label title]))
    (define topMenuBar (new menuBar% [menuParent mainFrame]))
    (define menuOptions (new panel%
                             [parent mainFrame]
                             [style '(border)]
                             [horiz-margin 100]
                             [vert-margin 100]
                             [enabled #t]))
    (define testButton1 (new button% [label "Test"] [parent menuOptions])) ; remove these buttons later in development
    (define testButton2 (new button% [label "Test"] [parent menuOptions])) ; current buttons all overlap with eachother - figure out how to do layouts
    (define testButton3 (new button% [label "Test"] [parent menuOptions]))
    (define/public (openWindow)
      (send mainFrame show #t)
      (send topMenuBar activate))
    ))

(provide mainWindow%)


