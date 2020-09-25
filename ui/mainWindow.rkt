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
    (define/public (openWindow)
      (send mainFrame show #t)
      (send topMenuBar activate))
    ))

(provide mainWindow%)


