; Starts the program
#lang racket
(require racket/gui/base)
(require "ui/mainWindow.rkt")

; creates the UI
(define game (new mainWindow% [title "Mathematical Education Project"]))
(send game startUI)
