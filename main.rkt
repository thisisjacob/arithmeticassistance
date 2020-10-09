; Starts the program
#lang racket
(require racket/gui/base)
(require "ui/mainWindow.rkt")
(require "constants/userInterfaceConstants.rkt")

; creates the UI
(define game (new mainWindow% [title programTitle]))
(send game startUI)
