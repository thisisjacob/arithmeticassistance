#lang racket
(require racket/gui/base)
(require "ui/mainWindow.rkt")

; creates the UI
(define game (new mainWindow% [title "test"]))
(send game startUI)