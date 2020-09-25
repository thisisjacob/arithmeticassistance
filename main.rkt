#lang racket
(require racket/gui/base)
(require "ui/mainWindow.rkt")

(define game (new mainWindow% [title "test"]))
(send game openWindow)