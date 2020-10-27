#lang racket
(require racket/draw)
; Constants for various UI elements of the program


(define programTitle "Mathematical Education Project")
(provide programTitle)

(define frameWidthAndHeight 800)
(define frameStyle '(no-resize-border))
(provide frameWidthAndHeight)
(provide frameStyle)

; Style and positions for generic containers
(define containerStyle '(border))
(define containerHorizMargin 200)
(define containerVerticalMargin 200)
(provide containerStyle)
(provide containerHorizMargin)
(provide containerVerticalMargin)

(define problemTextPushdown 100)
(define problemTextPushRight 100)
(provide problemTextPushdown)
(provide problemTextPushRight)

; top left position of scoreboard
; how many points to push down each element from element above
(define scoreboardPosition 150)
(define scoreboardPushNum 20)
(provide scoreboardPosition)
(provide scoreboardPushNum)

; Generic button constants
(define buttonMinWidth 300)
(define buttonMinHeight 30)
(define buttonVertMargin 10)
(provide buttonMinWidth)
(provide buttonMinHeight)
(provide buttonVertMargin)



