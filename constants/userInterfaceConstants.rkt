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

; Problems screen constants:
(define canvasHeight 550)
(define inputSectionHeight 50)
(provide canvasHeight)
(provide inputSectionHeight)

; Input element constants:
(define textBoxWidth 100)
(provide textBoxWidth)

; top left position of scoreboard
; how many points to push down each element from element above
(define scoreboardPosition 540)
(define scoreboardPushNum 20)
(provide scoreboardPosition)
(provide scoreboardPushNum)
; end of problems screen constants

; problem text constants
(define problemXPos 20)
(define problemYPos 440)
(define linePushDown 20)
(define geometryScaling 10)
(provide problemXPos)
(provide problemYPos)
(provide linePushDown)
(provide geometryScaling)

; Geometry problem drawing constants
(define geometryXPos 150)
(define geometryYPos 150)
(provide geometryXPos)
(provide geometryYPos)

; Generic button constants
(define buttonMinWidth 300)
(define buttonMinHeight 30)
(define buttonVertMargin 10)
(provide buttonMinWidth)
(provide buttonMinHeight)
(provide buttonVertMargin)

; Coloring constants
(define problemCanvasBackground (send the-color-database find-color "lavenderblush"))
(provide problemCanvasBackground)



