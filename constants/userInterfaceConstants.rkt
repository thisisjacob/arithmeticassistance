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
(define scoreboardXPosition 540)
(define scoreboardYPosition 0)
(define scoreboardPushNum 20)
(define scoreboardWidth 250)
(define scoreboardHeight 80)
(provide scoreboardXPosition)
(provide scoreboardYPosition)
(provide scoreboardPushNum)
(provide scoreboardWidth)
(provide scoreboardHeight)

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
(define problemCanvasTextColor (send the-color-database find-color "black"))
(define geometricShapeBackground (send the-color-database find-color "crimson"))
(define geometricShapeBackgroundStyle (quote solid))
(define geometricShapeTextColor (send the-color-database find-color "crimson"))
(define penBackgroundColor (send the-color-database find-color "black"))
(define penWidth 3)
(define penStyle (quote solid))
(define problemMenuBackgroundColor (send the-color-database find-color "lemonchiffon"))
(define problemMenuStyle (quote solid))
(define problemMenuTextColor (send the-color-database find-color "sienna"))
(provide problemCanvasBackground)
(provide problemCanvasTextColor)
(provide geometricShapeBackground)
(provide geometricShapeBackgroundStyle)
(provide geometricShapeTextColor)
(provide penBackgroundColor)
(provide penWidth)
(provide penStyle)
(provide problemMenuBackgroundColor)
(provide problemMenuStyle)
(provide problemMenuTextColor)



