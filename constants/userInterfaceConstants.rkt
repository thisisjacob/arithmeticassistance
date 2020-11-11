#lang racket
(require racket/draw)

; Constants for various UI elements of the program
; These can be modified to change the appearance of different areas of the interface

; General program
(define programTitle "Mathematical Education Project")
(provide programTitle)

; Window frame appearances
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

; Menu specific options
(define menuTitleFont (make-object font% 18 #f (quote swiss)
                           )
  )
(provide menuTitleFont)

; Problems screen canvas constants
(define canvasHeight 550)
(define inputSectionHeight 50)
(provide canvasHeight)
(provide inputSectionHeight)

; Problem screen textbox constants
(define textBoxWidth 100)
(provide textBoxWidth)

; Problem screen scoreboard constants
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

; Problem screen problem description positional constants
(define problemXPos 20)
(define problemYPos 440)
(define problemTextPushdown 100)
(define problemTextPushRight 100)
(define linePushDown 20)
(define geometryScaling 10)
(provide problemXPos)
(provide problemYPos)
(provide problemTextPushdown)
(provide problemTextPushRight)
(provide linePushDown)
(provide geometryScaling)

; Geometry problem drawing positional constants
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

; Problem screen coloring constants
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
(define menuTopTextMargin 10)
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
(provide menuTopTextMargin)



