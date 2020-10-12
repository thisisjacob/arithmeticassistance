#lang racket
(require racket/draw)

; Draws text onto the screen, to be used for drawing non geometric math problems
; Parameters:
; device-context: the device context
; string: a string holding the problem
; x: x position
; y: y position

(define (draw-text-problem device-context string x y)
  (send device-context draw-text string x y)
  )


(provide draw-text-problem)

