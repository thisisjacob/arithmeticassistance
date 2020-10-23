#lang racket
(require racket/draw)
(require "textProblem.rkt")


; Draws a circle onto the given device context
; Parameters:
; device-context: the device context
; problem-radius: the radius in the math problem (for drawing text)
; pixel-radius: the radius on the screen of the circle
; x: x position
; y: y position

; TODO: Add text next to the circle for problem information
(define (draw-rectangle device-context xPos yPos problemWidth problemHeight problemDescription)
  (send device-context draw-rectangle xPos yPos 50 50)
  (draw-text-problem device-context problemDescription xPos (+ yPos 100))
  )


(provide draw-rectangle)

