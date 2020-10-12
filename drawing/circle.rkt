#lang racket
(require racket/draw)


; Draws a circle onto the given device context
; Parameters:
; device-context: the device context
; problem-radius: the radius in the math problem (for drawing text)
; pixel-radius: the radius on the screen of the circle
; x: x position
; y: y position

; TODO: Add text next to the circle for problem information
(define (draw-circle device-context problem-radius pixel-radius x y)
  (send device-context draw-ellipse x y pixel-radius pixel-radius)
  )


(provide draw-circle)

