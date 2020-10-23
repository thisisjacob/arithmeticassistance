#lang racket
(require racket/draw)
(require "textProblem.rkt")
(require "../constants/userInterfaceConstants.rkt")


; Draws a rhomboid parallelogram and a problem description onto the given device context
; Parameters:
; device-context: the device context to draw on
; bottom-length: the length of the "bottom" in the problem
; top-length: the length of the "bottom" side in the problem
; problemDescription: the text description of the problem

(define (draw-trapezoid device-context bottom-length top-length problemDescription)
  (send device-context draw-polygon (list (cons 80 30) (cons 130 30) (cons 150 60) (cons 75 60) ))
  (draw-text-problem device-context problemDescription problemTextPushRight problemTextPushdown)
  )


(provide draw-trapezoid)

