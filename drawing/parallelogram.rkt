#lang racket
(require racket/draw)
(require "textProblem.rkt")
(require "../constants/userInterfaceConstants.rkt")


; Draws a rhomboid parallelogram and a problem description onto the given device context
; Parameters:
; device-context: the device context to draw on
; vert-length: the length of the "vertical" sides in the problem
; horiz-length: the length of the "horizontal" sides in the problem
; problemDescription: the text description of the problem

(define (draw-parallelogram device-context vert-length horiz-length problemDescription)
  (send device-context draw-polygon (list (cons 100 30) (cons 150 30) (cons 140 60) (cons 90 60)))
  (draw-text-problem device-context problemDescription problemTextPushRight problemTextPushdown)
  )


(provide draw-parallelogram)

