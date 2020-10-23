#lang racket
(require racket/draw)
(require "textProblem.rkt")
(require "../constants/userInterfaceConstants.rkt")


; Draws an equilateral triangle and a problem description onto the given device context
; Parameters:
; device-context: the device context
; side-length: the problem length of each side
; problemDescription: the text description of the problem

(define (draw-equi-triangle device-context side-length problemDescription)
  (send device-context draw-polygon (list (cons 125 30) (cons 100 60) (cons 150 60)))
  (draw-text-problem device-context problemDescription problemTextPushRight problemTextPushdown)
  )


(provide draw-equi-triangle)

