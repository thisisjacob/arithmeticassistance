#lang racket
(require racket/draw)
(require "textProblem.rkt")
(require "../constants/userInterfaceConstants.rkt")


; Draws a rectangle and a problem description onto the given device context
; Parameters:
; device-context: the device context
; xPos: the x position of the rectangle on the device context
; yPos: the y position of the rectangle on the device context
; problemWidth: the width of the rectangle within the problem
; problemHeight: the height of the rectangle within the problem
; problemDescription: a text description of the problem. positioned based on constants

(define (draw-rectangle device-context xPos yPos problemWidth problemHeight problemDescription)
  (send device-context draw-rectangle xPos yPos 50 50)
  (draw-text-problem device-context problemDescription problemTextPushRight problemTextPushdown)
  )


(provide draw-rectangle)

