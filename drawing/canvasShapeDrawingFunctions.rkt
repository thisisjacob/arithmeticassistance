#lang racket
(require racket/draw)
(require "../constants/userInterfaceConstants.rkt")

; This file holds a collection of functions that are used to draw math and geometrical problems onto a given device context
; These are designed specifically for this math project


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

; Draws text of the given scores and current player onto the given device context
; Parameters:
; device-context: the device context to draw one
; currentPlayer: string for the current player
; playerOneScore: the score of player one to draw
; playerTwoScore: the score of player two to draw
(define (draw-score device-context currentPlayer playerOneScore playerTwoScore)
  (send device-context draw-text currentPlayer 150 0)
  (send device-context draw-text (string-append "Player One: " (make-string playerOneScore)) 150 20)
  (send device-context draw-text (string-append "Player Two: " (make-string playerTwoScore)) 150 40)
  )

(provide draw-score)


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
