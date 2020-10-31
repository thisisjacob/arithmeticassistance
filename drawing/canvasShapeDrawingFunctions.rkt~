#lang racket
(require racket/draw)
(require "../constants/userInterfaceConstants.rkt")

; This file holds a collection of functions that are used to draw math and geometrical problems onto a given device context
; These are designed specifically for this math project


; Draws text onto the screen, to be used for drawing non geometric math problems
; Parameters:
; device-context: the device context
; string: a string holding the problem
(define (draw-text-problem device-context string)
  (send device-context draw-text string problemXPos problemYPos)
  )

(provide draw-text-problem)

; Draws multiple lines of text onto the screen
; Parameters:
; device-context: the device context
; string-list: a list of strings, where each string is drawn onto a separate line
(define (draw-text-problem-with-multiple-lines device-context string-list)
  (define (create-text list iterator)
    (cond
      [(empty? list) 0]
      [else
       (send device-context draw-text (first list) problemXPos (+ problemYPos (* linePushDown iterator)))
       (create-text (rest list) (+ iterator 1))]))
  (create-text string-list 0)
  )

(provide draw-text-problem-with-multiple-lines)


; Draws text of the given scores and current player onto the given device context
; Parameters:
; device-context: the device context to draw one
; currentPlayer: string for the current player
; playerOneScore: the score of player one to draw
; playerTwoScore: the score of player two to draw
(define (draw-score device-context currentPlayer playerOneScore playerTwoScore)
  (send device-context draw-text currentPlayer scoreboardPosition 0)
  (send device-context draw-text (string-append "Player One: " (number->string playerOneScore)) scoreboardPosition scoreboardPushNum)
  (send device-context draw-text (string-append "Player Two: " (number->string playerTwoScore)) scoreboardPosition (* 2 scoreboardPushNum))
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
(define (draw-rectangle device-context problemWidth problemHeight problemDescription)
  (send device-context draw-rectangle geometryXPos geometryYPos problemWidth problemHeight)
  (send device-context draw-text (number->string problemWidth) (+ geometryXPos (/ problemWidth 2)) (- geometryYPos 30))
  (send device-context draw-text (number->string problemHeight) (- geometryXPos 30) (+ geometryYPos (/ problemHeight 2)))
  (draw-text-problem device-context problemDescription)
  )


(provide draw-rectangle)

; Draws a rhomboid parallelogram and a problem description onto the given device context
; Parameters:
; device-context: the device context to draw on
; bottom-length: the length of the "bottom" in the problem
; top-length: the length of the "bottom" side in the problem
; problemDescription: the text description of the problem
(define (draw-trapezoid device-context bottom-length top-length height problemDescriptionList)
  (send device-context draw-polygon (list (cons geometryXPos geometryYPos)
                                          (cons (+ geometryXPos top-length) geometryYPos)
                                          (cons (+ geometryXPos bottom-length) (+ geometryYPos height))
                                          (cons (+ geometryXPos 5) (+ geometryYPos height)) ))
  (send device-context draw-text (number->string bottom-length) (+ geometryXPos (/ bottom-length 2)) (+ geometryYPos height 10))
  (send device-context draw-text (number->string top-length) (+ geometryXPos (/ top-length 2)) (- geometryYPos 30))
  (send device-context draw-text (string-append "Height: " (number->string height)) (- geometryXPos 100) geometryYPos)
  (draw-text-problem-with-multiple-lines device-context problemDescriptionList)
  )


(provide draw-trapezoid)

; Draws a rhomboid parallelogram and a problem description onto the given device context
; Parameters:
; device-context: the device context to draw on
; vert-length: the length of the "vertical" sides in the problem
; horiz-length: the length of the "horizontal" sides in the problem
; problemDescription: the text description of the problem
(define (draw-parallelogram device-context vert-length horiz-length height problemDescription)
  (send device-context draw-polygon (list (cons  (+ geometryXPos 10) geometryYPos)
                                          (cons (+ geometryXPos horiz-length 10) geometryYPos)
                                          (cons (+ geometryXPos horiz-length) (+ geometryYPos height))
                                          (cons geometryXPos (+ geometryYPos height))))
  (send device-context draw-text (string-append "Horizontal: " (number->string horiz-length)) (- (+ geometryXPos (/ horiz-length 2)) 30) (+ geometryYPos height 10))
  (send device-context draw-text (string-append "Vertical: " (number->string vert-length)) (- geometryXPos 100) (+ geometryYPos 20))
  (send device-context draw-text (string-append "Height: " (number->string height)) (- geometryXPos 100) geometryYPos)
  (draw-text-problem device-context problemDescription)
  )


(provide draw-parallelogram)

; Draws an equilateral triangle and a problem description onto the given device context
; Parameters:
; device-context: the device context
; side-length: the problem length of each side
; problemDescription: the text description of the problem
(define (draw-equi-triangle device-context side-length problemDescription)
  (send device-context draw-polygon (list (cons 125 30) (cons 100 60) (cons 150 60)))
  (draw-text-problem device-context problemDescription)
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
(define (draw-circle device-context radius problemDescription)
  (send device-context draw-ellipse geometryXPos geometryYPos radius radius)
  (send device-context draw-text (string-append "Radius: " (number->string radius)) (- geometryXPos 100) geometryYPos)
  (draw-text-problem device-context problemDescription)
  )


(provide draw-circle)
