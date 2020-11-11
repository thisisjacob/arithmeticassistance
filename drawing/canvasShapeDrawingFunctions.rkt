#lang racket
(require racket/draw)
(require "../constants/userInterfaceConstants.rkt")

; This file holds a collection of functions that are used for drawing onto the problemInputScreen canvas
; These include geometric problems, scoreboards and problem descriptions
; If generating a geometric problem, draw-text-problem or draw-text-problem-with-multiple-lines should be called
; If in multiplayer, then draw-score should be called
; If generating a geometric problem, then the drawing function of the geometric type should be called

(define problemDescBoxWidth (- frameWidthAndHeight 7))

; Draws text onto the screen, to be used for drawing text onto the canvas text box
; This should not generally be called outside of the program
; It should instead be called by draw-text-problem-with-multiple-lines
; Parameters:
; device-context: the device context
; string: a string holding the problem
(define (draw-text-problem device-context string)
  (send device-context set-brush problemMenuBackgroundColor problemMenuStyle)
  (send device-context set-text-foreground problemMenuTextColor)
  (send device-context draw-rectangle 0 (+ problemYPos) problemDescBoxWidth) (/ frameWidthAndHeight 4)
  (send device-context draw-text string problemXPos (+ problemYPos menuTopTextMargin))
  )

(provide draw-text-problem)

; Draws multiple lines of text onto the screen onto the canvas text box
; Should be used for problem descriptions
; Parameters:
; device-context: the device context
; string-list: a list of strings, where each string is drawn onto a separate line
(define (draw-text-problem-with-multiple-lines device-context string-list)
  (send device-context set-brush problemMenuBackgroundColor problemMenuStyle)
  (send device-context set-text-foreground problemMenuTextColor)
  (send device-context draw-rectangle 0 (+ problemYPos) problemDescBoxWidth (/ frameWidthAndHeight 4))
  (define (create-text list iterator)
    (cond
      [(empty? list) 0]
      [else
       (send device-context draw-text (first list) problemXPos (+ problemYPos (* linePushDown iterator) menuTopTextMargin))
       (create-text (rest list) (+ iterator 1))]))
  (create-text string-list 0)
  )

(provide draw-text-problem-with-multiple-lines)


; Draws the scoreboard onto the canvas
; Parameters:
; device-context: the device context to draw on
; currentPlayer: string for the current player (such as "Player One" or "John")
; playerOneScore: the score of player one to draw
; playerTwoScore: the score of player two to draw
(define (draw-score device-context currentPlayer playerOneScore playerTwoScore)
  (send device-context set-brush problemMenuBackgroundColor problemMenuStyle)
  (send device-context set-text-foreground problemMenuTextColor)
  (send device-context draw-rectangle scoreboardXPosition scoreboardYPosition scoreboardWidth scoreboardHeight)
  (send device-context draw-text currentPlayer (+ scoreboardXPosition 10) (+ scoreboardYPosition 10))
  (send device-context draw-text (string-append "Player One: " (number->string playerOneScore)) (+ scoreboardXPosition menuTopTextMargin) (+ scoreboardPushNum menuTopTextMargin))
  (send device-context draw-text (string-append "Player Two: " (number->string playerTwoScore)) (+ scoreboardXPosition menuTopTextMargin) (+ (* 2 scoreboardPushNum) menuTopTextMargin))
  )

(provide draw-score)


; Draws a rectangle and a problem description onto the given device context
; Parameters:
; device-context: the device context
; xPos: the x position of the rectangle on the device context
; yPos: the y position of the rectangle on the device context
; problemWidth: the width of the rectangle within the problem
; problemHeight: the height of the rectangle within the problem
; problemDescriptionList: a list of strings, with each string being a separate line of text
(define (draw-rectangle device-context problemWidth problemHeight problemDescriptionList)
  (send device-context draw-rectangle geometryXPos geometryYPos (* geometryScaling problemWidth) (* geometryScaling problemHeight))
  (send device-context draw-text (number->string problemWidth) (+ geometryXPos (* geometryScaling (/ problemWidth 2))) (- geometryYPos 30))
  (send device-context draw-text (number->string problemHeight) (- geometryXPos 30) (+ geometryYPos (* geometryScaling (/ problemHeight 2))))
  (draw-text-problem-with-multiple-lines device-context problemDescriptionList)
  )


(provide draw-rectangle)

; Draws a trapezoid and a problem description onto the given device context
; Parameters:
; device-context: the device context to draw on
; bottom-length: the length of the "bottom" in the problem
; top-length: the length of the "top" side in the problem
; problemDescriptionList: a list of strings, with each string being a separate line of text
(define (draw-trapezoid device-context bottom-length top-length height problemDescriptionList)
  (send device-context draw-polygon (list (cons geometryXPos geometryYPos)
                                          (cons (+ geometryXPos (* geometryScaling top-length)) geometryYPos)
                                          (cons (+ geometryXPos (* geometryScaling bottom-length)) (+ geometryYPos (* geometryScaling height)))
                                          (cons (+ geometryXPos (* geometryScaling 5)) (+ geometryYPos (* geometryScaling height))) ))
  (send device-context draw-text (number->string bottom-length) (+ geometryXPos (* geometryScaling (/ bottom-length 2))) (+ geometryYPos (* geometryScaling height) 10))
  (send device-context draw-text (number->string top-length) (+ geometryXPos (* geometryScaling (/ top-length 2))) (- geometryYPos 30))
  (send device-context draw-text (string-append "Height: " (number->string height)) (- geometryXPos 100) geometryYPos)
  (draw-text-problem-with-multiple-lines device-context problemDescriptionList)
  )


(provide draw-trapezoid)

; Draws a rhomboid parallelogram and a problem description onto the given device context
; Parameters:
; device-context: the device context to draw on
; base: the base of the parallelogram
; height: the height of the parallelogram
; problemDescriptionList: a list of strings, with each string being a separate line of text
(define (draw-parallelogram device-context base height problemDescriptionList)
  (send device-context draw-polygon (list (cons  (+ geometryXPos 10) geometryYPos)
                                          (cons (+ geometryXPos (* base geometryScaling) 10) geometryYPos)
                                          (cons (+ geometryXPos (* base geometryScaling)) (+ geometryYPos (* height geometryScaling)))
                                          (cons geometryXPos (+ geometryYPos (* height geometryScaling)))))
  (send device-context draw-text (string-append "Base: " (number->string base)) (- (+ geometryXPos (* (/ base 2) geometryScaling)) 30) (+ geometryYPos (* height geometryScaling) 10))
  (send device-context draw-text (string-append "Height: " (number->string height)) (- geometryXPos 100) geometryYPos)
  (draw-text-problem-with-multiple-lines device-context problemDescriptionList)
  )


(provide draw-parallelogram)

; Draws an equilateral triangle and a problem description onto the given device context
; Parameters:
; device-context: the device context
; side-length: the problem length of each side
; problemDescriptionList: a list of strings, with each string being a separate line of text
(define (draw-equi-triangle device-context side-length problemDescriptionList)
  (send device-context draw-polygon (list (cons 125 30) (cons 100 60) (cons 150 60)))
  (draw-text-problem-with-multiple-lines device-context problemDescriptionList)
  )


(provide draw-equi-triangle)

; Draws a generic triangle and a problem description onto the given device context
; Parameters:
; device-context: the device context to draw on
; base: the base of the triangle
; height: the height of the triangle
; problemDescriptionList: a list of strings, with each string being a separate line of text
(define (draw-regular-triangle device-context base height problemDescriptionList)
  (send device-context draw-polygon (list (cons geometryXPos geometryYPos)
                                          (cons (- geometryXPos (* geometryScaling (/ base 2))) (+ geometryYPos (* geometryScaling height)))
                                          (cons (+ geometryXPos (* geometryScaling (/ base 2))) (+ geometryYPos (* geometryScaling height)))
                                          )
        )
  (send device-context draw-text (string-append "Base: " (number->string base)) (- (+ geometryXPos (* (/ base 2) geometryScaling)) 30) (+ geometryYPos (* height geometryScaling) 10))
  (send device-context draw-text (string-append "Height: " (number->string height)) (- geometryXPos 100) geometryYPos)
  (draw-text-problem-with-multiple-lines device-context problemDescriptionList)
  )

(provide draw-regular-triangle)

; Draws a circle onto the given device context
; Parameters:
; device-context: the device context
; radius: the radius of the circle
; problemDescriptionList: a list of strings, with each string being a separate line of text
(define (draw-circle device-context radius problemDescriptionList)
  (send device-context draw-ellipse geometryXPos geometryYPos (* radius geometryScaling) (* radius geometryScaling))
  (send device-context draw-text (string-append "Radius: " (number->string radius)) (- geometryXPos 100) geometryYPos)
  (draw-text-problem-with-multiple-lines device-context problemDescriptionList)
  )



(provide draw-circle)
