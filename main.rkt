#lang racket

;(require math)
(include "tdmath.rkt")
(require racket/gui)
(require table-panel)

(define (mybutton the_parent the_label the_func) ;Creates button with function
                 (new button%
                     [parent the_parent]
                     [label the_label]
                     [min-width 20]
                     [min-height 20]
                     [callback (lambda (button event) (eval the_func))]))

(define myframe (new frame%
                     [width 300]
                     [height 300]
                     [label "3Dtestground"]))

(define grid (new table-panel%
                  [parent myframe]
                  [alignment '(center top)]
                  [dimensions '(2 1)]))

(define (paintTriangle canvas dc t) ;Paint a triangle
  (send dc set-smoothing 'smoothed)
  (send dc set-brush (make-object brush% (make-color (list-ref t 6)(list-ref t 7)(list-ref t 8)) 'solid))
  (send dc set-alpha 1)
  (send dc draw-polygon (list (make-object point% (list-ref t 0) (list-ref t 1))
                              (make-object point% (list-ref t 2) (list-ref t 3))
                              (make-object point% (list-ref t 4) (list-ref t 5))
                               )0 0 'winding) )

(define (paintTriangles canvas dc ts) ;Paint triangles
  (for ([t ts])
    (paintTriangle canvas dc t)))


(define ts '((100 100 100 150 200 100 0 100 0)
             (200 220 200 250 250 200 0 255 0)
             (300 320 300 350 350 300 0 0 255)))


(define my_canvas (new canvas%
                 [parent grid]
                  [min-width 400]
                  [min-height 400]
                  [paint-callback 
                     (lambda (canvas dc)
                       (paintTriangles canvas dc ts))] 
                  ))

(define button_grid (new table-panel%
                         [parent grid]
                         [alignment '(center bottom)]
                         [dimensions '(4 6)]))

(define buttons (let* (
                    [button_1 (mybutton button_grid "+Zroll" '(+ 1 1))]
                    [button_2 (mybutton button_grid "-Zroll" '(+ 1 1))]
                    [button_3 (mybutton button_grid "+Xroll" '(+ 1 1))]
                    [button_4 (mybutton button_grid "-Xroll" '(+ 1 1))]
                    [button_5 (mybutton button_grid "+Yroll" '(+ 1 1))]
                    [button_6 (mybutton button_grid "-Zroll" '(+ 1 1))]
                    [button_7 (mybutton button_grid "+Z" '(+ 1 1))]
                    [button_8 (mybutton button_grid "-Z" '(+ 1 1))]
                    [button_9 (mybutton button_grid "+X" '(+ 1 1))]
                    [button_10 (mybutton button_grid "-X" '(+ 1 1))]
                    [button_11 (mybutton button_grid "+Y" '(+ 1 1))]
                    [button_12 (mybutton button_grid "-Y" '(+ 1 1))]
                  )
                  '()))

(define refreshTimer
  (new timer% [notify-callback (lambda () (send my_canvas refresh))]))


(define dc (send my_canvas get-dc))

(send myframe show #t)
(send refreshTimer start 16 #f)