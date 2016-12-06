#lang racket

(require math)

(define (vectorCrossProduct vec_one vec_two) ; Crossproduct of two vectors
  (let(
       [x (- (* (list-ref vec_one 1)(list-ref vec_two 2))(* (list-ref vec_one 2)(list-ref vec_two 1)))]
       [y (- (* (list-ref vec_one 2)(list-ref vec_two 0))(* (list-ref vec_one 0)(list-ref vec_two 2)))]
       [z (- (* (list-ref vec_one 0)(list-ref vec_two 1))(* (list-ref vec_one 1)(list-ref vec_two 0)))])
    (list x y z)
))

;;;(vectorCrossProduct '(1 2 3) '(4 5 6))

(define (vectorAdd vec_one vec_two) ; Vector addition 
  (let(
       [x (+(list-ref vec_one 0)(list-ref vec_two 0))]
       [y (+(list-ref vec_one 1)(list-ref vec_two 1))]
       [z (+(list-ref vec_one 2)(list-ref vec_two 2))])
    (list x y z)
    ))

;;;(vectorAdd '(1 2 3) '(4 5 6))

(define (vectorSub vec_one vec_two) ; Vector substraction
  (let(
       [x (-(list-ref vec_one 0)(list-ref vec_two 0))]
       [y (-(list-ref vec_one 1)(list-ref vec_two 1))]
       [z (-(list-ref vec_one 2)(list-ref vec_two 2))])
    (list x y z)
    ))

;;;(vectorSub '(1 2 3) '(4 5 6))

(define (dotProductO point mod_matrix) ; 4*4 matrix (multiplikation) the old function
  (for/list ([m mod_matrix])
    (+ (*(list-ref m 0)(list-ref point 0))
       (*(list-ref m 1)(list-ref point 1))
       (*(list-ref m 2)(list-ref point 2))
       (*(list-ref m 3)(list-ref point 3))
       )
    ))

;;;(dotProductO '(1 1 1 1) '((2 2 2 1) (2 2 2 1) (2 2 2 1) (2 2 2 1)))

(define (dotProduct point mod_matrix); any size 1*1 matrix and upp... (multiplikation)
  (for/list ([m mod_matrix])
    (apply +
           (for/list ([p point] [n m])
             (* p n)))
    ))

;;;(dotProduct '(1 1 1 1) '((2 2 2 1) (2 2 2 1) (2 2 2 1) (2 2 2 1)))

(define (vectorVectorAngle vec_one vec_two) ; Angle between vectors
  (acos (/ (+ (*(list-ref vec_one 0)(list-ref vec_two 0))
              (*(list-ref vec_one 1)(list-ref vec_two 1))
              (*(list-ref vec_one 2)(list-ref vec_two 2)))
           (* (sqrt(+ (expt(list-ref vec_one 0) 2) (expt(list-ref vec_one 1) 2) (expt(list-ref vec_one 2) 2)))
              (sqrt(+ (expt(list-ref vec_two 0) 2) (expt(list-ref vec_two 1) 2) (expt(list-ref vec_two 2) 2)))
              )
           )))

;;;(*(vectorVectorAngle '(5 0 0) '(0 5 0)) (/ 180 pi))

(define (makeVector frompoint topoint) ;Make a vector
  (for/list ([f frompoint] [t topoint])
    (- f t))
  )

;;;(makeVector '(2 2 2) '(4 4 4))

(define (zInTriangle a b c point) ;;;3 point triangle and point to check for z
  (let*([v1 (makeVector a b)]
        [v2 (makeVector a c)]
        [n  (vectorCrossProduct v1 v2)]
        [k  (dotProduct n (list a))])
     (* (/ 1 (list-ref n 2))
        (- (list-ref k 0) (* (list-ref n 0)(list-ref point 0)) (* (list-ref n 1)(list-ref point 1)))
        )
    )
  )

;;;(zInTriangle '(0 0 1) '(5 0 1) '(0 5 1) '(2 2 0))

(define (pointInTriangle a b c point) ;;; Check if point is inside triangle
  (let* (
        [dn (+(*(-(list-ref b 1)(list-ref c 1))(-(list-ref a 0)(list-ref c 0)))
             (*(-(list-ref c 0)(list-ref b 0))(-(list-ref a 1)(list-ref c 1))))]
        [an (/(+(*(-(list-ref b 1)(list-ref c 1))(-(list-ref point 0)(list-ref c 0)))
             (*(-(list-ref c 0)(list-ref b 0))(-(list-ref point 1)(list-ref c 1)))) dn)]
        [bn (/(+(*(-(list-ref c 1)(list-ref a 1))(-(list-ref point 0)(list-ref c 0)))
             (*(-(list-ref a 0)(list-ref c 0))(-(list-ref point 1)(list-ref c 1)))) dn)]
        [cn (- 1 an bn)])
    (if (and (<= 0 an)(<= an 1)(<= 0 bn)(<= bn 1)(<= 0 cn)(<= cn 1))
        true
        false)))

;;;(pointInTriangle  '(1 1 0) '(5 0 0) '(0 5 0) '(2 2 0))
;;;(pointInTriangle  '(1 1 0) '(5 0 0) '(0 5 0) '(5 5 0))

(define (axisRotation matrix rx ry rz)
  (let* (
        [x (list (list 1 0 0 0)
                 (list 0 (cos rx) (* (sin rx) -1) 0)
                 (list 0 (sin rx) (cos rx) 0)
                 (list 0 0 0 1))]
        [y (list (list (cos ry) 0 (sin ry) 0)
                 (list 0 1 0 0)
                 (list (* (sin ry)-1) 0 (cos ry) 0)
                 (list 0 0 0 1))]
        [z (list (list (cos rz) (* (sin rz)-1) 0 0)
                 (list (sin rz) (cos rz) 0 0)
                 (list 0 0 1 0)
                 (list 0 0 0 1))]
        [rxy  (for/list ([rty y]) (dotProduct rty x))]
        [rxyz (for/list ([rtz z]) (dotProduct rtz rxy))])
        (for/list ([xyz matrix]) (dotProduct xyz rxyz))
    ))

;;;(axisRotation '((1 1 0 1)(3 3 0 1)(0 0 0 1)) 0 0 1)

(define (translationMatrix matrix dx dy dz) ; Moves object around
  (let ([tm (list (list 1 0 0 dx)
                  (list 0 1 0 dy)
                  (list 0 0 1 dz)
                  (list 0 0 0 1 ))])
    (for/list ([xyz matrix])(dotProduct xyz tm))))

;;;(translationMatrix '((2 2 2 1)(3 3 3 1)) 1 2 3)

(define (wDivide matrix) ; Makes the magic 3D effect come to life!
  (for/list ([point matrix])
    (list (/(list-ref point 0)(list-ref point 3))
          (/(list-ref point 1)(list-ref point 3))
          (/(list-ref point 2)(list-ref point 3))
          (/(list-ref point 3)(list-ref point 3)))
    ))

;;;(wDivide '((2 3 4 0.5)(2 3 4 0.2)))

(define (projectionMatrix d dx dy dz rx ry rz matrix) ;  d = angle of view, xyz position, xyz rotation, world. 
  (let* ([PM (list (list 1 0 0 0)
                   (list 0 1 0 0)
                   (list 0 0 1 0)
                   (list 0 0 (/ 1 d) 0))]
         [MatrixT (translationMatrix matrix (* -1 dx) (* -1 dy) (* -1 dz))]
         [MatrixTR (axisRotation MatrixT rx ry rz)]
         [cam (for/list ([xyz MatrixTR]) (dotProduct xyz PM))]
         [camc (for/list ([point cam] #:when (> (list-ref point 2) 0)) point )]
         )
    (if (> (length camc) 0) (wDivide camc) camc)))

;;;(projectionMatrix 50 1 2 3 0 0 0 '((1 2 1 1)(7 8 9 1)(4 5 6 1)))