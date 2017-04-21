#lang racket
;;The following is a function to estimate the area of the given reigon using
;;monte-carlo simulation, you need to provide a predicate for the reigon you
;;want to calculate area and a bounding box for that reigon
(define (is-it-inside? pred-area point)
  (pred-area point)) ;point is (cons x y)

(define (scaled-random x1 x2)
(+ x1 (* (random)
(- x2 x1))))

(define circle1 (lambda (p)
                  (let*([x (car p)]
                        [y (cdr p)])
                    (< (+ (* (- x 5) (- x 5)) (* (- y 7) (- y 7))) (* 3 3)))))

(define (monte-carlo pred-area ubx uby lbx lby n_trials)
  (define (monte-carlo-looper n_inside count)
    (if (> count n_trials)
        (let*([box_area (* (- uby lby) (- ubx lbx))])
          (* (/ n_inside n_trials) box_area))
        (let*([rand_x (abs (scaled-random ubx lbx))]
              [rand_y (abs (scaled-random uby lby))]
              [rand_p (cons rand_x rand_y)])
          (if (pred-area rand_p)
              (monte-carlo-looper (+ n_inside 1) (+ count 1))
              (monte-carlo-looper n_inside (+ count 1))))))
  (monte-carlo-looper 0 0))
