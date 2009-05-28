#lang scheme/base

(require scheme/match
         scheme/math
         scheme/foreign
         (planet schematics/schemeunit:3/test)
         (planet schematics/numeric:1/matrix)
         (planet schematics/numeric:1/f64vector)
         "levmar.ss")

;; Data taken from a noiseless sine.
;;
;; 2sin(x + pi/2)   [I.e. 2cos(x)]
;;
;; Task is to fit the phase parameter and amplitude
(define sine-output
  (vector 2 (* 2 (cos (/ pi 4))) 0        (* 2 (cos (* 3/4 pi))) -2))
(define sine-input
   (vector 0 (/ pi 4)             (/ pi 2) (* 3/4 pi)             pi))

(define (sine params _)
  (define a (f64vector-ref params 0))
  (define p (f64vector-ref params 1))
  (define (f x) (* a (sin (+ x p))))

  (for/f64vector ([i (vector-length sine-output)]
                  [x (in-vector sine-input)]
                  [y (in-vector sine-output)])
                 (- y (f x))))

(define/provide-test-suite levmar-tests
  (test-case
   "fit sine"
   (define start-point (f64vector 1.9 0.001))
   (let-values (([start-point covariance]
                 (minimize sine
                           (vector-length sine-output)
                           start-point
                           #f)))
     (let ([a (f64vector-ref start-point 0)]
           [p (f64vector-ref start-point 1)])
       (check-= a 2.0 0.00001)
       (check-= p (/ pi 2) 0.00001))
     ;; Check Covariance is complete and symmetric
     (check-eq? (matrix-rows covariance) 2)
     (check-eq? (matrix-cols covariance) 2)
     (check-false (zero? (matrix-ref covariance 0 0)))
     (check-false (zero? (matrix-ref covariance 0 1)))
     (check-false (zero? (matrix-ref covariance 1 0)))
     (check-false (zero? (matrix-ref covariance 1 1)))
     (check-= (matrix-ref covariance 0 1) (matrix-ref covariance 1 0) 0.00001))))
  