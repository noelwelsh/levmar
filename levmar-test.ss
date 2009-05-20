#lang scheme/base

(require scheme/match
         scheme/math
         scheme/foreign
         (planet schematics/schemeunit:3/test)
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
     (check-eq? (cvector-length covariance) 4)
     (check-false (zero? (cvector-ref covariance 0)))
     (check-false (zero? (cvector-ref covariance 1)))
     (check-false (zero? (cvector-ref covariance 2)))
     (check-false (zero? (cvector-ref covariance 3)))
     (check-= (cvector-ref covariance 1) (cvector-ref covariance 2) 0.00001))))
  