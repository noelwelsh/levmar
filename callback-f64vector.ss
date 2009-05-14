#lang scheme/base

(require scheme/foreign)

(unsafe!)

;; (hashof f64vector cpointer)
(define callback-f64vector-map (make-hash))

(define (callback-f64vector-ref v)
  (hash-ref callback-f64vector-map v
            (lambda (v)
              (error (format "No ptr value associated with f64vector ~a\n" v)))))

(define (callback-f64vector-set! v ptr)
  (hash-set! callback-f64vector-map v ptr))

(define ((callback-f64vector-update-C! length) v)
  (let ([ptr (callback-f64vector-ref v)])
    (for ([idx (in-range length)])
         (ptr-set! ptr _double idx (f64vector-ref v idx)))
    ptr))

(define (f64vector-copy! src dest)
  (let ([l (f64vector-length src)])
    (for ([i (in-range l)])
         (f64vector-set! dest i (f64vector-ref src i)))))

;; Define an f64f64vector C type that can be used as
;; an input and output parameter in a callback
(define (callback-f64vector length)
  (_cpointer 'callback-f64vector
             _pointer
             ;; Scheme -> C
             (callback-f64vector-update-C! length)
             ;; C -> Scheme
             (lambda (ptr)
               (let ([v (make-f64vector length)])
                 (for ([offset (in-range length)])
                   (f64vector-set! v offset (ptr-ref ptr _double offset)))
                 (callback-f64vector-set! v ptr)
                 v))
             ))

(provide
 callback-f64vector
 f64vector-copy!
 callback-f64vector-update-C!)