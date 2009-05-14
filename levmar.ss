#lang scheme/base

(require scheme/foreign
         scheme/runtime-path
         (planet schematics/numeric:1/f64vector)
         "callback-f64vector.ss")

(define-runtime-path here ".")


(unsafe!)

(define liblevmar (ffi-lib (build-path here "levmar-2.4" "liblevmar")))

(define (_levmar_eval_fun n-functions n-points)
  (_fun
   (callback-f64vector n-functions)
   (callback-f64vector n-points)
   _int ;; n-functions, m
   _int ;; n-points, n
   _scheme
   ->
   _void))

;; evaluate-shim : ((f6vector any) -> f64vector) -> _lm_evaluate_ftype -> void
(define (evaluate-shim f)
  ;; f64vector int f64vector any _ -> void
  (lambda (current-params current-point n-functions n-points data)
    (f64vector-copy! (f current-params data) current-point)
    ((callback-f64vector-update-C! (f64vector-length current-point)) current-point)))

(define (_dlevmar_dif n-functions n-points)
  (_fun
   (_levmar_eval_fun n-functions n-points)
                   ;; void (*func)(double *p, double *hx, int m, int n, void *adata)
                   ;; functional relation describing measurements.  A p \in R^m yields a \hat{x} \in  R^n
   (_f64vector i) ;; I/O: initial parameter estimates. On output contains the estimated solution
   (_f64vector i)  ;; I: measurement vector. NULL implies a zero vector
   _int            ;; I: parameter vector dimension (i.e. #unknowns), m
   _int            ;; I: measurement vector dimension, n n >= m
   _int            ;; I: maximum number of iterations 
   _pointer  ;; I: opts[0-4] = minim. options [\tau, \epsilon1, \epsilon2, \epsilon3, \delta]. Respectively the
                   ;; scale factor for initial \mu, stopping thresholds for ||J^T e||_inf, ||Dp||_2 and ||e||_2 and the
                   ;; step used in difference approximation to the Jacobian. If \delta<0, the Jacobian is approximated
                   ;; with central differences which are more accurate (but slower!) compared to the forward differences
                   ;; employed by default. Set to NULL for defaults to be used.
                    
   _pointer  ;; O: information regarding the minimization. Set to NULL if don't care
                   ;; info[0]= ||e||_2 at initial p.
                   ;; info[1-4]=[ ||e||_2, ||J^T e||_inf,  ||Dp||_2, \mu/max[J^T J]_ii ], all computed at estimated p.
                   ;; info[5]= # iterations,
                   ;; info[6]=reason for terminating: 1 - stopped by small gradient J^T e
                   ;;                                 2 - stopped by small Dp
                   ;;                                 3 - stopped by itmax
                   ;;                                 4 - singular matrix. Restart from current p with increased \mu 
                   ;;                                 5 - no further error reduction is possible. Restart with increased mu
                   ;;                                 6 - stopped by small ||e||_2
                   ;;                                 7 - stopped by invalid (i.e. NaN or Inf) "func" values. This is a user error
                   ;; info[7]= # function evaluations
                   ;; info[8]= # Jacobian evaluations
                   ;; info[9]= # linear systems solved, i.e. # attempts for reducing error
                     
   _pointer        ;; I: working memory, allocated internally if NULL. If !=NULL, it is assumed to point to 
                   ;; a memory chunk at least LM_DIF_WORKSZ(m, n)*sizeof(double) bytes long
                    
   (_f64vector i)  ;; O: Covariance matrix corresponding to LS solution; Assumed to point to a mxm matrix.
                   ;; Set to NULL if not needed.
                    
   _scheme
   ->
   _void))       ;; I: pointer to possibly needed additional data, passed uninterpreted to func.
                   ;; Set to NULL if not needed
                    
(define (lm-minimize n-points n-functions)
  (get-ffi-obj
   "dlevmar_dif"
   liblevmar
   (_dlevmar_dif n-points n-functions)))


(define (minimize f n-points start-params data)
  (let* ([n-functions (f64vector-length start-params)]
         [start-params-copy (f64vector-copy start-params)]
         [cov (make-f64vector (* n-functions n-functions))])
    (unless (>= n-points n-functions)
      (raise-mismatch-error
       'minimize
       (format "The number of points, ~a, is greater than or equal to the number of functions ~a." n-points n-functions)
       n-points))

    ((lm-minimize n-functions n-points)
     (evaluate-shim f)
     start-params-copy
     (f64vector-zeros n-points)
     n-functions
     n-points
     100
     #f
     #f
     #f
     cov
     data)
    (values start-params-copy cov)))

(provide
 minimize)