(include "math.rkt")
(include "types.rkt")


;;vector stuff
(define (vector . nums)
  (cond [(null? nums) (list 'vector)]
        [(not (number-list? nums))
         (error "vector takes either no arguments or arguments of type number. you gave: " nums)]
        [else 
          (cons 'vector nums)]))
(define empty-vector
  (vector))
(define (empty-vector? v)
  (equal? v empty-vector))
(define (coords v)
  (cdr v))
(define (vector? v)
  (if (null? v)
    false
    (and (eq? 'vector (car v)) (number-list? (coords v)))))
(define (vector-list? vs)
  (type-list? vs vector?))

;;made a change such that the dimension of a matrix is a tuple (n, m)... where
;;A = [T]_{st}, T:V->W, n=dim(V), m=dim(W).
(define (dim t)
  (cond [(vector? t) (length (coords t))]
        [(matrix? t)
         (let ((vs (vectors t)))
           (if (null? vs)
             0
             (list (dim (car vs)) (length vs))))]
        [else
          (error "Invalid input. dim requires either a vector or a matrix. You gave: " t)]))

(define (dot-product v1 v2)
  (cond [(not (and (vector-list? (list v1 v2)) (= (dim v1) (dim v2))))
         (error "Dot product is a binary operation that takes two vectors of equal dimension. You gave: " v1 v2)]
        [else
          (define (product-coords c1 c2)
            (if (null? c1)
              null
              (cons (* (car c1) (car c2)) (product-coords (cdr c1) (cdr c2)))))
          (sum (product-coords (coords v1) (coords v2)))]))

(define (equal-dim? d vs)
  (if (null? vs)
    true
    (and (= (dim (car vs)) d) (equal-dim? d (cdr vs)))))

(define (magnitude v)
  (sqrt (dot-product v v)))






;;matrix stuff

(define (matrix . vs)
  (cond [(null? vs) (list 'matrix)]
        [(not (vector-list? vs))
         (error "matrix takes either no arguments or arguments of type vector. you gave: " vs)]
        [else 
          (if (equal-dim? (dim (car vs)) (cdr vs))
            (cons 'matrix vs)
            (error "matrix takes either no arguments or vectors of the same dimension. you gave: " vs))]))
(define (vectors m)
  (cdr m))
(define empty-matrix
  (matrix))


(define (empty-matrix? m)
  (equal? m empty-matrix))
(define (matrix? m)
  (cond [(null? m) false]
        [(not (eq? (car m) 'matrix)) false]
        [(empty-matrix? m) true]
        [else
          (let ((vs (vectors m)))
              (and (vector-list? vs) (equal-dim? (dim (car vs)) (cdr vs))))]))
(define (transpose m)
  (cond [(matrix? m)
         ;;(matrix v1 v2 v3) -> (matrix (map f v1 v2 v3))
         (define (->transpose vs)
           (apply matrix (apply map (cons (lambda cs (apply vector cs)) (map coords vs))))) ;; ((c1, c2) -> (vector (car c1) (car c2)) cs=(c1, c2) = ((coords v1) (coords v2))
         (->transpose (vectors m))]
        [else
          (error "transpose is a unary operator with one parameter of type matrix. you gave: " m)]))


(define (matrix-multiplication m1 m2)
  (let* ((d1 (dim m1)) ;; dim(m1) = d1 = (a1 b1)
         (a1 (car d1))
         (b1 (cadr d1))
         (d2 (dim m2)) ;; dim(m2) = d2 = (a2 b2)
         (a2 (car d2))
         (b2 (cadr d2)))
      (if (not (= b1 a2))
        (error "tbd " m1 m2)
        (let* ((m2t (transpose m2))
               (cols (vectors m2t)))
          (define (generate-row row)
            (apply vector (map (lambda (col) (dot-product row col)) cols)))
          (let ((rows (vectors m1)))
            (define (loop i)
              (if (= i b1)
                null
                (let ((row_i (list-ref rows i)))
                  (cons (generate-row row_i) (loop (+ i 1))))))
            (apply matrix (loop 0)))))))





;;playing around
;;(define I3 (matrix (vector 1 0 0) 
;;                   (vector 0 1 0) 
;;                   (vector 0 0 1)))
;;
;;(define A (matrix (vector 1 2 3)
                  (vector 5 4 3)
                  (vector 3 1 5)))



