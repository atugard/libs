#lang racket 

(provide dec inc square abs sqrt)

(define (dec n)
  (- n 1))
(define (inc n)
  (+ n 1))
(define (square n)
  (* n n))
(define (abs n)
  (if (>= n 0)
      n
      (- n)))
(define (sqrt n)
    (define (sqrt-iter lower upper tolerance)
      (let ((middle (/ (+ lower upper) 2)))
        (cond [(< (abs (- (square middle) n)) tolerance) middle]
              [(< (square middle) n) (sqrt-iter middle upper tolerance)]
              [else
               (sqrt-iter lower middle tolerance)])))
  (sqrt-iter 0.0 n .00000000001))
             
