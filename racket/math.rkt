;;#lang racket 
;;
;;(provide dec inc square abs sqrt sum product derivative definite-integral indefinite-integral)

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

(define (range n)
  (define (generate k)
    (if (eq? k n)
      null
      (cons k (generate (+ k 1)))))
  (generate 0))

(define (definite-integral f)
  (lambda (a b)
    (let ((_a (* a 1.0))
          (_b (* b 1.0)))
      ;;split interval [a,b] into [t_0, t_1], ..., [t_n-1, t_n], where t_0=a, t_n=b, and t_{i+1}-t_{i}=b-a/n,
      ;;so t_i = i(b-a)/n + a 
      ;;Our integral will be approximated by \sum_{i=0}^{n-1} f((t_i + t_i+1})/2)(b-a)/n
      ;;we can find (t_i+t_{i+1})/2 = i(b-a)/n + (b-a)/2n, so our integral approximation is
      ;;\sum_{i=0}^{n-1} f(i(b-a)/n + (b-a)/2n)(b-a)/n

      (define (rec i n)
        (if (eq? i n)
          0
          (let ((t_i  (+ (* i (/ (- _b _a) n)) (/ (- _b _a) (* 2 n)))))
            (+ (f t_i) (rec (+ i 1) n)))))
      (* (rec 0 10000) (/ (- _b _a) 10000)))))

(define (indefinite-integral f)
  (let ((defintegralf (definite-integral f)))
    (lambda (x) (defintegralf 0 x))))

(define (derivative f)
  (lambda (x)
    (define (approx h)
      (/ (- (f (+ x h)) (f x)) h))
    (approx 0.000000001)))

(define (sum numbers)
  (apply + numbers))
(define (product numbers)
  (apply * numbers))
(define (factorial n)
  (product (cdr (range (+ n 1)))))

;;Make n-dimensional vectors as coordinates
;;square root approximation?






