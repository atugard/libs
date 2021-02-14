(require (planet williams/science/random-distributions/gaussian))
(require (planet williams/science/random-distributions/bernoulli))
(require math/distributions)


(define (replicate n e)
  (if (eq? n 0)
      null
      (cons e (replicate (- n 1) e))))
;Exercise 2.5
(define rewards
  (replicate 10 0))
(define (max-index l)
  (define (iter i max_index max_val l)
    (cond ((null? l) max_index)
          ((> (car l) max_val)
           (iter (+ i 1) i (car l) (cdr l)))
          (else
           (iter (+ i 1) max_index max_val (cdr l)))))
  (iter 0 0 0 l))
           
(define (zip l1 l2)
  (map list l1 l2))
(define (zipWith f l1 l2)
  (map (lambda (x) (apply f x)) (zip l1 l2)))
(define (sum ls)
  (apply + ls))
(define (average ls)
  (/ (sum ls) (length ls)))

(define (increment l)
  (map (lambda (x) (+ (random-gaussian 0 0.01))) l))
(define (update-estimate N Q R)
  (+ Q (* (/ 1 N) (- R Q))))

(define (epsilon-greedy trials n Qs Rs epsilon)
  (cond ((= trials 0)
         (/ (average (map abs (zipWith - Rs Qs))) (average Rs)))
        (else 
         (let ((choice 0))
           (if (= 1 (random-bernoulli epsilon))
               (set! choice (random (length Qs)))
               (set! choice (max-index Qs)))
           (epsilon-greedy (- trials 1)
                           (+ n 1)
                           (append (take Qs choice) (list (update-estimate n (list-ref Qs choice) (list-ref Rs choice))) (drop Qs (+ choice 1)))
                           (increment Rs)
                           epsilon)))))
          
(define (runs n average)
  (

;To see how long it takes to get a positive, for any given value of epsilon.
