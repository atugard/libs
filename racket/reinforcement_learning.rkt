(require (planet williams/science/random-distributions/gaussian))
(require math/distributions)


(define (replicate n e)
  (if (eq? n 0)
      null
      (cons e (replicate (- n 1) e))))
;Exercise 2.5
(define test2
  (random-gaussian 0 0.1))

(define initial-rewards
  (replicate 10 0))

;need to do quite a bit at the same time... want to modularize this.
(define (rewards n)
  (define (increment k l)
    (if (= k 0)
        l
        (increment (- k 1) (map (lambda (x) (+ (random-gaussian 0 0.1) x)) l))))
  (increment n initial-rewards))
