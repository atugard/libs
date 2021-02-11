(require (planet williams/science/random-distributions/gaussian))
(require (planet williams/science/random-distributions/bernoulli))
(require math/distributions)


(define (replicate n e)
  (if (eq? n 0)
      null
      (cons e (replicate (- n 1) e))))
;Exercise 2.5
(define initial-rewards
  (replicate 10 0))
;need to do quite a bit at the same time... want to modularize this.
;Can implement epsilon-greedy with Bernoulli distribution with p=epsilon
;Then if the return value is 0, you greedy select...
;if the return value is 1, you randomly select amongst options...
(define (rewards n)
  (define (increment k l)
    (if (= k 0)
        l
        (increment (- k 1) (map (lambda (x) (+ (random-gaussian 0 0.1) x)) l))))
  (increment n initial-rewards))

;To see how long it takes to get a positive, for any given value of epsilon.
(define (test n epsilon)
  (if (= 1 (random-bernoulli epsilon))
      n
      (test (+ n 1) epsilon)))
