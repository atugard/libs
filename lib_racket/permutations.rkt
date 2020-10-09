(include "types.rkt")
(include "functions.rkt")
;;motivation for developing machinery to deal with permutations is to implement
;;general determinant formula for square matrices in vectors.rkt

(define (transposition a b)
  (lambda (x) 
    (cond [(= x a) b]
          [(= x b) a]
          [else x])))
(define (compose-transposition t1 t2)
  (lambda (x)
    (t1 (t2 x))))

(define (permutation . nums)
  (define (nums->transpositions _nums)
    (if (null?  (cdr _nums))
      null
      (cons (transposition (car _nums) (cadr _nums))  (nums->transpositions (cdr _nums)))))
  (apply compose (nums->transpositions nums)))

(define (compose-permutation p1 p2)
  (lambda (x) (p1 (p2 x))))

(define p (permutation 1 2 3 4))
(define q (permutation 1 2 3))
(define s (compose-permutation p q))
