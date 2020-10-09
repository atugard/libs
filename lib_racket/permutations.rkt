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

(define (sgn p)
  (define (find-max n)
    (if (not (= (p n) n))
      (find-max (+ n 1))
      n))
  (define (loop i j n result)
    (cond [(= i n) result]
          [(< j n)
           (if (< (p j) (p i))
             (loop i (+ j 1) n (+ result 1))
             (loop i (+ j 1) n result))]
          [else
            (loop (+ i 1) (+ i 2) n result)]))
 (expt -1 (loop 0 1 (find-max 1) 0)))


;;generate a list of all permutations of {1, 2, ..., n}
(define (S_n n)
  (display 'tbd))



(define p (permutation 1 2 3 4))
(define q (permutation 1 2 3))
(define s (compose-permutation p q))

