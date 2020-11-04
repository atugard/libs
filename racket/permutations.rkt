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

(define identity-permutation
  (compose))

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


;;1) generate a list of generators. An easy one is just (1 2), (2 3), ..., (n-1 n).
(define (G n)
  (define (rec m)
    (if (= m n)
      null
      (cons (transposition m (+ m 1)) (rec (+ m 1)))))
  (rec 1))
;;Will return ((1 2) ... (n-1 n))

;;2) use this list to generate S_n.
;;(S 2) = {e (12)}, (G 2)={(12)}: 
;;; 0
;;(S 3) = {e (12) (23) (13) (123) (132)}, (G 2)={(12) (23)}: 
;;; 0, 1, 01, 10, 010
;;(S 4) = {e (12) (23) (34) (13) (14) (123) (132) (124) (142) (1234) (1243) (1324) (1342) (1423) (1432)}
;;(G 4) = {(12), (23), (34)}
;;;0, 1, 2, 01, 10, 02, 20, 03, 30, 010, 020,
(define (S n)
  (display "TBDDDDDDDDDD"))
