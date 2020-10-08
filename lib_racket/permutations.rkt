(include "types.rkt")
;;motivation for developing machinery to deal with permutations is to implement
;;general determinant formula for square matrices in vectors.rkt

(define identity-transposition
  '(transposition))
(define (identity-transposition? a)
  (equal? identity-transposition))

;;Develop transposition level machinery, and use it to recursively define higher order permutation machinery.
(define (transposition . nums )
  (cond [(null? nums) identity-transposition]
        [(and (number-list? nums) (= (length nums) 2)) (cons 'transposition nums)]
        [else
          (error "transposition takes either no arguments or two arguments of type number. you gave: " nums)]))
(define (transposition? t)
  (or (not (null? t)) 
      (and (eq? 'transposition (car t)) 
           (or (= (length (cdr t)) 0 ) (= (length (cdr t) 2))))))
(define (transposition-list? ts)
  (type-list? ts transposition?))


;;We have sig = t_n t_{n-1} ... t_0. Transpositions are not commutable. 
;;I'll adopt composition right  to left to match the way its written in math, so
;;sig = compose-tranpose(t_n t_{n-1} ... t_0)
;;compose-transpose will be defined recursively, with base case of two transposes.

(define (transpose->function t)
  (lambda (num)
    (let ((nums (cdr t)))
      (if (= (car nums) num)
        (cadr nums)
        (if (= (cadr nums) num)
          (car nums)
          num)))))
;we always represent permutations as lists of transpositions
;;and define a binary operator op:: (transposition, permutation)->permutation | (permutation, transposition)-> permutation
(define (permutation . params)
  (cond [(number-list? params)
         (define (nums->transpositions nums)
           (define (rec-decomp ns)
             (if (null? (cdr ns))
               null
               (cons (transposition (car ns) (cadr ns)) (rec-decomp (cdr ns)))))
           (rec-decomp nums))
           (cons 'permutation (nums->transpositions params))]
        [(transposition-list? params)
         (cons 'permutation params)]
        [else
          (error "permutation is an n-ary operator that either takes all arguments of type number or all arguments of type transposition. you gave: " params)]))

(define p (permutation 5 4 3 2 1))
