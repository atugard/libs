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

(define (equal-transposition? t1 t2)
  (let ((_t1 (cdr t1))
        (_t2 (cdr t2)))
    (or (equal? _t1 _t2) (and (equal? (car _t1) (cadr _t2)) (equal? (car _t2) (cadr _t1))))))

(define (permutation . params)
  (define (num-occurences t ts)
    (define (iter i ts)
      (cond [(null? ts) i]
            [(equal-transposition? (car ts) t) (iter (+ i 1) (cdr ts))]
            [else 
              (iter i (cdr ts))]))
    (iter 0 ts))
  (define (pre . params)
    (cond [(number-list? params)
           (define (nums->transpositions nums)
             (define (rec-decomp ns)
               (if (null? (cdr ns))
                 null
                 (cons (transposition (car ns) (cadr ns)) (rec-decomp (cdr ns)))))
             (rec-decomp nums))
           (nums->transpositions params)]
          [(transposition-list? params) params]
          [else
            (error "permutation is an n-ary operator that either takes all arguments of type number or all arguments of type transposition. you gave: " params)]))
  (define (filter-duplicates t occurences ts)
    (cond  [(= occurences 1) ts]
           [(= (remainder occurences 2) 0) 
            (filter (lambda (x) (not (equal-transposition? x t))) ts)]
           [(> occurences 1)
            (cons t (filter (lambda (x) (not (equal-transposition? x t))) ts))]))
  (define (loop ts)
    (define (iter _ts result)
      (if (null? _ts)
        result
        (iter (cdr _ts) (filter-duplicates (car _ts) (num-occurences (car _ts) result) result))))
    (iter ts ts))
  (cons 'permutation (loop (apply pre params))))




(define p (permutation 5 4 3 2 1))
