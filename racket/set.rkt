(define (set . nums)
;;a) Filter out duplicates from nums.
;;b) Tag the list with 'set.
  (define (iter index result)
    (cond [(null? index) result]
          [(memq (car index) result)
           (iter (cdr index) result)]
          [else 
            (iter (cdr index) (cons (car index) result))]))
  (cons 'set (iter nums null)))
(define empty-set
  (set))
(define (union s1 s2)
  (apply set (append (cdr s1) (cdr s2))))
(define (intersection s1 s2)
  ;; s1 = {a1 a2 ... an} s2 = {b1 b2 ... bm}
  (define (iter nums result)
    (cond [(null? nums) result]
          [(memq (car nums) (cdr s2))
           (iter (cdr nums) (cons (car nums) result))]
          [else 
            (iter (cdr nums) result)]))
  (cons 'set (iter (cdr s1) null)))
(define (difference s1 s2)
  ;; s1 = {a1 a2 ... an} s2 = {b1 b2 ... bm}
  (define (iter nums result)
    (cond [(null? nums) result]
          [(memq (car nums) (cdr s2))
            (iter (cdr nums) result)]
          [else 
           (iter (cdr nums) (cons (car nums) result))]))
  (cons 'set (iter (cdr s1) null)))


(define s1 (set 5 4 3 2 1))
(define s2 (set 7 2 3 4 9))
