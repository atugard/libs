;;#lang racket
;;
;;
;;(provide mlist mlist? insert-mlist! in-mlist? list->mlist mlist->list mlength mmap mcadr mcdar mcaar mcaaar mcddr mcaddr mcdddr mcadddr)


(define (mlist . args)
  (if (null? args)
      null
      (mcons (car args) (apply mlist (cdr args)))))

(define (mlist? ml)
  (cond [(null? ml) true] 
        [(not (mpair? ml)) false]
        [else
         (mlist? (mcdr ml))]))

(define (list->mlist l)
  (apply mlist l))


;;List operations
(define (mlength ml)
  (define (iter size result)
    (if (null? result)
        size
        (iter (+ size 1) (mcdr result))))
  (iter 0 ml))

(define (mlist-ref ml i)
  (cond [(< i 0) (error "Index must be non-negative. index: " i)]
        [(>= i (mlength ml)) (error "list-ref: index too large for list. index: " i "in: " ml)]
        [(not (mpair? ml)) (error "list-ref: index reaches a non-pair. index: " i "in: " ml)]
        [(= i 0) (mcar ml)]
        [else (mlist-ref (mcdr ml) (- i 1))]))
(define (mappend ml1 ml2)
  (if (null? ml1)
      ml2
      (mcons (mcar ml1) (mappend (mcdr ml1) ml2))))

(define (mmap prod . mls)
  (if (null? (car mls))
      null
      (mcons (apply prod (map mcar mls)) (apply mmap (cons prod (map mcdr mls))))))

(define (mlist->list ml)
  (if (null? ml)
      null
      (cons (mcar ml) (mlist->list (mcdr ml)))))

;;List Searching
(define (mmember-factory pred)
  (define (func a ml)
    (cond [(null? ml) false]
          [(pred (mcar ml) a) ml]
          [else
           (func a (mcdr ml))]))
  func)
(define mmember
  (mmember-factory equal?))
(define mmemv
  (mmember-factory eqv?))
(define mmemq
  (mmember-factory eq?))
(define (mmemf pred ml)
  (cond [(null? ml) false]
        [(pred (mcar ml)) (mcar ml)]
        [else
         (mmemf pred (mcdr ml))]))

;;Pair Accesor Shorthands
(define (mcaar ml)
  (mcar (mcar ml)))
(define (mcadr ml)
  (mcar (mcdr ml)))
(define (mcdar ml)
  (mcdr (mcar ml)))
(define (mcddr ml)
  (mcdr (mcdr ml)))
(define (mcaaar ml)
  (mcar (mcaar ml)))
(define (mcaadr ml)
  (mcar (mcadr ml)))
(define (mcadar ml)
  (mcar (mcdar ml)))
(define (mcaddr ml)
  (mcar (mcddr ml)))
(define (mcdaar ml)
 (mcdr (mcaar ml)))
(define (mcdadr ml)
  (mcdr (mcadr ml)))
(define (mcdddr ml)
  (mcdr (mcddr ml)))
(define (mcadddr ml)
  (mcar (mcdddr ml)))
(define (mcaaadr ml)
  (mcar (mcaadr ml)))
(define (mcaadar ml)
  (mcar (mcadar ml)))
(define (mcaaddr ml)
  (mcar (mcaddr ml)))
(define (mcadaar ml)
  (mcar (mcdaar ml)))
(define (mcadadr ml)
  (mcar (mcdadr ml)))
(define (mcaddar ml)
  (mcar (mcddar ml)))
(define (mcadddr ml)
  (mcar (mcdddr ml)))
(define (mcdaaar ml)
  (mcdr (mcaaar ml)))
(define (mcdaadr ml)
  (mcdr (mcaadr ml)))
(define (mcdadar ml)
  (mcdr (mcadar ml)))
(define (mcdaddr ml)
  (mcdr (mcaddr ml)))
(define (mcddaar ml)
  (mcdr (mcdaar ml)))
(define (mcddaar ml)
  (mcdr (mcdaar ml)))
(define (mcddadr ml)
  (mcdr (mcdadr ml)))
(define (mcdddar ml)
  (mcdr (mcddar ml)))
(define (mcddddr ml)
  (mcdr (mcdddr ml) ))

(mmap (lambda (x y) (sqrt (+ x y))) (mlist 1 2 3) (mlist -1 -2 -3))












