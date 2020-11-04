#lang racket
(require "types.rkt")
(require "mutable.rkt")

(provide make-table table? get-val insert-table!)

(define (make-table . args)
  (cond [(null? args) (mlist '*table*)]
        [(null? (cdr args)) 
         (let ((keys (car args)))
           (define (rec ks)
             (if (null? ks)
                 null
                 (mcons (mcons (car ks) null) (rec (cdr ks)))))
           (mcons '*table* (rec keys)))]
        [(null? (cddr args))
         (let ((keys (car args))
               (vals (cadr args)))
           (cond [(= (length keys) (length vals))
                  (define (rec ks vs)
                    (if (null? ks)
                        null
                        (mcons (mcons (car ks) (car vs)) (rec (cdr ks) (cdr vs)))))
                  (mcons '*table* (rec keys vals))]
                 [else (error "Arguments must be lists of equal length. You gave: " keys vals)]))]
        [else
         (error "MAKE-TABLE takes at most two arguments. You gave: " (length args))]))
(define (record? record)
  (and (mpair? record) (not (mpair? (mcar record))) (not (mpair? (mcdr record)))))
(define (table? t)
  (if (mpair? t)
      (eq? (mcar t) '*table*)
      false))
(define (record->key record)
  (mcar record))
(define (record->val record)
  (mcdr record))
(define (get-val key t)
  (if (table? t)
      (let ((data (mcdr t)))
        (cond [(not (null? data))
               (define (rec d)
                 (cond [(null? d) null]
                       [(equal? key (record->key (mcar d))) (record->val (mcar d))]
                       [else (rec (mcdr d))]))
               (rec data)]
              [else
               null]))
      null))
(define (insert-table! key val table)
  (if (table? table)
      (cond [(null? (mcdr table))
             (set-mcdr! table (mlist (mcons key val)))]
            [else
             (define (rec t)
               (cond [(null? (mcdr t))
                      (set-mcdr! t (mlist (mcons key val)))]
                     [(equal? (record->key (mcar t)) key)
                      (set-mcdr! (mcar t) val)]
                     [else
                      (rec (mcdr t))]))
             (rec (mcdr table))])
        [error "Second argument must be a table. You gave: " table]))
