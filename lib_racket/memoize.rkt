#lang racket

(require "table.rkt")

(provide memoize)

(define (memoize f)
  (let ((tbl (make-table)))
    (lambda x
      (let ((val (get-val x tbl)))
        (if (null? val)
            (let ((ans (apply f x)))
              (insert-table! x ans tbl)
              ans)
            val)))))

