#lang racket

(require "mutable.rkt")

(provide rand-init)

(define (rand-init seed)
  (when (= (remainder seed 2) 1)
    (if (> (string-length (number->string (+ seed 1))) (string-length (number->string seed)))
        (set! seed (- seed 1))
        (set! seed (+ seed 1))))
  (let ((already_seen null)
        (n (string-length (number->string seed)))
        (s 13091206342165455529)
        (w 0))
    (define (zfill z fillnum)
      (define (add-zeroes z fillnum)
        (define (generatestring c len)
          (define (build-list i)
            (if (= i 0)
                null
                (cons c (build-list (- i 1)))))
          (apply string (build-list len)))
        (string-append z (generatestring '#\0 fillnum)))
      (let ((m (string-length z)))
        (cond [(<= fillnum m) z]
              [else
               (let ((k (- fillnum m)))
                 (add-zeroes z k))])))
    (define (generator number)
      (cond [(not (member number already_seen))
             (set! already_seen (cons number already_seen))
             number]
            [else
             (set! w (+ w s))
             (let ((next
                     (string->number
                      (substring
                       (zfill
                        (number->string (+ (* number number) w))
                        (* 2 n))
                       (floor (/ n 2))
                       (+ (floor (/ n 2)) n)))))
               (generator next))]))
    (lambda ()
      (generator seed))))


(define (vector v)
