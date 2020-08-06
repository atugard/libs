(require "mutable.rkt")

;;(provide rand)
;;
;;(define (rand n)
;;  (let ((already_seen null))
;;    (define (loop)
;;      (insert-mlist! n already_seen)
;;      (define m
;;        (middle-n-digits (* n n (expt 10 n))))
;;      (set! n m))
;;    (loop)))

;;m=2n
(define (middle-n-digits m n)
  (let* ((sn (number->string m))
         (k (string-length sn))
         (n (/ m 2))
         (lower (floor (/ n 2))))
    (define (rec i)
      (if (= i (- (+ lower n) 1))
          null
          (cons (string-ref sn i) (rec (+ i 1)))))
    (rec lower)))
k


    



;;seed_number = int(input("Please enter a four digit number:\n[####] "))
;;number = seed_number
;;already_seen = set()
;;counter = 0
;;
;;while number not in already_seen:
;;    counter += 1
;;    already_seen.add(number)
;;    number = int(str(number * number).zfill(8)[2:6])  # zfill adds padding of zeroes
;;    print(f"#{counter}: {number}")
;;
;;print(f"We began with {seed_number}, and"
;;      f" have repeated ourselves after {counter} steps"
;;      f" with {number}.")
