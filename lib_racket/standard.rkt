;;(require "mutable.rkt")

;;(provide rand)
;;substring, string-append, string-length, build-string...
;;We can simply find the difference between the length and desired length, and append a string of that many 0s to the end.
;;I think it may be easiest to tranfer everything to lists, rather than strings, in Racket, because I suspect, anyways, strings are lists.


;
(define (number->list a)
  (string->list (number->string a)))
(define (list->number a)
  (number->string (string-> a)))

(define (zfill z n)
  (define (add-zeroes z n)
    (define (generatestring c len)
      (define (build-list i)
        (if (= i 0)
            null
            (cons c (build-list (- i 1)))))
      (apply string (build-list len)))
    (string-append z (generatestring '#\0 n)))
  (let ((m (string-length z)))
    (cond [(= n m) z]
          [(< n m) (error "String length must be less than fill number. String length: " m "Fill number: " n)]
          [else
           (let ((k (- n m)))
             (add-zeroes z k))])))


(define (rand seed)
  (let* ((already_seen null)
        (s (number->string seed))
        (n (length s)))
    (define (unnamed number)
      (if (not (membq already_seen))
          (pad-to-n (number->string (* number number))
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
