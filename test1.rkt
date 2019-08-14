#lang racket

(require "main.rkt")

(require rackunit)


(let ()
  (define (nat-range/p min max)
    (lambda (s)
      (let ([n (string->number s)])
        (unless (and (integer? n) (>= n min) (<= n max))
          (raise-user-error (format "expected integer between ~a and ~a"
                                    min max)))
        n)))
  
  (define/command-line-options
    #:argv (list "--a" "v" "1" "2" "3" "4")
    #:options
    [foo (choice #:required
                 ["--a" v "set foo" v])]
    #:arguments
    x
    [y simple-argument]
    [z (checked-argument "foo" (nat-range/p 1 5))]
    #:rest [r (checked-argument "foo" (nat-range/p 1 5))])
  (check-equal? x "1")
  (check-equal? y "2")
  (check-equal? z 3)
  (check-equal? r (list 4))
  (check-equal? foo "v"))