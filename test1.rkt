#lang racket

(require "main.rkt")

(require rackunit)


(let ()
  (define (int-range/p min max)
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
    [z (checked-argument "foo" (int-range/p 1 5))]
    #:rest [r (checked-argument "foo" (int-range/p 1 5))])
  (check-equal? x "1")
  (check-equal? y "2")
  (check-equal? z 3)
  (check-equal? r (list 4))
  (check-equal? foo "v"))

(let ()
  (define/command-line-options
    #:argv (list "--verbose" "--lf" "a" "--lf" "b" "--optimize-level" "2")
    #:options
    [verbose (switch/o "--verbose" "enable verbose output")]
    [link-flags (list/o "--lf" lf "add a link flag")]
    [opt-level (choice #:default 0
                       ["--optimize-level" [lvl (int-range/argt 0 3)]
                                           "set optimization level to <lvl>" lvl]
                       (numbered-flags/f "--o" [0 3] "optimization level"))])
  (check-equal? verbose #t)
  (check-equal? link-flags (list "a" "b"))
  (check-equal? opt-level 2)
  )
