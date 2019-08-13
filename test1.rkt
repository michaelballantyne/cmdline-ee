#lang racket

(require "main.rkt")

(require rackunit)

(let ()
  (define/command-line-options
    #:argv (list "1" "2")
    #:arguments
    x
    [y simple-argument])
  (check-equal? x "1")
  (check-equal? y "2"))