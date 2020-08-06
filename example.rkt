#lang racket

(require "main.rkt"
         (for-syntax racket/base
                     (rename-in syntax/parse [define/syntax-parse def/stx])))

(define-flag-syntax numbered-flags/f
  (syntax-parser
    [(_ flags:flag-names [min:number max:number] desc:string)
     (def/stx (f ...)
       (for/list ([n (in-range (syntax-e #'min) (syntax-e #'max))])
         (def/stx names (for/list ([s (syntax->datum #'flags.names)]) (format "~a~a" s n)))
         (def/stx this-desc (format "set ~a to ~a" (syntax-e #'desc) n))
         #`[names this-desc #,n]))
     #'(begin f ...)]))

(define (existing-file/p str)
  (if (file-exists? str)
      str
      (raise-user-error "expected path to exisiting file")))

(define/command-line-options
  #:options
  [verbose-mode (switch/o ("-v" "--verbose") "Compile with verbose messages")]
  [profiling-on (switch/o ("-p" "--profile") "Compile with profiling")]
  [optimize-level
   (choice/o #:default 0
         ["--optimize-level" [lvl (int-range/p 0 3)]
                             "set optimization level to <lvl>" lvl]
         (numbered-flags/f "--o" [0 3] "optimization level"))]
  [output
   (required/o "-o" outfile "the output filename" outfile)]
  [link-flags (list/o ["-l" "--link-flags"] lf "Add a flag <lf> for the linker")]
  #:arguments
  [file-to-compile existing-file/p])
