#lang racket/base

(require
  (only-in racket/cmdline parse-command-line)
  (for-syntax
   racket/base
   (rename-in syntax/parse [define/syntax-parse def/stx])
   racket/syntax
   syntax/stx
   ))

(provide define/command-line-options)

(begin-for-syntax
  ; option transformer
  (struct option-transformer-rep [proc])
  ; flag transformer?
  )


(define-syntax define/command-line-options
  (syntax-parser
    [(_ (~optional (~seq #:program name-expr))
        (~optional (~seq #:argv argv-expr))
        (~optional (~seq #:options [option-id:id option-spec] ...))
        (~optional (~seq #:arguments [arg-id:id arg-spec] ...))
        (~optional (~seq #:rest [rest-id:id rest-arg-spec])))
     #'(void)
     ]))

(define-syntax define-option-syntax
  (syntax-parser
    [(_ name:id e)
     #'(define-syntax name (option-transformer-rep e))]))

(begin-for-syntax
  (define-syntax-class flag-names
    #:attributes [names]
    (pattern s:string
             #:attr names (list (syntax->datum #'s)))
    (pattern (s:string ...)
             #:attr names (syntax->datum #'(s ...)))))

#;(define-option-syntax switch/o
  (syntax-parser
    [(_ flags:flag-names description:string)
     #'(option #f (once flags
                        #:transform (lambda (val) #t)
                        #:description description))]))

#;(define-flag-syntax numbered-flags/o
  (syntax-parser
    [(_ flags:flag-names [min:number max:number] description:string)
     (def/stx (f ...)
       (for/list ([n (in-range (syntax-e #'min) (syntax-e #'max))])
         (def/stx names (map (lambda (s) (format "~a~a" s n)) (attribute flags.names)))
         (def/stx description (format "set ~a to ~a" (syntax-e #'description) n))
         #`[names () #:transform (lambda (v) #,n) #:description description]))
     #'(once-any f ...)]))

#;(define-option-syntax list/o
  (syntax-parser
    [(_ flags:flag-names arg-spec description:string)
     #'(option '()
               (multi flags arg-spec
                      #:transform (lambda (v arg) (cons arg v))
                      #:description description))]))


(define string/p (lambda (s) s))
#;(define-arg-syntax string/arg
  (syntax-parser
    [(_ name:string desc:string)
     #'(simple-arg name desc string/p)]))

(define (nat-range/p min max)
  (lambda (s)
    (let ([n (string->number s)])
      (unless (and (integer? n) (>= min n) (<= n max))
        (raise-user-error "expected natural number between ~a and ~a"
                          min max))
      n)))
#;(define-arg-syntax nat-range/arg
  (syntax-parser
    [(_ name:string [min:number max:number])
     (def/stx desc (format "natural number between ~a and ~a" (syntax-e #'min) (syntax-e #'max)))
     #'(simple-arg name desc (nat-range/p min max))]))
