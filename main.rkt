#lang racket/base

(require
  (only-in racket/cmdline parse-command-line)
  ee-lib/define
  (for-syntax
   racket/base
   (rename-in syntax/parse [define/syntax-parse def/stx])
   racket/syntax
   syntax/stx
   ee-lib
   ))

(provide define/command-line-options)

(define-literal-forms cmdline-literals
  "command line option, flag, and argument specifiers cannot be used as expressions"
  (choice
   multi
   simple-argument
   checked-argument))

(define-extensible-syntax option-syntax)
(define-extensible-syntax flag-syntax)
(define-extensible-syntax argument-syntax)

(begin-for-syntax
  (define-syntax-class flag-names
    #:attributes [names]
    (pattern s:string
             #:attr names (list (syntax->datum #'s)))
    (pattern (s:string ...+)
             #:attr names (syntax->datum #'(s ...))))
  
  (define-syntax-class arg-spec
    #:attributes [name type]
    (pattern name:id
             #:attr type #'simple-argument)
    (pattern [name:id type]))
  
  (define-splicing-syntax-class required-or-default
    (pattern (~seq #:required default-expr))
    (pattern (~seq #:default)))

  (define/hygienic (expand-option stx) #:definition
    stx)

  (define/hygienic (expand-flag stx) #:definition
    stx)

  (define/hygienic (expand-argument-type stx) #:definition
    stx)
  )

(define-syntax define/command-line-options
  (syntax-parser
    [(_ (~optional (~seq #:program name-expr)
                   #:defaults ([name-expr #'(find-system-path 'run-file)]))
        (~optional (~seq #:argv argv-expr)
                   #:defaults ([argv-expr #'(current-command-line-arguments)]))
        (~optional (~seq #:options [option-id:id option-spec] ...)
                   #:defaults ([(option-id 1) '()] [(option-spec 1) '()]))
        (~optional (~seq #:arguments arg:arg-spec ...)
                   #:defaults ([(arg 1) #'()]))
        (~optional (~seq #:rest [rest-id:id rest-arg:arg-spec])))
     (ee-lib-boundary
      (def/stx (expanded-option-spec ...) (stx-map expand-option #'(option-spec ...)))
      (def/stx (expanded-arg-type ...) (stx-map expand-argument-type #'(arg.type ...)))
      (def/stx expanded-rest-arg-type (and (attribute rest-arg.type)
                                           (expand-argument-type #'rest-arg.type)))

      (def/stx table #''())
      (def/stx finish-proc #'(lambda (opts . args) (apply values args)))
      (def/stx arg-help-strs #''())
      
      #'(define-values (option-id ... arg.name ... (~? rest-arg.name))
          (parse-command-line
           name-expr
           argv-expr
           table
           finish-proc
           arg-help-strs)))]))


(define-option-syntax switch/o
  (syntax-parser
    [(_ flags:flag-names desc:string)
     #'(choice #:default #f [flags desc #t])]))

(define-option-syntax list/o
  (syntax-parser
    [(_ flags:flag-names arg:arg-spec desc:string)
     #'(multi '()
              [flags arg desc (lambda (acc) (cons arg.name acc))])]))

(define-flag-syntax numbered-flags/o
  (syntax-parser
    [(_ flags:flag-names [min:number max:number] desc:string)
     (def/stx (f ...)
       (for/list ([n (in-range (syntax-e #'min) (syntax-e #'max))])
         (def/stx names (map (lambda (s) (format "~a~a" s n)) (attribute flags.names)))
         (def/stx this-desc (format "set ~a to ~a" (syntax-e #'desc) n))
         #`[names this-desc #,(datum->syntax #'here n)]))
     #'(begin f ...)]))

(define (nat-range/p min max)
  (lambda (s)
    (let ([n (string->number s)])
      (unless (and (integer? n) (>= min n) (<= n max))
        (raise-user-error "expected integer between ~a and ~a"
                          min max))
      n)))
(define-argument-syntax int-range/arg
  (syntax-parser
    [(_ min:number max:number)
     (def/stx desc (format "integer between ~a and ~a" (syntax-e #'min) (syntax-e #'max)))
     #'(checked-argument desc (nat-range/p min max))]))

(module+ test
  (require rackunit)

  (let ()
    (define/command-line-options
      #:argv (list "1" "2")
      #:arguments
      x
      [y simple-argument])
    (check-equal? x "1")
    (check-equal? y "2"))
  
  )