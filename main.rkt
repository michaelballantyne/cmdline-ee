#lang racket/base

(require
  racket/list
  (only-in racket/cmdline parse-command-line)
  ee-lib/define
  (for-syntax
   racket/base
   (rename-in syntax/parse [define/syntax-parse def/stx])
   racket/syntax
   syntax/stx
   ee-lib
   racket/list
   racket/sequence
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

(define unset
  (let () (struct unset ()) (unset)))

(define (finish-proc option-keys required-option-keys positional-parsers maybe-parse-rest)
  (define (f option-procs . args)
    (define-values (positional-args rest-args) (split-at args (length positional-parsers)))
   
    (define option-map
      (for/fold ([option-map (hash)])
                ([option-proc option-procs])
        (option-proc option-map)))

    (for ([key required-option-keys])
      (when (eq? (hash-ref option-map key) unset)
        (raise-user-error "missing value for required option ~a" key)))
    
    (define options
      (for/list ([key option-keys])
        (hash-ref option-map key)))

    (define positionals
      (for/list ([arg positional-args]
                 [parse positional-parsers])
        (parse arg)))

    (define maybe-rest
      (if maybe-parse-rest
          (list (maybe-parse-rest rest-args))
          '()))

    (append
     options
     positionals
     maybe-rest))

  (if maybe-parse-rest
      (procedure-reduce-arity f (arity-at-least (+ 1 (length positional-parsers))))
      (procedure-reduce-arity f (+ 1 (length positional-parsers)))))

(begin-for-syntax
  (define (required-option-key option-stx option-key)
    (syntax-parse option-stx
      #:literal-sets (cmdline-literals)
      [(choice #:required . _) option-key]
      [_ #f]))

  (define (initial-option-value-stx option-stx)
    (syntax-parse option-stx
      #:literal-sets (cmdline-literals)
      [(choice #:required . _)
       #'unset]
      [(choice #:default default-expr . _)
       #'default-expr]
      [(multi init-expr . _)
       #'init-expr]))

  (define (compile-option option-stx)
    #'(list)
    #;(syntax-parse option-stx
        #:literal-sets (cmdline-literals)
        [(choice #:required flag ...)
         ]
        [(choice #:default default-expr flag ...)
         ]
        [(multi init-expr flag ...)]))

  (define/hygienic (expand-define/command-line-options stx) #:definition
    (syntax-parse stx
      [(form-id (~optional (~seq #:program name-expr))
                (~optional (~seq #:argv argv-expr))
                (~optional (~seq #:options option-bindings ...))
                (~optional (~seq #:arguments positional-bindings ...))
                (~optional (~seq #:rest rest-arg:arg-spec)))
       #:with ([option-name:id opt-spec] ...) #'(~? (option-bindings ...) ())
       #:with (arg:arg-spec ...) #'(~? (positional-bindings ...) ())

       (def/stx (expanded-opt-spec ...) (stx-map expand-option #'(opt-spec ...)))
       (def/stx (expanded-arg-type ...) (stx-map expand-argument-type #'(arg.type ...)))
       (def/stx rest
         (if (attribute rest-arg)
             (list #'rest-arg.name (expand-argument-type #'rest-arg.type))
             #f))
       
       (qstx/rc
        (form-id (~? name-expr (find-system-path 'run-file))
                 (~? argv-expr (current-command-line-arguments))
                 ([option-name expanded-opt-spec] ...)
                 ([arg.name expanded-arg-type] ...)
                 rest))]))

  (define (compile-table-expr option-specs option-keys)
    #'(list))

  (define (compile-finish-proc-expr option-specs option-keys arg-names arg-types maybe-rest-type)
    #'(lambda (o . args) (apply values args)))

  (define (compile-arg-help arg-names arg-types)
    #'(list))
  )



(define-syntax (define/command-line-options stx)
  (syntax-parse (expand-define/command-line-options stx)
    [(_ name-expr argv-expr
        ([option-name option-spec] ...)
        ([arg-name arg-type] ...)
        (~or* #f [rest-name rest-type]))
     (displayln this-syntax)
     (define option-keys (for/list ([o (in-syntax #'(option-name ...))]) (gensym (syntax-e o))))
     (def/stx table-expr (compile-table-expr (syntax->list #'(option-spec ...)) option-keys))
     (def/stx finish-proc-expr
       (compile-finish-proc-expr
        (syntax->list #'(option-spec ...)) option-keys
        (syntax->list #'(arg-name ...)) (syntax->list #'(arg-type ...))
        (attribute rest-type)))
     (def/stx arg-help-strs-expr
       (compile-arg-help (syntax->list #'(arg-name ...)) (syntax->list #'(arg-type ...))))
     #`(define-values #,(stx-map syntax-local-identifier-as-binding
                                 #'(option-name ... arg-name ... (~? rest-name)))
         (parse-command-line
          name-expr
          argv-expr
          table-expr
          finish-proc-expr 
          arg-help-strs-expr))]))



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
