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

(provide define/command-line-options
         choice multi
         simple-argument checked-argument)

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
             #:attr names #'(s))
    (pattern (s:string ...+)
             #:attr names #'(s ...)))
  
  (define-syntax-class arg-spec
    #:attributes [name type]
    (pattern name:id
             #:attr type #'simple-argument)
    (pattern [name:id type]))
  
  (define/hygienic (expand-option stx) #:definition
    (syntax-parse stx
      #:literal-sets (cmdline-literals)
      [(choice #:required flag ...+)
       (def/stx (flag^ ...) (stx-map expand-flag #'(flag ...)))
       (qstx/rc (choice #:required flag^ ...))]
      [(choice #:default flag ...+)
       (def/stx (flag^ ...) (stx-map expand-flag #'(flag ...)))
       (qstx/rc (choice #:default flag^ ...))]
      [(multi init-expr flag ...+)
       (def/stx (flag^ ...) (stx-map expand-flag #'(flag ...)))
       (qstx/rc (multi init-expr flag^ ...))]))

  (define/hygienic (expand-flag stx) #:definition
    (syntax-parse stx
      #:literal-sets (cmdline-literals)
      [[names:flag-names arg:arg-spec ... desc:string e]
       (def/stx (arg-type^ ...) (stx-map expand-argument-type #'(arg.type ...)))
       (qstx/rc [names.names ([arg.name arg-type^] ...) desc e])]))

  (define/hygienic (expand-argument-type stx) #:definition
    (syntax-parse stx
      #:literal-sets (cmdline-literals)
      [simple-argument this-syntax]
      [(checked-argument desc:string parser) this-syntax]))
   
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
         (and (attribute rest-arg)
              (list #'rest-arg.name (expand-argument-type #'rest-arg.type))))
       
       (qstx/rc
        (form-id (~? name-expr (find-system-path 'run-file))
                 (~? argv-expr (current-command-line-arguments))
                 ([option-name expanded-opt-spec] ...)
                 ([arg.name expanded-arg-type] ...)
                 rest))])))

(define unset
  (let () (struct unset ()) (unset)))

(define identity/p (lambda (x) x))

(define (make-finish-proc option-keys initial-option-values required-option-keys
                          arg-names arg-parsers maybe-parse-rest)
  (define (f option-procs . args)
    (define-values (positional-args rest-args) (split-at args (length arg-parsers)))
   
    (define option-map
      (for/fold ([option-map (make-immutable-hash (map cons option-keys initial-option-values))])
                ([option-proc option-procs])
        (option-proc option-map)))

    (for ([(key flags) required-option-keys])
      (when (eq? (hash-ref option-map key) unset)
        (raise-user-error (format "one of these flags must be specified: ~a" flags))))
    
    (define options
      (for/list ([key option-keys])
        (hash-ref option-map key)))

    (define positionals
      (for/list ([arg positional-args]
                 [parse arg-parsers])
        (parse arg)))

    (define maybe-rest
      (if maybe-parse-rest
          (list (map maybe-parse-rest rest-args))
          '()))

    (apply values
           (append options
                   positionals
                   maybe-rest)))

  (if maybe-parse-rest
      (procedure-reduce-arity f (arity-at-least (+ 1 (length arg-parsers))))
      (procedure-reduce-arity f (+ 1 (length arg-parsers)))))

(begin-for-syntax
  (define (identifier->string-literal id) #`#,(symbol->string (syntax-e id)))
  
  (define (initial-option-value-stx option-stx)
    (syntax-parse option-stx
      #:literal-sets (cmdline-literals)
      [(choice #:required . _)
       #'unset]
      [(choice #:default default-expr . _)
       #'default-expr]
      [(multi init-expr . _)
       #'init-expr]))

  (define-syntax-class exp-flag-spec
    (pattern [[name:string ...] ([arg:id arg-spec] ...) desc:string e]))
  (define (compile-option option-stx key)
    (define (compile-flags flags type)
      (for/list ([flag (in-syntax flags)])
        (syntax-parse flag
          [[[name:string ...] ([arg:id arg-spec] ...) desc:string e]
           (def/stx fn
             (case type
               [(choice) #`(lambda (ignore arg ...)
                             (lambda (acc) (hash-set acc '#,key e)))]
               [(multi) #`(lambda (ignore arg ...)
                            (lambda (acc) (hash-update acc '#,key e)))]))
           #``[(name ...) ,fn (desc #,@(stx-map identifier->string-literal #'(arg ...)))]])))
    (syntax-parse option-stx
      #:literal-sets (cmdline-literals)
      [(choice (~or #:required (~seq #:default default-expr)) flag ...)
       #`(list 'once-any #,@(compile-flags #'(flag ...) 'choice))]
      [(multi init-expr flag ...)
       #`(list 'multi #,@(compile-flags #'(flag ...) 'multi))]))

  (define (compile-table-expr option-specs option-keys)
    #`(list #,@(map compile-option option-specs option-keys)))

  (define (compile-finish-proc-expr option-specs option-keys arg-names arg-types maybe-rest-type)
    (define required-option-keys+names
      (for/fold ([acc (hash)])
                ([spec option-specs]
                 [key option-keys])
        (syntax-parse spec
          #:literal-sets (cmdline-literals)
          [(choice #:required [flag:flag-names . _] ...)
           (hash-set acc key (apply append (syntax->datum #'(flag.names ...))))]
          [_ acc])))

    (define arg-parsers
      (for/list ([t arg-types])
        (syntax-parse t
          #:literal-sets (cmdline-literals)
          [simple-argument #'identity/p]
          [(checked-argument desc parser) #'parser])))

    (define maybe-parse-rest
      (if maybe-rest-type
          (syntax-parse maybe-rest-type
            #:literal-sets (cmdline-literals)
            [simple-argument #'identity/p]
            [(checked-argument desc parser) #'parser])
          #'#f))

    (define arg-names-exprs
      (for/list ([arg arg-names])
        (identifier->string-literal arg)))

    #`(make-finish-proc '#,option-keys
                        (list #,@(map initial-option-value-stx option-specs))
                        '#,required-option-keys+names
                        '#,arg-names-exprs
                        (list #,@arg-parsers)
                        #,maybe-parse-rest))

  (define (compile-arg-help arg-names maybe-rest-name)
    #`'#,(append (map symbol->string (map syntax-e arg-names))
                 (if maybe-rest-name (list (symbol->string (syntax-e maybe-rest-name))) '())))
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
       (compile-arg-help (syntax->list #'(arg-name ...)) (attribute rest-name)))
     #`(define-values (option-name ... arg-name ... (~? rest-name))
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
         (def/stx names (map (lambda (s) (format "~a~a" s n)) (syntax->datum #'flag.names)))
         (def/stx this-desc (format "set ~a to ~a" (syntax-e #'desc) n))
         #`[names this-desc #,(datum->syntax #'here n)]))
     #'(begin f ...)]))

(define (nat-range/p min max)
  (lambda (s)
    (let ([n (string->number s)])
      (unless (and (integer? n) (>= n min) (<= n max))
        (raise-user-error (format "expected integer between ~a and ~a"
                                  min max)))
      n)))
(define-argument-syntax int-range/arg
  (syntax-parser
    [(_ min:number max:number)
     (def/stx desc (format "integer between ~a and ~a" (syntax-e #'min) (syntax-e #'max)))
     #'(checked-argument desc (nat-range/p min max))]))
