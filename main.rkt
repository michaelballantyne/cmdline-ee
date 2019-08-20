#lang racket/base

(require
  racket/list
  (only-in racket/cmdline parse-command-line)
  ee-lib/define
  
  (for-syntax
   racket/base
   (rename-in syntax/parse [define/syntax-parse def/stx])
   syntax/stx
   ee-lib
   racket/list
   racket/sequence
   ))

(provide define/command-line-options
         choice multi
         simple-argument checked-argument
         
         define-option-syntax
         define-flag-syntax
         define-argtype-syntax
         (for-syntax flag-name flag-names)

         switch/o list/o
         int-range/argt)

(define-literal-forms cmdline-literals
  "command line option, flag, and argument specifiers cannot be used as expressions"
  (choice
   multi
   simple-argument
   checked-argument))

(define-extensible-syntax option-syntax)
(define-extensible-syntax flag-syntax)
(define-extensible-syntax argtype-syntax)

; Expander
(begin-for-syntax
  (define-syntax-class flag-name
    #:description #f
    (pattern _:string
             #:cut
             #:fail-unless
             (regexp-match? #rx"^([-+][^-+]$|(--|[+][+])[^-+])" (syntax-e this-syntax))
             "bad flag string"
             #:fail-when
             (regexp-match? #rx"^[-+][0-9]$" (syntax-e this-syntax))
             "number flag not allowed"
             #:fail-when
             (regexp-match? #rx"^(-h|--help)$" (syntax-e this-syntax))
             "pre-defined flag not allowed"))
  
  (define-syntax-class flag-names
    #:description #f
    #:attributes [names]
    (pattern s:flag-name
             #:attr names #'(s))
    (pattern (s:flag-name ...+)
             #:attr names #'(s ...)))
  
  (define-syntax-class arg-spec
    #:attributes [name type]
    (pattern name:id
             #:attr type #'simple-argument)
    (pattern [name:id type]))
  
  (define/hygienic-metafunction (expand-option stx) #:definition
    (syntax-parse stx
      #:literal-sets (cmdline-literals)
      [(choice #:required flag ...+)
       (qstx/rc (choice #:required (~@ . (expand-flag flag)) ...))]
      [(choice #:default default-expr flag ...+)
       (qstx/rc (choice #:default default-expr (~@ . (expand-flag flag)) ...))]
      [(multi init-expr flag ...+)
       (qstx/rc (multi init-expr (~@ . (expand-flag flag)) ...))]
      [(head:id . rest)
       #`(expand-option #,(option-syntax-transform (lookup #'head) this-syntax))]))

  (define/hygienic-metafunction (expand-flag stx) #:definition
    (syntax-parse stx
      #:literal-sets (cmdline-literals) #:literals (begin)
      [(begin f ...)
       (qstx/rc ((~@ . (expand-flag f)) ...))]
      [[names:flag-names arg:arg-spec ... desc:string e]
       (qstx/rc ([names.names [arg.name (expand-argument-type arg.type)] ... desc e]))]
      [(head:id . rest)
       #`(expand-flag #,(flag-syntax-transform (lookup #'head) this-syntax))]))

  (define/hygienic-metafunction (expand-argument-type stx) #:definition
    (syntax-parse stx
      #:literal-sets (cmdline-literals)
      [simple-argument this-syntax]
      [(checked-argument parser) this-syntax]
      [(head:id . rest)
       #:when (argtype-syntax? (lookup #'head))
       #`(expand-argument-type #,(argtype-syntax-transform (lookup #'head) this-syntax))]
      [_ (raise-syntax-error 'define/command-line-options
                             "invalid argument type syntax" this-syntax)]))
   
  (define/hygienic (expand-define/command-line-options stx) #:definition
    (syntax-parse stx
      [(form-id (~optional (~seq #:program name-expr))
                (~optional (~seq #:argv argv-expr))
                (~optional (~seq #:options option-bindings ...))
                (~optional (~seq #:arguments positional-bindings ...))
                (~optional (~seq #:rest rest-arg:arg-spec)))
       #:with ([option-name:id opt-spec] ...) #'(~? (option-bindings ...) ())
       #:with (arg:arg-spec ...) #'(~? (positional-bindings ...) ())
       (qstx/rc
        (form-id (~? name-expr (find-system-path 'run-file))
                 (~? argv-expr (current-command-line-arguments))
                 ([option-name (expand-option opt-spec)] ...)
                 ([arg.name (expand-argument-type arg.type)] ...)
                 (~? [rest-arg.name (expand-argument-type rest-arg.type)] #f)))])))

; runtime
(define identity/p (lambda (x) x))
(define unset
  (let () (struct unset ()) (unset)))

; TODO: I'd like errors to also mention the argument and flag names, as appropriate
(define current-program-name (make-parameter #f))
(define (run-parser parser context arg)
  (define (rewrite-message e)
    (raise-user-error (format "~a: ~a\n  in ~a" (current-program-name) (exn-message e) context)))
  (with-handlers ([exn:fail:user? rewrite-message])
    (parser arg)))

(define (make-finish-proc option-keys initial-option-values required-option-keys
                          arg-names arg-parsers maybe-parse-rest)
  (procedure-reduce-arity
   (lambda (option-procs . args)
     (define-values (positional-args rest-args) (split-at args (length arg-parsers)))

     (define initial-option-map
       (make-immutable-hash (map cons option-keys initial-option-values)))
     (define final-option-map
       (for/fold ([option-map initial-option-map])
                 ([option-proc option-procs])
         (option-proc option-map)))

     (for ([(key flags) required-option-keys])
       (when (eq? (hash-ref final-option-map key) unset)
         (raise-user-error (format "one of these flags must be specified: ~a" flags))))
    
     (define options
       (for/list ([key option-keys])
         (hash-ref final-option-map key)))

     (define positionals
       (for/list ([arg positional-args]
                  [name arg-names]
                  [parse arg-parsers])
         (run-parser parse (format "positional argument <~a>" name) arg)))

     (define maybe-rest
       (if maybe-parse-rest
           (list (map (lambda (arg) (run-parser maybe-parse-rest "variadic arguments" arg))
                      rest-args))
           '()))

     (apply values
            (append options
                    positionals
                    maybe-rest)))
   (if maybe-parse-rest
       (arity-at-least (+ 1 (length arg-parsers)))
       (+ 1 (length arg-parsers)))))

; compiler
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

  (define (arg-type-parser-expr arg-type-stx)
    (syntax-parse arg-type-stx
      #:literal-sets (cmdline-literals)
      [simple-argument #'identity/p]
      [(checked-argument parser) #'parser]))

  (define-syntax-class exp-flag-spec
    (pattern [[name:string ...] ([arg:id arg-spec] ...) desc:string e]))

  (define ((compile-flag key type) flag-stx)
    (syntax-parse flag-stx
      [[[name:string ...] [arg:id arg-spec] ... desc:string e]
       (def/stx (arg-p ...) (stx-map arg-type-parser-expr #'(arg-spec ...)))
       (def/stx hash-op (case type [(choice) #'hash-set] [(multi) #'hash-update]))
       (def/stx (arg-name-str-e ...) (stx-map identifier->string-literal #'(arg ...)))
       (def/stx fn
         #`(lambda (flag-name arg ...)
             (let ([arg (run-parser arg-p
                                    (format "<~a> argument to flag ~a" arg-name-str-e flag-name)
                                    arg)] ...)
               (lambda (acc) (hash-op acc '#,key e)))))
       #``[(name ...) ,fn (desc #,@(stx-map identifier->string-literal #'(arg ...)))]]))
  
  (define (compile-option option-stx key)
    (syntax-parse option-stx
      #:literal-sets (cmdline-literals)
      [(choice (~or #:required (~seq #:default default-expr)) flag ...)
       #`(list 'once-any #,@(stx-map (compile-flag key 'choice) #'(flag ...)))]
      [(multi init-expr flag ...)
       #`(list 'multi #,@(stx-map (compile-flag key 'multi) #'(flag ...)))]))

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

    (define arg-parsers (map arg-type-parser-expr arg-types))

    (define maybe-parse-rest
      (if maybe-rest-type (arg-type-parser-expr maybe-rest-type) #'#f))

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

  ) ; end compiler begin-for-syntax

(define-syntax (define/command-line-options stx)
  (syntax-parse (expand-define/command-line-options stx)
    [(_ name-expr argv-expr
        ([option-name option-spec] ...)
        ([arg-name arg-type] ...)
        (~or* #f [rest-name rest-type]))
     
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
         (let ([name name-expr])
           (parameterize ([current-program-name name])
             (parse-command-line
              name
              argv-expr
              table-expr
              finish-proc-expr 
              arg-help-strs-expr))))])) 

; sugar
(define-option-syntax switch/o
  (syntax-parser
    [(_ flags:flag-names desc:string)
     #'(choice #:default #f [flags desc #t])]))

(define-option-syntax list/o
  (syntax-parser
    [(_ names:flag-names arg:arg-spec desc:string)
     #'(multi '()
              [names arg desc (lambda (acc) (append acc (list arg.name)))])]))

(define (int-range/p min max)
  (lambda (s)
    (define n (string->number s))
    (unless (and (integer? n) (>= n min) (<= n max))
      (raise-user-error (format "expected integer between ~a and ~a" min max)))
    n))
(define-argtype-syntax int-range/argt
  (syntax-parser
    [(_ min:number max:number)
     (def/stx desc (format "integer between ~a and ~a" (syntax-e #'min) (syntax-e #'max)))
     #'(checked-argument (int-range/p min max))]))

