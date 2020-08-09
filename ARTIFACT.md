`main.rkt` contains the entire implementation, with comments indicating
the portions belonging to the expander, runtime, compiler, and syntactic
sugar.

This implementation uses `syntax/parse` heavily, and uses that library's
template metafunctions (https://docs.racket-lang.org/syntax/Experimental.html#%28form._%28%28lib._syntax%2Fparse%2Fexperimental%2Ftemplate..rkt%29._define-template-metafunction%29%29) rather than normal functions to implement its
expander. Our library supports this via the
`define/hygienic-metafunction` syntax.

The library defines its expander environment representations for option
and flag syntax using the `define-extensible-syntax` helper from our
`ee-lib` library. This helper syntax defines generic interfaces and
structure types with a field for a macro transformer procedure.

Part of the runtime (`parse-command-line`) is re-used from Racket's built-in command line parsing DSL, `racket/cmdline`.

`example.rkt` and `test.rkt` demonstrate the features of the library.
