#lang racket/base

;; The RPN parser is used to "unroll" a metacircular Forth
;; description, reconstructing a part of the compiler in terms of Scat
;; (2stack) primitives.

;; It uses the following tricks:

;; * The parser is duplicated (uses rpn-parse to build an s-expression
;;   form instead of the low-level description in the .f file).
;;
;; * Immediate words are implemented in terms of the purely functional
;;   SCAT language.

;; To make this work, the .f kernel's syntax is simplified so it can
;; be parsed both by rpn-parse and by itself.


(provide (all-defined-out))

(require "../rpn.ss"
         "../tools.ss"
         "../ns.ss"
         "../macro.ss"
         "../forth/forth-lex.ss"
         racket/stxparam
         racket/splicing
         racket/pretty
         (for-syntax racket/base
                     racket/pretty
                     ;; "../tools.ss"
                     "../tools/stx.ss"
                     "../tools/grabbag.ss"
                     "../forth/forth-tx.ss"
                     "../rpn.ss"))

;; Target code parser.  This is associated to the (mcf) prefix parsers
;; which define the Forth language syntax.  The semantics however is
;; twofold: code is interpreted as on-target executable Forth, while
;; some code needs to be lifted to run at compile time as Scat code to
;; compile the target code.

;; The difference in semantics is handled by indirection using these
;; syntax parameters:

(define-for-syntax (mcf-not-defined stx)
  (raise-syntax-error #f "undefined syntax parameter" stx))
(define-syntax-rule (mcf-params: p ...)
  (begin (define-syntax-parameter p mcf-not-defined) ...))

(mcf-params: mcf mcf-push mcf: word variable immediate)

(define-syntax-rule (mcf-parse begin-dict code ...)
  (rpn-parse (begin-dict
              (mcf)
              scat-apply  
              mcf-push
              mcf-push
              mcf:
              (word #f)    ;; dictionary init
              ) code ...))

;; Forth parsing words ( : ; variable immediate postpone)
(ns (mcf)
    (define-syntax \:
      (make-rpn-forth-definition-transformer
       (lambda (name)
         #`(word #,name)))))
(ns (mcf)
    (define-syntax variable
      (make-rpn-forth-definition-transformer
       (lambda (name)
         #`(variable #,name)))))
(ns (mcf)
    (define-syntax immediate
      (make-rpn-transformer
       (lambda (w d k)
         (define (->immediate lst)
           (cons #'immediate (cdr lst)))
         (k (w-cdr w)
            (d-on-last d ->immediate))))))
(prefix-parsers
 (mcf)
 ((|;|)           (exit))
 ((postpone word) ('word |compile,|)))

(define-syntax-rule (quote-dict entry ...)
  (quote (entry ...)))

;; Simply ignore the dictionary's first entry (the Forth code before
;; the first named definition).  FIXME: add some error checking.
(define-syntax-rule (begin-dict _ entry ...) (begin entry ...))

;; Tie into the instantiation mechanism.  FIXME: implement this.
(define-syntax-rule (define-word name . code)
  (ns (postponed) (define name (rpn-lambda . code))))



(define-for-syntax slv syntax-local-value)

;; Compile with target word semantics (instantiated concatenative
;; macros).  This makes use of the lifted immediate words.
(define-syntax-rule (target-begin code ...)
  (splicing-syntax-parameterize
   ((mcf       (slv #'macro))
    (mcf-push  (slv #'macro-push))
    (word      (slv #'define-word))
    (immediate (slv #'define-word))
    (variable  (slv #'define-word)))
   (mcf-parse begin-dict code ...)))

;; Lift immediate words and dependencies to scat code.
(define-syntax-rule (lifted-begin code ...)
  (splicing-syntax-parameterize
   ((mcf       (slv #'scat))
    (mcf-push  (slv #'scat-push))
    (word      (slv #'lift-word))
    (immediate (slv #'lift-immediate))
    (variable  (slv #'lift-variable)))
   (mcf-parse begin-dict-pruned code ...)))
  
(define-syntax-rule (forth-begin code ...)
  (begin
    (target-begin code ...)
    (lifted-begin code ...)))



;; Immediate words are lifted to be implemented in terms of (scat)
;; primitives, with the resulting function stored in the (macro)
;; namespace.  Other words are implemented as plain (scat) words.
;; This implements words on which the immediate words depend.

;; Note that not all words need to function in order to compile the
;; Forth kernel.  Some Forth primitives might be stubs that raise
;; errors when they are attempted to be run as (scat) code.

(define-syntax-rule (lift target-ns name . code)
  (ns target-ns (define name (rpn-lambda . code))))

(define-syntax-rule (lift-immediate . def) (lift (macro) . def))
(define-syntax-rule (lift-word . def)      (lift (scat) . def))

;; Variables need special treatment since scat is a functional
;; language.  Care needs to be taken to bridge the simulation of the
;; Forth memory model with the abstract compiler interface.
(define-syntax-rule (lift-variable name)
  (ns (scat) (define name #f)))


;; Remove all non-immediate words that are not used in the definition
;; of immediate words.  This effectively breaks the circular
;; dependency.  For this to work it is required that the source code
;; doesn't exhibit any circular dependency (immediate words depend
;; upon themselves).  To break such cycles requires more complicated
;; bootstrapping.  Since the .f compiler produces reflective code
;; (doLit, branch, ?branch), the non-reflective scat host would need
;; to emulate this too.

  
(define-syntax (begin-dict-pruned stx)
  (define entries (cddr (syntax->list stx)))

  (define-hashes imms dict deps dropped)

  ;; It's easier to directly interpret the forms to fish out the deps
  ;; than to associate macros to the identifiers and let the expander
  ;; do it: that would require some macro CPS tricks to make expansion
  ;; happen before the processing we do here.
  (define (code->deps stx)
    (filter
     id
     (for/list ((c (syntax->list stx)))
       (syntax-case c (mcf)
         ((_ (mcf id)) (syntax->datum #'id))
         (_ #f)))))
    
  ;; Builds the set of immediate words, and a map from definition to
  ;; dependencies for all words.
  (define (parse!)
    (for ((e entries))
      (syntax-case e ()
        ((semantics name . code)
         (when (eq? 'immediate (datum #'semantics))
           (id-reg! imms #'name))
         (id-reg! dict #'name (code->deps #'code))))))

  ;; Mark dependencies from the immediate word roots.
  (define (mark!)
    (for ((d (dependencies
              (lambda (id) (id-find dict id))
              (ids imms))))
      (id-reg! deps d)))

  ;; Keep only immediate words and their deps.
  (define (necessary!? stx)
    (syntax-case stx (immediate)
      ((immediate . _) #t)
      ((_ name . _)
       (if* (id-find deps #'name) it
            (false (id-reg! dropped #'name))))))
  (define (sweep!)
    (let ((filtered-entries
           (filter necessary!? entries)))
      (printf "used:    ~a\n" (ids deps))
      (printf "dropped: ~a\n" (ids dropped))
      #`(begin #,@filtered-entries)))
  
  (parse!)
  (mark!)
  (sweep!))
     

;; Scat wrapper compiler.

(define-syntax-rule (boot file)
  (forth-lex-file/cps forth-begin file))

