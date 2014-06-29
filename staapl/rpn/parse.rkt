#lang racket/base

(require
 "../ns.rkt"
 (for-syntax
  racket/base
  racket/pretty
  "../ns-tx.rkt"
  "parse-tx.rkt"))

(provide (all-defined-out))

;; An RPN transformer is a primitive taking arguments
;;   W : code stack
;;   D : dictionary (parser output)
;;   K : parser continuation




;; RPN PARSER

;; (rpn-parse (mk semantics ...) code ...)

;; The rpn syntax is currently implemented as a single transformer to
;; be able to get at the provided semantics macros through lexical
;; scope.  (Previous implementation used compile-time paramters, which
;; became hard to understand.)

;; The parser can be parameterized as follows:
;;   * semantics for built-in in RPN language constructs.
;;   * prefix parsers bound to local syntax
;;   * prefix parsers found in the input stream



(define-syntax (rpn-parse stx)
  (let* ((insp (current-code-inspector))
         (recertify (lambda (new-stx)
                      (syntax-recertify new-stx stx insp #f)))
         (args (stx-args stx)))
    (syntax-case (car args) ()
      ((tx-dict    ;; macro continuation <- dictionary output form
        (ns ...)   ;; identifier namespace
        function   ;; semantics macros for different forms
        immediate
        immediate-program
        program:
        init-dict)
       (let*
           ((map-id ;; (1)
             (lambda (id)
               (ns-prefixed #'(ns ...) id)))
            (->parse
             (lambda (it)
               (and (rpn-transformer? it)
                    (rpn-transformer-tx it))))
            (mapped-syntax-value
             (lambda (stx)
               ;; (printf "msv: ~a\n" (syntax->datum stx))
               (and (identifier? stx)
                    (syntax-local-value
                     (map-id stx)
                     (lambda () #f)))))

            (qq
             ;; Build a quasiquoted immediate by traverseing an sexp
             ;; tree and performing proper unquotes.
             (lambda (unquote-tx)
               (lambda (atom-stx)
                 (define (uq stx)
                   (syntax-case stx (unquote)
                     ((unquote atom) (unquote-tx #'atom))
                     ((car . cdr)    #`(#,(uq #'car) . #,(uq #'cdr)))
                     (atom           #'atom)))
                 #`(immediate (quasiquote #,(uq atom-stx))))))
                
            (quoter
             ;; All quoters take one arguement.
             (lambda (fn stx)
               (syntax-case stx ()
                 ((_ atom) (fn #'atom))
                 (other    (raise-syntax-error
                            #f "takes a single argument" stx)))))
            (quoted
             ;; Quote supports unquote as a way to introduce
             ;; arbitrary scheme values into s-expressions.
             (qq (lambda (atom) #`(unquote #,atom))))
            (quasiquoted
             ;; Quasiquotation is intended to build datastructures
             ;; containing function objects, not to substitute
             ;; scheme values.  It supports both identifiers and
             ;; compositions.
             (qq (lambda (atom)
                   (syntax-case atom ()
                     ((e ...) #`(unquote (program: e ...)))
                     (e       #`(unquote #,(map-id #'e)))))))
            (unquoted
             ;; Unquote takes an expression from the surrounding
             ;; Scheme environment and uses it as a function.
             (lambda (atom-stx)
               #`(function #,atom-stx)))

            (primitive
             (lambda (element)
               (syntax-case element
                   (quote quasiquote unquote) 
                 ((quote . e)        (quoter quoted element))
                 ((quasiquote . e)   (quoter quasiquoted element))
                 ((unquote . e)      (quoter unquoted element))
                 ((e ...)            #`(immediate-program (program: e ...)))
                 (e (if (identifier? #'e)
                        #`(function (ns ... e))
                        #`(immediate #,element))))))
            (primitive-parse
             (lambda (element)
               (lambda (w d next)
                 (next (w-cdr w)
                       (d-compile
                        (primitive element) d))))))

         ;; * MAIN LOOP *
         ;; Read elements from the list of syntax elements, parse
         ;; and compile.  When done, pass the dictionary to the
         ;; dictionary transformer macro.
         (recertify
          (let next ((w (cdr args))
                     (d (foldl d-compile
                               (d-create)
                               (syntax->list #'init-dict))))
            (if (w-null? w)
                (let ((forms (d->forms d)))
                  ;; (pretty-print (syntax->datum #`( #,@forms )))
                  #`(tx-dict #,@forms))
                (let* ((element (w-car w))
                       (parse
                        ;; Determine if the element represents a parser
                        ;; extension.  Either directly in the input
                        ;; stream, possibly wrapped in a syntax object,
                        ;; or bound to a transformer binding.
                        (or (->parse element)
                            (->parse (syntax->datum element))
                            (->parse (mapped-syntax-value element))
                            (primitive-parse element))))
                  
                  ;; All parsers are invoked in tail position and need
                  ;; to call 'next to continue looping with updated
                  ;; state.
                  (parse w d next))))))))))



;; Notes
;;
;; (1) In order to access transformer bindings containing
;;     rpn-transformer instances, compile time identifiers specified
;;     by the form (namespace ... id) are _interpreted_ : the form
;;     which is a valid macro form is _not_ expanded.  This is to
;;     prevend recursive macro expansion inside the rpn-parse macro,
;;     which I've not been able to figure out how to do correctly.
;;     (i.e. using 'local-expand ...).  However, if this process fails
;;     and no transformer binding is found, the identifier is replaced
;;     with the form in the output of rpn-parse, which allows for
;;     abstract identifier mapping.
