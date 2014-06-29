#lang racket/base
;; Syntax transformer utilities for pattern.rkt


(require
 "../ns-tx.rkt"
;; "../tools.rkt"
 "../tools/list.rkt"
 "../op/static.rkt"
 racket/pretty
 (for-template
  racket/base
  "../op.rkt"
  "../tools.rkt"
  "../scat.rkt"
  "../ns.rkt"
  "pattern-runtime.rkt"
  "rep.rkt"
  racket/match
  ;; (lib "match.rkt")
  ))


;; Output assembly is accumulated in reverse on the stack for
;; efficiency (cons). The pattern matcher macros will present a
;; normal ordering, so they require to reverse both the pattern at
;; compile time and the eventual expression at run time. This module
;; exports a single function that performs the pattern compilation:

(provide
 asm-pattern-tx
 asm-template-tx
 asm-lambda-tx
 asm-transforms-tx
 with-asm-transforms-tx
 )


;; The core pattern transformer produces a 'match' clause by reversing
;; the pattern, at compile time for the matcher macro to use, and the
;; result of the expression, postponed to runtime.  Pack the result as
;; (name-symbol match-clause-tx)

;; Pattern/Template syntax.

;; This is a layer on top of the 'raw' pattern syntax that assumes all
;; list elements referred in pattern and template have a tag symbol
;; and removes the burden of quoting/unquoting, simulating
;; struct-style matching.

;; These will register all references to assembler opcodes for error
;; checking.

;; The transformer adds the appropriate quoting to the pattern and
;; unquoting to the template.


;; This parameter is bound to the source-level syntax object upon
;; entry.  It is only used for error reporting.
(define pattern-stx (make-parameter #f))


(define (syntax->warning stx)
  (let ((src  (syntax-source stx))
        (line (syntax-line stx))
        (col  (syntax-column stx)))
    (or
     (and src line col
          (format "~a:~a:~a" src line col)) ;; (add1 col)
     "<???>")))
  
(define (check-ins type ins)
  (syntax-case ins ()
    ((rator rand ...)
     (if (not (identifier? #'rator))
         (when #f ;; don't complain.  FIXME: add run-time checker
           (printf "~a: parametric ~a ~a\n"
                   (syntax->warning ins)
                   type
                   (syntax->datum ins)))
         (op-check-syntax ins)))))

;; Pass source location to runtime.
(define (srcloc stx)
  #`(make-pattern-srcloc
     #,(syntax-source stx)
     #,(syntax-line stx)
     #,(syntax-column stx)))

(define (asm-pattern-tx stx)
  (map
   (lambda (ins)
     (syntax-case ins (unquote)
       (,instruction           #'instruction)
       ((,tag arg ...)         #`(list tag arg ...))
       ((,tag arg ... . args)  #`(list-rest tag arg ... args))
       ((tag arg ...)          (begin
                                 (check-ins "pattern" ins)
                                 #`(list ;; (? (match-asm (asm: tag) 'tag #,(srcloc #'tag)))
                                         ;; 'tag
                                         (? (ns (op ?) tag))
                                         arg ...)))))
   (syntax->list stx)))

;; Parse the template as either the specific assembler syntax, or an
;; arbitrary scheme expression.
(define (asm-template-tx stx log-stx)
  (syntax-case stx (macro:)
    (((rator rand ...) ...)
     (begin
       (for ((_ins (syntax->list stx))) (check-ins "template" _ins))
       ;; #`(quasiquote ((op (unquote arg) ...) ...))
       ;; #`(list (list (asm: op) arg ...) ...)
       #`(begin
           ;; #,log-stx
           (list
            #,@(for/list ((ins (syntax->list stx)))
                 (syntax-case ins (unquote)
                   (((unquote rator) rand ...) #`(list rator rand ...))
                   ((rator rand ...)           #`(op: rator rand ...))
                   ))))
       ))
    (_  stx)))
    
  

(define (spec->name/match-clause stx)
  (syntax-case stx () 
    (((asm-pattern ... name) expr)
     (values 
      (name->identifier #'name)         ;; Convert strings to identifiers.
      (let ((pattern-lst
             (reverse                   ;; Reverse pattern at compile time.
              (asm-pattern-tx
               #'(asm-pattern ...))))
            (template
             #`(macro/append-reverse    ;; Reverse asm code result at runtime.
                #,(asm-template-tx #'expr
                                   #`(begin
                                       (display '#,stx)
                                       (newline)))
                rest)))
        (if (null? pattern-lst)
            #`(rest #,template)
            #`((list-rest #,@pattern-lst rest) #,template)
            ))))))
               


;; Apply the above transformation for all patterns, and collect all
;; clauses for a single macro.

;; (<macro-name> <orig-patterns> <clauses>)

(define (specs->clause-dict specs)
  (map*
   ;; Transpose output of collect.
   (lambda (name . orig/clause)
     (list name
           (map first orig/clause)
           (map second orig/clause)))
   ;; Collect clauses per macro.
   (collect
    free-identifier=?
    (map
     (lambda (orig)
       (let-values
           (((name clause)
             (spec->name/match-clause orig)))
         (cons name                  ;; collect name
               (list orig clause)))) ;; payload
     ;; Straight from syntax object.
     (syntax->list specs)))))


;; Represent a list of match clauses.
(define (clauses->word name clauses)
  ;; (pretty-print (syntax->datum #`(#,name #,clauses)))
  #`(pattern-tx->macro
     '#,name
     #,(quasisyntax/loc
        name
        (match-lambda
         #,@clauses
         ;; Give some decent console feedback in case a target code
         ;; rule doesn't match.  Here we know only the definition site
         ;; of the pattern macro, not the call site.
         (else
          (error 'pattern-mismatch
                 "in macro: `~a'\nDefined at:\n~a\n~a\n"
                 '#,name
                 '#,(syntax->warning
                     ;; (pattern-stx)
                     name)
                 (continuation-mark-set->context
                  (current-continuation-marks))
                 ))
         ))))




(define (transform-bindings specs)
  (map*
   (lambda (name origs clauses)
     #`(#,name
        (make-primitive-macro #,(clauses->word name clauses)
                              '#,origs)))
   (specs->clause-dict specs)))

;; Main transformer. This uses 'specs->clause-dict' and
;; 'clauses->word' to construct the outer scheme
;; expression. Redefinitions are allowed, and have the 'super' word
;; bound in the namespace.

(define (asm-transforms-tx namespace specs)
  (parameterize ((pattern-stx specs))
    #`(ns #,namespace
          #,(syntax-case (transform-bindings specs) ()
              (((name expr) ...)
               #'(define-values (name ...) (values expr ...)))))))
  
;  #`(redefinitions!-ns
;     #,namespace
;     #,@(transform-bindings specs)))

;; Define multiple parameter words.

(define (with-asm-transforms-tx namespace specs)
  #`(lambda (thunk)
      (parameterize-words-ns! #,namespace
       #,(transform-bindings specs)
       (thunk))))



;; Anonymous transformer. The name in the pattern is ignored. By
;; convention _ is used.
(define (asm-lambda-tx specs)
  (let* ((clause-dict
          (specs->clause-dict specs)))
    (apply
     (lambda (origs clauses)
       #`(make-word
          #,(clauses->word #'<anonymous> clauses)
          '((pattern . #,origs))
          ))
     (list
      (apply append (map second clause-dict))
      (apply append (map third clause-dict))))))


