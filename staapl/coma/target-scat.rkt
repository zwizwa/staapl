#lang scheme/base

(require
 "../target.ss"
 "../scat.ss"
 scheme/match
 (for-syntax
  ;; "../tools.ss"
  "../tools/stx.ss"
  scheme/pretty
  scheme/base))
  
(provide
 make-target-value-compiler
 tv:)

;; The target-value compiler uses scat: to construct assembler
;; expressions.  (FIXME: this sould later be full parameterization of
;; which assembler to use).

;; FIXME: this was moved from pic18-macro-unit.ss

;; Why?  Probably because it's special-case?

(define-syntax-rule (tv: . code)
  (make-target-value-compiler scat: . code))


;; Assembly time evaluation of scat-like code.


;; On top of the generic delayed target-value mechanism this module
;; provides an RPN language called TV, which is used in the MACRO
;; dialect to implement assembler patterns like:
;;
;;        (([qw a] [qw b] +)    ([qw (tv: a b +)]))
;;
;;   * produces target value computations
;;   * takes lexical names as quoted values, and performs evaluation
;;   * takes non-lexical names as functions from the SCAT namespace
;;

;; Wrap parentheses after partial evaluation for unquote-splicing scat
;; source flattener.
(define (wrap-pe-scat expr)
  (let ((thing
         (cond
          ((target-value? expr) (target-value-source expr))
          ((target-word? expr) (target-word-name expr))
          (else expr))))
    (if (list? thing)
        thing
        (list thing))))

;; Wrap scat functions.
(define (wrap-scat fn)
  (match (fn (make-state:stack '()))
         ((struct stack (ctor (list val)))
          val)
         ((struct stack (ctor lst))
          (error 'meta/scat-garbage-state
                 "~s" lst))))



;; Create delayed expressions for evaluation and partial evaluation in
;; an RPN dialect which has lexical refs.
(define-syntax (make-target-value-compiler stx)
  (syntax-case stx ()
    ((_ ae: . code)
     ;; Operate on rpn code body, processing lexical and other variables.
     (let* ((lex-mapper
             (lambda (fn-lex [fn-no-lex (lambda (x) x)])
               (lambda (stxs)
                 (map
                  (lambda (stx)
                    (if 
                     (and (identifier? stx)
                          (lexical-binding? stx))
                     (fn-lex stx)
                     (fn-no-lex stx)))
                  (syntax->list stxs)))))
            (scat-eval
             #`(wrap-scat
                (ae:
                 #,@((lex-mapper
                      (lambda (id)
                        #`(quote (unquote (target-value-eval #,id)))))
                     #'code))))
            (scat-partial-eval
             #`(quasiquote
                #,((lex-mapper
                    (lambda (id)
                      #`(unquote-splicing (wrap-pe-scat #,id))))
                   #'code))))
       ;; (pretty-print (syntax->datum stx))
       #`(target-value-delay
          #,scat-eval
          #,scat-partial-eval)))))





;; The asmexp: language used to be scat: but now it is limited to a
;; small number of expressions such that it can be compiled to
;; external assembler syntax easily.

;; This list has been obtained using the pretty-print statement in the
;; tv: compiler.

;; (define-syntax-rule (asmexp . _) (begin))

;; (asmexp
;;   and or xor pow >>> <<< / * -)

