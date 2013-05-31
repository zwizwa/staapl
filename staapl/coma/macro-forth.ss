#lang scheme/base
(require "../rpn.ss"
         "../macro.ss"
         "../tools.ss"
         (for-syntax scheme/base
                     "../tools.ss"
                     "../tools/stx.ss"
                     "../forth/lexer-tx.ss"
                     "../rpn.ss"
                     "../forth/forth-tx.ss"))

;; Toplevel forms for dictionary construction and compilation for the
;; Staapl macro forth.

;; These forms assume particular namespace organization:
;;   * macro   op transformer namespace
;;   * target  instantiated op lists
;;   * inline  postponed instantiation macros for target words
;;
;; The dictionary is in a special format: (define register! wrap name compile code ...)
;;   - define    = define-macro or define-forth
;;   - register! = called for each inline macro (used for instantiation)
;;   - wrap      = wrapper functions provided by compiler
;;   - name      = word name
;;   - compile   = semantics of (code ...)

(provide (all-defined-out))



(define-syntax macro-word
  (syntax-rules ()
    ((_ _ _ #f . _)  (begin)) ;; mode marker
    ((_ register! wrap name compile code ...)
     (ns (macro)
         (define name
           (wrap 'name #f (compile code ...)))))))

(define-syntax-rule (word-trap-anon) (begin))


(define-syntax define/false
  (syntax-rules ()
    ((_ _ #f _)   (begin)) ;; don't define anonymous words
    ((_ (n ...) name value) (ns (n ...) (define name value)))))

(define-syntax forth-word
  (syntax-rules ()
    ((_ register! wrap name compile code ...)
     (begin
       (define-values
         (label wrapper inline)
         (wrap 'name #f (compile code ...)))
       (define/false (target) name label)
       (define/false (macro)  name wrapper)
       (define/false (inline) name inline)
       (register! inline)))))

;; For debug purposes, the dictionary expression produced by rpn-parse
;; is stored in this parameter in quoted form before it is expanded further.
(define forth-dictionary-log (make-parameter void))

;; Toplevel forms.
(define-syntax (forth-compile-dictionary stx)
  (define (cleanup it)
    (cond
     ((syntax? it) (cleanup (syntax-e it)))
     ((pair? it) (cons (cleanup (car it))
                       (cleanup (cdr it))))
     ((or
       (null? it)
       (string? it)
       (number? it)
       (symbol? it)) it)
     (else
      (format "~a" it)))) ;; can't marshall other..
  (syntax-case stx ()
    ((_ form ...)
     #`(begin
         ((forth-dictionary-log) '#,(cleanup #'(form ...)))
         form ...))))
  

;; Similar to 'macro: but for a toplevel form.  Initialize dictionary
;; in 'forth' mode, but make sure the first line is invalid.
(define-syntax-rule (forth-begin/init init code ...)
  (rpn-parse (forth-compile-dictionary
              (macro)     
              scat-apply  
              macro-push
              macro-push
              macro:
              init)
             code ...))


(define-syntax-rule (provide-words w ...)
  (provide
   ;; (ns-out (target) w) ...   ;; don't provide target words
   ;; (ns-out (inline) w) ...)
   (ns-out (macro) w) ...
  ))
           

;; Forth parsers.

(prefix-parsers
 (macro)
;; ((provide-all) (|{| provide (all-defined-out) |}|))

 ((|'| name)       (',(macro name)))
 ((|`| name)       ('name))

 ((provide w)       (|{| provide-words w |}| ))
 


 )


;; This doesn't work for some bizarre reason..
(require (for-syntax "macro-forth-tx.ss"))

(ns (macro)
    (define-syntax provide-all
      (make-rpn-transformer
       (lambda (w d k)
         (k (w-cdr w)
            (rpn-compile-toplevel
             (datum->syntax (w-car w) ;; context needs to come from module
                            '(provide (all-defined-out)))
             d))))))


     
;; (*) The first word is anonymous.  It doesn't result in namespace
;; bindings but the postponed word is passed to register!  This is
;; mainly there to support compiler/assembler directives like 'org.

(ns (macro) (define-syntax load  ;; nested files
              (make-rpn-include-transformer
               file->forth-syntax
               stx->path
               (lambda (filename) ;; logger
                 (printf " include ~s\n" (path->string filename))))))

;; Inline s-expressions.  Note that if your current lexer allows, once
;; inside this construct ordinary s-expressions can be used.
(ns (macro) (define-syntax |{| rpn-curly-brace-transformer))

;; Ignore the '#lang syntax
(ns (macro) (define-syntax |#lang|
              (rpn-syntax-rules (planet)
                                ((_ planet spec) ())
                                ((_ spec) ()))))

    
;; Local lexical variables.
(require scheme/match)
(require "../comp/state.ss")
(define (macro-pop state n)
  (let-values (((state+ popped) (state-pop state n (ns (op ? qw)))))
    (apply values (cons state+ popped))))
  
(define-syntax-rule (macro-locals . a)
  (rpn-let-locals ((macro) macro: macro-pop) . a))
(ns (macro) (define-syntax \| (make-rpn-locals-transformer #'macro-locals)))
(ns (macro) (define-syntax path (make-rpn-path-transformer stx->path)))

(ns (macro) (define-syntax library
              (make-rpn-path-transformer
               (lambda (stx)
                 (build-path (home) (stx->path stx))))))

(ns (macro) (define-syntax \[ (make-rpn-quotation-transformer
                               (lambda (expr) #`(macro-push (macro: #,@expr))))))


;; Since there are no strings, how should this work?  Maybe use two
;; words: one that takes filenames directly and another one that uses
;; scheme symbols.
(define-syntax (require-id stx)
  ;; The whole form should have the caller's context to use caller's
  ;; 'require.
  (define req
    (syntax-case stx ()
      ((_ _ id)
       (lambda (sexpr)
         (datum->syntax #'id `(require ,sexpr))))))
  
  (syntax-case stx (spec file planet staapl)
    ((_ file id)   (req `(file ,(path->string (stx->path #'id)))))
    ((_ planet id) (req `(planet ,#'id)))
    ((_ staapl id) (req `(planet ,(string->symbol
                                   (format "zwizwa/staapl/~a"
                                           (syntax->datum #'id))))))))

