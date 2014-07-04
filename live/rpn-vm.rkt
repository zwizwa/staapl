#lang racket/base

(require
 "reflection.rkt"
 "rpn-target.rkt"
 "../target.rkt"
 "../scat.rkt"
 "../ns.rkt"
 "../rpn.rkt"
 "../macro.rkt"
 "tethered.rkt"
 "commands.rkt"
 "../forth/forth-lex.rkt"
 "rpn-live.rkt"
 (for-syntax
  "../forth/forth-tx.rkt"
  "../ns-tx.rkt"
  racket/base))

(provide (all-defined-out))

;; The vm: language is used to interact with a Forth VM written in the
;; macro dialect.  It uses underscore prefixed names to identify
;; primitive words, and the words "interpret", "literal" and "compile"
;; to simulate a parser.

;; Note that this is basicly a hack that lives inbetween two worlds
;; and is only for debugging!  (It is used to test a standard
;; stand-alone 16-bit Forth on top of a Staapl 8-bit Forth layer.)

;; LITERALS
(define (vm-interpret-data x) (live: ',x _>t))
(define-syntax-rule (vm-push  im p sub)
  (let ((p ((vm-interpret-data im) p))) sub))

;; IDENTIFIERS

;; The 'address identifier is defined in the forth and late bound.
(define maddress (eval '(macro address)))

(define (vm-prefix sym)
  (string->symbol (format "_~a" (symbol->string sym))))
(define (vm-interpret sym)
  (let* ((psym (vm-prefix sym))
         (m (ns-name '(macro) psym))
         (t (target-find-code psym)))
    (if (not (and m t))
        (eval `(target: ,psym)) ;; delegate to super (eval -> also prefix parsers)
        (target: ,(vm-interpret-data t) interpret))))

(define-syntax-rule (vm id)
  (vm-interpret 'id))

(define-syntax-rule (vm: code ...)
  (target-parse ((vm)
                 vm-push)
                code ...))
             
(define-syntax-rule (vm> code ...)
  (void ((vm: code ...) (state:stack))))


(ns (vm) (define-syntax slurp rpn-slurp))

;; Compile a single dictionary word based on namespace lookup.  Note
;; that all names in native Forth are macros, but we need to to unwrap
;; this abstraction and distinguish between true macros, and the 2
;; target forms macro expand to: target code and target variables.

(define (vm-compile name . words)
  `(target>
    : ,(vm-prefix name)
    enter
    ,@(apply
       append
       (for/list ((w words))
         (if (not (symbol? w))
             `(',w literal)                     
             (let* ((defined?
                      (make-ns-defined?
                       (vm-prefix w)))
                    ;; get its macro wrapper
                    (m (defined? '(macro)))
                    (t (defined? '(target))))
               (unless m
                 (error 'not-found "~s" w))
               (if (not t)
                   `(',m i)
                   (case (target-word-realm t)
                     ((code) `(',m compile))
                     ((data) `(m literal))))))))))

(prefix-parsers
 (vm)
 ((vm-definition (c ...)) (,(!: (eval (vm-compile 'c ...)))))
 ((:) (slurp vm-definition)))

;; Escape from target: -> vm:
(prefix-parsers
 (target)
 ((vm-code (c ...)) (,(vm: c ...)))
 ((_) (slurp vm-code)))
 
