#lang scheme/base

;; Compile time tools for rpn-parse.ss

(require
 "../tools/list.ss"
 "../ns.ss"
 "../ns-tx.ss"
 "../tools/stx.ss"
 scheme/match
 (for-template
  "../ns.ss"
  scheme/base)
 (for-syntax
  scheme/base))

(provide (all-defined-out))

;; 2 extra objects used in the parser
         
;; The W stack (word stack = lexed forth code) is implemented
;; abstractly using these macros.
(define-syntax-rule (w-null? w) (null? w))
(define-syntax-rule (w-car w) (car w))
(define-syntax-rule (w-cadr w) (cadr w))
(define-syntax-rule (w-cdr w) (cdr w))
(define-syntax-rule (w-cddr w) (cddr w))
(define-syntax-rule (w-append . f) (append . f))

(define (w-map w) (ns-prefixed #'(rpn) w))


;; The output of rpn-parse is a dictionary.  A dictionay is a list of
;; lists which will be passed off to a preprocessor macro.  The
;; content of the list is really arbitrary, but the canonical use is
;; to fill it with macros that expand to toplevel forms.  This gives
;; the most flexibility to give semantics to a flat language.

;; There are only 2 important operations in a dictionary:
;;
;;   * compile:  append an item to the current form.
;;   * start:    start a new form

(define (d-create)           (make-sos 2))        ;; create a blank dictionary
(define (d-start d)          (sos-collapse d 1))  ;; start a new line
(define (d-compile instr d)  (sos-push d instr))  ;; append to the current line
(define (d->forms d)         (sos->list d))       ;; reduce to list of forms
(define (d-last d)           (reverse (car d)))   ;; get last entry
(define (d-on-last d fn)     (cons                ;; operate on last
                              (reverse (fn (d-last d)))
                              (cdr d)))

(define (d-insert entry d)   (sos-stash d entry)) ;; insert a full expression

;; Used to tag rpn transformer objects for compile time bindings.
;; These will be called from rpn-parse.
(define-struct rpn-transformer (tx))

;; TOOLS
(define (stx-args stx)
  (cdr (syntax->list stx)))



;; Parser nesting.

;; The rpn-parse macro is a simple loop that runs until end-of-input.
;; However, it calls extensions in tail position which leaves the
;; possibility to the extensions to perform arbitrary dynamic nesting
;; depending on the input data.
;;
;; In order to make nested invocation work, we call 'next with a word
;; stream terminated with an escape continuation wrapped as an
;; rpn-transformer.  This will be detected by rpn-parse and invoked
;; before the end of the list is reached.

(define (rpn-parse-nested w-nest d k)
  (let-values (((_ d+ next)
                (let/ec ec
                  (k (w-append w-nest
                               (list (make-rpn-transformer ec)))
                     d))))
    ;; Instead of continuing, the continuation is passed upstream.
    (values d+ next)))


;; Prefix parsers can be made to behave as syntax-rules.  This is a
;; scheme form that defines such rewriting macros as a convenient way
;; to construct parsing words from primitive parsers.

(define-syntax-rule
  (rpn-syntax-rules (literal ...)
                    ((pattern ...) (template ...)) ...)
  (make-rpn-transformer
   (lambda (w d k)
     (syntax-case w (literal ...)
       ((pattern ... . w+)
        (k (syntax->list/recertify #`(template ... . w+))
           d)))
     ...)))

;; Same for lowlevel 'syntax-case style pattern macros.  The first one
;; performs a full multi-clause pattern match with literals and
;; updates the dictionary.  The second one is specialized to a single
;; clause and no literals.

(define-syntax-rule
  (rpn-dict-case dict
                 (literal ...)
                 ((pattern ...) expr) ...)
  (make-rpn-transformer
   (lambda (w dict k)
     (syntax-case w (literal ...)
       ((pattern ... . w+)
        (k (syntax->list #'w+) expr))
       ...))))

(define-syntax-rule (rpn-dict-rule d (pattern ...) expr)
  (rpn-dict-case d () ((_ pattern ...) expr)))