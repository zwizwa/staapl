#lang scheme/base

;; Simplified wrapper around c.plt for C code analysis and synthesis.


;; FIXME: There is another variant of this file in libprim.

(require (planet dherman/c:3:2)
         scheme/control
         scheme/pretty
         scheme/match
         "cpp.ss")

(define (parse-cpp . args)
  (parse-program (apply open-input-cpp args)))



(require (for-syntax scheme/base))

;; Some simplified pattern matching syntax, ignoring the source
;; location, with implicit `struct' and one level of parens removed.

(define-syntax (cmatch stx)
  (define (tx-pattern stx)
    (syntax-case stx ()
      ((tag . args)
       #`(struct tag (src #,@(map tx-pattern (syntax->list #'args)))))
      (var #'var)))
  (syntax-case stx ()
    ((_ in (pat expr) ...)
     #`(match in
              #,@(for/list ((p (syntax->list #'(pat ...)))
                            (e (syntax->list #'(expr ...))))
                   (list (tx-pattern p) e))
              (error 'cmatch "~s" in)))))


;; Naive pretty-printing: all expressions are printed in nested form
;; on one line.  Statements are printed one per line.  Each block
;; indents.  All built-in operations have explicit parenthesis to
;; avoid precedence problems.

(define (ast-emit ast [emit display] [tab-spacing "  "])
  (define (string x) (emit (format "~a" x)))
  (define-syntax emit/call
    (syntax-rules ()
      ((_ (op . args)) (op . args))
      ((_ datum) (emit datum))))
  (define-syntax-rule (e: it ...)
    (begin (emit/call it) ...))
  (let down ((ast ast) (tab ""))
    ;; `es' means emit substructure
    (define (es x) (down x tab))  
    (define (es/tab x) (down x (string-append tab-spacing tab)))
    (define (es/sep lst [sep ", "])
      (let ((n (length lst)))
        (when (> n 0)
          (es (car lst))
          (when (> n 1)
            (for ((l (cdr lst)))
              (emit sep)
              (es l))))))
    (cond
     ((symbol? ast) (emit (symbol->string ast)))
     ((number? ast) (emit (number->string ast)))
     ((list? ast) (for-each es ast))
     ((not ast) (void))
     (else
      (cmatch
       ast

       ((id:var name)           (es name))
       ((id:op  name)           (es name))
       ((type:primitive name)   (es name))
       ((expr:int val _)        (es val))
       ((expr:float val _)      (es val))
       ((expr:ref id)           (es id))
       ((expr:postfix e op)     (e: "(" (es e) ")" (es op)))
       ((expr:assign v op e)    (e: "(" (es v) " " (es op) " " (es e) ")"))
       ((expr:call fun args)    (e: (es fun) "(" (es/sep args ", ") ")"))
       ((expr:array-ref a i)    (e: (es a) "[" (es i) "]"))
       ((stmt:for i c u s)      (e: tab "for (" (es/sep (list i c u) "; ")
                                    ")\n" (es s)))

       ((stmt:return expr)      (e: tab "return " (es expr) ";\n"))
       ((stmt:block items)      (e: tab "{\n" (es/tab items) tab "}\n"))
       ((stmt:expr expr)        (e: tab (es expr) ";\n"))
       ((expr:binop l op r)     (e: "(" (es l) " " (es op) " " (es r) ")"))
       ((type:function r fs)    (e: (es r) "(" (es/sep fs ", ") ")"))

       ((init:expr init)        (e: " = " (es init)))
       
       ((decl:declarator i t =) (e: (es i) (es t) (es =)))
       ((decl:formal _ t d)     (e: (es t) " " (es d)))

       ((decl:vars _ t d)       (e: tab (es t) " " (es/sep d ", ") ";\n"))
       ((decl:function
         _ _ rt dec _ body)     (e: (es rt) " " (es dec) " " (es body)))
       
       )))))


(define (test-ast) (parse-cpp "test.c"))
(define (test)
  (ast-emit (test-ast)))

;; )

(test)

;(define (pf file)
;  (parse-cpp file "-I/home/tom/packetforth"))


;; Simplified functions for AST generation in terms of Scheme symbols
;; and constants.
(define c-src (make-parameter (lambda () #f)))

(define (function type name formals body)
  (make-decl:function ((c-src)) #f #f
                      type
                      (declarator
                       (variable-id name)
                       (make-type:function ((c-src)) #f formals))
                      #f body))
(define (formal type name)
  (make-decl:formal ((c-src)) #f type (declarator (variable-id name))))
(define (declarator id [type #f] [init #f])
  (make-decl:declarator ((c-src)) id type init))

(define (block lst) (make-stmt:block ((c-src)) lst))
(define (variable-id name) (make-id:var ((c-src)) name))

(define (primitive-type sym) (make-type:primitive ((c-src)) sym))


(define (var type name [init #f])
  (make-decl:vars ((c-src)) #f
                  (primitive-type type)
                  (list (declarator (variable-id name)
                                    #f
                                    (if init (initializer init) #f)))))
(define (vars type names)
  (make-decl:vars ((c-src)) #f
                  (primitive-type type)
                  (for/list ((n names)) (declarator (variable-id n) #f #f))))
                                  

(define (initializer expr)   (make-init:expr ((c-src)) expr))
(define (int i)  (make-expr:int ((c-src)) i '()))
(define (float f) (make-expr:float ((c-src)) f '()))

(define (idop op) (make-id:op ((c-src)) op))
(define (binop op a b) (make-expr:binop ((c-src)) a (idop op) b))
(define (assign op a b) (make-expr:assign ((c-src)) a (idop op) b))
(define (postfix a op) (make-expr:postfix ((c-src)) a (idop op)))
                                          
(define (vref x) (make-expr:ref ((c-src)) x))

(define (se expr) (make-stmt:expr ((c-src)) expr))
(define (return expr) (make-stmt:return ((c-src)) expr))
(define (aref name index) (make-expr:array-ref ((c-src)) name index))



(define (loop i n s) (make-stmt:for
                      ((c-src))
                      (assign '= (vref i) 0)
                      (binop '< (vref i) (vref n))
                      (postfix (vref i) '++)
                      s))
                    
                    

;; TEST
(define (codegen-test)
  (define src #f)
  (define void (primitive-type 'void))
  (define int (primitive-type 'int))
  (define _ (primitive-type '_))  ;; FIXME

  ;; Dynamic types (libprim/pf)
  (define (function/dt name fs body)
    (function _ name (for/list ((f fs)) (formal _ f)) body))

  (function/dt 'foo '(a b) (block '())))
                      

;; (ast-emit (codegen-test))


;; ** SSA C-code generator **

;; Produce C code fragments in serialized dataflow form (SSA).

;; This representation is a stx object containing a number of
;; name.expr bindings.  The last one is the value of the expression
;; (compiled to `return' or possibly later a GCC ({ ... }) form.

;; Next to simple nested expression trees, the SSA form can represent
;; DAGs / memoization.  It is executable by a serial machine and
;; allows straightforward recovery of the DF graph.

(define (ssa->c stx [T (primitive-type 'float)])
  (syntax-case stx ()
    (((name expr) ...)
     (let ((ns (syntax->datum #'(name ...)))
           (es (syntax->list #'(expr ...))))
       (block
        (append
         (list
          (vars T ns))
         (for/list ((n ns) (e es))
           (se (assign '= n (stx->c-expr e))))))))))


; Maybe not so useful
;; (define (let*->c stx [T (primitive-type 'float)])
;;   (syntax-case stx ()
;;     (((name expr) ...)
;;      (let ((ns (syntax->datum #'(name ...)))
;;            (es (syntax->list #'(expr ...))))
;;        (block
;;         (append
;;          (for/list ((n ns) (e es))
;;            (var T n (stx->c-expr e)))
;;          (list
;;           (return (vref (car (reverse ns)))))))))))


(define (in-stx stx) (in-list (syntax->list stx)))




(define (c-binop? stx)
  (case (syntax->datum stx)
    ((+ - * / ==) #t)
    (else #f)))

(define (c-unop? stx)
  (case (syntax->datum stx)
    ((- ~ & *) #t)
    (else #f)))

(define (stx->c-stmt stx)
  (define (stx->block stx)
    (block (for/list ((s (in-stx stx)))
             (stx->c-stmt s))))
  (syntax-case stx (in-range for begin)
    ;; Only a subset
    ((begin . stmts) (stx->block #'stmts))
    ((for ((i (in-range n))) . stmts)
     (loop (syntax->datum #'i)
           (syntax->datum #'n)
           (stx->block #'stmts)))
    (else (se (stx->c-expr stx)))))

(define (stx->c-expr stx)
  (define (bin fn op a b)
    (fn (syntax->datum op)
        (stx->c-expr a)
        (stx->c-expr b)))
      
  (syntax-case stx (ref set!)
    ((ref array index)
     (aref (stx->c-expr #'array)
           (stx->c-expr #'index)))
    ((set! a b)
     (bin assign #'= #'a #'b))
    ((op a b)
     (c-binop? #'op)
     (bin binop #'op #'a #'b))
    ;; ((op a) (c-unop? #'op) (error 'ni))
    (else
     (let ((x (syntax->datum stx)))
       (cond
        ((symbol? x)  (vref x))
        ((inexact? x) (float x))
        ((exact? x)   (int x)))))))
           

;; (ast-emit (let*->c #'(let* ((a 123) (b 345)) foo)))

;; ** ARRAYS and LOOPS **

(require "../algebra/z.ss"
         "../algebra/stx.ss")

;; (ast-emit (stx->c-stmt (tilde->ref #'((a (+ (~ c 1) (~ c 0))) ((~ x 0) (* a a))))))
;; (ast-emit (stx->c-stmt (tilde->ref (dfl/z #'((a (+ (z c) c) (x (* a a))))))))
