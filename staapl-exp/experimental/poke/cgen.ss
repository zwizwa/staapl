#lang scheme/base

;; C code generator
;; generates a small subset of C language constructs

;; for C BNF see
;; http://lists.canonical.org/pipermail/kragen-hacks/1999-October/000201.html


;; since C code is nested, it doesn't fit the flat assembler code
;; generation framework in BADNOP. however, it is easy to use just a
;; small subset on top of this abstraction that is flat, and can be
;; used as an assembler target language.

;; both EXPRESSION and STATEMENT sublanguages are organized as:
;; - a minimal set of syntax -> string formatting primitives
;; - a symbolic set of syntax -> syntax transformers on top of that

;; the formatter uses an s-expression interpreter interpreter defined
;; in the 'interpreter' macro.

;; note that the extension mechanism (s-expr only) is different from
;; the definition mechanism 'c-code-generators'.

;; This uses syntax objects for the convenience of 'syntax-case' over
;; 'match' only: lexical information is not used.


;; (set-selective-display 3)


(require
 "../tools/stx.ss"
 scheme/control
 )

(provide
 ;; default functionality
 statement->string
 expression->string

 ;; util
 map-stx
 transformer
 
 ;; extensions
 statements
 expressions

)

;; FMT



;; INDENTATION

(define (spaces->string spaces)
  (let loop ((n spaces) (l '()))
    (if (< n 1)
        (list->string l)
        (loop (- n 1) (cons #\space l)))))

(define (no-tab x) x)
(define (default-tab x) (+ 2 x))

(define tab   (make-parameter default-tab))
(define depth (make-parameter 0))

(define (indent . args)
  (string-append
   (apply indent/n args)
   "\n"))

(define (indent/n . args)
  (string-append (spaces->string (depth))
                 (apply format-stx args)))

(define (indented e)
  (parameterize
      ((depth ((tab) (depth))))
    (statement->string e)))


(define (not-indented e)
  (statement->string e))

(define (expression-statement exp)
  (indent "~a;" (e exp)))
(define (expand-statements stx)
  (apply string-append
         (map-stx not-indented stx)))


;; ENTRY POINTS

(define (expression->string s-exp)
  (transform expression s-exp simple-expression))
(define (statement->string s-exp)
  (transform statement s-exp expression-statement))

;; The namespaces are managed in a hash table. Note: this should be such
;; that later extension to prefixed namespaces is easy to do.

(define expression (cons 'expression (make-hash)))
(define statement  (cons 'statement  (make-hash)))
(define table-name car)
(define table-hash cdr)

(define (resolve table name
                 [not-found
                   (lambda ()
                    (error 'undefined
                           (format-stx "~a ~a undefined"
                                       (table-name table)
                                       name)))])
  (hash-ref (table-hash table)
            (->sexp name)
            not-found))

(define (register table name value)
  (when (resolve table name (lambda () #f))
    (error 'already-defined
           (format-stx "~a ~a already defined"
                       (table-name table) name)))
  (hash-set! (table-hash table) name value))

(define (transform table expr
                   [default
                     (lambda (stx)
                       (error 'invalid-syntax
                        (format-stx "~a in ~a"
                                    (table-name table) stx)))])
   (if (not (syntax? expr))
       expr ;; done
       (transform
        table
        (prompt
         (syntax-case expr ()
           ((id . args)
            ((resolve table
                      (->sexp #'id)
                      (lambda ()
                        (abort (default expr))))
             #'args))
           (e (abort (default #'e)))))
        default)))


(define-syntax-rule (transformer . clauses)
  (lambda (stx) (syntax-case stx () . clauses)))

(define-syntax-rule (transformers table
            ((name . formals) body ...) ...)
  (begin
    (register table 'name
              (transformer
               (formals body ...)))
    ...))

(define-syntax-rule (statements . args)  (transformers statement . args))
(define-syntax-rule (expressions . args) (transformers expression . args))





;; EXPRESSION FORMATTER

;; using short names since they are used a lot. expressions are not
;; indented: they are nested on one line.

;; 2 entry points:

;; e  : expression
;; pe : parenthesized expression if infix
;; for symbol classes

(define-syntax-rule (member-tests (thing? lst) ...)
  (begin
    (define thing?
      (let ((symbols (map string->symbol lst)))
        (lambda (x)
          (and (member
                (syntax->datum x)
                symbols) #t))))
    ...))

(member-tests
 (c-keyword?
  '("return" "goto" "break" "continue"))
 (nospace?
  '("->" "."))
 (infix?
  '("+" "-" "*" "/" "&" "|" "&&" "||" "<<" ">>"
    ">" "<" "<=" ">=" "!=" "=="
    "=" "+=" "-=" "|=" "&=" "<<=" ">>="
    "->" "."
    )))

(define (e exp) (expression->string exp))
(define (pe exp)
  (parameterize
      ((paren do-wrap-paren))
    (e exp)))

(define (join separator args)
  (if (null? args) ""
      (apply string-append
             (cons 
              (format-stx "~a" (car args))
              (map
               (lambda (arg)
                 (format-stx "~a~a" separator arg))
               (cdr args))))))


;; default expression formatter
;; need to wrap infix operators in parens if they are not toplevel.

(define (do-wrap-paren x)    (format-stx "(~a)" x))
(define (dont-wrap-paren x) x)
(define paren (make-parameter dont-wrap-paren))

(define (simple-expression exp)
  (syntax-case exp ()
    ((op left right)
     (infix? #'op)
     (let ((space (if (nospace? #'op) "" " ")))
       ((paren)
        (format-stx "~a~a~a~a~a"
                    (pe #'left)
                    space #'op space
                    (pe #'right)))))
    ((op . args)
     (format-stx "~a~a" #'op
                 (format-stx
                  (cond
                   ((c-keyword? #'op)    " ~a")
                   (else                 "(~a)"))
                  (join ", " (map-stx e #'args)))))
    (var
     ;; not a list -> literal/variable
     ;; this needs to be '~s' instead of '~a' to enable literal strings
     (format-stx "~s" exp))))




(expressions
 ((post  op arg) (format-stx "~a~a"   (e #'arg) #'op))
 ((pre   op arg) (format-stx "~a~a"   #'op (e #'arg)))
 ((index name i) (format-stx "~a[~a]" (pe #'name) (e #'i)))
 
 ((if test yes no)
  (format-stx "~a ? ~a : ~a"
              (pe #'test) (pe #'yes) (pe #'no)))
 
 ;; downward let using gcc extension "Statements and
 ;; Declarations in Exressions".
 ((let (decls ...) body ...)
  (format-stx "({\n~a~a"
              (indented
               #`(statements (vars decls ...) body ...))
              (indent/n "})")))
 )



;; STATEMENT FORMATTER

;; statements are indented.


(define (null->void lst) (if (null? lst) '("void") lst))
(define (declaration d)
  (syntax-case d ()
    ((type name)
     (format-stx "~a ~a" #'type #'name))))
(define (declarations lst)
  (null->void (map-stx declaration lst)))


(statements
 ((statements . body) (expand-statements #'body))
 ;;    ((append str) #'str)
 ((label-head name) (indent "~a:" #'name))
 ((comment str) (indent "// ~a" #'str))
 ((line str) (indent "~a" #'str))        
 
 ((fun-head terminator fn . args)
  (format-stx "~a(~a)~a"
              (declaration #'fn)
              (join ", " (declarations #'args))
              #'terminator))
 
 ((indented . statements)
  (apply string-append
         (map-stx indented #'statements)))
 
 ((for-head . exp-lst)
  (indent "for (~a)" (join "; " (map-stx e #'exp-lst))))
 
 ((var type name . vallist)
  (indent "~a ~a~a;" (e #'type) #'name
          (syntax-case #'vallist ()
            (() "")
            ((v) (format-stx " = ~a" (e #'v)))))))




;; TRANSFORMERS
;; highlevel statement/expression transformers

(statements

 ((def (decls ...) . body) 
  #`(statements
     (fun-head "\n" decls ...)
     (block . body)))

 ((decl decls ...)
  #`(fun-head ";\n" decls ...))

 ((label name . body)
  #`((label-head ,name)
     (indented ,@body)))
 
 ((vars decl ...) 
  #`(statements (var . decl) ...))
 
 ((for (exp ...) . body)
  #`(statements
     (for-head exp ...)
     (block . body)))
 
 ((bind (decls ...) . body)
  #`(block
     (vars decls ...) . body))
 
 ((block statement ...)
  #`(statements
     (line "{")
     (indented statement ...)
     (line "}"))))




;; TEST
(define (pstat expr)
  (display
   (statement->string
    (datum->syntax #f expr))))