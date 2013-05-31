#lang scheme/base

;; Transformer routines for the asm/dasm syntax.

(provide
 asm/dasm-lambda-tx)


(require
 "../op/static.ss"  ;; make-op-static 
  
 (for-template
  "../op.ss"    ;; make-asm
  "operand.ss"      ;; asm+ 
  scheme/base))

;; Parameter classes.

;; Assembly operands are single-character variables.  This is to
;; facilitate instruction bit vector layout specification.  In
;; addition, certain letters have special properties.

;; Determine field assembler in terms of parameter class.
(define (paramclass->asm name)
  (case name
    ((R)  #'asm+/pcr)    ;; used for relative jumps
    (else #'asm+)))      ;; assemble value ignoring overflow

(define (paramclass->dasm name)
  (case name
    ((R)  #'dasm/pcr)
    (else #'dasm/unsigned)))


;; SEXP FORM


;; (assembler-body '((118  7) (s  1) (k  8)))
(define (assembler-body opcode-body)
  (syntax-case opcode-body ()
    (((opcode . opcode-bits) . operands)
     (foldl
      (lambda (p/b inner)
        (syntax-case p/b ()
          ((param bits)
           #`(#,(paramclass->asm (syntax-e #'param))
              pc param bits #,inner))))
      #'opcode
      (syntax->list #'operands)))))

(define (asm-lambda-tx name formals template-list)
  #`(let ((#,name
           (lambda (address #,@formals)
             (let ((pc (+ address #,(length template-list))))  ;; address->pc translation
               (list #,@(map assembler-body template-list))))))
      #,name))


;; STRING FORM

;; (bitstring->list "0101 kkkk ffff ffff")
(define (lex-bits str-stx)
  (define (char->stx char)
    (datum->syntax str-stx ;; preserve context
                   ((lambda (string)
                      (or (string->number string)
                          (string->symbol string)))
                    (string char))))
  (define (valid-char? char)
    (not (equal? char #\space)))
  (map
   char->stx
   (filter valid-char?
           (string->list (syntax->datum str-stx)))))

;; Parse bit string
(define (parse-bits stx)
  (syntax-case stx ()
    ((bit ...)
     (let down ((stx (for/list ((el (syntax->list #'(bit ...)))) (list el 1))))
       (syntax-case stx ()
         (((k  n) (l  m) . rest)
          (let ((_n (syntax-e #'n))
                (_m (syntax-e #'m))
                (_k (syntax-e #'k))
                (_l (syntax-e #'l)))
            (cond
             ;; Collect symbol's bits
             ((and (symbol? _k) (eq? _k _l))
              (down #`((k  #,(+ _n _m)) . rest)))
             ;; Collect number's bits
             ((and (number? _k) (number? _l))
              (down #`((#,(+ (* 2 _k) _l)  #,(+ _n _m)) . rest)))
             ;; Done, move to next.
             (else
              #`((k  n) #,@(down #`((l  m) . rest)))))))
         (other #'other))))))




;; DISASSEMBLER

;; A disassembler is a "parser": it consumes a stream of numbers in
;; the form of a lazy list (delay (cons head tail)) | (delay '()) and
;; produces a (syntax/#f ll) value pair.

;; Compiling a disassembling works in two steps:
;;   * compile an opcode matcher
;;   * run the generic disassembler



;; Generates binding form for disassembler from instruction field
;; description.

;; ((disassemble-bind #'(((aaa 1) (bbb 2)) ((ccc 3) (ddd 4)))) #'123)
;; =>
;; (lambda (temp20 temp21)
;;  (let-values (((aaa bbb) (disassemble/values '(1 2) temp20))
;;               ((ccc ddd) (disassemble/values '(3 4) temp21)))
;;    123))


(define-syntax-rule (push! stack x) (set! stack (cons x stack)))
(define-syntax-rule (lambda* formals . body) (lambda (a) (apply (lambda formals . body) a)))
(define (generate-temp) (car (generate-temporaries #'(#f))))

(define (dasm-lambda-tx opcode-name formals body-stx)
  (define literals '())
  (define variables (make-hash))
  (define (fix-names! names bits)
    (for/list ((n (syntax->list names))
               (b (syntax->list bits))
               (i (in-naturals)))
      (let ((_n (syntax-e n)))
        (if (number? _n)
          (let ((__n (generate-temp)))
            (push! literals (list __n _n))
            __n)
          (begin
            (hash-set! variables _n b)
            n)))))
  (let ((ws (generate-temporaries body-stx)))
    (syntax-case (list ws body-stx) ()
      (((w ...)  (((name bits) ...) ...))
       #`(let ((#,opcode-name
                (lambda (assembler) ;; to bind to assembler instance later
                  (lambda (address w ...)  ;; address is passed explicitly: no asm pointers context dependency here.
                    ;; (printf "dasm: ~a ~a ~a\n" '#,opcode-name '#,formals (list pc w ...))
                    (let ((pc (+ address #,(length ws))))  ;; address -> pc translation
                      (let-values 
                          ;; Transpose it
                          #,(for/list ((stx (syntax->list #'((w (name ...) (bits ...)) ...))))
                              (syntax-case stx ()
                                ((w ns bs)
                                 #`(#,(fix-names! #'ns #'bs)
                                    (disassemble/values 'bs w)))))
                        (and
                         #,@(map (lambda* (name value)
                                          #`(= #,name #,value))
                                 literals)
                         (list assembler 
                               ;; address -- Won't give address since all words
                               ;; are converted to absolute addressing already.
                               #,@(for/list ((f (syntax->list formals)))
                                    (let ((_f (syntax-e f)))
                                      #`(#,(paramclass->dasm _f)
                                         pc
                                         #,f
                                         #,(hash-ref variables _f))))))))))))
           #,opcode-name)))))
      
      

;; Main entry point for asm/dasm spec -> function bodies + static info.
(define (asm/dasm-lambda-tx stx)
  (syntax-case stx ()
    ((_ name formals . body)
     ;; (printf "asm: ~a\n" (syntax-e #'name))
     (let ((body-stx
            (syntax-case #'body ()
              ((((param . bits) ...) ...) ;; sexp syntax
               (syntax->list #'body))
              ((str ...) ;; string syntax
               (map (compose parse-bits lex-bits)
                    (syntax->list #'(str ...)))))))
       (values
        (asm-lambda-tx   #'name #'formals body-stx)
        (dasm-lambda-tx  #'name #'formals body-stx))))))



;; (define (proto->disassembler stx)
;;   #`(lambda (ll)
;;       (let ((
  
;;   (syntax-case stx ()
;;     ((name formals)
;;      #`(lambda (opcode) '(name)))
;;     ((name formals 
      
;;       #`(lambda (opcode)
;;           (match
;;            (cadr
;;             (chain ;; construct a chain of argument shifts
;;              `(,opcode ())
;;              #,@(map
;;                  (match-lambda
;;                   ((param . bits)
;;                    #`(dasm-step '#,param #,bits)))  ;; one shift tick
;;                  (reverse (cdr (car binary-words))))))
;;            (#,(map car
;;                    (cdr (car binary-words)))
;;             (list '#,name #,@formals))))))

    

        