#lang scheme/base

;; Assembler dictionary.

;; Separate file, because the assembler pattern transformer depends on
;; 'asm-find' to check symbols used in the patterns.

;; FIXME: check if assembler name resolution can be moved to compile
;; time + check if contracts can be used for assembler type checking.


(require
 scheme/promise
 scheme/pretty
 ;; "decoder.ss"
 "../op.ss"
 "../scat.ss"
 "pointers.ss"
 "../tools.ss"
 "../target.ss"
 "../ns.ss"
 (for-syntax
  scheme/base
  "../ns.ss"))

(provide
 dasm-parse
 dasm-arity
 define-dasm-collection   ;; call another macro with the visible disassembler collection
 disassemble->word)


(define-syntax (define-dasm-collection stx)
  (syntax-case (list stx (datum->syntax stx (ns-mapped-symbols '(op dasm)))) ()
    (((_ name) (opcode ...))
     #`(define name
         (filter dasm? (list (ns (op dasm opcode)) ...))))))
                 

(define (dasm-arity d) (procedure-arity (dasm-fn d)))


;; The disassembler can't be decentralized like the assembler.
;; Therefore a disassembler is represented by an aggregate object
;; built of individual disassembler objects, one for each instruction.

;; The access method used to be a binary search tree based on the top
;; nb of bits (opcode) in the instruction, but this is inadequate
;; (i.e. for MIPS there is also a "function" field).

;; For now this uses simple incremental search.  Later we can use some
;; grouping that ensures that if an instruction doesn't match for the
;; group's predicate, it won't match for the whole group.


;; Dissembly works simplest as a lazy list operation.

(define (zeros) (delay (cons 0 (zeros))))

(define (dasm-parse dasm-list ll-bin addr)
  (let loop ((ll-bin ll-bin)
             (addr addr))
    (delay
      (let ((bin (force ll-bin)))
        (if (null? bin)
            '()
            (let next ((ds dasm-list))
              (let ((disassembler (car ds))
                    (ds+ (cdr ds)))
                (let ((n (sub1 (dasm-arity disassembler)))) ;; don't count PC argument
                  (when (zero? n) (error 'dasm-arity-error))
                  (let-values (((instruction-words ll-bin+)
                                (ll-take n ll-bin zeros)))  ;; pad with zeros
                    (let ((sym (apply (dasm-fn disassembler)
                                      addr
                                      instruction-words)))
                      (if sym
                          (cons (list sym instruction-words addr)
                                (loop ll-bin+ (+ addr n)))
                          (next ds+))))))))))))


(define (disassemble->word dasm-list
                           bin addr wordsize
                           [resolve (lambda (x) x)])
  (define default-dasm 
    (make-dasm (lambda (here word) (op: dw word))))
  (define dasm-list+default
    (append dasm-list (list default-dasm)))
  (define (resolve-op lst)
    (cons (car lst) (map resolve (cdr lst))))
  (let ((l (ll->l
            (dasm-parse dasm-list+default
                        (seq->ll bin)
                        addr))))
    (let ((asm   (map (compose resolve-op car) l))
          (bin   (map cadr l))
          (addrs (map caddr l)))

      '(pretty-print
       (split-list
        (lambda (addr . _) (symbol? (resolve addr)))
        list
        addrs asm bin))

      (unless (list? l)
        (error 'disassemble->word "~s" l))
      (new-target-word #:name    '<raw-dasm>
                       #:realm   'code
                       #:address addr
                       #:code    (reverse asm)
                       #:bin     (reverse bin)))))

;; Note that this is for RISC instruction sets only. All instructions
;; have the same size and are word-addressed. Any mult-word
;; instructions need to be parsed in a later step. (This works well
;; for PIC18 because the 2nd word is a valid NOP instruction, but
;; might need some reworking).

;; (define (disassemble->word binary address wordsize
;;                            ;; + 1 because base is AFTER instruction. i think
;;                            ;; this is as good as universal, so hardcoded here.
;;                            [resolve (lambda (x) x)]
;;                            [rel->abs (lambda (addr rel) (+ 1 (+ addr rel)))])
;;   (define *bin* '())
;;   (define *code* '())

;;   (define (dasm addr ins)
;;     (match ((dasm-find ins wordsize) ins)
;;        ((rator . rands)
;;         (cons rator
;;               (map
;;                (match-lambda
;;                 ((type . value)
;;                  (case type 
;;                    ((R) (resolve (rel->abs addr value)))
;;                    (else value))))
;;                rands)))))

;;   (for ((a (in-naturals address))
;;         (b binary))
;;     (push! *bin*  (list b))
;;     (push! *code* (dasm a b)))

;;   (new-target-word #:realm 'code
;;                    #:address address
;;                    #:code *code*
;;                    #:bin *bin*))

    