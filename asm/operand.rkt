#lang racket/base

;; Operand processing for assembler and disassembler.

;; NOTES:

;;  * PC-relative addressing is assumed to use relative instruction
;;    word addresses (not byte addresses) with PC pointing to the
;;    _next_ instruction.  This seems to be standard.  If it needs to
;;    be changed, look at the asm/pcr and dasm/pcr functions to
;;    parameterize.

;;  * Since prior to the invokation of an assembler function we don't
;;    have information about the size of the instruction, the PC can't
;;    be passed to the assembler function.  Therefore they accept the
;;    instruction address.  Translation is performed in
;;    asmgen-lambda-tx


(provide (all-defined-out))

(require "../tools.rkt"
         "environ.rkt")


;; Instruction assemblers get expanded to a nested construct which
;; builds an instruction using consecutive shifts, starting from the
;; high field and shifting left for consecutive fields.

;; (assembler-body '((118  7) (s  1) (k  8)))
;;  => (asm+ k 8 (asm+ s 1 118))

(define operand:signed -1)
(define operand:unsigned 1)

(define (asm+/unsigned . a) (apply asm+/overflow operand:unsigned a))
(define (asm+/signed . a)   (apply asm+/overflow operand:signed a))

(define (asm+/overflow type pc value bits acc)
  (unless (or
           (> (asm-phase) 0) ;; ignore overflows in phase 0
           (asm-fits? value bits type))
    ((asm-error) 'overflow type value bits))
  (asm+ pc value bits acc))

(define (asm+ pc value bits acc)
  (bior
   (band (int value) (bitmask bits))
   (<<< acc bits)))

(define (asm+/pcr pc value . a)
  (apply asm+/signed pc (- value pc) a))



;; To check overflow, we need to know whether the byte is signed or
;; unsigned. For a word of b bits, we inspect the bits left of the
;; first b-1 bits.

(define (asm-fits? value bits type)
  (let ((rest (>>> value (- bits 1))))
    (or
     (zero? rest)    ;; always correct: fits in both signed and unsigned rep.
     (eq? rest type) ;; the other legal value is 1 for unsigned and -1 for signed.
     )))


;; DASM

(define (dasm/unsigned pc value bits)
  (band value (bitmask bits)))  ;; probably not necessary, but mask it anyway.

(define (dasm/signed pc value bits)
  (sign-extend (dasm/unsigned pc value bits) bits))

;; Please not that the combination between lazy lists and dynamic
;; parameters isn't the best fit.
(define dasm-pcr-enable (make-parameter #t))

(define (dasm/pcr pc value bits)
  (let ((signed (dasm/signed pc value bits)))
    (if (dasm-pcr-enable) ;; HACK.  Sim is better off with relative word addresses.
        (+ signed pc)  ;; Doesn't use asm pointer environment.
        signed)))



;; This uses to be implemented with a binary search tree decoder, but
;; disassembling has never been a bottleneck so I'm taking that out.

(define (disassemble fields word)
  (define (<< x b) (arithmetic-shift x b))
  (define (>> x b) (<< x (- b)))
  (let next ((f (reverse fields))
             (w word)
             (e '()))
    (if (null? f)
        (begin
          ;; (printf "~a ~a ~a\n" fields word e)
          (unless (zero? w) (error 'disassemble-residue "~s" w))
          e)
        (let* ((bits (car f))
               (f+   (cdr f))
               (mask (- (<< 1 bits) 1))
               (p    (bitwise-and w mask))
               (w+   (>> w bits)))
          (next f+ w+ (cons p e))))))

(define (disassemble/values . a)
  (apply values (apply disassemble a)))
