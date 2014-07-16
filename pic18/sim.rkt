#lang racket/base
(require racket/control
         racket/promise
         racket/match
         racket/dict
         racket/pretty
         "../target/rep.rkt" ;; instruction->string
         "../tools.rkt"      ;; ll->l
         "asm.rkt"           ;; pic18
         "../asm.rkt")       ;; generic

;; Is it just crazy midnight hacking to want to emulate the PIC18?
;; Not really.  There is no semantics to Staapl other than what the uC
;; core does with the binary compiler output, so a hackable emulator
;; is the closest thing.



(define-syntax-rule (params p ...)
  (begin (define p (make-parameter #f)) ...))
    

(params mem ip wreg ram stack fsr)

(define (fsr-set! f v) (vector-set! (fsr) f v))
(define (fsr-ref f)    (vector-ref  (fsr) f))

(define (push x) (stack (cons x (stack))))
(define (pop) (let ((s (stack)))
                (stack (cdr s))
                (car s)))
  

(define (ram-set! addr val) (vector-set! (ram) addr val))
(define (ram-ref  addr)     (vector-ref  (ram) addr))

(define (load-mem filename)
  (for/list ((chunk (read (open-input-file filename))))
    (list (list-ref chunk 0)
          (apply vector (list-ref chunk 1)))))

(define (mem-ref addr [word #f])
  (prompt
   (for ((chunk (mem)))
     (let-values (((chunk-addr chunk-data) (apply values chunk)))
       (let ((offset (- addr chunk-addr)))
         (when (and (>= offset 0) 
                    (< offset (vector-length chunk-data)))
           (let ((lo (vector-ref chunk-data offset)))
             (abort
              (if word
                  (let ((hi (vector-ref chunk-data (add1 offset))))
                    (+ lo (* #x100 hi)))
                  lo)))))))
   (if word
       #xFFFF
       #xFF)))

(define (next-word)
  (let ((w (mem-ref (ip) #t)))
    (ip (+ (ip) 2))
    w))


;; Disassemble

;; This non-hygienic form collects all disassembler functions visible
;; in this module namespace.  This is used during live interaction.
(define-dasm-collection dasm-collection)
(define dasm-collection+dw
  (append dasm-collection (list default-dasm)))
(define (dasm-ip)
  (let* ((here (ip))
         ;; Feed the dasm two words of context
         (w0 (mem-ref here #t))
         (w1 (mem-ref (+ 2 here) #t)))
    (car ;; only interested in first instruction
     (ll->l
      (dasm-parse dasm-collection+dw
                  (list w0 w1)
                  here)))))

(define-syntax-rule (define-opcodes opcodes (name args . body) ...)
  (define opcodes
    `((name . ,(lambda args . body)) ...)))

;; Generic FSR access
(define (indirect f [pre void] [post void])
  (list (lambda (v) (pre) (ram-set! (fsr-ref f) v) (post))
        (lambda ()  (pre) (let ((rv (ram-ref (fsr-ref f)))) (post) rv))))
(define (fsr-update f upd)
  (lambda () (fsr-set! f (upd (fsr-ref f)))))
(define (preinc f)  (indirect f (fsr-update f add1) void))
(define (postdec f) (indirect f void (fsr-update f sub1)))

(define (sfr-fixme tag)
  (define (dummy . args)
    (printf "-- sfr-fixme ~s\n" tag))
  (list dummy dummy))

;; Regs are lists of write, read operations
(define STKPTR   #xFC)
(define TRISB    #x93)

(define sfrs
  `((#xFC  . ,(sfr-fixme 'STKPTR))
    (#x93  . ,(sfr-fixme 'TRISB))
    (#xEC  . ,(preinc 0))
    (#xED  . ,(postdec 0))
    ))

(define (reg-write r v) ((car r) v))
(define (reg-read  r)   ((cadr r)))

(define (lohi lo hi) (+ lo (* #x100 hi)))
(define (ipw lo [hi 0])  (ip (* 2 (lohi lo hi))))
                  
(define-opcodes opcodes
  (bra   (addr)  (ipw addr))
  (_goto (lo hi) (ipw lo hi))  
  (_call (s lo hi)
         (unless (zero? s) (raise 'call-s=1))
         (push (ip))
         (ipw lo hi))
  (return (s)
         (unless (zero? s) (raise 'call-s=1))
         (ip (pop)))
          
          
  (movlw (l)     (wreg l))
  (movwf (reg b)
         (unless (zero? b) (raise 'movwf-b=1))
         (if (>= reg #x80)
             (reg-write (dict-ref sfrs reg) (wreg))
             (ram-set! reg (wreg))))
  (movf (reg a d)
        (unless (zero? a) (raise 'movf-a=1))
        (unless (zero? d) (raise 'movf-d=1))
        (wreg
         (if (>= reg #x80)
             (reg-read (dict-ref sfrs reg))
             (ram-ref reg))))

  

  (_lfsr (f l h) (fsr-set! f (lohi l h)))
  
  )

(define (execute-next)
  (match (dasm-ip)
    ((list (list-rest asm args) words addr)
     (begin
       (ip (+ (ip) (* 2 (length words))))
       (let ((mnem (asm-name asm)))
         (for ((w words)) (printf "~x " w))
         (printf "~s " mnem)
         (printf "~s \n" args)
         (let ((op (dict-ref opcodes mnem)))
           (apply op args)))))))

(define (run)
  (execute-next)
  (run))
    

;; Testing
(define (test)
  (mem (load-mem "/home/tom/staapl/app/test.sx"))
  (ip 0)
  (wreg 0)
  (ram (make-vector #x100 #f))
  (fsr (make-vector 3 #f))
  (stack '())
  (run))


;; (dasm-ins '(#xD01F))
