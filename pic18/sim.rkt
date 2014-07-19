#lang racket/base
(require racket/control
         racket/promise
         racket/match
         racket/dict
         racket/pretty
         (except-in racket/bool true false)
         "../target/rep.rkt" ;; instruction->string
         "../tools.rkt"      ;; ll->l
         "asm.rkt"           ;; pic18
         "../asm.rkt")       ;; generic

;; Is it just crazy midnight hacking to want to emulate the PIC18?
;; Not really.  There is no semantics to Staapl other than what the uC
;; core does with the binary compiler output, so a hackable emulator
;; is the closest thing.

;; Machine constants
(define reg-access #x60) ;; #x80 FIXME: depends on core version

(define-syntax-rule (params p ...)
  (begin (define p (make-parameter (void))) ...))
    

(params
 trace  ;; instruction trace addresses
 jit    ;; cache of scheme op lookups (about 10x speedup?)
 flash  ;; chunked flash memory: (list-of (addr (vector-of byte)))
 ip     ;; next instruction to execute.  updated before executing op
 wreg   ;; current working register
 ram    ;; flat (vector-of byte)
 stack  ;; call stack (list-of address)
 stkptr ;; stack pointer for call stack
 fsr    ;; fsr pointers (vector-of byte)
 bsr    ;; bank select register
 Z C DC N OV  ;; flags, broken out as bool
 )

;; stack
(define (push x)
  (let ((p (stkptr)))
    (vector-set! (stack) p x)
    (stkptr (add1 p))))
(define (pop)
  (let ((p (sub1 (stkptr))))
    (stkptr p)
    (vector-ref (stack) p)))
  
;; ram
(define (ram-set! addr val) (vector-set! (ram) addr val))
(define (ram-ref  addr)     (vector-ref  (ram) addr))

;; flash
(define (load-flash filename)
  (for/list ((chunk (read (open-input-file filename))))
    (list (list-ref chunk 0)
          (apply vector (list-ref chunk 1)))))
(define (flash-ref addr [word #f])
  (prompt
   (for ((chunk (flash)))
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
  (let ((w (flash-ref (ip) #t)))
    (ip (+ (ip) 2))
    w))

;; abstract register access
(define-struct register
  (read
   write
   read-modify-write  ;; separate due to pre/post inc/dec on FSRs
   ))

;; if register access does not have side effects (see FSRs), just
;; implement rmw in terms of read & write
(define (make-rw-register read write)
  (define (read-modify-write update)
    (let ((v (update (read))))
      (write v)
      v))
  (make-register read write read-modify-write))

(define (make-param-register param)
  (make-rw-register param param))

(define (make-ni-register tag)
  (define (ni . _) (error 'register-not-implemented "~s" tag))
  (make-register ni ni ni))

;; Map data address space to ram or sfrs.
(define (data-register addr)
  (if (> addr (+ #xF00 reg-access))
      (sfr addr)
      (make-rw-register
       (lambda ()  (ram-ref addr))
       (lambda (v) (ram-set! addr v)))))

;; map 8bit register selector + access bit used in most instructions
;; to an abstract register accessor.
(define (ab-register reg a)
  (unless (zero? a) (raise 'banked))
  (let ((addr
         (if (>= reg reg-access)
             (+ #xF00 reg)
             reg)))
    (data-register addr)))

;; ops using ab-register addressing
(define (store reg a v [d 1])
  (if (zero? d)
      (wreg v) ;; also support 'd' flag
      ((register-write (ab-register reg a)) v)))
(define (load reg a)
  (unless (zero? a) (raise 'banked-read))
  ((register-read (ab-register reg a))))
(define (read-modify-write fun reg d a)
  (if (zero? d)
      (let ((v (fun (load reg a)))) (wreg v) v)
      ((register-read-modify-write (ab-register reg a)))))

;; fsr
(define (fsr-set! f v) (vector-set! (fsr) f v))
(define (fsr-ref f)    (vector-ref  (fsr) f))
(define (fsr-update f upd)
  (lambda () (fsr-set! f (upd (fsr-ref f)))))

(define (indirect f [pre void] [post void])
  (define (reg)      (data-register (fsr-ref f)))  ;; Abstract accessor to reg.
  (define (pp thunk) (pre) (let ((v (thunk))) (post) v))
  (define (read)     (pp (lambda () ((register-read (reg))))))
  (define (write v)  (pp (lambda () ((register-write (reg)) v))))
  (define (rmw fun)  (pp (lambda () ((register-read-modify-write (reg)) fun))))
  (make-register read write rmw))

(define (preinc f)  (indirect f (fsr-update f add1) void))
(define (postdec f) (indirect f void (fsr-update f sub1)))
(define (indf f)    (indirect f void void))


;; SFRs that behave as configuration (as opposed to I/O ports) can be
;; implemented simply as RAM read/write.
(define (sfr-ram addr)
  (cons addr
        (make-rw-register
         (lambda ()  (ram-ref addr))
         (lambda (v) (ram-set! addr v)))))

;; FIXME get names from machine const def modules
(define sfrs
  `((#xFFC . ,(make-param-register stkptr))
    ,(sfr-ram #xF92) ;; TRISA
    ,(sfr-ram #xF93) ;; TRISB
    ,(sfr-ram #xF94) ;; TRISC
    ,(sfr-ram #xF95) ;; TRISD
    ,(sfr-ram #xF96) ;; TRISE
    ,(sfr-ram #xF89) ;; LATA
    ,(sfr-ram #xF8A) ;; LATB
    ,(sfr-ram #xF8B) ;; LATC
    ,(sfr-ram #xF8C) ;; LATD
    ,(sfr-ram #xF8D) ;; LATE
    ,(sfr-ram #xFF6) ;; TBLPTRL
    ,(sfr-ram #xFF5) ;; TABLAT
    (#xFED . ,(postdec 0))
    (#xFEC . ,(preinc  0))
    (#xFE8 . ,(make-param-register wreg))
    (#xFE7 . ,(indf 1))
    (#xFE5 . ,(postdec 1))
    (#xFE4 . ,(preinc  1))
    (#xFDD . ,(postdec 2))
    (#xFDC . ,(preinc  2))
    (#xF6d . ,(make-ni-register 'UCON))
    ))
(define (sfr reg)
  (dict-ref sfrs reg
            (lambda ()
              (error 'sfr-not-found "~x" reg))))


(define (lohi lo hi) (+ lo (* #x100 hi)))

(define (ipw lo [hi 0] [offset 0])
  (ip (+ offset (* 2 (lohi lo hi)))))
(define (ipw-rel lo [hi 0])
  (ipw lo hi (ip)))





(define (N-from v)
  (N (not (zero? (bitwise-and #x80 v)))))


;; FIXME: word/byte addresses and rel/abs is a bit messed up in the dasm.

(define (bp flag p rel)
  (unless (not (xor (flag) (bit->bool p)))
    (ipw-rel rel)))

;; 2s complement add truncated to 8 bit + flag updates

(define (unsigned8 x) (band #xFF x))
(define (unsigned4 x) (band #x0F x))

(define (add/flags! a b)
  (let* ((usum (+ (unsigned8 a) (unsigned8 b)))
         (rv (band #xFF usum)))
    (N/Z! rv)
    (C  (bit-set? usum 8))
    (DC (bit-set? (+ (unsigned4 a)
                     (unsigned4 b))
                  4))
    ; (OV ...) ;; haha FIXME!
    rv))
(define (N/Z! result)
  (N (bit-set? result 7))
  (Z (zero? result)))

  
(define-syntax-rule (define-opcodes opcodes ((name . args) . body) ...)
  (begin
    (begin (define (name . args) . body) ...)
    (define opcodes (make-hash `((name . ,name) ...)))))

;; FIXME: check flags updates for all
(define-opcodes opcodes
  ;; ((_nop arg) (void))  ;; Probably means we've hit a bug in the sim.
  
  ((bpc p rel) (bp C p rel))
  ((bpz p rel) (bp Z p rel))  ;; hangs it on synth code

  ((bra   rel)  (ipw-rel rel))
  ((rcall rel)  (push (ip)) (bra rel))
  
  ((_goto lo hi) (ipw lo hi))  
  ((_call s lo hi)
   (unless (zero? s) (raise 'call-shadow=1))
   (push (ip))
   (_goto lo hi))
  ((return s)
   (unless (zero? s) (raise 'call-shadow=1)) ;; shadow
   (ip (pop)))

  ((btfsp pol reg bit a)  ;; FIXME: check polarity
   (let ((v (load reg a)))
     (when (xor
            (bit->bool pol)
            (bit->bool (bitwise-and 1 (arithmetic-shift v (- bit)))))
       (ip (+ (ip) 2)))))
  
  ; movff ;; no STATUS
  ((movlw l) (wreg l)) ;; no STATUS
  ((movwf reg a) (store reg a (wreg))) ;; no STATUS
  ((movf reg d a)
   (let ((v (load reg a)))
     (when (zero? d) (wreg v)) ;; otherwise just flag effect
     (Z (zero? v))
     (N-from v)
     ))
  ((incf reg d a) (read-modify-write (lambda (x) (add/flags! x  1)) reg d a))
  ((decf reg d a) (read-modify-write (lambda (x) (add/flags! x -1)) reg d a))
  ((clrf reg a)   (store reg a 0) (Z #t))

  ((_lfsr f l h)  (fsr-set! f (lohi l h)))
  ((tblrd*+) (store #xF5 0 0)) ;; FIXME
  )

    


(define-struct ins-jit (op args ip+ words))

(define (trace! x) (trace (cons x (trace))))
;; (define (<< x) (arithmetic-shift x 1))

(define (last-trace [n 5])
  (for ((ip (reverse (take n (trace)))))
    (match (vector-ref (jit) (2/ ip))
      ((struct ins-jit (op args ip+ words))
       (print-dasm words ip+)))))


;; CORE:
;; - disassemble
;; - execute-next

;; Gather disassembler functions visible in this module namespace
;; (i.e. pic-specific opcodes imported from staapl/pic18/dasm)
(define-dasm-collection dasm-collection)
;; `default-dasm' contains a catch-all `dw' opcode.
(define dasm-collection+dw (append dasm-collection (list default-dasm)))
(define (dasm-ip)
  (let* ((here (ip))
         ;; Feed the dasm two words of context
         (w0 (flash-ref here #t))
         (w1 (flash-ref (+ 2 here) #t)))
    (car ;; only interested in first instruction
      (parameterize ((dasm-pcr-enable #f)) ;; turn off PC-relative to abs translation
        (ll->l
         (dasm-parse dasm-collection+dw
                     (list w0 w1)
                     here))))))
;; see tsee in tethered.rkt
(define (print-dasm words ip+)
  (let ((ip (- ip+ (<< (length words)))))
    (print-target-word
     (disassemble->word dasm-collection+dw
                        words
                        (2/ ip)
                        16
                        (lambda (addr) (format "~x" addr))
                        ))))
(define (execute-next)
  (let* ((ip-prev (ip))
         (jit-index (2/ ip-prev))
         (jitted (vector-ref (jit) jit-index)))
    (let-values
        (((op args)
          (if (not jitted)
              ;; Disassemble, lookup, 
              (let ((dasm (dasm-ip)))
                (match dasm
                  ((list (list-rest asm args) words addr)
                   (begin
                     (let ((ip-next (+ (ip) (* 2 (length words))))
                           (mnem (asm-name asm)))
                       (ip ip-next)
                       (let ((op (dict-ref opcodes mnem)))
                         (vector-set! (jit) jit-index
                                      (make-ins-jit op args ip-next words))
                         (values op args)
                         ))))))
              ;; Caching opcode lookup gives about an order of magnitude
              ;; better performance.  Not a luxury when running wait loops.
              (match jitted
                ((struct ins-jit (op args ip+ dasm))
                 (begin
                   (ip ip+)
                   (values op args)
                   )))
              )))
      ;; Keep instruction trace for debug.  Might want to limit the
      ;; size of this.
      (trace! ip-prev)
      (apply op args))))
      
(define (run)
  (execute-next)
  (run))





;; To make this more immediately useful, it might be good to focus on
;; running subroutines in isolation.  Booting a real world image is
;; going to have a lot of HW init that needs to be supported to be
;; able to get it to run correctly at all.


;; Testing
(define (test)
  (flash (load-flash "/home/tom/staapl/app/synth.sx"))
  (ip 0)
  (ram (make-vector #x1000 #f))
  (fsr (make-vector 3 #f))
  (jit (make-vector #x2000 #f))
  (stack (make-vector 31 #f)) ;; FIXME: implement this as 31-word vector + STKPTR reg
  (trace '())
  (run))


;; (dasm-ins '(#xD01F))
