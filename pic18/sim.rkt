#lang racket/base
(provide (all-defined-out)) ;; user will probably need to prefix
 
(require racket/control
         racket/promise
         racket/match
         racket/dict
         racket/pretty
         (except-in racket/bool true false)
         "sim-tools.rkt"     ;; register and other tools
         "../target/rep.rkt" ;; instruction->string
         "../tools.rkt"      ;; ll->l
         "../code.rkt"       ;; code->binary
         "../target.rkt"     ;; target-word-address
         "asm.rkt"           ;; pic18
         "../asm.rkt")       ;; generic

;; Is it just crazy midnight hacking to want to emulate the PIC18?
;; Not really.  There is no semantics to Staapl other than what the uC
;; core does with the binary compiler output, so a hackable emulator
;; is the closest thing.

;; Machine constants
(define reg-access     #x60) ;; #x80 FIXME: depends on core version
(define ram-nb-bytes   #x1000)
(define flash-nb-bytes #x8000)

(define d-stack #x80)
(define r-stack #xA0)

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

 ;; simulate EUSART input/output stream
 eusart-read  
 eusart-write
 )

;; Define bool flag parameters and 8bit register.
(flag-params
 (status (N OV Z DC C))
 (pir1   (SPPIF ADIF RCIF TXIF SSPIF CCP1IF TMR2IF TMR1IF))
 (pir2   (OSCFIF CMIF USBIF EEIF BCLIF HLVDIF TMR3IF CCP2IF))
 )


(define ui (make-uninitialized))

(define (make-stack) (make-vector 31 ui))
(define (make-fsr)   (make-vector 3  ui))
(define (make-ram)   (vector-memory (make-vector #x1000 ui)))
(define (make-jit)   (make-vector (2/ flash-nb-bytes) #f))
(define (make-flash) (make-vector flash-nb-bytes ui))

;; These can be initialized for global use.  Keep other params at #f
;; to force manual init.  Just clear jit buffer when loading code.
(trace '())
(jit (make-jit))

;; stack
(define (push x)
  (let* ((p (add1 (stkptr))))
    (stkptr (band #x1F p)) ;; FIXME
    (vector-set! (stack) p x)
    ))
(define (pop)
  (let* ((rv (tos-read)))
    (stkptr (band #x1F (sub1 (stkptr)))) ;; FIXME
    rv))
(define (tos-read [n #f])
  (let ((v (vector-ref (stack) (stkptr))))
    ;; (printf "tos ~s, stack ~s\n" v (stack))
    (if (uninitialized? v) v
        (if (not n)
            (band #x1FFFFF v)
            (band #xFF (>>> v (* n 8)))))))

(define (tos-write val [n #f])
  (if (not n)
      (vector-set! (stack) (stKptrx) val)
      (for/fold ((a 0))
          ((i '(0 1 2)))
        (let ((byte (if (= n i) val (tos-read i)))
              (shift (* i 8)))
          (bior a (<<< byte shift))))))
(define (tos-register n)
  (make-rw-register
   (lambda ()  (tos-read n))
   (lambda (v) (tos-write v n))))



  
;; ram
;; support raw vector and abstract memory
(define (ram-set! addr val)
  ((memory-write (ram)) addr val))

(define (ram-ref  addr)
  ((memory-read (ram)) addr))


;; flash

;; "Binary" files are (list-of (list-of addr (list-of byte)))
;; The way they come out of code->binary.
(define (load-binary filename)
  (read (open-input-file filename)))
;; Translate lists to vectors for faster access.
(define (binary->flash code-chunks)
  (apply vector
         (for/list ((chunk code-chunks))
           (list (list-ref chunk 0)
                 (apply vector (list-ref chunk 1))))))
  
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
   ;; Should be uninitialized but parser reads ahead.
   (if word
       #xFFFF
       #xFF)))

(define (next-word)
  (let ((w (flash-ref (ip) #t)))
    (ip (+ (ip) 2))
    w))






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
      ((register-read-modify-write (ab-register reg a)) fun)))

;; fsr
(define (fsr-set! f v) (vector-set! (fsr) f v))
(define (fsr-ref f)    (vector-ref  (fsr) f))
(define (fsr-update f upd)
  (lambda () (fsr-set! f (upd (fsr-ref f)))))

(define (fsr-lref f)
  (let ((v (fsr-ref f)))
    (if (uninitialized? v) v
        (band #xFF v))))

(define (fsr-href f)
  (let ((v (fsr-ref f)))
    (if (uninitialized? v) v
        (band #xFF (>>> v 8)))))

(define (fsr-lhset! f l h)
  (if (or (uninitialized? l)
          (uninitialized? h))
      (fsr-set! f (make-uninitialized))
      (fsr-set! f (lohi l h))))

(define (fsrl f)
  (make-rw-register
   (lambda ()  (fsr-lref f))
   (lambda (v) (fsr-lhset! f v (fsr-href f)))))
(define (fsrh f)
  (make-rw-register
   (lambda ()  (fsr-href f))
   (lambda (v) (fsr-lhset! f (fsr-lref f) v))))

(define (indirect f [pre void] [post void])
  (define (reg)      (data-register (fsr-ref f)))  ;; Abstract accessor to reg.
  (define (pp thunk) (pre) (let ((v (thunk))) (post) v))
  (define (read)     (pp (lambda () ((register-read (reg))))))
  (define (write v)  (pp (lambda () ((register-write (reg)) v))))
  (define (rmw fun)  (pp (lambda () ((register-read-modify-write (reg)) fun))))
  (make-register read write rmw))

(define (inc x) (band #xFFF (add1 x)))
(define (dec x) (band #xFFF (sub1 x)))

(define (preinc f)  (indirect f (fsr-update f inc) void))
(define (postdec f) (indirect f void (fsr-update f dec)))
(define (postinc f) (indirect f void (fsr-update f inc)))
(define (indf f)    (indirect f void void))


    
    



;; SFRs that behave as configuration (as opposed to I/O ports) can be
;; implemented simply as RAM read/write.
(define (sfr-ram addr)
  (cons addr
        (make-rw-register
         (lambda ()  (ram-ref addr))
         (lambda (v) (ram-set! addr v)))))


;; Peripherals
(define rcreg (make-r-register (lambda () ((eusart-read)))))
(define txreg (make-w-register (lambda (v) ((eusart-write) v))))


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
    (#xFFD . ,(tos-register 0))
    (#xFFE . ,(tos-register 1))
    (#xFFF . ,(tos-register 2))
    (#xF9E . ,pir1)
    (#xFA1 . ,pir2)
    (#xFAE . ,rcreg)
    (#xFAD . ,txreg)
    (#xFED . ,(postdec 0))
    (#xFEC . ,(preinc  0))
    (#xFE8 . ,(make-param-register wreg))
    (#xFE7 . ,(indf 1))
    (#xFE5 . ,(postdec 1))
    (#xFE4 . ,(preinc  1))
    (#xFDD . ,(postdec 2))
    (#xFDE . ,(postinc 2))
    (#xFDC . ,(preinc  2))
    (#xFDA . ,(fsrh 2))
    (#xFD9 . ,(fsrl 2))
    (#xFD8 . ,status)
    (#xF6d . ,(make-ni-register 'UCON))
    ))
(define (sfr reg)
  (dict-ref sfrs reg
            (lambda ()
              (error 'sfr-not-found "~x" reg))))



(define (ipw abs)      (ip (* 2 abs)))
(define (ipw-rel rel)  (ip (+ (ip) (* 2 rel))))

(define (bp flag p rel)
  (unless (not (xor (flag) (bit->bool p)))
    (ipw-rel rel)))

;; logic ops + set N/Z flags

(define (logic! op a b)
  (let ((rv (band #xFF (op a b))))
    (N/Z! rv)
    rv))

;; add + signed/unsigned flag updates
(define (add a b #:flags! [update-flags #f])
  (let* ((usum (+ a b))
         (carries (bxor usum (bxor a b))) ;; vector of internal carries
         (rv (band #xFF usum)))
    (when update-flags
      (N/Z! rv)
      (C  (bit-set? carries 8))
      (DC (bit-set? carries 4))
      ;; OV = negative result from positive operands or vicee versa. so
      ;; it is the nagation of the XOR of two input signs and output
      ;; sign.  According to wikipedia, often generated as the XOR of
      ;; carry into and out of sign bit.
      (OV (xor (bit-set? carries 8)
               (bit-set? carries 7))))
    rv))
(define (N/Z! v)
  (if (uninitialized? v)
      (begin (N v) (Z v))
      (begin
        (N (bit-set? v 7))
        (Z (zero? v)))))


(define (skip!)
  (ip (+ 2 (ip))))
  
(define-syntax-rule (define-opcodes opcodes ((name . args) . body) ...)
  (begin
    (begin (define (name . args) . body) ...)
    (define opcodes
      (make-parameter
       `((name . ,name) ...)))))

(define (rot8<<< byte bits #:flags! [set-flags #f])
  (let* ((x (band #xFF byte))
         (rv (band #xFF
                   (bior (>>> x (- 8 bits))
                         (<<< x bits)))))
    (when set-flags (N/Z! rv))
    rv))


;; FIXME: check flags updates for all
(define-opcodes opcodes
  ;; ((_nop arg) (void))  ;; Probably means we've hit a bug in the sim.

  ;; Note that most of the PIC bit mnemonics are replaced with a ___i
  ;; op that has an extra invert flag.
  
  ((bci i rel) (bp C i rel))
  ((bzi i rel) (bp Z i rel))  ;; hangs it on synth code
  ((bsfi p f b a)
   (let ((v (load f a)))
     (when (bit->bool (bxor p (>>> v b)))
       (skip!))))

  ((bra   rel)  (ipw-rel rel))
  ((rcall rel)  (push (ip)) (bra rel))
  
  ((goto lo hi) (ipw (lohi lo hi)))
  ((call s lo hi)
   (unless (zero? s) (raise 'call-shadow=1))
   (push (ip))
   (goto lo hi))
  ((return s)
   (unless (zero? s) (raise 'call-shadow=1)) ;; shadow
   (ip (pop)))

  ((retfie s)
   ;; (printf "WARNING: retfie as return\n")
   (return s))

  ((btfssi inv reg bit a)
   (let ((v (load reg a)))
     (when (xor
            (bit->bool inv)  ;; invert polarity
            (bit-set? v bit))
       (skip!))))

  ((decfsnz f d a)
   (read-modify-write
    (lambda (x)
      (let ((v (add x  1 #:flags! #f)))
        (when (zero? v) (skip!))))
    f d a))
  
  ((movlw l) (wreg l)) ;; no STATUS
  ((movwf reg a) (store reg a (wreg))) ;; no STATUS
  ((movf reg d a)
   (let ((v (load reg a)))
     (when (zero? d) (wreg v)) ;; otherwise just flag effect
     (N/Z! v)
     ))
  ((movff src dst) ;; no STATUS
   (let ((rsrc (data-register src))
         (rdst (data-register dst)))
     ((register-write rdst) ((register-read rsrc)))))
  
  ((incf   f d a) (read-modify-write (lambda (x) (add x  1         #:flags! #t)) f d a))
  ((decf   f d a) (read-modify-write (lambda (x) (add x -1         #:flags! #t)) f d a))
  ((addwf  f d a) (read-modify-write (lambda (x) (add x (wreg)     #:flags! #t)) f d a))
  ((subfwb f d a) (read-modify-write (lambda (x) (add (wreg) (- x) #:flags! #t)) f d a))

  ((swapf  f d a) (read-modify-write (lambda (x) (rot8<<< x 4 #:flags! #f)) f d a))
  ((rlncf  f d a) (read-modify-write (lambda (x) (rot8<<< x 1 #:flags! #t)) f d a))
  ((rrncf  f d a) (read-modify-write (lambda (x) (rot8<<< x 7 #:flags! #t)) f d a))
  
  ((addlw l)      (wreg (add (wreg) l #:flags! #t)))
  ((andlw l)      (wreg (logic! band (wreg) l)))
  
  ((clrf reg a)   (store reg a 0) (Z #t))

  ((lfsr f l h)   (fsr-set! f (lohi l h)))
  ((tblrd*+) (store #xF5 0 0)) ;; FIXME
  )

    

(define-struct ins-jit (op args ip+ words))

(define (trace! x)
  (let ((t (trace)))
    (if (procedure? t) ;; allow for abstract trace
        (t x)
        (trace (cons x t)))))

(define (print-trace-item addr)
  (if (number? addr)
      (match (vector-ref (jit) (2/ addr))
        ((struct ins-jit (op args ip+ words))
         (print-dasm words ip+)))
      ;; Allow user to add tags to trace.
      (pretty-print addr)))

(define (print-trace [n #f])
  (unless (list? (trace))
    (error 'trace-type-error "~s" (trace)))
  (let* ((full (trace))
         (chunk (if n (take n (trace)) (trace)))
         (offset (- (length full) (length chunk))))
    (for ((i (in-naturals))
          (addr (reverse chunk)))
      (printf "~s\t" (+ offset i))
      (print-trace-item addr))))


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
                       (let ((op (dict-ref (opcodes) mnem)))
                         (vector-set! (jit) jit-index
                                      (make-ins-jit op args ip-next words))
                         (values op args)
                         ))))))
              ;; Caching opcodes solves two problems: it's a lot
              ;; faster for busy loops, and it avoids worry about
              ;; optimizing the dasm :)
              (match jitted
                ((struct ins-jit (op args ip+ _))
                 (begin
                   (ip ip+)
                   (values op args)
                   )))
              )))
      (trace! ip-prev)
      (apply op args))))
      
(define (run)
  (execute-next)
  (run))


(define-syntax-rule (while cond . body)
  (let next ()
    (when cond (begin . body) (next))))

;; This doesn't set up context other than ip.
(define (call-word addr)
  (let ((addr (if (number? addr) addr
                  (* 2 (target-word-address addr)))))
    (push (make-uninitialized)) ;; termination mark
    (ip addr)
    (while (not (uninitialized? (ip)))
      (execute-next))))

(define (with-local-context thunk)
  (parameterize ((ip 0)
                 (wreg 0)
                 (stkptr 0)
                 (stack (make-stack))
                 (ram (make-ram))
                 (fsr (make-fsr)))
    (thunk)))



  




;; Provide a somewhat sane initial state to start running naked
;; functions.  Alternatively, start the kernel at the reset vector and
;; let it do its own init, though that might need more peripheral
;; support.
(define (reg-defaults!)
    (ip 0)
    (wreg 0)
    (stkptr 0)
    (stack (make-stack))
    (ram (make-ram))
    (flash (make-flash))
    (fsr (vector d-stack r-stack 0))
    ((register-write status) 0)
    ((register-write pir1) 0)
    ((register-write pir2) 0)
    )
    
(define (flash-from-code!)
  (flash (binary->flash (code->binary))))




;; Testing
#;(define (test-synth.sx)
(flash (binary->flash (load-binary "/home/tom/staapl/app/synth.sx")))
(jit (make-jit)))

#;(define (test1)
(test-synth.sx)
(reg-defaults!)
(run))


#;(define (test2)
(test-synth.sx)
(trace '())
(call-word #x03A8))
