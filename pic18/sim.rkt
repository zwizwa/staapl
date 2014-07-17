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



(define-syntax-rule (params p ...)
  (begin (define p (make-parameter (void))) ...))
    

(params
 flash ip wreg ram stack fsr Z C DC N OV)

(define (fsr-set! f v) (vector-set! (fsr) f v))
(define (fsr-ref f)    (vector-ref  (fsr) f))

(define (push x) (stack (cons x (stack))))
(define (pop) (let ((s (stack)))
                (stack (cdr s))
                (car s)))
  

(define (ram-set! addr val) (vector-set! (ram) addr val))
(define (ram-ref  addr)     (vector-ref  (ram) addr))

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


;; Disassemble

;; This non-hygienic form collects all disassembler functions visible
;; in this module namespace.  This is used during live interaction.
(define-dasm-collection dasm-collection)
(define dasm-collection+dw
  (append dasm-collection (list default-dasm)))
(define (dasm-ip)
  (let* ((here (ip))
         ;; Feed the dasm two words of context
         (w0 (flash-ref here #t))
         (w1 (flash-ref (+ 2 here) #t)))
    (car ;; only interested in first instruction
     (ll->l
      (dasm-parse dasm-collection+dw
                  (list w0 w1)
                  here)))))

(define-syntax-rule (define-opcodes opcodes ((name . args) . body) ...)
  (define opcodes
    `((name . ,(lambda args . body)) ...)))

;; Generic FSR access
(define (indirect f [pre void] [post void])
  (list (lambda ()  (pre) (let ((rv (ram-ref (fsr-ref f)))) (post) rv))
        (lambda (v) (pre) (ram-set! (fsr-ref f) v) (post))
        ))
(define (fsr-update f upd)
  (lambda () (fsr-set! f (upd (fsr-ref f)))))
(define (preinc f)  (indirect f (fsr-update f add1) void))
(define (postdec f) (indirect f void (fsr-update f sub1)))

(define (sfr-fixme addr)
  (define (dummy . args)
    (printf "-- sfr-fixme ~s\n" addr))
  (list addr dummy dummy))

;; Most SFR are implemented as RAM read/write.
(define (sfr-ram addr)
  (list
   addr
   (lambda ()  (ram-ref addr))
   (lambda (v) (ram-set! addr v))))

(define sfrs
  `(,(sfr-ram #xFC) ;; STKPTR
    ,(sfr-ram #x92) ;; TRISA
    ,(sfr-ram #x93) ;; TRISB
    ,(sfr-ram #x94) ;; TRISC
    ,(sfr-ram #x95) ;; TRISD
    ,(sfr-ram #x96) ;; TRISE
    ,(sfr-ram #x89) ;; LATA
    ,(sfr-ram #x8A) ;; LATB
    ,(sfr-ram #x8B) ;; LATC
    ,(sfr-ram #x8C) ;; LATD
    ,(sfr-ram #x8D) ;; LATE
    ,(sfr-ram #xF6) ;; TBLPTRL
    ,(sfr-ram #xF5) ;; TABLAT
    (#xE8 ,wreg ,wreg)
    (#xED . ,(postdec 0))
    (#xEC . ,(preinc  0))
    (#xE5 . ,(postdec 1))
    (#xE4 . ,(preinc  1))
    (#xDD . ,(postdec 2))
    (#xDC . ,(preinc  2))
    ))

(define (reg-read  r)   ((car r)))
(define (reg-write r v) ((cadr r) v))

(define (lohi lo hi) (+ lo (* #x100 hi)))
(define (ipw lo [hi 0])  (ip (* 2 (lohi lo hi))))

;; 8-bit operand read/write
(define (store reg a v)
  (unless (zero? a) (raise 'banked-write))
  (if (>= reg #x80)
      (reg-write (dict-ref sfrs reg) v)
      (ram-set! (+ #xF00 reg) v)))
  
(define (load reg a)
  (unless (zero? a) (raise 'banked-read))
  (if (>= reg #x80)
      (reg-read (dict-ref sfrs reg))
      (ram-ref (+ #xF00 reg))))

(define (N-from v)
  (N (not (zero? (bitwise-and #x80 v)))))


;; FIXME: word/byte addresses and rel/abs is a bit messed up in the dasm.

(define-opcodes opcodes
  ;; ((_nop arg) (void))  ;; Probably means we've hit a bug in the sim.
  
  ((bpc p dst) ;; FIXME: probably wrong
   (unless (xor (C) (bit->bool p))
     (ip dst)))

  ((bra addr)    (ipw addr))
  ((rcall addr)  (ip addr) (push (ip)))
  
  ((_goto lo hi) (ipw lo hi))  
  ((_call s lo hi)
   (unless (zero? s) (raise 'call-shadow=1))
   (push (ip))
   (ipw lo hi))
  ((return s)
   (unless (zero? s) (raise 'call-shadow=1)) ;; shadow
   (ip (pop)))
          
  ; movff ;; no STATUS
  ((movlw l) (wreg l)) ;; no STATUS
  ((movwf reg a) (store reg a (wreg))) ;; no STATUS
  ((movf reg a d)
   (unless (zero? d) (raise 'movf-d=1)) ;; d=1 just sets N,Z flags
   (let ((v (load reg a)))
     (wreg v)
     (Z (zero? v))
     (N-from v)
     ))
  ((incf reg a d)
   (unless (zero? d) (raise 'movf-d=1)) ;; d=1 just sets N,Z flags
   (let ((v (bitwise-and #xFF (+ 1 (load reg a)))))
     (store reg a v)
     (C (zero? v))
     (Z (zero? v))
     (N-from v)
     ;; DC
     ;; OV
     ))
  ((clrf reg a) (store reg a 0)) ;; FIXME: Z
  ((_lfsr f l h) (fsr-set! f (lohi l h)))

  ((tblrd*+) (store #xF5 0 0)) ;; FIXME
  )
(define (bit->bool bit) (not (zero? bit)))
   
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
  (flash (load-flash "/home/tom/staapl/app/test.sx"))
  (ip 0)
  (wreg 0)
  (ram (make-vector #x1000 #f))
  (fsr (make-vector 3 #f))
  (stack '())
  (run))


;; (dasm-ins '(#xD01F))
