#lang scheme/base

;; TETHERED INTERPRETER / COMPILER

;; FIXME: This is highly PIC/8bit-specific. Porting to a new
;; architecture is going to involve some disentangling, but the code
;; here is quite straightforward bit and buffer twiddling.

(provide (all-defined-out))

(require
 "../op.ss"
 "../tools.ss"
 "../ns.ss"
 "../scat.ss"
 "../target.ss"
 "../asm.ss"
 "../coma/macro.ss"
 "reflection.ss"
 "../comp/state.ss" ;; state for macro eval
 "../code.ss"

 scheme/system
 scheme/match)


;; Commands for interaction mode.
(define (stat) ((comm-stat)))
(define (scan)
  (let ((targets (target-count)))
    (printf "Found ~a target(s).\n" targets)))
(define (cold)
  ((comm-reset))
  (msleep 100))





;; CHIP SPECIFIC CONFIG
(define datastack (make-parameter #f))

(define (pic18-datastack)
  (list
   (macro-constant 'stack-data)
   (macro-constant 'stack-data-ptr)))
(datastack pic18-datastack)


;; Debug
(define-syntax-rule (d: fmt . e)
  (let ((val (begin . e)))
    ; (printf fmt val)
    val))


;; CONNECTION

(define comm-in    (make-parameter (lambda _ (error 'no-input-connected))))
(define comm-out   (make-parameter (lambda _ (error 'no-output-connected))))
(define comm-close (make-parameter void))

(define comm-reset (make-parameter (lambda () (display "Reset not implemented.\n"))))
(define comm-poll  (make-parameter (lambda () (target-sync) #t)))
(define comm-stat  (make-parameter (lambda () (display "No stat available.\n"))))

(define comm-reconnect (make-parameter (lambda () (display "Reconnect not implemented.\n"))))
(define comm-on        (make-parameter (lambda () (display "Target power-on not implemented.\n"))))
(define comm-off       (make-parameter (lambda () (display "Target power-off not implemented.\n"))))

(define comm-timeout 'timeout)
(define (comm-timeout? ex) (eq? comm-timeout ex))


(define (on) ((comm-on)))
(define (off) ((comm-off)))
(define (reconnect) ((comm-reconnect)))



;; Daisy-chain serial.  Abstract this.

(define target-id (make-parameter 0)) ;; Current receiver
(define (target! id)
  (let ((tc (target-count)))
    (when (or (< id 0) (>= id tc))
      (error 'invalid-target-id "~a" id)))
  (target-id id))

;; This sends a dummy command through the ring.  Each client will
;; decrement the address and forward.
(define (target-count)
  (let ((max-id 255))
    (out/b max-id)
    (out/b 0)
    (let* ((id (in/b)))
      (in/b) ;; size
      (- max-id id))))





;; (comm-simulator)

;; Shortcut access.

;; (define (io-debug x) (x))
;; (define default-portspec '("/dev/ttyUSB0" 9600))
;; (define (io-debug x)
;;   (with-io-device default-portspec x))
;; (define-syntax-rule (io> . expr)
;;   (io-debug (lambda () . expr)))



(define (in/b)       ((comm-in)))
(define (out/b byte) ((comm-out) byte))

;; word/byte lists
(define (bytes->words lst) (join-nibble-list  lst 0 8))
(define (words->bytes lst) (split-nibble-list lst 0 8))

;; values
(define (void/values lst)
  (if (null? lst) (void) (apply values lst)))


;; All messages are prepended with address + size to make them
;; self-delimited.  This is in order to facilitate routing without the
;; need for interpretation.

;; Asynchronous send / receive.  All communication code will be built
;; on top of this.

(define (target-send/b . bytes)  (let ((len (length bytes)))
    (when (> len 255) (error 'message-too-long))
    (out/b (target-id))
    (out/b len)
    (for ((b bytes)) (out/b (int8 b)))))
(define (target-send/w id . words)
  (apply target-send/b id (words->bytes words)))

;; Receive returns lists.
(define (target-receive+id/b)
  (define (no-answer? id) (< id 128))
  (let* ((id (in/b))
         (size (in/b)))
    ;; (printf "id:~s size:~s\n" id size)
    (let ((payload (for/list ((i (in-range size))) (in/b))))
      (when (no-answer? id)
        (target! 0)
        (error 'bad-reply "id:~a msg:~a" id payload))
      (values payload id))))


;; RPC returns values.
(define (target-rpc/b . args)
  (apply target-send/b args)
  (void/values (target-service/b)))
(define (target-rpc/w . args)
  (apply target-send/w args)
  (void/values (target-service/w)))

;; Last arg is list.
(define (target-rpc/b* . args)
  (apply target-rpc/b (apply list* args)))


;; BOOT MONITOR COMMAND P  TOCOL


;; ASYNC COMMANDS

(define (tnop)           (target-send/b))         ;; nop = empty message
(define (tstart/b addr)  (target-send/w 3 addr)) 

;; (*) The command 'start in live/commands.ss uses this. Because the
;; console closes the serial port after every command, running 'start
;; on a word that DOES return an ack byte is ok: the ack byte is just
;; ignored.


;; RPC COMMANDS


(define (>t* . vals)     (target-rpc/b* 1 (length vals) vals))
(define (t>* n)          (target-rpc/b 2 n))
(define (~texec/b addr)  (target-send/w 3 addr))
(define (a! addr)        (target-rpc/w 4 addr))
(define (f! addr)        (target-rpc/w 5 addr))
(define (target-sync)    (target-rpc/b 0))



;; Interpreter extension.  The first two bytes are the address of the
;; extension.
(define (intr . args)    (apply target-rpc/b (cons 7 args)))



;; RPC LIST COMMANDS

;; Receive data packets as lists.  These postincrement the a and f
;; pointers, and are limited to 255 byte lists.  Use the composite
;; commands instead.
(define (~a>/b n)        (target-send/b 8 n) (target-service/b))
(define (~f>/b n)        (target-send/b 9 n) (target-service/b))
(define (~>a/b lst)      (apply target-rpc/b 10 (length lst) lst))
(define (~>f/b lst)      (apply target-rpc/b 11 (length lst) lst))






;; COMPOSITE COMMANDS


;; Written in terms of the low level commands defined above.

(define (chunked-receive command addr!)
  (lambda (total-size [at #f])
    (when at (addr! at))
    (if (<= total-size 0)
        '()
        (flatten
         (map command
              (chunk-size-list
               total-size
               #x20 ;; (*)
               ))))))

;; (*) was #x80 for allowing header wrapping, but this didn't work for
;; daisy-chained operation, so lowered to #x20 

;; PK2 also has a limit of about 55-odd bytes for receive, 29 for send.

(define a>/b  (chunked-receive ~a>/b a!))
(define f>/b  (chunked-receive ~f>/b f!))

;; Fixed-arity for automatic lifting as live: words.
(define (>t val)         (>t* val))
(define (t>)             (t>* 1))



;; FIXME: implement chunked message send correctly.
(define (chunked-send command addr!)
  (lambda (lst [at #f])
    (when at (addr! at))
    (unless (zero? (length lst))
      (command lst))))

(define >a/b  (chunked-send ~>a/b a!))
(define >f/b  (chunked-send ~>f/b f!))

(define (f>/w n) (bytes->words (f>/b (<<< n 1))))

;; Target fetch/store + double.
(define (t@  addr)     (a! addr) (car (a>/b 1)))
(define (t!  val addr) (a! addr) (>a/b (list val)))
(define (_t@ addr)     (a! addr) (car (bytes->words (a>/b 2))))
(define (_t! val addr) (a! addr) (>a/b (words->bytes (list val))))

(define (stackbottom) #x80) ;; FIXME: query dictionary
(define (stackptr)    (texec/sym 'stackptr) (_t>))
(define (stacksize) (- (stackptr) (stackbottom)))

(define (a@) (texec/sym 'a@@) (_t>))
(define (f@) (texec/sym 'f@@) (_t>))

(define (erase)    (texec/sym 'ferase)) ;; erase current flash block
(define (program)  (texec/sym 'fprog))  ;; program current flash line

(define (ts-copy)
  (reverse (a>/b (stacksize)
                 (+ 1 (stackbottom)))))
(define (_ts-copy)
  (join-nibble-list (ts-copy) 8 0))




;; Run the interpreter for target->host commands.  This is called
;; whenever control is passed to the target.

(define (printable n)
  (if (or (< n 32)
          (> n 126))
      46
      n))

(define (trace-hook addr)
  (printf "~x: " addr) (ts)
  ;(abd 0)
  )

(define (hexdump reply [ascii #f])
  (for ((byte reply))
    (printf "~a " (hex->string 2 byte)))
  ;; Row formatting optional
  (when ascii
    (display " ")
    (display (list->bytes
              (map printable reply)))
    (newline)))


;; Some useful host RPC commands.

;; These make only sense to be called from target.  Note that Scheme
;; functions are automatically lifted to scat words, and target will
;; push a byte buffer to the host stack before these words are
;; executed.
(define (pb  bytes) (display (list->bytes bytes)))
(define (ph  bytes) (hexdump bytes))
(define (pha bytes) (hexdump bytes #t))
(define (trc bytes) (trace-hook (car (bytes->words bytes))))

;; Dump RAM memory region, word size arguments.

(define (_ad bytes)
  (apply (hex-dump-bytes a! a>/b)
         (bytes->words bytes)))

(define (_fd bytes)
  (apply (hex-dump-bytes f! f>/b)
         (bytes->words bytes)))

(define (texec/sym sym [texec texec/b])
  (let ((addr (target-find-realm sym 'code)))
    (unless addr (error 'texec/sym "~s" sym))
    (texec addr)))



;; A host stack accessible by the target for target->host RPC calls.
;; Note that this is isolated from all other (temporary) stacks used
;; by console commands.
(define host-stack (make-parameter (state:stack)))
(define (update-host-stack fn)
  (let ((s (host-stack)))
    (let ((s_ (fn s)))
      (host-stack s_))))
(define (hs) (stack-print (stack-list (host-stack))))



;; Evaluate a single symbolic host command.   

(define (host-rpc-cmd reply)
  (update-host-stack (eval `(live: ,(list->symbol reply)))))

(define (host-rpc id msg)
  (case id
    ((0) (update-host-stack (scat: ',msg))) ;; push byte packet
    ((1) (host-rpc-cmd msg)) ;; execute symbolic command
    ;; Optimization: while redundant, these slots could be used as an
    ;; optimization to avoid multiple packed round-trips.
    (else
     (error 'host-rpc "~s ~s" id msg))))

   
(define (target-service/w)
  (bytes->words (target-service/b)))

(define list->symbol
  (compose string->symbol
           bytes->string/utf-8
           list->bytes))

;; Transfer control to target.


(define (target-service/b)
  (let again ()
    (let-values (((msg id) (target-receive+id/b)))
      (case id
        ;; Target reply: return to caller.
        ((#xFF)
         msg)

        ;; Handle target RPC and continue listening.
        ((#xFE)
         (match msg
                ((list-rest id payload)
                 ;; On a target->host RPC call, target falls into
                 ;; interpreter so we can perform multiple entry.
                 ;; When done, we break that loop and target continues
                 ;; execution.  Target saves a & f so host can
                 ;; clobber.
                 (host-rpc id payload)
                 (texec/sym 'continue ~texec/b)
                 (again))))

        ;; Unknown address
        (else
         (error 'target-service/b "~s ~s" id msg))))))


(define (texec/b addr)
  (~texec/b addr)
  (target-service/b)
  (void))




;; Initial state for macro evaluation.
(define (init-state [lst '()])
  (state:compiler lst))


;; Macros that perform compile time computations can be simulated at
;; the console using the current run-time stack as an input.  This
;; uses a "lazy stack" to avoid having to tunnel data back and forth
;; all the time.  (Communication to the device might be slow, but host
;; CPU time is free.)

;; Simulate a macro by wrapping lazy pops in target values.
(define (tsim coma)
  (define (eval-macro m lst)
    (state->code (m (init-state lst))))
  (let* ((lp (pop->lp (stacksize) t>))
         (stack-in (lp->lazy-stack lp))
         (stack-out (eval-macro coma stack-in)))
    ;; Skip code that has remained the same.
    (let-values (((in out)
                  (diff-lists (reverse stack-in) ;; code stack -> sequential code
                              (reverse stack-out))))
    
      ;; Dummy-interpret to force all values in the remaining output
      ;; _and_ input for the side effect of popping the stack.
      (interpret-cw/qw void void out)
      (interpret-cw/qw void void in)

      ;; Check which values were actually used, and take code from there.
      (let* ((used (lp-have lp))
             (not-used (- (length in) used))
             (nb-ins (- (length out) not-used)))
        (interpret-cw/qw
         texec/b >t  ;; proper interpret
         (reverse
          (take nb-ins stack-out)))))))




(define-struct lp (vector have pop!) #:mutable)

(define (pop->lp n pop!)
  (make-lp (make-vector n) 0 pop!))

(define (lp-ref lp i)
  (define v (lp-vector lp))
  (define (pop!)
    (let ((have (lp-have lp)))
      ;; (printf "popping ~a\n" have)
      (vector-set! v have ((lp-pop! lp)))
      (set-lp-have! lp (add1 have))))
  ;; (printf "deref ~a\n" i)
  (when (>= i (vector-length v))
    (error 'lazy-pop-underflow))
  (let next ()
    (if (< i (lp-have lp))
        (vector-ref v i)
        (begin (pop!) (next)))))

(define (lp->lazy-stack lp)
  (for/list ((i (in-range (vector-length (lp-vector lp)))))
    (op: qw (make-target-value
             (lambda () (lp-ref lp i))
             'lazy-pop))))

(define (interpret-cw/qw _cw _qw code)
  (define *stack* '())
  (define num target-value->number)
  (for ((ins code))
    ;; (printf "interpret ~a\n" ins)
    (match ins
      ([list (? qw?) n] (_qw (num n)))
      ([list (? cw?) a] (_cw (target-byte-addr (num a) 'code)))
      ([list-rest opc _]
       (error 'cannot-simulate-opcode "~a\n~a"
              (asm-name opc)
              (reverse code))))))








;; Double stack.
(define (_>t val) (apply >t* (words->bytes (list val))))
(define (_t>)     (let-values (((hi lo) (t>* 2)))
                    (car (bytes->words (list lo hi)))))


;; Block access

;; FIXME: PIC18 specific.
;; Flash blocks: 64 bytes (erase unit).
;; RAM blocks: 16 bytes (so 8 bits spans the 4KB RAM address space)

(define a-block-size (make-parameter 16))
(define f-block-size (make-parameter 64))

(define (check-block)
  (texec/sym 'chkblk)
  (t>))


(define (bf! n) (f! (* (f-block-size) n)))
(define (ba! n) (a! (* (a-block-size) n)))
(define (free-block? b)
  (bf! b)
  (= #xff (check-block)))

;; To minimise mistakes, erase and program will all set f.  This as
;; opposed to the readout, which uses words relative to current
;; position.

(define (erase-block b) 
  ;; (printf "erase block ~a\n" b)
  (bf! b) (erase))

(define (erase-blocks b n)
  (unless (zero? n)
    (erase-block b)
    (erase-blocks (+ b 1) (- n 1))))

(define (erase-from-block b)
  (define erasing #f)
  ;; (printf "erase from block ~a\n" b)
  (let next ((b b))
    (if (free-block? b)
        (when erasing
          (printf "memory clear.\n"))
        (begin
          (unless erasing
            (printf "erasing blocks: ")
            (set! erasing #t))
          (printf "~s " b)
          (flush-output)
          (erase-block b)
          (next (add1 b))))))

(define (erase-from/w addr)
  (erase-from-block
   (ceiling-block addr 32))) ;; 32 words in a block


(define (upload-bytes-line org bytes [bits 3])
  (define n (<<< 1 bits))
  (unless (= n (length bytes))
    (error 'non-normalized-line "~s" bytes))

  (or (fast-prog org bytes)
      (slow-prog org bytes)
      ))

(define (slow-prog org bytes)
  (target-sync) ;; make sure target is live
  (>f/b bytes org)
  (program))


;; If the one-shot fast programming code is defined on target it can
;; be used instead of the standard approach which performs several
;; handshakes.  That approach is slow across the USB delay for PK2.

(define (fast-prog org bytes)
  (let ((intr.fast-prog (target-find-realm 'intr.fast-prog 'code)))
    (and intr.fast-prog
         (apply intr
                (append (words->bytes `(,intr.fast-prog ,org))
                        bytes)))))


;; Upload line-aligned bytes obtained from flattened code chunks.
;; Default is from PIC18, which uses 8 bytes per write line.
;; (require scheme/pretty)
(define (upload-bytes bin [align-bits 3])
  ;; (pretty-print bin)
  (for ((chunk (bin-flatten bin)))
    (for (((org line) (in-binchunk/lines chunk align-bits -1)))
       (display ".")
       (upload-bytes-line org line align-bits))))


;; PRINTING

(define printf-stack stack-print)

(define (psu lst)  (printf-stack lst " ~s"))
(define (psx lst)  (printf-stack (map byte->string lst) " ~a"))
(define (pss lst)  (printf-stack (map (sign-extender 8) lst) " ~s"))
(define (_psx lst) (printf-stack (map word->string lst) " ~a"))
(define (_pss lst) (printf-stack (map (sign-extender 16) lst) " ~s"))

;; Target stack printing.
(define (ts)  (psu (ts-copy)))
(define (tsx) (psx (ts-copy)))
(define (tss) (psu (ts-copy)))

(define (_ts)  (psu (_ts-copy)))
(define (_tsx) (_psx (_ts-copy)))
(define (_tss) (_pss (_ts-copy)))



;; Print a memory map of the first n kb.  Useful for getting a general
;; idea of chip content.
(define (kb n)
  (define (current-block)
    (if (= #xff (check-block)) ". " "x "))
  (define (print-line)
    (printf
     "~a\n"
     (apply
      string-append
      (for/list ((i (in-range 8))) (current-block)))))

  ;; Use the fast chkblk extension if available which uses a single
  ;; command per 512 bytes to cut down on handshake delays.
  (define (print-line-fast)
    (and (target-find-realm 'chkblk8 'code)
         (let ((mask (begin (texec/sym 'chkblk8) (t>))))
           (let print-bits ((n 8)
                            (bits mask))
             (if (zero? n)
                 (newline)
                 (begin
                   (display (if (= #x80 (band bits #x80)) ". " "x "))
                   (print-bits (sub1 n) (<<< bits 1))))))))

  (bf! 0)
  (let ((lines (* 2 n)))
    (for ((i (in-range lines)))
      (or (print-line-fast)
          (print-line)))))

;; Read raw bytes from input and print.  See tools/print.ss for args.
(define (hex-dump sequence . args)
  (for ((s sequence)
        (p (apply in-hex-printer args)))
    (p s)))

(define (hex-dump-bytes start read)
  (lambda (size addr)
    (start addr)
    (hex-dump (in-list (read size))
              addr 3 2 8)))

(define (abd b)
  (let ((bs (a-block-size)))
    (ba! b)
    (hex-dump (in-list (a>/b bs))
              (* bs b) 3 2 8)))
(define (fbd b)
  (let ((bs (f-block-size)))
    (bf! b)
    (hex-dump (in-list (f>/w (/ bs 2)))
              (* bs b) 4 4 4)))


;; Disassembly.

(define (dasm-resolve addr)
  (let ((rec (code-resolve addr 'code)))
    (or (let ((name (and rec (car rec))))
          name)
        addr)))

(define (dasm-dont-resolve addr)
  (format "~x" addr))


;; Raw disassembler.  Use sea / print-tword instead.
(define (tsee word [n #x10])
  (define addr
    (or
     (cond ((number? word) word)
           ((symbol? word) (target-find-code word))
           (else #f))
     (error 'not-found "~s" word)))
  (define dict (code-labels))
  (f! addr)
  (print-target-word
   (disassemble->word
    (eval 'dasm-collection) ;; dasm-list
    (f>/w n) ;; bin
    (>>> addr 1) ;; addr
    16 ;; wordsize
    dasm-dont-resolve ;; resolver
    ;; dasm-resolve
    )))

(define (bd block)
  (tsee (* 64 block) 32))

(define (print-wlist lst)
  (for ((w lst))
     (printf "~a " (symbol->string w)))
  (newline))

;; All words are accessible through macros.
(define (pmacros)   (print-wlist (ns-mapped-symbols '(macro))))
(define (pcommands) (print-wlist (ns-mapped-symbols '(target))))
(define (pwords)    (print-wlist
                    (filter (lambda (l)
                              (not (eq? #\. (car (string->list (symbol->string l))))))
                            (map car (code-labels)))))

;; Empty clears the target from the 'code pointer onwards.  This is
;; executed on startup to make sure the target is ready for upload.
(define (clear-flash [bits 5]) ;; 2^5 words in a block
  (define (get x) (cadr (assq x (code-pointers))))
  (let ((code (bit-ceil (get 'code) bits))  ;; (*)
        (data (get 'data)))
    (code-pointers-set! `((code ,code)
                          (data ,data)))
  
    (erase-from-block (>>> code bits))))
;; (*) We can only start compiling at the next block-erase boundary.



;; INCREMENTAL UPLOAD
(define *debug* #f)
(define (debug-on)  (set! *debug* #t))
(define (debug-off) (set! *debug* #f))
(define (commit [bin (code->binary)])
  (unless (null? bin)
    (when *debug* (code-print))
    (upload-bytes bin)
    (code-clear!)))


(define (target-sync/timeout seconds)
  (let* ((thread (thread target-sync))
         (ev (sync/timeout seconds thread)))
    (kill-thread thread)
    (if ev #t #f)))


(define (OK)
  (define (sync)
    (and ((comm-poll))
         ; (target-sync)  ;; extra handshake?
         )) 
  (code-clear!)     ;; don't keep stuff around
  (display (if (sync)
               "OK\n"
               "BUSY\n")))

;; PIC18 specific
(define (access-bank x)
  (let ((x (band x #xFF)))
    (if (zero? (band x #x80))
        x
        (bior #xF80 x))))

(define (tfbuffer addr)
  (let ((n (car (f>/b 1 addr))))
    (f>/b n (+ addr 1))))

(define (tfstring addr)
  (list->bytes (tfbuffer addr)))

(define (clear-ram-block b [val 0])
  (>a/b (build-list 64 (lambda _ val))
        (* 64 b)))



;; Interactive init code.


