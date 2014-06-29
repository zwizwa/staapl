#lang racket/base

(require
 "mem.ss"
 "eforth-tools.ss"
 racket/control
 (for-syntax racket/base))

;; Dr. C.H. Ting's eForth, a highly portable forth, written in terms
;; of the primitives:

;;   BYE ?RX TX! !IO doLIT doLIST EXIT EXECUTE next ?branch branch ! @
;;   C! C@ RP@ RP! R> R@ >R SP@ SP! DROP DUP SWAP OVER 0< AND OR XOR
;;   UM+ $NEXT D$ $USER $COLON $CODE


;; Completely functional implementation is difficult due to the memory
;; array nature of Forth, so we use an abstract array in terms of mem@
;; and mem! (see "mem.ss" for default), which can be implemented
;; functionally if necessary.  It would be interesting to see how much
;; of the indirection can be optimized away later.

;; Step 1: implement the primitives in Scheme.  This implementation
;;         will serve as a reflective compiler in Staapl.

;; Step 2: bootstrap the Forth code.

;; Step 3: find a translation to Staapl primitives for the 18F for
;;         running part of the machine on the 18F.



;; Stacks are impemented in-memory, not as abstract entities.  This is
;; closest to real implementation.

(define *SP* #x0000)  ;; Stack Pointer
(define *RP* #x0001)  ;; Return/Retain Stack Pointer
(define *IP* #x0002)  ;; Interpreter Pointer

(define (stack-push sp-addr val)
  (notrace
   (let ((sp (sub1 (mem@ sp-addr))))
     (mem! sp val)
     (mem! sp-addr sp))))
(define (stack-pop sp-addr)
  (notrace
   (let* ((sp (mem@ sp-addr))
          (val (mem@ sp)))
     (mem! sp-addr (add1 sp))
     val)))

;; Parameter access.
(define (lit . ns) (for ((n ns)) (stack-push *SP* n)))
(define (pop)   (stack-pop *SP*))
(define (top)   (notrace (mem@ (mem@ *SP*))))
(define-syntax locals
  (lambda (stx)
    (syntax-case stx ()
      ((_ formals . body)
       (syntax-case (reverse (syntax->list #'formals)) ()
         ((rf ...)
          #`(let* ((rf (pop)) ...) . body)))))))


;; DEBUG
(define (.S [n 10])
  (notrace
   (for ((val (reverse
               (for/list ((i (in-range n)))
                         (mem@ (+ (mem@ *SP*) i))))))
        (printf " ~a" val))
   (newline)))

;; Testing order of locals
;; (define (ab) (locals (a b) (printf "a=~a b=~a\n" a b)))

       



(define (not-implemented name) (error 'not-implemented "~a" name))
(define-syntax CODE
  (syntax-rules ()
    ((_ name)        (CODE name (not-implemented 'name)))
    ((_ name . body) (define name (lambda () . body)))))



(CODE @     (locals (addr) (lit (mem@ addr))) ($NEXT))
(CODE !     (locals (val addr) (mem! addr val)) ($NEXT))

(define C! !)
(define C@ @)

(CODE DROP  (pop) ($NEXT))
(CODE SWAP  (locals (a b) (lit b a)) ($NEXT))
(CODE DUP   (locals (a)   (lit a a)) ($NEXT))
(CODE OVER  (locals (a b) (lit a b a)) ($NEXT))

  
(CODE SP@   (lit (mem@ *SP*)) ($NEXT))
(CODE RP@   (lit (mem@ *RP*)) ($NEXT))
(CODE SP!   (mem! *SP* (pop)) ($NEXT))
(CODE RP!   (mem! *RP* (pop)) ($NEXT))

(define (binop op) (locals (a b) (lit (op a b))))

(CODE AND   (binop bitwise-and) ($NEXT))
(CODE OR    (binop bitwise-ior) ($NEXT))
(CODE XOR   (binop bitwise-xor) ($NEXT))

(CODE UM+   (locals (a b)
               (let ((sum (+ a b)))
                 (lit sum
                      (arithmetic-shift sum (- word-size)))))
            ($NEXT))
      

(CODE R>    (lit (stack-pop *RP*)) ($NEXT))
(CODE R@    (lit (mem@ *RP*)) ($NEXT))
(CODE >R    (stack-push *RP* (pop)) ($NEXT))

(CODE 0<    (locals (a) (lit (if (< a 0) -1 0))) ($NEXT))

(CODE BYE   (printf "BYE\n") (abort (void)))

(CODE ?RX   (let ((c (read-char)))
              (if (eof-object? c)
                  (lit 0)
                  (lit (char->integer c) -1)))
            ($NEXT))

(CODE TX!   (write-char (integer->char (pop))) ($NEXT))

(CODE !IO   ($NEXT)) ;; Init serial port.

  

;; Inner interpreter.
(define (*IP++) (stack-pop *IP*))
(define (IP! ip) (mem! *IP* ip))


(CODE doLIT  (lit (*IP++)) ($NEXT))

           
(CODE doLIST (locals (addr)
                (stack-push *RP* (mem@ *IP*))
                (mem! *IP* addr))
             ($NEXT))

(CODE EXIT (mem! *IP* (stack-pop *RP*)) ($NEXT))

;; The jump/goto machine instruction normally present in NEXT
;; dereferences the numeric code to a thunk and executes it in tail
;; position.  Here continue will start executing primitive machine
;; code represented by number, first mapped to a thunk.

;; The difference with a real machine is that a primitive is usually a
;; list of concrete machine instructions, while we have an extra
;; indirection here that maps a single numeric representation to a
;; closure.  This indirection is necessary to serialize memory images.

(define (goto xt) (continue (mem@ xt)))
(define ($NEXT) (goto (*IP++)))
(CODE EXECUTE (goto (pop)))

(CODE branch  (let ((xt (*IP++))) (mem! *IP* xt)) ($NEXT))
(CODE ?branch (locals (flag) (let ((ip (*IP++))) (when (zero? flag) (IP! ip)))) ($NEXT))

;; TODO
   
(CODE next  (let*
                ((ip (*IP++))
                 (count (sub1 (stack-pop *RP*))))
              (unless (zero? count)
                (IP! ip)
                (stack-push *RP* count)))
            ($NEXT))
                


(CODE D$)
(CODE $USER)
(CODE $COLON)
(CODE $CODE)



;; The instruction table: map from numbers -> thunks.  The ordering
;; here is quite arbitrary except for keeping BYE = 0 for safety.

(define instruction-table
  (list->vector
   (list
    BYE ?RX TX! !IO doLIT doLIST EXIT EXECUTE next ?branch branch ! @
    C! C@ RP@ RP! R> R@ >R SP@ SP! DROP DUP SWAP OVER 0< AND OR XOR UM+
    D$ $USER $COLON $CODE)))

(define (continue ins)
  (let ((primitive (vector-ref instruction-table ins)))
    (primitive)))


;; BOOT

;; For the interpreter to work we need to wrap all primitives in high
;; level words for $NEXT indirection to work.  No other dict structure
;; is necessary though (only for outer interpreter).

(define *DICT* #x100)

(define (instruction->xt ins)
  (+ *DICT*
     (prompt
      (for ((i (in-range (vector-length instruction-table)))
            (p instruction-table))
           (when (eq? ins p) (abort i)))
      0)))
  
(define (compile: . lst)
  (for/list ((ins lst))
     (if (number? ins)
         ins (instruction->xt ins))))
         
(define (hello)
  (append
   (compile: doLIT 3 >R)
   (apply append (for/list ((c "Staapl eForth 1.0\n"))
                    (compile: doLIT (char->integer c) TX!)))
   (compile: next #x1003)
   (compile: BYE)
   ))
         

;; Initialize the machine
(define (boot)
  (newline)
  (mem! *SP* #x0000)  ;; Top of data stack - 1
  (mem! *RP* #xFF00)  ;; Top of return stack - 1.
  (mem! *IP* #x1000)  ;; Threaded boot code.

  (apply mem! *DICT*
         (for/list ((i (in-range
                        (vector-length instruction-table))))
                   i)) ;; Primitive wrapper words.

  (apply mem! #x1000 (hello))

  (prompt ($NEXT)))

(boot)



