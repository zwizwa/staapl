#lang racket/base
(require
 "tethered.rkt"
 "../tools.rkt")

(provide comm-simulator)

;; SIMULATOR

;; A simulator for testing and definition of target interpreter
;; semantics.  Obviously, this doesn't execute native code.

(define (make-memory [size #x4096] [filler #xFF])
  (define v (make-vector size filler))
  (define (addr x) (modulo x size))
  (case-lambda
    ((ref) (vector-ref v (addr ref)))
    ((ref val) (vector-set! v (addr ref) val))))

(define (make-simulator [amem (make-memory)]
                        [fmem (make-memory)])
  ;; machine state
  (define a 0)
  (define f 0)
  (define stack '())
  
  ;; functionality
  (define (p x) (printf "target: ~a\n" x))
  (define (push x) (push! stack x))
  (define (pop)
    (with-handlers
        ((void (lambda _
                 (p 'stack-underflow)
                 0)))
      (pop! stack)))
  (define I (make-channel))
  (define O (make-channel))
  (define (recv) (channel-get I))
  (define (trns x) (channel-put O x))
  (define (ack) (trns 0))
  (define (ferase) (p '(ferase)) (ack))
  (define (fprog)  (p '(fprog)) (ack))
  (define (recv2) (b->w (recv) (recv)))
  (define (b->w l h) (car (bytes->words (list l h))))

  (define (chkblk)
    (let ((x #xff))
      (for ((i (in-range 64)))
        (set! x (band x (fetch++ fmem f))))
      (trns 1)
      (trns x)))

  (define (stackptr)
    (trns 1)
    (trns (length stack)))

  (define-syntax-rule (fetch++ mem ptr)
    (let ((x (mem ptr)))
      (set! ptr (add1 ptr)) x))
  (define-syntax-rule (store-buf mem ptr)
    (begin
      (for ((i (in-range (recv))))
        (mem ptr (recv))
        (set! ptr (add1 ptr)))
      (ack)))
  (define-syntax-rule (fetch-buf mem ptr)
    (let ((n (recv)))
      (trns n)
      (for ((i (in-range n)))
         (trns (fetch++ mem ptr)))))
  
  (define (interpret cmd)
    (case cmd
      ((0) (ack))
      ((1) (push (recv)) (ack))
      ((2) (trns 1) (trns (pop)))
      ((3) (p (list 'jsr (recv2))) (ack))
      ((4) (set! a (recv2)) (ack))
      ((5) (set! f (recv2)) (ack))
      ((6) (ack))      
      ((7) (p (list 'jsr (recv2))))
      ((8) (fetch-buf amem a))
      ((9) (fetch-buf fmem f))
      ((10) (store-buf amem a))
      ((11) (store-buf fmem f))
      ((12) (chkblk)) 
      ((13) (stackptr))
      ((14) (ferase))
      ((15) (fprog))))

  (define (interpreter)
    (unless (zero? (recv))
      (interpret (recv)))
    (interpreter))


  (thread interpreter)
  (values I O))


(define (comm-simulator)
  (let-values (((to from) (make-simulator)))
    (comm-in
     (lambda () (channel-get from)))
    (comm-out
     (lambda (b) (channel-put to b)))))
