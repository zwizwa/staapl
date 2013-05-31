#lang scheme/base

;; Routines for creating PICkit2 command and script assemblers.

;; For easier reuse, this doesn't depend on Staapl code: all
;; dependencies can be injected.

(provide interpreter
         interpreter-snd
         interpreter-rcv
         dasm
         (struct-out scr)

         ;; To inject Staapl dependencies:
         current-hex->string
         )
(define (fake-hex->string nibbles val) (format "~s" val))
(define current-hex->string (make-parameter fake-hex->string))


;; Patch with genuine snd/rcv
(define interpreter-snd
  (make-parameter
   (lambda (bytes)
     (printf "~a\n" (bytes->list bytes)))))
(define interpreter-rcv
  (make-parameter
   (lambda () (make-bytes 64))))

;; SCRIPT COMPILER
(define-struct scr (code))

(define (make-opcode name opcode nin [nout #f])
  (lambda tree-args
    ;; Flatten enables script concatenation for multiple arguments.
    (define (concatenate x)
      (cond
       ((number? x) (list x))
       ((scr? x)    (apply append (map concatenate (scr-code x))))
       (else (error 'not-a-script "~a" x))))
    (define args (concatenate (make-scr tree-args)))
    (define (prefix lst) (cons (length lst) lst))
    (define code
      (cons opcode
            (case nin
              ((255) (prefix args)) ;; variable args, first arg is count
              ((254) (cons (car args)
                           (prefix (cdr args)))) ;; script #, then count
              (else
               (unless (= nin (length args))
                 (error 'invalid-argument "~a" (cons name args)))
               args))))
    (if nout
        (send/reply nout code)  ;; compile + execute
        (make-scr code))))      ;; compile only

;; Perform simple dasm: only map opcode, don't parse arguments.
;; The script file contains a 'type' byte. #xAA = opcode
(define opcode-table (make-vector 256))
(define (dasm script-hash)
  (display ;; ??
   (string-append
    (format "~a\n\n~a:"
            (bytes->string/utf-8 (hash-ref script-hash 'Comment))
            (hash-ref script-hash 'ScriptName))
    (apply string-append
      (for/list ((val (hash-ref script-hash 'Script)))
        (let ((low (bitwise-and val #xff))
              (hi  (arithmetic-shift val -8)))
          (case hi
            ((#xAA) (format "\n\t~a" (vector-ref opcode-table low)))
            ((#xBB) (format " ~a" ((current-hex->string) 2 low)))
            ((#x00) (format " ~a" low))))))
     "\n")))



(define-syntax-rule (interpreter (name opcode . spec) ...)
  (begin
    (begin
      (define name (make-opcode 'name opcode . spec))
      (vector-set! opcode-table opcode 'name))
    ...))
  
;; WIRE PROTOCOL

(define (pack-cmd code)
  (append code
          (build-list (- 64 (length code))
                      (lambda _ #xAD)))) ;; END_OF_BUFFER
(define (unpack-reply nout rx)
  (bytes->list
   (case nout
     ((255) (subbytes rx 1 (+ 1 (bytes-ref rx 0)))) ;; length-prefixed
     (else (subbytes rx 0 nout)))))                 ;; fixed size

(define (send/reply nout code)
  ((interpreter-snd) (apply bytes (pack-cmd code)))
  (if (zero? nout)
      '()
      (unpack-reply nout ((interpreter-rcv)))))

