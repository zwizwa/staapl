#lang racket/base
;; FORMATTING

(provide (all-defined-out))

(require
 "binary.rkt"
 "seq-tools.rkt")

(require  (lib "78.rkt" "srfi"))
(check-set-mode! 'report-failed)


;; (hexdigit 15)
(define hexdigit
  (let ((table
         (apply vector
                (string->list "0123456789ABCDEF"))))
    (lambda (x)
      (vector-ref table x))))

(define (hex->string nibbles val)
  (let-values
      (((digits _)
        (for/fold ((d '())
                   (v val))
                  ((i (in-range nibbles)))
           (let ((nibble (band v #xF)))
             (values
              (cons (hexdigit nibble) d)
              (>>> v 4))))))
    (list->string digits)))
    
(check (hex->string 6 #x1234FF) => "1234FF")

(define (word->string x) (hex->string 4 x))
(define (byte->string x) (hex->string 2 x))





;; A column print loop turned into a sequence.

(define (in-hex-printer start
                        address-nibbles
                        data-nibbles
                        items-per-line)
  (in-acor
   (lambda (yield)
     
     (define-syntax-rule (lp formals . args)
       (yield (lambda formals (printf . args))))
     (define (addr x) (hex->string address-nibbles x))
     (define (data x) (hex->string data-nibbles x))

     (for ((row (in-naturals)))
       (lp (x) "~a  ~a" (addr (+ start (* row items-per-line))) (data x))
       (for ((i (in-range (- items-per-line 2)))) (lp (x) " ~a" (data x)))
       (lp (x)  " ~a\n" (data x))))))


(define (pad-string str len)
  (string-append str (make-string (- len (string-length str)) #\space)))

        
