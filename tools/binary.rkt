;; Utilities for manipulation of binary words and buffers.

#lang racket/base

(require
 (lib "match.rkt")
 "list.rkt"
 "tree.rkt")


;; Tests really are a cheap form of documentation :)
(require  (lib "78.rkt" "srfi"))
(check-set-mode! 'report-failed)

(provide
 (all-defined-out))


;; binary

(define (sign-extender n)
  (lambda (x) (sign-extend x n)))

(define (sign-extend x n)
  (let ((signmask (<<< 1 (- n 1))))
    (- (bxor signmask
             (band x (bitmask n)))
       signmask)))

(define (bit? n bit)
  (let ((mask (<<< 1 bit)))
    (= (band n mask) mask)))

(define (bitmask bits)
  (- (<<< 1 bits) 1))

(define (make-mask bits)
  (let ((bm (bitmask bits)))
    (lambda (x) (bitwise-and bm x))))

(define <<< arithmetic-shift)
(define (>>> val shift)
  (<<< val (* -1 shift)))

(define (<< x) (<<< x 1))
(define (2/ x) (>>> x 1))  ;; scheme's ints are 2-adic
(define 2* <<)

(define (bit address n)
  (bitwise-and 1 (>>> address n)))

(define (bit-floor n bits) (band n (bxor -1 (bitmask bits))))
(define (bit-ceil  n bits) (+ (bit-floor (- n 1) bits) (<<< 1 bits)))

(define (block-floor n bits) (>>> n bits))
(define (block-ceil  n bits) (>>> (bit-ceil n bits) bits))

;; convert anything that might be passed to the assembler
;; representing a number to integer

(define (int x)
  (cond
   ((number? x) (inexact->exact (round x)))
   (else (error 'cannot-convert-to-int "~a" x))))

(define (int8 x)
  (bitwise-and #xFF (int x)))

(define (band x y) (bitwise-and (int x) (int y)))
(define (bior x y) (bitwise-ior (int x) (int y)))
(define (bxor x y) (bitwise-xor (int x) (int y)))

(define (invert  b)  (bxor b -1)) ;; all bits
(define (flip    b)  (bxor b 1))  ;; one bit

(define (negate x) (* -1 x))

(define (bit->bool bit)  (not (zero? bit)))
(define (bool->bit bool) (if bool 1 0))

(define (bit-set? value bit)
  (bit->bool (band 1 (>>> value bit))))

;; signed and unsigned
(define (lohi lo hi)
  (+ (<<< hi 8) (band #xFF lo)))

;; ;; symbol generation. not going to make a separate module for this...
;; (define (generated-label? x)
;;   (and (symbol? x)
;;        (let ((chars
;;               (string->list
;;                (symbol->string x))))
;;          (if (eq? #\L (car chars))
;;              (string->number
;;               (list->string (cdr chars)))
;;              #f))))

;; (define make-label
;;   (let ((n -1))
;;     (lambda ()
;;       (set! n (+ n 1))
;;       (string->symbol
;;        (format "L~s" n)))))


;; BLOCK/LIST


;; determine next available block from address
(define (ceiling-block address blocksize)
  (+ 1 (floor (/ (- address 1) blocksize))))

;; split a number into a list of chunk sizes.
(define (chunk-size-list initial max)
  (let next ((total initial))
    (if (> total max)
        (cons max (next (- total max)))
        (list total))))
(check (chunk-size-list 13 4) => '(4 4 4 1))

;; split a list of words into parts.
;; (left right) = (0 n)  little endian
;;              = (n 0)  big endian

(define (split-nibble-list lst left right)
  (unless (or (zero? left) (zero? right))
    (error 'split-nibble-list-need-zero))
  (let ((mask (make-mask (max left right))))
    (flatten
     (map
      (lambda (x)
        (list (mask (>>> x left))
              (mask (>>> x right))))
      lst))))

(check (split-nibble-list '(#x102 #xFFAA) 0 8)
       => '(#x02 #x01 #xAA #xFF))

;; (post) inverse  of above
(define (join-nibble-list lst left right)
  (if (= 1 (bitwise-and 1 (length lst)))
      (error 'odd-list-length "join-nibble-list: odd list length: ~a" lst)
      (let
          ((mask
            (make-mask (max left right)))
           (select
            (lambda (lst which)
              (let rest ((l lst))
                (if (null? l) l
                    (cons (which l)
                          (rest (cddr l))))))))
        (map
         (lambda (l r)
           (bior (<<< (mask l) left)
                 (<<< (mask r) right)))
         
         (select lst first)
         (select lst second)))))

(check (join-nibble-list '(#x01 #x02 #x03 #x04) 0 8)
       => '(#x201 #x403))



;; FIXME: it's probably easier to use the bin and binchunk
;; comprehensions for code that needs this..

(define (list->table lst size)
  (let next ((in  lst)
             (out '())
             (current '(0)))
    (match (cons in current)
           ((() 0)  (reverse out)) ;; done
           ((_  n . l)
            (if (or (null? in)
                    (= n size))
                ;; row finished
                (next in
                      (cons (reverse l) out)
                      '(0))
                ;; accumulate row
                (next (cdr in)
                      out
                      (cons (+ 1 n)
                            (cons (car in) l))))))))

(check (list->table '(1 2 3 4 5) 2)
       => '((1 2) (3 4) (5)))





(define (->byte-list x)
  (cond
   ((symbol? x) (->byte-list (symbol->string x)))
   ((string? x) (->byte-list (string->bytes/utf-8 x)))
   ((bytes? x)  (bytes->list x))
   ((list? x)   x)
   (else (error 'byte-list "~a" x))))
      

;; Expose components of a single parameter bye performing masked read/write.
;; Application is parameterized to allow for abstract interpretation, e.g. propagation of undefined values.

(define (check-word-index size index)
  (when (or (negative? index) (>= index size))
    (error 'word-index "~s ~s" size index)))

(define (masked-reader read size bits
                       #:ai [ai: (lambda (fn . args) (apply fn args))])
  (let ((mask (bitmask bits)))
    (lambda (index)
      (check-word-index size index)
      (ai: band mask
           (ai: >>> (read) (* index bits))))))

(define (masked-writer read write size bits
                       #:ai [ai: (lambda (fn . args) (apply fn args))])
  (let ((mask (bitmask bits)))
    (lambda (index value)
      (check-word-index size index)
      (let* ((shift  (* index bits))
             (svalue (ai: <<< (ai: band mask value) shift))
             (pmask  (<<< mask shift))
             (nmask  (bxor -1 pmask)))
        (write (ai: bior
                    (ai: band nmask (read))
                    (ai: band pmask svalue)))))))
  

(define (hex-string->bytes str)
  (for/list ((i (in-range (/ (string-length str) 2))))
    (let* ((str-byte (string-append "#x" (substring str i (+ 2 i)))))
      (string->number str-byte))))

;; (hex-string->bytes  "c0f017a214853d147fc06469fd8089499e42c3f8")
           
      
