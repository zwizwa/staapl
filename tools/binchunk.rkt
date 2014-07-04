#lang racket/base

(require
 "list.rkt"
;; "seq.rkt"
 "seq-tools.rkt"
 "grabbag.rkt"
 "binary.rkt"
 (lib "match.rkt"))
(provide (all-defined-out))

(require  (lib "78.rkt" "srfi"))
(check-set-mode! 'report-failed)

;; CHUNKS

;; Using list structures instead of structure types.
;; FIXME: does it need word size?

(define (number/false? x)
  (and x (number? x)))

(define (binchunk? x)
  (fail/false
   (and (= 2 (length x))
        (number? (car x))
        (pair? (cadr x))      ;; need at least one element
        (andmap number/false? ;; allow undefined bytes (means don't overwrite)
                (cadr x)))))

(define (bin? x)
  (fail/false
   (andmap binchunk? x)))

(define binchunk-address car)
(define binchunk-code cadr)
  


(define (binchunk-split  chunk left right) ;;
  (match chunk
         ((addr things)
          `(,(<<< addr 1)
            ,(split-nibble-list things left right)))))

(check (binchunk-split '(0 (#x0102 #x0304)) 0 8)
       => '(0 (#x02 #x01 #x04 #x03)))



;; Flatten binchunks if they are consecutive (FIXME: within limit?)

(define (in-binchunk chunk)
  (in-parallel
   (in-naturals (binchunk-address chunk))
   (in-list (binchunk-code chunk))))



(define (in-bin bin)
  (apply in-append (map in-binchunk bin)))



;; Using the stack-of-stacks abstraction to combine consecutive
;; binchunks into one.
(define (bin-flatten dirty-bin)
  (let ((bin (filter binchunk? dirty-bin))) ;; remove empty chunks
   (if (null? bin) '()
    (let-values
        (((sos _)
          (for/fold ((sos (make-sos 2))
                     (expected (binchunk-address (car bin))))
                    (((address codeword) (in-bin bin)))
            (values
             (sos-push 
              (if (= address expected)
                  sos
                  (begin
                    ;; (printf "rechunk: ~a (~a)\n" address expected)
                    (sos-collapse sos 1)))
              (list address codeword))
             (+ 1 address)))))
      (map address/code->binchunk
           (sos->list sos))))))

;; Convert intermediate representation to binchunk, assuming all
;; addresses are in sequence.
(define (address/code->binchunk acl)
  (list (caar acl)
        (map cadr acl)))

       
;; Split binchunk in aligned lines.
(define (in-binchunk/pad-align chunk bits [filler #f])
  (in-acor
   (lambda (yield end)
     (define addr  (binchunk-address chunk))
     (define endx  (+ addr (length (binchunk-code chunk))))
     (define _addr (bit-floor addr bits))
     (define _endx (bit-ceil endx bits))
     
     (for ((a (in-range _addr addr)))   (yield a filler))
     (for (((a c) (in-binchunk chunk))) (yield a c))
     (for ((a (in-range endx _endx)))   (yield a filler)))))


(check (for/list (((a c) (in-binchunk/pad-align '(3 (1 2 3)) 3 #f)))
                 (list a c))
       => '((0 #f)
            (1 #f)
            (2 #f)
            (3 1)
            (4 2)
            (5 3)
            (6 #f)
            (7 #f)))


(define (in-binchunk-sequence/lines seq bits)
  (in-acor
   (lambda (yield)
     (let ((mask (bitmask bits)))
       (for/fold ((line '()))
                 (((a c) seq))
         (let ((line+ (cons c line)))
           (if (= mask (band a mask))
               (begin
                 (yield (- a mask)
                        (reverse line+))
                 '())
               line+)))))))

(define (in-binchunk/lines chunk bits [filler #f])
  (in-binchunk-sequence/lines
   (in-binchunk/pad-align chunk bits filler)
   bits))
  
                
;; Convert list of chunks to a minimum size list starting at addr = 0.
(define (chunks->list/0 chunks size pad)
  (define mem (make-vector size pad))
  (define top -1)
  (for ((c chunks))
     (for (((addr byte) (in-binchunk c)))
        (when (< addr size)
          (set! top (max top addr))
          (vector-set! mem addr byte))))
  (for/list ((i (in-range (add1 top)))) (vector-ref mem i)))
 


;; These are used in:

;; - internal PIC18: convert words to bytes + flatten

;; - upload: algin + divide in lines.