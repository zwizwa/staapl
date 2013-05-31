;; binary tree instruction decoder

#lang scheme/base

(require
 "../tools.ss")

(provide
 decoder-index
 decoder-set!
 decoder-get
 decoder-leaf)

(define (decoder-leaf)
  (let ((dw (lambda (opcode) `(dw (w . ,opcode)))))
    (decoder-cons dw dw)))

(define decoder-cons mcons)

;; (decoder-index '(((a . b) . (c . d)) . e) 1)
(define (decoder-index t i)
  (if (zero? i) (mcar t) (mcdr t)))

(define (decoder-index-set! t i value)
  (if (zero? i)
      (set-mcar! t value) (set-mcdr! t value)))

(define (decoder-index-force! make-tree t i)
  (let ((maybe
         (decoder-index t i)))
    (if (mpair? maybe)
        maybe
        (let ((subtree (make-tree)))
          (decoder-index-set! t i subtree)
          subtree))))

;; (register-disassembler! 0 2 123)
(define (decoder-set! make-tree disassemblers
                      address bits code)
  (let set-node! ((b bits)
                  (d disassemblers))

    (let ((i (bit address (- b 1))))
      (if (= 1 b)  ;; at a node?
          (decoder-index-set! d i code)
          (set-node! (- b 1)
                     (decoder-index-force! make-tree
                                           d i))))))

(define (decoder-get disassemblers address bits)
  (let get-node ((b bits)
                 (d disassemblers))
    
    (let ((i (bit address (- b 1))))
      (if (= 1 b)  ;; ran out of bits -> return value
          (decoder-index d i)
          (let ((next-node (decoder-index d i)))
            (if (not (mpair? next-node))
                next-node  ;; found it (no subtree)
                (get-node (- b 1)
                          (decoder-index d i))))))))

