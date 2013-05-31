;; Intel hex format writing.

;; 16 bytes per line, each line has a checksum.

#lang scheme/base


(require
 (lib "match.ss")
 "../tools.ss")

(provide
 ;; (all-defined-out)
 write-ihex
 )



;; (checksum '(1 2 200 200))
(define checksum
  (let ((mask (make-mask 8)))
    (lambda (lst)
      (mask (* -1 (foldl + 0 lst))))))

;; split a 16 bit address in 2 bytes: always big endian
(define (ihex-split-address address)
  (split-nibble-list `(,address) 8 0))

;; create one ihex line
;; (ihex-line-list 4 1000 '(1 2 3 4 5 6))
(define (ihex-line-list type address bytes)
  (let ((line
         `(,(length bytes)
           ,@(ihex-split-address address)
           ,type
           ,@bytes)))
    (append line `(,(checksum line)))))

;; (ihex-line-string (ihex-line-list 4 1000 '(1 2 3 4 5)))
(define (ihex-line-string lst)
  (apply string-append
         `(":" ,@(map byte->string lst) "\r\n")))

;; (ihex-line 4 1000 '(1 2 3))
(define (ihex-line . args)
  (ihex-line-string
   (apply ihex-line-list args)))

;; (display (ihex (sequence (lambda (x) (random 256)) 1 1000) 16 #xaab0))
(define (ihex-chunk bytes chunksize address)
  (let next ((in   (list->table bytes chunksize))
             (out  '())
             (addr address))

    (match in
           (()
            (apply string-append
                   `(;; address high word
                     ,(ihex-line 4 0 (ihex-split-address
                                      (>>> address 16)))
                     ;; body
                     ,@(reverse out))))

           ;; accumulate body, increment address
           ((line . rest)
            (next rest
                  (cons
                   (ihex-line 0 addr line)
                   out)
                  (+ addr chunksize))))))

(define (ihex-done)
  (ihex-line 1 0 '()))


(define (chunks->ihex lst)
  (append
   (map
    (match-lambda
     ((addr code)
      (ihex-chunk code 16 addr)))
    lst)
   `(,(ihex-done))))


(define (write-ihex lst [port (current-output-port)])
  (for-each
   (lambda (x) (display x port))
   (chunks->ihex lst)))



;; terminology:
;; a chunk =  (addr things)    can be bytes / words

