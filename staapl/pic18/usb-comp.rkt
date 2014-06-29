
#lang racket/base
(provide
 Fields
 byte word istring
 Descriptor
 TotalLength
 Table
 StringTable)

(define-syntax-rule (Fields (name typ) ...)
  (begin (define name typ) ...))


;; High-level specification of USB descriptor structures.

;; Compilation buffer
(define *rbytes* (make-parameter '()))
(define (bytes) (reverse (*rbytes*)))
(define (push-byte x) (*rbytes* (cons x (*rbytes*))))
(define (push-word nb-bytes value)
  (when (> nb-bytes 0)
    (push-byte (bitwise-and value #xFF))
    (push-word (sub1 nb-bytes) (arithmetic-shift value -8))))
(define (bytes-chunk thunk)
  (parameterize ((*rbytes* '()))
    (thunk)
    (reverse (*rbytes*))))
(define (push-bytes bs)
  (when (not (null? bs))
    (push-byte (car bs))
    (push-bytes (cdr bs))))
                 

;; String buffer
(define *rstrings* (make-parameter '()))
(define (strings) (reverse *rstrings*))
(define (push-string s)
  (if (not (string? s))
      (push-byte s)
      (let ((str (*rstrings*)))
        (push-byte (length str))
        (*rstrings* (cons s str)))))

;; Types
(define (byte arg) (push-word 1 arg))
(define (word arg) (push-word 2 arg))
(define (istring arg) (push-string arg))


;; Structure


;; `Descriptor` : check size
(define (_descriptor thunk)
  (let* ((chunk (bytes-chunk thunk)))
    (let ((l1 (length chunk))
          (l2 (list-ref chunk 0)))
      (when (not (= l1 l2))
        (raise `(descriptor-size error ,l1 ,l2)))
      (push-bytes chunk))))

(define-syntax-rule (Descriptor forms ...)
  (_descriptor (lambda () forms ...)))


;; `TotalLength` : run twice with second time parameterized by total size.
(define (_twopass body)
  (let* ((chunk (bytes-chunk (lambda () (body -1)))))
    (body (length chunk))))
     
(define-syntax-rule (TotalLength (nb_bytes) forms ...)
  (_twopass (lambda (nb_bytes) forms ...)))


;; Prefix table length
(define (prefix-length x) (cons (length x) x))
(define (_table thunk)
  (let ((chunk (bytes-chunk thunk)))
    (prefix-length chunk)))
(define-syntax-rule (Table forms ...)
  (_table (lambda () forms ...)))


(define (StringTable n)
  (let ((strs (strings)))
    (if (> n (len strs))
        #f
        (prefix-length (list-ref strs n)))))

