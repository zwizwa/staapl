#lang racket/base

;; High-level specification of USB descriptor structures.
;; Provides management for:
;;   - Symbolic field names
;;   - Descriptor size checks
;;   - String management

(require
 racket/pretty)
(provide
 Fields
 byte word istring
 DescriptorContext
 Descriptor
 TotalLength
 chunk
 prefix-length
 string-descriptors
 )

(define-syntax-rule (Fields (name typ) ...)
  (begin (define name typ) ...))



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
                 

(define-syntax-rule (chunk forms ...)
  (bytes-chunk (lambda () forms ...)))

;; String buffer
(define *rstrings* (make-parameter '()))
(define (strings) (reverse (*rstrings*)))
(define (push-string s)
  (if (not (string? s))
      (push-byte s)
      (let ((strs (*rstrings*)))
        (push-byte (add1 (length strs)))
        (*rstrings* (cons s strs)))))
        

;; String to UTF-16
(define convert-UTF-16 (bytes-open-converter "UTF-8" "UTF-16"))
(define (string->bytes/utf-16 str)
  (let-values (((rv n status)
                (bytes-convert convert-UTF-16 (string->bytes/utf-8 str))))
    rv))


;; Types
(define (byte arg) (push-word 1 arg))
(define (word arg) (push-word 2 arg))
(define (istring arg) (push-string arg))


;; Context
(define-syntax-rule (DescriptorContext body ...)
  (parameterize ((*rstrings* '())
                 (*rbytes*   '()))
    body ...))

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

;; Convert native string to descriptor.
(define (StringDescriptor str)
  (let ((bytes (bytes->list (string->bytes/utf-16 str))))
    (append (list 
             (+ 2 (length bytes)) ;; bLength
             4)                   ;; bDescriptorType
            bytes)))


(define (string-descriptors)
  (cons (list 4 3 #x04 #x09)  ;; US English
        (map StringDescriptor (strings))))

    
    
