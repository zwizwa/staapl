;; Forth (syntax) reader

;; Part of the brood experiment is to transform the highly
;; reflective/selfmodifying forth structure into a feed-forward
;; layered language approach. This approach facilitates source code
;; processing: representations are more transparent: the meaning
;; doesn't change while staying in one layer.

;; This approach allows a purely functional, purely concatenative
;; language (PCL) as intermediate representation.  However, this
;; approach necessarily gives up some simplicity: feedforward parsing
;; is reduced to a preprocessing step and requires a separate meta
;; language to describe transformations from Forth to the pure
;; concatenative language.

;; While doing this, some requirements need to be met

;; 1) forth should be character-delimited: simplifies on-target lexing
;; 2) first stage tokenizer in brood = word/white/comment segmentation
;; 3) the parser needs to be extensible

;; Here 1) is embedded in 2) to make source processing in an editor
;; (like emacs) easier by preserving the original source characters,
;; enhanced with markup. This representation should not introduce
;; structures that 1) can't interpret. For special purposes, the
;; system should be extensible to allow low-level forth parsing
;; extensions.

#lang racket/base

(require
 (for-syntax racket/base)
 (lib "match.rkt")
 "../tools/stx.rkt"
 "../tools/list.rkt"
 "../tools/grabbag.rkt"
 )

(provide read-forth-syntax
         read-forth
         string->forth-syntax
         file->forth-syntax
         )


;; Convert (syntax) string to lexed Forth syntax, preserving attributes.
(define (string->forth-syntax _str)
  (syntax-case _str ()
    (str
     (let ((code
            (port->list
             (port (syntax->datum #'str))
             read-forth)))
       (datum->syntax #'str
                      (map 
                       (lambda (atom)
                         (datum->syntax #'str atom))
                       code))))))


;; Syntax has source location from the file, but lexical context is
;; pluggable.
(define (file->forth-syntax file [context-stx #f])
  (let* ((port
          (open-input-file file))
         (forth
          (map
           (lexical-context-from context-stx)
           (port->list
            port
            (lambda (x)
              (read-forth-syntax file x))))))
    (close-input-port port)
    forth))


;; Convert file to lexed Forth syntax with proper lexical context.


(define-syntax in?
  (syntax-rules ()
    ((_ var lst)
     (case var
       (lst #t)
       (else #f)))))


(define (newline? x)
  (in? x (#\newline   ;; unix
          #\return))) ;; windows

(define (white? x)
  (and (char? x)
       (char-whitespace? x)))

(define (not-white? x)
  (not (white? x)))

;; This will emulate a standard forth reader, but preserves all
;; source characters for source text processing.

;; Read the next word, upto but excluding the delimiting whitespace
;; character. This produces either an eof-object, or a token with 3
;; elements: leading whitespace, word characters and delimiting
;; whitespace. Leading whitespace can contain zero
;; elements. Delimiting whitespace can contain zero elements only in
;; the EOF condition. Each element is tagged with it's starting
;; line/column/position.


(define (forth-collect p done?)

  (define (drop) (read-char p))
  (define (head) (peek-char p))
  (define stack '())
  (define (str!)
    (list->string (reverse stack)))

  (define l/c/p
    (let-values
        (((line column pos)
          (port-next-location p)))
      (list
       (or line 1)
       (or column 0)
       pos)))

  (define (done-at delim)
    (drop)
    (list l/c/p (str!) delim))

  (define (collect)
    (cond
     ((eof-object? (head))
      (if (not (null? stack)) ;; allow EOF terminated identifiers
          (begin
            ;; (printf "WARNING: EOF terminated identifer: ~s ~s\n" l/c/p stack)
            (done-at #\newline)) ;; fake newline
          (head)))
     ((done? (head))
      (done-at (head)))
     (else
      (push! stack (head))
      (drop)
      (collect))))
  
  (port-count-lines! p)
  (collect))


;; Read next word or whitespace
(define (forth-white/word p)
  
  (port-count-lines! p)
  (let ((head (peek-char p)))
    (cond
     ((eof-object? head)
      head)
     (else
      (cons (if (white? head)
                'white
                'word)
            (forth-collect p white?))))))


;; Implement character parsing words.
(define (forth-accept p)
  (match
   (forth-white/word p)

   ;; make this extensible
   (('word pos "\\" delimiter)
    (match
     (forth-collect p newline?)
     ((cpos str cdel)
      `(comment ,pos ,str ,cdel))))
   
   (other
    other)))


(define (num/sym str)
  (or
   (string->number str)
   (string->symbol str)))

;; Read discards comments and whitespace.

(define (read/map port fn)
  (match
   (forth-accept port)
   (('word (l c p) str del)
    (fn str l c p))
   ((and obj (= eof-object? #t)) obj)
   (other (read/map port fn))))

(define (read-forth port)
  (read/map
   port
   (lambda (str l c p)
     (num/sym str))))                     

(define (read-forth-syntax file port)
  (read/map
   port
   (lambda (str l c p)
     (datum->syntax
      #f (num/sym str)
      (list file l c p #f)
      #f #f))))
