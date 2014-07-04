#lang racket/base
(require racket/match
         racket/pretty)

;; Zipper dictionary for one-pass compilation where the main action is
;; to "add the next instruction".  In addition it provides means to
;; perform operations that modify the dictionary as a whole,
;; performing wrapping and grouping.  It's behaviour is modeled after
;; a Forth compiler, and has a similar imperative interface.

;; In short, a zd is a collection of named objects in _unwrapped_
;; intermediate representation in the form of sequential instructions.

(provide zd-open     ;; create zd from name and semantics
         zd-close    ;; seal + convert to (list-of (list name object))
         zd-start    ;; seal current + start new object
         zd-compile  ;; add instruction to current object's code
         zd-repack)  ;; transform the semantics of the current object

;; The dictionary representation contains the current entry unwrapped.
(define-struct dict (stack         ;; current compile stack (reverse of list)
                     pack          ;; instruction-list -> object 
                     name          ;; current object's name
                     sealed        ;; sealed (list name object) stack
                     default-pack  ;; default semantics
                     ;; attrib     ;; anything else tagged to the current name
                     ))
                   

(define (zd-open
         [name #f]             ;; An open zipper dictionary always has a
                               ;; current entry which has a name.
                               ;; (non-reachable code).
         [pack (lambda (x) x)] ;; When an entry is closed, its code list is
                               ;; passed to its pack function to to
                               ;; produce a representation of the object.
         )
  (make-dict  '() pack name '() pack))


;; Record instruction on the compile stack.
(define (zd-compile zd value)
  (match zd
         ((struct dict (stack c n s p))
          (make-dict (cons value stack)
                     c n s p))))

;; Start a new entry with optional semantics.  As a side effect, the
;; current one is sealed.
(define (zd-start zd _name [pack #f])
  (define default-pack (dict-default-pack zd))
  (make-dict '() (or pack default-pack) _name ;; new open entry
             (seal-current zd)
             default-pack))


;; Pack last and return dictionary as (list-of (list name object)) in
;; the order in which it was compiled.
(define (zd-close zd)
  (reverse (seal-current zd)))

;; Extract the current entry (packed) and update dictionary
;; accordingly.


;; Now, in Forth parsing it happens that a certain parsing word
;; changes the meaning of subsequent code.  An example is the "locals"
;; construct, which introduces bindings.  In terms of the dictionary,
;; what this does is two steps:

;;   1. take the current object (abstractly) and semantics.
;;   2. replace with empty entry and new semantics

;; Here the updated semantics will probably include the compiled
;; object in some way.  This is modeled as a procedure which takes the
;; current object and produces a semantics.

(define (zd-repack zd make-pack)
  (match zd
         ((struct dict (stack pack name sealed default-pack))
          (make-dict '()
                     (make-pack
                      (pack-current zd) ;; hide the real data!
                      pack              ;; semantics the object was packed with
                      default-pack)     ;; default semantics
                     name sealed default-pack))))

;; Internal methods.
(define (pack-current zd)
  ((dict-pack zd) (reverse (dict-stack zd))))
(define (seal-current zd)
  (cons (list (dict-name zd)
              (pack-current zd))
        (dict-sealed zd)))

  

