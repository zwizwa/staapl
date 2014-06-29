#lang racket/base

(require "../tools.rkt"
         (for-syntax
          racket/base
          syntax/stx))

;; Interface
(provide   make-word
           ;; word-behaviour!
           word?

           make-parameter-word
           parameter-word?
           word-parameter
           
           ;; change the behaviour of words
           word-swap!
           word-parameter!
           ;; upgrade-to-parameter-word!
           

           )
;; This represents the basic RPN language components which are unary
;; scheme procedures: state -> state.

;; Words are implemented using a PLT struct with property
;; prop:procedure present, so it behaves as a procedure:
;;  * integer -> procedure is present in a field at index
;;  * procedure -> call the procedure with (instance . other-arguments)

(define (word-run w state)
  ((word-ref w 0) w state))

(define (word-print word port write?)
  (let* ((props (word-ref word 1)))
    (write-string
     (format "#state->state") port)))

;;(define (word-run w . args)
;;  (with-continuation-mark 'word w
;;                          (apply (word-proc w) args)))

(define-values
  (struct:word make-word-internal word? word-ref word-set!)
  (begin
    ;; (printf "creating REP struct\n")
    (make-struct-type
     'word    ;; name-symbol
     #f       ;; super-struct-type
     2        ;; init-field-k
     0        ;; auto-field-k
     #f       ;; auto-v
     (list    ;; prop-value-list
      (cons prop:custom-write word-print))
     #f       ;; inspector or false
     word-run ;; word-run or 0
     
     )))

;; Ordinary words just apply the procedure.

(require racket/pretty)
(define (make-word fn [prop #f])
  ;; (when prop (pretty-print prop))
  (make-word-internal
   (lambda (w state) (fn state))
   prop))

;; Change implementation. Use this with care.
(define (word-behaviour! word fn)
  (word-set! word 0 (lambda (w state) (fn state))))

;; Parameter words can be recognized by the 1st field, which is a
;; procedure that interprets the second field as a word wrapped in a
;; parameter.

(define (run-pw pw state)
  (((word-ref pw 1)) state))

(define (parameter-word? pw)
  (and (word? pw)
       (eq? (word-ref pw 0) run-pw)))

(define (word->parameter-word w)
  (when (parameter-word? w)
    (error 'recursive-parameter-word))
  (make-word-internal
   run-pw (make-parameter w)))
  
(define (make-parameter-word . args)
  (word->parameter-word
   (apply make-word args)))

(define (word-parameter w)
  (unless (parameter-word? w)
    (raise
     (make-exn:fail:contract
      "Not a parameter word."
      (current-continuation-marks))))
  (word-ref w 1))

;; Wrap contents of word in a parameter.
(define (upgrade-to-parameter-word! w)
  (define inner #f)
  (define outer #f)
  (set! inner (make-word #f))
  (word-swap! w inner)
  (set! outer (word->parameter-word inner))
  (word-swap! w outer))


;; Automatically upgrade.
(define (word-parameter! w)
  (unless (parameter-word? w)
    (upgrade-to-parameter-word! w))
  (word-parameter w))
  
  


;; (define (word-proc w)      (word-ref w 0))  ;; procedural rep
;; (define (word-proc! w x)   (word-set! w 0 x)) 

;; Swap the contents of 2 word structures. This is used when
;; augmenting the functionality of a word without having to rebuild
;; its linkage.

(define (word-swap! w1 w2)
  (define (swap n)
    (let ((tmp (word-ref w1 n)))
      (word-set! w1 n (word-ref w2 n))
      (word-set! w2 n tmp)))
  (swap 0)
  (swap 1))


;; Parameterization of words. Since words are unary functions, they
;; cannot be represented by parameters, which are functions by
;; themselves. Therefore the 'parameterize' form is modified so the
;; fact that a word is a parameter can be hidden to most of the code.

;; As an extension, the previous binding is available as 'super'. But
;; this needs to play nice with namespaces, so needs to move elsewhere.


