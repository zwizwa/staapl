#lang racket/base

;; Target code registry.

;; During instantiation of a Forth module, postponed code can be
;; registered for later compilation/assembly.

(require "tools.rkt"
         "target.rkt"
         "sig.rkt"
         "comp/postprocess.rkt"
         racket/pretty)


;; CODE REGISTRY
(provide
 code-append-postponed!
 code-compile!
 code-debug
 code-print
 code-pointers
 code-pointers-set!
 code->binary
 code-clear!
 code-find   
 code-resolve  ;; fixme: in terms of code-find
 code-labels)



;; To keep track of possible multiple instances.
;; (printf "Instantiating target code repository.\n")

;; ACCUMULATION

;; Stack used during the 'forth-begin form.
(define *postponed-macro-stack* '()) ;; stack used for postponed word collection.
(define (code-append-postponed! code) (push! *postponed-macro-stack* code))


;(define (compile!)
;  (set! *cfg* (append (compile (code-postponed-pop-stack)) *cfg*))
;  (set! *inlined-macro-stack* '()))

;; COMPILATION

;; Stacks used to gather compiled and possibly assembled code.
(define *postponed* '())
(define *cfg* '())

;; Keep track of labels for debug inspection and reverse lookup.  This
;; should not be used for name resolution in code as it might contain
;; duplicate or protected names.
(define *labels* '())
(define (code-labels) *labels*)
(define (code-resolve addr [realm 'code])
  (let next ((l *labels*))
    (cond
     ((null? l) #f)
     ((and (eq? realm (cadar l))
           (= addr (caddar l)))
      (car l))
     (else (next (cdr l))))))

(define (code-find name)
  (assoc name *labels*))

;; Assembly state.
(define *pointers* '((code 0) (data 0)))
(define (code-pointers) *pointers*)
(define (code-pointers-set! p) (set! *pointers* p))

(define-syntax-rule (save! *store* lst)
  (set! *store* (append lst *store*)))

(define code-debug (make-parameter #f))

(define (code-compile! compile-words postproc assemble!)
  (define (compile in)
    (let ((cfg
           (words->cfg!
            (compile-words in)  ;; functional compiler from instantiated module
            postproc)))         ;; postprocessor (i.e. from postproc^)
      (when (code-debug) (code-print cfg))
      cfg))
  (let ((cfg (compile *postponed-macro-stack*)))
    (let-values (((_ pointers) (assemble! cfg *pointers*)))
      (save! *cfg* cfg)
      (save! *labels* (chain-list->labels cfg))
      (save! *postponed* *postponed-macro-stack*)
      (set! *pointers* pointers)
      (set! *postponed-macro-stack* '()))))

(define (code-print [cfg *cfg*])
  (for-each print-target-word (reverse cfg)))


;; QUERY
(define (code->binary [chain-list *cfg*])
  (map
   (lambda (c) (binchunk-split c 0 8))
   (or (target-chains->bin chain-list)
       (error 'code->binary))))


(define (code-clear!)
  (set! *postponed-macro-stack* '()) 
  (set! *postponed* '())
  (set! *cfg* '()))

;; Instead of going through the namespace, get target words (including
;; temporaries) from the CFG itself.  Note that this is a debugging
;; tool: it might introduce name conflicts!

(define (chain-list->labels chains)
  (apply append
         (for/list ((c chains))
           (for/list ((w (target-chain->list c))
                      #:when (symbol? (target-word-name w)))
             (list (target-word-name w)
                   (target-word-realm w)
                   (target-word-address w))))))

  
  