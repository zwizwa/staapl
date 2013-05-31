#lang scheme/base

(require
 "../tools.ss"
 "../target.ss"
 "../scat.ss"
 ;; "../asm/dictionary.ss"
 "../op.ss"
 
 ;; scheme/pretty
 scheme/match)

(provide
 (all-defined-out))

(define-struct pattern-srcloc (file line col))

(define format-pattern-srcloc 
  (match-lambda
   ((struct pattern-srcloc (file line col))
    (format "~a:~a:~a:" file line col))))

(define (macro/append-reverse code rest)
  (if (procedure? code)  ;; doesn't need list wrapper
      (cons code rest)
      (append (reverse code) rest)))

(define (at-most lst n [trunc-tail '()])
  (cond
   ((null? lst) '())
   ((zero? n)   trunc-tail)
   (else
    (cons (car lst)
          (at-most
           (cdr lst)
           (- n 1)
           trunc-tail)))))

(define (pattern-failed name asm)
  (error 'asm-pattern
         "match failed for: ~a, asm:\n~a"
         name
         (apply string-append
                (map (lambda (ins) (instruction->string ins "\n"))
                     (reverse (at-most asm 4 '(...)))))))

(define (with-match-error-handler name asm thunk)
  (with-handlers
      ((exn:misc:match?
        (lambda (ex)
          (pattern-failed name asm))))
    (thunk)))

;; Lift transformation code (which operates on list structure) to
;; operate on Scat state + execute continuations.
(define (pattern-tx->macro name xform)

  ;; map asm-in -> continuation + asm-out
  (define (k/asm asm-in)
    (let ((asm
           (with-match-error-handler
            name asm-in
            (lambda () (xform asm-in)))))
      (cond
       ((null? asm)            (values id '()))
       ((procedure? (car asm)) (uncons asm))      ;; pass egg
       ((list? (car asm))      (values id asm))   ;; type check
       (else
        (error 'pattern-result-type-error
               "~a" asm)))))

  (state-lambda stack
                (asm)
                (let-values
                    (((k asm+) (k/asm asm)))
                  (k (update asm+)))))
  

;; Assembler opcode + arity checking.
(define (check-ops asm-find records)
  (for-each*
   (lambda (name . occurances)
     (let ((asm (asm-find name)))
       (map* (lambda (arity f l c p s)
               (define (err msg)
                 (error msg "~a:~a:~a: ~a" f l c name)  ;; FIXME: disabled
                 ;; (printf "~a ~a:~a:~a: ~a\n" msg f l c name)
                 )
               (unless asm
                 (err 'undefined-opcode))
               ;; don't count first param (instruction address)
               (let ((n+1 (procedure-arity asm)))
                 (unless
                     (if (number? n+1)
                         (= arity  (- n+1 1))
                         (>= arity (- (arity-at-least-value n+1) 1)))
                   (err 'asm-arity-error)
                   )))
             occurances)))
   records))


;; This matches a particular assembler instance + performs safety check.
;; (define ((match-asm asm sym srcloc) x)
;;   (unless (asm? x)
;;     (error 'not-struct-asm "~a" x))
;;   (if (eq? x asm)
;;       (begin
;;         (printf "~a  opcode ``~a'' matches.\n"
;;                 (format-pattern-srcloc srcloc) sym)
;;         #t)
;;       (begin
;;         (when (eq? sym (asm-name asm))
;;           (printf "~a  opcode ``~a'' won't match opcode instance with same name.\n"
;;                   (format-pattern-srcloc srcloc) sym))
;;         #f)))
  