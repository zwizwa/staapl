#lang racket/base

;; POSTPROCESSING and OPTIMIZATION

;;  * The first pass of the compiler is purely functional: it uses
;;    labels tags for chunks/chains of assembly code.  The code refers
;;    back to the labels.

;;  * In the postprocessor and assembler, a graph structure is
;;    constructued by linking the labels back to code.

(require
 "../target.rkt"
 "../tools.rkt"
 "../ns.rkt"
 "../scat.rkt"
 "../coma/macro.rkt"
 "../control/2stack.rkt"
 racket/pretty
 racket/match)

(provide
 words->cfg!
 macro->postprocess

 empty-ctrl->asm
 assert-empty-ctrl
 
 )

(define (empty-ctrl->asm state [name ""])
   (match state
          ((struct 2stack (ctor asm ctrl))
           (unless (null? ctrl)
             (error 'non-null-compilation-stack
                    "~a ~s" name ctrl))
           asm)))

(define (assert-empty-ctrl . a) (void (apply empty-ctrl->asm a)))


;; Turn a (set-of (list-of (list word code))) into a linked up
;; imperative data structure and perform postprocessing optimizations.

(define (words->cfg! compiled-words [postproc-asm
                                             (lambda (x) x)])
  (define roots (link! compiled-words))
    
  (define (all: . fns)
    (let ((words (apply append (map target-chain->list roots))))
      (for-each (lambda (fn) (for-each fn words)) fns)))

  ;; Individual optimizations
  (all: (target-post! postproc-asm))

  ;; No global optimizations yet
  
  roots)

;; Converts the (list-of (list-of (list word code))) to (list-of
;; word), with all graph structure linked in (code + chain)

(define (link! chains)
  (define link-chain!
    (match-lambda
     ((list w) w)
     ((list-rest w+ w ws)
      (set-target-word-next! w w+)
      (link-chain! (cons w ws)))))
  (define (link-code! word code)
    (set-target-word-code! word code)
    word)
  (map link-chain!
       (map (lambda (chain)
              (map* link-code! chain))
            chains)))


;; PER-WORD optimizations


;; Hook for target specific assembly postprocessing.  I.e. for PIC18
;; this translates the pseudo ops QW JW CW to real assembly code, and
;; performs SAVE elimination.

;; FIXME: the real question: why not postpone all optimisations till
;; later, and have the core language be simple?


(define ((target-post! postproc-asm) word)
  (set-target-word-code! word
                         (postproc-asm (target-word-code word))))


;; Lift a macro to a function that postprocesses a list of reversed
;; assembly code, by executing the macro after pushing the next
;; instruction to the assembly state. (Note that these macros are only
;; allowed to use the 2stack state.)
(define (macro->postprocess macro [name #f])
  (define i->s instruction->string)
  (lambda (reverse-asm)
    (let next ((in-asm  (reverse reverse-asm))
               (out-asm '()))
        (if (null? in-asm)
            out-asm
            (let* ((in  (cons (car in-asm) out-asm))
                   (out (empty-ctrl->asm
                         (macro (make-state:2stack in '())))))
              (when name
                (printf "postproc: ~a\n" name)
                (for ((a (reverse in)))  (printf "\t~a\n" (i->s a))) (display "->\n")
                (for ((a (reverse out))) (printf "\t~a\n" (i->s a))) (display "\n\n"))
              (next (cdr in-asm) out))))))
                      


;; More optimizations:

;; * Serialize the code graph. Optimize jump sizes for words ending in
;;   'jw' (after dead code elimination), and eliminate jumps to the
;;   next word.

;; * jump chaining.


  