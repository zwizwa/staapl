#lang scheme/base

;; Compiler state
(require
 scheme/match
 "../tools.ss"
 "../target.ss"
 "../control/2stack.ss"
 "../machine/vm.ss")  ;; // for mu-lambda-struct


(provide
 state:compiler
 (struct-out compiler)
 (struct-out dict)
 (struct-out mcont)
 dict-label
 dict-terminate
 comp-print-state
 )


;; During compilation the assembly code (the result of instantiating
;; macros) is organized in the following hierarchy:

;;   * A word is a single entry point, represented by a target-word
;;     struct.  Each target-word struct is associated to a chunk A
;;     chunk is a list of consecutive assembly code instructions.
;;     Code inside a target-word struct can only be reached through a
;;     jump to its label.  This means that the target-word and its
;;     associated chunk is not observable to the world, and is
;;     completely abstracted by its label.  Therefore target-word
;;     structs serve as the unit of code generation (and
;;     recombination).  Any operation on code that doesn't alter
;;     linear code flow is legal within a chunk.

;;   * A code chain is a list of code chunks with implicit
;;     fallthrough.  Each chunk indicates a single entry point.
;;     Chains are terminated by exit points.  Chains are the unit of
;;     target address allocation: each chain can be associated to an
;;     address independent of other chains.  Some chains have a fixed
;;     address (org).
;;
;;   * The store is a set of recently constructed chains (implemented
;;     as a stack).  Chains in the store can be placed in arbitrary
;;     locations in the target's memory.


;; Summarized:
;;   - chunk  : ENTRY point
;;   - chain  : EXIT point
;;   
;; This hierarchy is necessary because Forth words can have multiple
;; entry and exit points.  A Forth word then consists of one or more
;; chains.  (A chain has a single EXIT point, but can have multiple
;; ENTRY points.)

;; Organizing it this way gives maximum flexibility: the compiler
;; implements the jump^ macro set.  On top of this the Forth-style
;; control^ is implemented, which provides efficient control flow word
;; bundled with post-processing control flow analysis.



(define-struct dict
  (current  ;; current word label
   chain    ;; list of words with fallthrough
   store))  ;; set (stack?) of fallthrough lists


;; Save code under label, but drop if there is no label, which means
;; the code is not reachable.

(define (log-dead code)
  (log:
   (format
    "dead:~a\n"
    (apply
     string-append
     (map (lambda (ins)
            (format " ~a"
                    (instruction->string ins)))
          (reverse code))))))

(define (dict-label d new-word code)
  (match d
         ((struct dict (current chain store))
          (make-dict new-word
                     (if current
                         (cons (list current code) chain)
                         (begin
                           (unless (null? code)
                             (log-dead code))
                           chain))
                     store))))


;; Terminate current fallthrough chain by moving it to the store.
(define dict-terminate
  (match-lambda
   ((struct dict (current chain store))
    (make-dict current '()
               (if (null? chain) ;; drop empty chains
                   store
                   (cons chain store))))))




;; To make it easier to convert Forth definitions that call ";" inside
;; the definition to macros, we provide a simulated local exit for
;; macros which jumps to past the macro's generated code.  This
;; requires a simulated return stack with 'macro continuations'.

;; Note that this is _not_ the control stack!  Forth's control
;; structures and the run-time return stack are independent.  This
;; serves to simulate the run-time stack, so needs a different
;; mechanism.  (FIXME: maybe the control stack can be implemented in
;; terms of this?  It looks strange to need 2 stacks for control.)

(define-struct mcont (label refs))


;; (printf "define-struct compiler\n")  ;; to check double instantiations
(define-struct (compiler 2stack)
  (dict      ;; dictionary object: keeps track of label -> code bindings
   rs        ;; 'return stack' for macros: a list of exit labels + refcount
   ext))     ;; user extension

(define update-compiler
  (case-lambda
    ((state asm)
     (update-compiler state asm
                      (2stack-ctrl-list state)))
    ((state asm ctrl) 
     (update-compiler state asm ctrl
                      (compiler-dict state)
                      (compiler-rs state)
                      (compiler-ext state)))
    ((state asm ctrl dict rs ext)
     (make-state:compiler asm ctrl dict rs ext))))

(define (make-state:compiler ctrl asm dict rs ext)
  (make-compiler update-compiler
                 ctrl asm dict rs ext))

(define (make-ext)
  ;; Representation could be abstract, but what else would we use?
  (make-immutable-hasheq '()))

(define (state:compiler [datastack '()])
  (make-state:compiler datastack
                       '()
                       (make-dict #f '() '())
                       '()
                       (make-ext)
                       ))


(define (format-chunk chunk)
  (apply string-append
         (for/list ((a (reverse chunk)))
           (format "\t~a\n" (instruction->string a)))))

(define (format-chain chain)
  (apply string-append
         (for/list ((word/chunk (reverse chain)))
           (format "~a~a"
                   (format-target-word (car word/chunk))
                   (format-chunk (cadr word/chunk))))))
(define (format-ctrl ctrl)
  (apply string-append
         (for/list ((wd ctrl))
           (if (target-word? wd)
               (format "~s\n" (target-word-name wd))
               (format "~a\n" wd)))))

(define (format-rs rs)
  (apply string-append
         (for/list ((mc rs))
           (match mc
             ((struct mcont (label refc))
              (format "~a ~a\n"
                      (target-word-name label)
                      refc))))))

(define (format-store store)
  (apply string-append (map format-chain (reverse store))))

;; State is "opened up" compared to the structure expected by
;; format-target-word, so specific printers are provided.

(define (comp-print-state s options)
  (define (maybe-display opt str)
    (when (memq opt options)
      (display str)))
  (match s
   ((struct compiler (update asm ctrl
                             (struct dict (current chain store))
                             rs
                             ext))
    (with-handlers ((void
                     (lambda (e)
                       (printf "print-state error: ~a\n" e)
                       (printf "ASM:~a\nCTRL:~a\nCURRENT:~a\nCHAIN:~a\nSTORE:~a\nRS:~a\n"
                               asm ctrl current chain store rs))))
      (maybe-display 'store (format ";; store\n~a\n" (format-store store)))
      (maybe-display 'chain (format ";; chain\n~a\n" (format-chain chain)))
      (maybe-display 'current (format ";; current\n~a\n"
                                      (and (target-word? current) (format-target-word current))))
      (maybe-display 'asm (format ";; asm\n~a\n" (format-chunk asm)))
      (maybe-display 'ctrl (format ";; ctrl\n~a\n" (format-ctrl ctrl)))
      (maybe-display 'rs (format ";; rs\n~a\n" (format-rs rs)))
      ))))


