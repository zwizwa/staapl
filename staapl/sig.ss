#lang scheme/base

;; This module exports signatures used to interface all components in
;; the Staapl language.


;; Components in staapl are collections of macros or "op
;; transformers".  Macros are Scat functions of type:
;;
;;   (stack-of op) -> (stack-of op)
;;
;; The these macros are implemented as scheme functions and thus not
;; to be confused with scheme syntax transformers.  In general the
;; compilation stage is shifted one level in Staapl code: target code
;; is always data in the scheme sense.
;;
;; The "op" objects represent the target machine's operations.  These
;; objects contain information that lives at macro compile time, they
;; are information embedded in the signature to allow separate
;; compilation of op transformers.



(require scheme/unit
         "op.ss"
         "ns.ss")
(provide (all-defined-out))

(define-syntax define-macro-set
  (syntax-rules (extends)
    ((_ name extends sig^ (w ...))
     (ns (macro) (define-signature name extends sig^ (w ...))))
    ((_ name (w ...))
     (ns (macro) (define-signature name (w ...))))))

(define-syntax-rule (define-scat-set name (w ...))
  (ns (scat) (define-signature name (w ...))))

(define-syntax-rule (define-op-set name (w ...))
  (begin
    (ns (op) (define-signature op^ (w ...)))
    (ns (asm) (define-signature asm^ (w ...)))
    (ns (dasm) (define-signature dasm^ (w ...)))
    (define-signature name ((open op^) (open asm^) (open dasm^)))))

;; OPCODES

;; These are signatures containing static information about the
;; opcodes.  Currently this is only the formal argument list used in
;; arity checking and error reporting.

;; FIXME: because qw is used at the very core of the macro code
;; transformer, scat needs to be re-implemented as a unit before this
;; will work.  I'll try to brin the code to a point where some
;; test-xxx.ss modules are operational so the change to scat can be
;; gradually propagated upward.

;; (define-op-signature op-quote-decl^ op-quote-impl^ ;; code/data distinction
;;   (cw val)
;;   (qw val))  

;; (define-op-signature op-jump-decl^ op-quote-impl^ ;; control flow
;;   (label l)
;;   (jw l)
;;   (jw/false l)
;;   (exit))



;; MACROS

;; The label^ set defines branch target representation (sym), branch
;; target marking (label:) and branch instructions (cw jw jw/if exit).
;; These behave as RPN assembler macros.
(define-macro-set jump^   (make-label enter: >label cw jw exit reachable))
(define-macro-set cjump^  (jw/false))
(define-macro-set ram^    (allot))

;; Forth style control words on top of the jump representation.
(define-macro-set control^ (m-swap m-dup m> >m word>m if else then begin again
                            do while repeat until end:))



;; Target code placement.
(define-macro-set org^ (org-begin org-end here))
 
;; Compiler internal interface.
(define-macro-set cfg^ (menter mleave mexit close-chain
                        word-org-begin
                        compile-macro/raw ;; Does not compile 'exit' !
                        compose-macro
                        >macro
                        semi ";" "."))

;; Basic stack manip + ALU
(define-macro-set stack^       (dup swap drop + - * / not))
(define-macro-set code^        (compose i execute compile nop))
(define-macro-set rstack^      (for next >r r> swap>r r- +r rdrop rl rh))

;; Compilation and manipulation of binary lists.  Note that `length'
;; and `cons' might need a rename.
(define-macro-set comma-extra^ (dw> "bin," sym>bin l:length l:cons))
(define-macro-set comma^       ("," byte-slots))


;; Machine parameters used to parameterize the CFG compiler.
(define-macro-set machine^  (code-size address))

;; PIC18 specific.  Move more of these to standard wordsets.
(define-macro-set stack-extra^ (and or xor pow >>> <<< 1+ 1- min max odd?
                                toggle neg >> << 2/ over nip))
(define-macro-set memory-extra^ (1-! 1+! swap! ! @ high low high? low? +! -!
                                 @! or! and! xor!))


;; COMPILER
(define-macro-set comp-debug^ (.cs))
(define-signature instantiate^
  (;; Compiler invokation.
   compile-words

   ;; These functions apply appropriate wrappers before
   ;; the objects are defined in the namespace.
   wrap-variable
   wrap-word
   wrap-macro

   ))


;; Code postprocessing state->state function.  This function is handed
;; the full compilation state to provide any postprocessing such as
;; optimizations and the elimination of pseudo ops, before the code is
;; handed to the assembler.
(define-signature postproc^ (postproc))

;; Access to the extended compiler state.
(define-signature state-tag^ (state-tag-label state-tag-ref state-tag-set))

;; Shallow Co-routines (SCR)
(define-macro-set scr^ (scr-begin scr-reg scr-yield scr-var))

;; This is the top-level "go" for the compiler.
(define-signature compiler^ (compile!))

