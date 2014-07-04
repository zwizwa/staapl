#lang racket/base
(require racket/unit
         "tools/signature-forms.rkt"
         "macro.rkt")
(provide label^)

;; FLAT TARGET CODE LABELS
;;
;; A collection of Scheme macros to create concatenative target code
;; structure.  This fits into the big picture as follows:
;;
;; * Concatenative macros (those defined with `macro:') are
;;   represented as compilation state transformers.
;;
;; * Compilation state is layered (similar to stacked Monads).  This
;;   is to represent different classes of concatenative macros,
;;   ranging from those that represent pure stack functions (such as
;;   `+'), to those that implement arbitrary target code
;;   transformations.
;;
;; * Target code is represented as a control flow graph (CFG), where
;;   basic blocks represent straight-line code and (conditional) jump
;;   instructions link to other basic blocks.
;;
;; It is important to note that the CFG is less abstract than i.e. a
;; graph of function calls.  I.e. it explicitly supports the notion of
;; ``fall through''.  This is necessary to implement flexible control
;; structures with direct machine control, without the need for an
;; optimizer to translate i.e. a graph of function calls to a CFG.
;;
;;
;; -------------------------------------------------------------------
;;
;; Implementation note:
;;
;; Because it's not possible to express macros in terms of multiple
;; unit signatures, we take an intermediate step where
;;
;; 1) a signature is defined that exports the macros, and references
;;    a separate collection of identifiers in the signature
;;
;; 2) a binding unit label-unit.rkt is defined that exports these
;;    identifiers from directly importing them from other signatures.
;;
;; Some renaming using a `label:' prefix is necessary in order to not
;; cause clashes when the original collection of signatures is
;; imported somewhere else, along side the label^ signature.



(define-signature label^ 

  ;; Identifiers used in the implementation of the macros.
  
  ;; Concatenative macros
  (label:exit        ;; to implement functions on top of CFGs
   label:allot       ;; to reserve RAM space
   label:org-begin   ;; root a CFG segment at a fixed physical address..
   label:org-end     ;; ..switch back to normal CFG generation.

   ;; Compiler support code
   label:wrap-word      ;; Post-process raw concatenative macro code for ..
   label:wrap-variable  ;; .. target words and variables.
   label:wrap-macro
   
   label:append!        ;; Append CFG segment to current segment list.



   ;; Macros

   ;; FIXME: some of these could be hidden in modules.

   ;; Common word entry form.  Each word identifier is represented by
   ;; 2 module level identifiers, one in the `target' namespace
   ;; pointing to the (still empty) CFG node and one in the `macro'
   ;; namespace to serve as a proxy for inserting target calls or
   ;; variable references.

   ;; The code generator body is appended to the global list of code
   ;; generators.  Source code order is preserved to support proper
   ;; code fall-through operation.  The functional composition of this
   ;; global list of codegens is the global code generator, which
   ;; builds the whole CFG starting from an empty compiler state.

   (define-syntax-rule (word-defs wrap name raw-macro)
     (begin
       (define-values (label wrapper codegen)
         (wrap 'name
               #f                      ;; source location
               raw-macro))
       ;; These 2 expand to `define' unless name is #f.
       (word-define (target) name label)
       (word-define (macro)  name wrapper)
       (label:append! codegen)))

   (define-syntax word-define
     (syntax-rules ()
       ((_ _ #f _) (begin)) ;; Don't define anonymous words.
       ((_ (n ...) name expr)
        (ns (n ...) (define name expr)))))
   
   ;; The most basic form translates a list of (name . code) pairs to
   ;; flat code with fallthrough.
   (define-syntax-rule (word-flat name rpn-code ...)
     (begin
       (define codegen (macro: rpn-code ...))
       (words-def label:wrap-word name codegen)))
   (define-syntax-rule (words-flat (name rpn-code ...) ...)
     (begin (word-flat name rpn-code ...) ...))

   
   
   ;; To support functions on top of flat words, the `exit' word is
   ;; appended to the flat rpn code list.
   (define-syntax-rule (word name rpn-code ...)
     (begin
       (define codegen (macro: rpn-code ... ,label:exit))
       (word-defs label:wrap-word name codegen)))
       
       
   (define-syntax-rule (words (name rpn-code ...) ...)
     (begin (word name rpn-code ...) ...))


   ;; Compile code to a fixed address.
   (define-syntax-rule (word-org-flat address rpn-code ...)
     (begin
       (define codegen
         (macro: 'address ,label:org-begin  ;; Switch to a new chain
                 rpn-code ...               ;; append the code there,
                 ,label:org-end))           ;; and switch back.
       (word-defs label:wrap-word #f codegen)))
   (define-syntax-rule (words-org-flat (addr rpn-code ...) ...)
     (begin (word-org-flat addr rpn-code ...) ...))
  
   ;; Variables are based on the `allot' compiler macro which reserves
   ;; RAM space in the compilation state.  The difference between flat
   ;; variables and non-flat ones is that the former are guaranteed to
   ;; be allocated in a contiguous area of RAM.
   
   (define-syntax-rule (variable-entry name size)
     (begin
       (define codegen (macro: size ,label:allot))
       (word-defs label:wrap-variable #f codegen)))

   (define-syntax-rule (variables-flat v ...)  (begin (variable-entry v 1) ...))
   (define-syntax-rule (2variables-flat v ...) (begin (variable-entry v 2) ...))

   ;; Currently, all variables are flat.
   
   (define-syntax-rule (variables . vs)  (variables-flat . vs))
   (define-syntax-rule (2variables . vs) (2variables-flat . vs))


   ;; Convenience rule for macros, in the same style:
   (define-syntax-rule (macros . defs) (compositions (macro) macro: . defs))
  
   ))
