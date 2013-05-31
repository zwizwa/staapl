#lang scheme/unit

;; Primitives for the control stack extension. This supports Forth's
;; control flow words, which are implemented in terms of jumps and
;; labels.

;; A practicaly Forth compiler implemented in instatiate.ss will
;; replace the underlying jump/label mechanism with a more powerful
;; control flow analysis mechanism.

(require
 "../sig.ss"
 "../ns.ss"
 "../tools.ss"
 "../scat.ss"
 "2stack.ss"
 "../coma/macro.ss"
 "../asm/directives.ss")

(import jump^ cjump^ stack^ comp-debug^)
(export control^)
  
(patterns
 (macro)

 ;; CONTROL STACK OPS
 
 ((m-swap)          (macro-prim: ctrl> ctrl> swap >ctrl >ctrl))
 ((m-dup)           (macro-prim: ctrl>       dup  >ctrl >ctrl))
 
 ((m>)              (macro-prim: ctrl> literal))
 (([qw a]  >m)      (macro-prim: ',a >ctrl))
 (([cw a]  word>m)  (macro-prim: ',a >ctrl))


 )

;; FORTH-STYLE CONTROL FLOW

(compositions
 (macro) macro:


 ;; Mark basic block end. The label is not used, only the effect it
 ;; has is to split the basic block.

 ;; For control flow analysis we need to obtain the conditional branch
 ;; target so a basic block can have two exit paths. If each block has
 ;; only a single conditional exit, this is possible. (Maybe mark the
 ;; block as a branch block too?)
 
 (end:    make-label enter:)


 ;; All conditional code is built on a single conditonal jump
 ;; instruction that implements all machine-specific peephole
 ;; optimizations.  Here we simply add `end:' which prevents arbitrary
 ;; optimizations to erase branching code, an operation that is
 ;; generally not safe.
 (jw/f     jw/false end:) 


 ;; Mark current point and load a reference on the control stack.
 (begin>m  make-label dup >m)
 
 (if       begin>m jw/f)
 (else     begin>m jw m-swap then)

 ;; Note that whil `until' can be implemented as `not while repeat',
 ;; this does introduce some spurious labels that prevent peephole
 ;; optimization of the common `begin <cond> until' condition spinning
 ;; loop, so we implement it directly in terms of the primitive
 ;; conditonal jump.
 (until    m> jw/f)
 
 (then    m> enter:) 

 (begin   begin>m enter:)
 (again   m> jw)

 (do      begin)
 (while   if)
 (repeat  m-swap again then)
 



 ;; Note: for .. next used to have an optimization wrapping the inner
 ;; loop in a dup .. drop construct. Given the current implementation,
 ;; this is not so straightforward to implement, so it's currently
 ;; disabled. The main problem being that the composition implementing
 ;; the loop body can't be recovered easily, and a more direct code
 ;; inspection mechanism is necessary.  However, using higher order
 ;; macros, this should be fairly trivial to do directly, so I'm not
 ;; bothering right now.
 
 ;; one with drop .. save wrapped around it. this generates better
 ;; code for loops that do 'read modify write'. platform specific
 ;; needs to define for0 ... next0
                                        ; (for1  dup for0 drop)
                                        ; (next1 save next0 drop)

 ;; amb-compile will non-deterministically compile (execute) one of
 ;; the two quoted macros. each macro quotes a macro implementing
 ;; its 'next' behavirour (next just executes macro from m>).
 
                                        ; (for   (for0 (constraint:label-nodup
                                        ;               next0) >m)
                                        ;        (for1 (next1) >m)
                                        ;        amb-compile)

                                        ; (amb-compile   swap >m >m m-amb-run/s)

                                        ; (next  m> compile)

)