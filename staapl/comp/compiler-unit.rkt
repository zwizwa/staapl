#lang racket/unit

;; Build structured assembly code graph from forth code. This uses an
;; extension to scat's 2-stack model to represent concatenative macros
;; with a Forth-style control stack.

;; The simple idea behind the compiler is to delimit basic blocks by
;; identifying _entry_ and _exit_ points.  Each chunk is an entry
;; point (a basic block in SSA parlance), while a chain represents a
;; list of contiguous chunks with an exit point at the end.

;;   chunk chunk chunk exit   chunk chunk exit
;;   ----------------------   ----------------
;;           chain                  chain

;; Compared to SSA:
;;
;;  - A chunk is +- a basic block.
;;
;;  - A chain is a linear succession of basic blocks, terminated by a
;;    jump.  The links between the chains are not jumps, but
;;    explicitly specified as code fallthrough.
;;
;;  - Conditional exits *do not* create basic block boundaries as they
;;    do in SSA.
;;
;; So different from SSA, the code is more strictly specified in its
;; linear structure.  This might prevent some optimizations, but it
;; helps keep a closer correspondence between Forth and machine code.



(require
 "state.rkt"
 racket/match
 "../op.rkt"
 "../asm/directives.rkt"
 "../tools.rkt"
 "../control/2stack.rkt" 
 "../scat.rkt"      ;; for make-word
 "../target.rkt"
 "../coma/macro.rkt"
 
 "postprocess.rkt"
 "../machine/vm.rkt"
 racket/pretty
 )


(import machine^ stack^ code^ control^)
(export jump^ cfg^ org^ instantiate^ ram^ comp-debug^ state-tag^)

;; The compiler provides instantiation for the 'forth-begin macro,
;; which uses Forth style syntax to define macros, variables and
;; compiled words.


;; Implement the jump^ signature using the control flow graph compiler.


(define-virtual-ops
  (jw word)
  (exit)
  )

;; LABELS

;; Target labels are target-word structs (tagged chunks).  See
;; state.ss for more info on the chain/chunk terminology.
(define (make-target-label [name (next-label)])
  (new-target-word #:name name))

(patterns
 (macro)

 ;; Redefine the stubs from control.ss to introduce labels used to
 ;; construct structured code graphs.

 ;; FIXME: maybe label generation should be local compiler store also?
 
 ((make-label)        ([qw (make-target-label)]))
 (([qw name] >label)  ([qw (make-target-label name)]))
 (([qw label] enter:) (entry-point label))

 ;; This is for code that's reachable through external means,
 ;; i.e. jump tables.
 ((reachable)         (entry-point (make-target-label)))


  

 )


;; CALL
(patterns
 (macro)

 (([qw label] cw)        ([cw label]))
;; (([qw label] jw)        ([jw label]))
;; (([qw label] jw/false)  ([jw/false label]))

 )

(compositions
 (macro) macro:
 (jw    cw exit))



;; MEXIT

;; Check if the macro is properly terminated (if ";" was called at
;; the end) by looking at the last instruction and the current exit
;; label.
;; (define (terminated? asm exit-label)
;;   (match asm
;;          ((list-rest [list (? (ns (op ?) jw)) label] _)
;;           (eq? label exit-label))
;;          (else #f)))

;; The ";" word inspects the macro return stack.  If there's context,
;; execute mexit.  Otherwise we're in straight line code and can
;; execute procedure-exit.
(define (semi state)
  (if (null? (compiler-rs state))
      ((ns (macro) procedure-exit) state)
      ((ns (macro) mexit) state)))


;; State update words in terms of the machine/vm macros.
(define-syntax-rule (state-update form)
  (make-word
   (mu-lambda-struct
    (compiler
     ;; The arguments are the names to use for the fields.  These do
     ;; not correspond to the original field names, but we keep them
     ;; the same.
     (update
      asm ctrl dict rs ext))
    form)))

(define jw? (ns (op ?) jw))
(define jw  (ns (op asm) jw))
(define qw? (ns (op ?) qw))
(define qw  (ns (op asm) qw))



;; The 'split' state update function: save the word currently being
;; compiled on the word stack (dictionary) and continue with an empty
;; assembly stack and a new current word.
(define (entry-point new-word)
   (state-update
    ((asm  -> '())
     (dict -> (dict-label dict new-word asm)))))



(compositions
 (macro) macro:

 ;; comp-debug^
 (.cs ,(lambda (state)
         (printf "** compiler state **\n")
         (comp-print-state state)
         state))
           
 
 (>enter:   >label enter:)

 ;; MEXIT

 ;; Push new label + reference count to the return stack.   
 (menter ,(state-update
           ((rs -> (cons (make-mcont (make-target-label) 0) rs)))))

 ;; Compile local exit + increment refcount.
 (mexit ,(state-update
          ((asm :  asm
                -> (cons [list jw label] asm))
           (rs  :  (cons (struct mcont (label refs)) rs+)
                -> (cons (make-mcont label (+ 1 refs)) rs+)))))

 ;; The ';' that terminates the macro can be removed.
 (mleave ,(state-update
           ((rs  : (cons (struct mcont (exit-label refs)) rs+) -> rs+)
            (asm : (cons instruction asm+)
                 -> (match
                     instruction
                     ;; Remove the last to the exit-label and
                     ;; leave a split continuation if there are
                     ;; more references.
                     
                     ([list (? jw?)
                            (? (lambda (x) (eq? exit-label x)))]
                      (cons [list qw ;; save cont.
                                  (if (> refs 1)
                                      (entry-point exit-label)
                                      (macro nop))]
                            asm+))
                     (else
                      (error 'non-terminated-macro))))))
         i) ;; run cont.
 

 ;; CHUNK/CHAIN management
 

 ;; Closing a chain effecitively marks unreachable code.
 (close-chain ,(entry-point #f) ;; close current asm
              ,(state-update ((dict -> (dict-terminate dict))))) ;; move chain to store

 ;; Move compilation state to and from the control stack to compile
 ;; code in a fresh chain somewhere not related to the current state.
 ;; This is probably followed by an 'org' label.

 ;; FIXME: this is simpler expressed as a primitive which exchanges
 ;; the current state with an object on one of the stacks.  It's like
 ;; a coroutine yield stack switch.
 
 (begin-chain ,(state-update
                ((asm  -> '())
                 (rs   -> '())
                 (ctrl -> (cons (list asm rs dict) ctrl))
                 (dict -> (make-dict #f '() '())))))

 (end-chain    close-chain
               ,(state-update
                 ((asm  : '() -> saved-asm)
                  (rs   : '() -> saved-rs)
                  (ctrl : (cons (list saved-asm saved-rs
                                      (struct dict (saved-current
                                                    saved-chain
                                                    saved-store)))
                                saved-ctrl)
                        -> saved-ctrl)
                  (dict : (struct dict (#f '() store))
                        -> (make-dict saved-current
                                      saved-chain
                                      (append
                                       (combine-chains store) ;; (*) 
                                       saved-store))))))

 ;; (*) The reason we combine here is is that org-push / org-pop
 ;; _guarantees_ the code will be compiled consecutively at a physical
 ;; address without re-arrangement by the optimizer. 


 )





;; Return 3 values:
;;   * a target word struct (label)
;;   * a referring macro 
;;   * a body code generator macro

(define (tagged tag name fn)
  (lambda args
    (printf "~a :: ~a\n" tag name)
    (apply fn args)))

(define (wrap-word name loc macro)
  (let ((label (new-target-word #:name name
                                #:realm 'code
                                #:srcloc loc
                                #:postponed macro)))
    (values label
            (macro-prim: ',label compile)
            (macro: ,(entry-point label)
                    ,macro))))

(define (wrap-variable name loc macro)
  (let ((label (new-target-word #:name name
                                #:realm 'data
                                #:srcloc loc)))
    (values label
            (macro-prim: ',label literal)
            (macro: ,(entry-point label)
                    ,macro
                    close-chain ;; FIXME: data variables are not
                                ;; chained. This should be: data variables
                                ;; are not chained with code chunks.
                    ))))

;; Macro representations need to be wrapped to implement 'mexit',
;; which jumps past the end of the code generated by macro.

;; For languages that do not use mexit, do not wrap the macro, as
;; this requires at least one call to mexit at the end.  FIXME: this
;; is why Forth-style macros always need ";".

(define (wrap-macro name loc macro)
  (make-word
   (lambda (state)
     (or
      ((macro: menter ,macro mleave) state)
      (match loc
             ((list file l c p s)
              (error
               'non-terminated-macro
               "~a:~a:~a: ~a"
               file l c name)))))))



(define (org-label tag address)
  (macro: ',(list tag address) >enter:))

;; Combine all chains in store to one chain.  This might be necessary
;; when there is extra information determining that the chains have
;; falltrough.


(define (combine-chains store)
  (let ((combined (apply append store)))
    ;; (print-target-word combined)
    ;; (pretty-print combined)
    (list combined)))



(patterns
 (macro)
 ;; EXIT

 (([cw word] procedure-exit) ([jw word]))
 ((procedure-exit)           ([exit]))
 
 ((exit)              (macro: procedure-exit close-chain))

 ;; Semicolon either compiles local macro exit (mexit) within macro
 ;; instantiation, or a procedure-exit. A dot is an 'exit' that doesnt
 ;; close the chain.  It means the code after the exit is
 ;; reachable. Mainly useful for jump tables using 'route'.

 ((semi)              semi)
 ((";")               semi)
 ((".")               (macro: procedure-exit))



 ;; ORG
 
 ;; Since the compiler has no access to code addresses, handling a
 ;; physical address specification needs to be postponed to the
 ;; assembly phase in the form of an instruction encoded in the label.

 (([qw address] word-org-begin) (macro: begin-chain ',`(org ,address) >enter:))
 ((org-end)                     (macro: end-chain))


 ;; Compiling words.

 ;; Like ORG, but for words accessible by label.  This will also join
 ;; all the chains together.  Not sure if that's necssary though..
 ;; This interface is not exposed.  Look at compile-macro instead.
 
 (([qw label] label-org-begin)  (macro: begin-chain ',label enter:))
 
 ;; Wrap quoted item as macro.
 (([qw v] >macro) ([qw (macro: ',v)]))

 ;; Compose two macros.
 (([qw m1] [qw m2] compose-macro)
  ([qw (macro: ,m1 ,m2)]))
 
 ;; Compile a macro as a word somewhere else.  Note this will NOT
 ;; compile an 'exit'.
 (([qw label] [qw macro] compile-macro/raw)
  (macro: ',label label-org-begin
          ,macro
          org-end))
 

 ;; RAM

 ;; While the rest of this file is about code graph construction,
 ;; there's also data to allocate.  This is done with a link to the
 ;; assembler: macros that execute in the assembler context can modify
 ;; its internal state.  (see asm/directives.ss)
 
 ;; (([qw realm] [qw n] allot) ([allot realm n]))
 (([qw n] allot) ([allot-data n]))
 ((here)         ([here]))

 
 )
(compositions
 (macro) macro:

 (org-begin  code-size / word-org-begin)

 )




;; 'compile-forth' sets up the internal state for compilation of
;; Forth-like functionality. Compared to ordinary 'macro->code' which
;; operates only on the parameter stack and an extra assembly stack,
;; Forth code has access to:

;;  * labels = entry points of target code chains
;;  * fallthrough words
;;  * macro return stack (local exit in macros)

;; This code needs to be passed to target-postprocess! which will
;; turn it into a linked graph and perform post-processing
;; optimizations.

(define (compile-forth macro)
  ;; Execute macro on empty state + terminate properly.
  (let ((state
         ((macro: ,macro close-chain)
          (state:compiler))))

    ;; Type check (macros are not allowed to change the state type!)
    ;; and require empty compilation state.
    (unless (compiler? state)
      (error 'compile-state-type-error))

    ;; (comp-print-state state)
    (assert-empty-ctrl state)

    ;; Return the dict-store list. This is then passed to
    ;; target-post! and onward to the assembler.
    (dict-store (compiler-dict state))))




;; Project word structs to the compiled state.
(define (compile-words words)
  (append
   ;; compile one big macro from all procedures
   (compile-forth
    (apply compose
           (filter procedure?
                   (cons
                    (lambda (x) x) ;; sentinel
                    words))))
   ;; copy non-procedures (already compiled)
   (filter (lambda (x)
             (not (procedure? x)))
           words)))



;; Access to the extended compiler state.  Modules can create a
;; private tag which allows storage of state in the ext slot's
;; functional hash table.


(define-struct state-tag (name))

;; Create a unique tag.  Name is optional and is not used for
;; addressing.
(define (state-tag-label [name #f])
  (make-state-tag name))

(define (state-tag-ref state tag)
  (hash-ref (compiler-ext state) tag (lambda () #f)))

(define (state-tag-set tag v)
  (state-update
   ((ext -> (hash-set ext tag v)))))
 

