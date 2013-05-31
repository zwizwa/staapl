#lang scheme/base

;; The assembler is a destructive operation on a source code tree,
;; which:
;;   * allocates target addresses
;;   * maps primitive symbolic expressions -> binary
;;   * evaluates delayed expressions depending on target addresses

;; The assembler does not perform
;;   * code linking or serialization (input = linked graph's node list)
;;   * dead code elimination (all input is assembled)
;;   * assembly at different orgs

;; Data type dependencies
;;   * word representation struct (to set the word address)
;;   * meta evaluation struct (to evaluate delayed expressions)

(require
 scheme/control
 scheme/match

 "../target.ss"
 "../tools.ss"
 "../op.ss"
 "../ns.ss"

 ;; dynamic environment for assembler:
 "pointers.ss"
 "environ.ss"
)

(provide
 assemble!        ;; assemble a target-word graph
 assemble-simple  ;; non-destructive version, input = list

 )

;; For the imperative algos, use a hash table data structure.
(define get          hash-ref)
(define put!         hash-set!)
(define table        alist->hash)
(define table->alist hash->alist)


;; Side-effect free and referentially transparent. Maps reversed
;; assembly code to reversed binary code.
(define (assemble-simple asm-code . args)
  (let-values
      (((bin pointers)
        (apply assemble!
               (list (new-target-word #:name 'test
                                      #:realm 'code
                                      #:code asm-code))
               args)))
    (values (car bin)
            pointers)))

;; Assembler error handler. This catches all possible errors that
;; occur inside an assembler function.

(define (proto->asm-error-handler asm arguments)
  (match-lambda*
   ((list 'overflow type value bits)
    (error 'asm-overflow
           "~a overflow error in ~a at ~a (~a doesnt fit in ~a bits)"
           (cond
            ((eq? type 1) 'unsigned)
            ((eq? type -1) 'signed)
            (else 'unknown-type))
           (instruction->string (asm-current-instruction))
           (pointer-get 'code)
           value bits))))




;; Assembler top driver: keeps track of all state necessary to perform
;; address allocation and implement the relaxation
;; algorithm.

;; word-chains is a list of head words with internal fallthrough

;; Note that ALL code is stored as reversed list, so sequencing the
;; assembly requires reversing those lists.

(define (assemble! word-chains   
                   [pointers
                    '((code 0)
                      (data 0))])
  ;; TABLES

  (define all-words
    (apply append (map (lambda (w)
                         (reverse (target-chain->list w)))
                       word-chains)))

  ;; Current memory address pointers.
  (define *pointers* #f)
  (define (init-pointers!)
    (set! *pointers*
          (table pointers)))

  ;; Instruction span table to track size changes.
  (define *spans*
    (table
     (map (lambda (w)
            (cons w
                  (map (lambda (ins) 0)
                       (target-word-code w))))
          all-words)))
    
  ;; Address table to track address changes.
  (define *addresses*
    (table
     (map (lambda (w)
            (cons w (target-word-address w)))
          all-words)))


  ;; ERROR REPORTING
  (define (current-src-location)
    (or (target-word->error-string (asm-current-word))
        (target-word->error-string (asm-current-chain))
        ""))

  ;; FIXME: combine with proto->asm-error-handler
  (define (report-error ex)
    ;; At this point the datastructure is not in a consistent state:
    ;; addresses are recorded in place, while binary code is
    ;; accumulated outside and not present in the structure yet. Strip
    ;; off all addresses before printing.
    (let ((chain (asm-current-chain)))
      (for-each
       (lambda (w) (set-target-word-address! w #f))
       (target-chain->list chain))
      (printf "~a\n" (current-src-location))
      (print-target-word chain)
      (raise ex)))

    
  
  ;; CODE
    
  ;; Record current addresses and check if they changed.
  (define (relaxed?!)
    (define relaxed #t)
    (define (also! p)
      (set! relaxed (and relaxed p)))
    (for-each
     (lambda (word)
       (let ((now  (target-word-address word))
             (last (get *addresses* word
                        (lambda ()
                          (error 'undefined-word "~a"
                                 (target-word-name word))))))
         (put! *addresses* word now)
         (also! (and last now (= last now)))))
     all-words)
    relaxed)

  ;; Assemble a single word, keeping track of instruction spans.
  (define *nop* (nop)) ;; eval once per assemble!
  (define (asm-single-word! word)
    (parameterize ((asm-current-word word))
      (let* ((realm (target-word-realm word))
             (addr (pointer-get realm)))
        (set-target-word-address! word addr)
        ;; (printf "addr: ~a ~a\n" realm addr)
        (let next
            ((span   (reverse (get *spans* word)))
             (asm    (reverse (target-word-code word)))
             (bin   '())
             (span+ '()))
          (if (null? asm)
              (begin
                (put! *spans* word span+)
                bin)
              (let
                  ((padded
                    (with-handlers
                        ((void report-error))
                      (assemble/pad (car asm)
                                    (car span)
                                    *nop*))))
                (next
                 (cdr span)
                 (cdr asm)
                 (cons padded bin)
                 (cons (length padded) span+))))))))
    
  ;; Assemble a word chain. Each word chain can have a per-chain
  ;; directive, currently only used for setting code location.
  (define (asm-word-chain! w)
    (parameterize ((asm-current-chain w))
      (let* ((name (target-word-name w))
             (org  target-value->number)
             (go!  (lambda ()
                     (map asm-single-word!
                          (reverse (target-chain->list w))))))
        (match name
               ((list 'org addr)
                ;; (print-target-word w)
                (with-pointer 'code (org addr) go!))
               (_ (go!))))))
    
  ;; Run a single assembly pass over all word chains.
  (define (asm-pass!)
    (init-pointers!)
    (parameterize
        ((asm-pointers *pointers*))
      (apply append
             ;; Map runs in order, but chains are stored backward.
             (reverse
              (map asm-word-chain!
                   (reverse word-chains))))))

  ;; Run until relaxed.
  (define (asm!)
    (parameterize ((asm-phase 0))
      (let ((bin
             (let next ((b (asm-pass!)))
               ;; (printf "END PASS ~a\n" n)
               (when (> (asm-phase) 100)
                 ;; If this triggers, there's probably a bug in the relaxation.
                 (error 'asm-relax-loop))
               (if (not (relaxed?!))
                   (begin
                     (asm-phase (add1 (asm-phase)))
                     (next (asm-pass!)))
                   (map (lambda (inss word)
                          (set-target-word-bin! word inss)
                          (apply append inss)) ;; flatten
                        b all-words))))
            (pointers
             (table->alist *pointers*)))
        (values bin pointers))))

  ;;(printf "~a\n" pointers)
  
  ;; go
  (asm!))


;; Assemble an instruction + handle padding in case the current
;; phase's result produces a smaller instruction.

;; If evaluation fails due to unresolved target words, a 0-word
;; placeholder is inserted.

(define (assemble/pad ins span nop)
  (parameterize
      ((asm-current-instruction ins)) ;; for logging
    (let pad ((i
               (or (target-value-catch-undefined
                    (lambda () (apply resolve/assemble ins)))
                   '())))
      (if (< (length i) span)
          (pad (cons nop i))
          i))))

;; Resolve name + arguments and call core assembler for op.
(define (resolve/assemble opcode . arguments)
  (unless (asm? opcode)
    (error 'invalid-instruction
           "~a" (cons opcode arguments)))
  ;; (printf "~a ~a\n" opcode arguments)
  (let*
      ((asm
        (or (asm-fn opcode)
            (error 'virtual-op "~a" (asm-name opcode))))
       (args
        (map
;;          (lambda (arg)
;;            (target-value->number
;;             arg
;;             (lambda _
;;               (error 'ill-specified-argument
;;                      "~a ~a" arg (cons opcode arguments)))))
         target-value->number
         arguments))
       (code
        (parameterize
            ((asm-error
              (proto->asm-error-handler asm args)))
          (reverse ;; assembler functions return proper lists. we use reverse lists.
           (apply asm
                  (pointer-get 'code)
                  args))
          )))
    (pointer-allot! 'code (length code))
    code))

(define (nop) 0)  ;; FIXME!!
;;  (car (resolve/assemble 'nop))
;;  (asm: nop))



;; NOTES
;; -----


;; It is assumed that the assembler can handle delayed values
;; (meta.ss). These can eventually reference the word structures
;; presented to the assembler input, and their resolution depends on
;; the assembler resolving addresses. This means that we can't
;; generate TEXT output to feed to an assembler, unless there is a way
;; to compile the meta code to the expression evaluator of the
;; assembler, or run the assembler itself multiple times.


;; Olin Shivers about T
;; http://www.paulgraham.com/thist.html
;;
;;   "Norman Adams turned his assembler into a master's degree. It
;;   also was a cool piece of software. His assembler didn't take a
;;   linear text stream; the compiler handed it a *graph
;;   structure*. It serialised the graph on its own to minimise the
;;   spans of the jump instructions, and had other neat features
;;   (e.g., it was actually a portable framework for building
;;   assemblers)."


