#lang racket/base

;; Code representation is independent of the compiler.
(require
 racket/control
 racket/serialize
 racket/match
 "../op.rkt"
 "../tools.rkt")


(provide

 ;; printing
 print-target-word
 format-target-word
 target-print-word-bytes      ;; code size for printing
 target-print-address-bits   ;; address width for printing

 
 new-target-word
 word?->name
 instruction->string
 (struct-out target-word)
 target-word->error-string

 ;; expression evaluation
 target-value-delay   ;; create generic delayed meta expressions
 target-value->number ;; evaluate delayed meta expressions
 target-value-eval
 target-value-source
 
 target-value-abort   ;; used in constants.rkt to indicate undefined values
 
 (struct-out target-value)

 target-value-catch-undefined   ;; used by the assembler to catch assembly
                        ;; aborts due to unresolved addresses.

 target-chain->list    ;; unpack multiple entry points into a list
 ;;target-chain->binary  ;; get all binary code in a list

 target-chains->bin    ;; convert all code in a code chain to 'bin' datastructure

 target-value-equal?
 )

;; A forth word is an instantiated macro. It is represented by 2 parts:
;;  * a code word structure.
;;  * a macro which compiles a reference to the code word
 
;; If code is a list, it represents assembly code. If it is a
;; procedure, it represents code in postponed form (macro to by
;; applied to empty state).

(define-serializable-struct target-word
  (name realm code srcloc address bin next postponed)
  #:mutable)




(define (new-target-word #:name      [name (void)]
                         #:realm     [realm 'code]
                         #:code      [code #f]
                         #:srcloc    [srcloc #f]
                         #:address   [address #f]
                         #:bin       [bin #f]
                         #:next      [next #f]
                         #:postponed [postponed #f]
                         )
  (make-target-word name
                    realm code
                    srcloc address
                    bin next postponed))

;; Target words have internal link structure: if they do not end in a
;; jump instruction, they fall through to the next one. This function
;; unlinks a word into a list of words in compilation order (reversed,
;; so it can act as an 'append' list).

(define (target-chain->list word [l '()])
  (let ((next (target-word-next word))
        (l+ (cons word l)))
    (if next
        (target-chain->list next l+)
        l+)))


;; Convert a word chain to an ordered list of machine words.

(define (target-chains->bin chain-list [realm 'code])
  (prompt
   (bin-flatten
    (map
     (lambda (c)
       (list
        (target-word-address c) 
        (reverse
         (flatten
          (map (lambda (word)
                 (let ((bin (target-word-bin word)))
                   (or bin (abort #f))))
               (target-chain->list c))))))
     (reverse
      (filter
       (lambda (w)
         (eq? (target-word-realm w) realm))
       chain-list))))))



;; TARGET WORD EXPRESSION EVALUATION

;; Abstraction to create and evaluate computations depending on
;; (numerical) target word addresses.

;; Evaluation of these computations is directed by the assembler,
;; which will catch aborts due to unavailability of addresses during a
;; certain phase and will restart computations until all words are
;; defined and the machine code is relaxed.

;; Each target value contains a thunks that computes the value, and a
;; symbolic representation of the expression the value-thunk computes.

(define-struct target-value (thunk source))

(define target-value-tag
  (make-continuation-prompt-tag 'meta))
(define (target-value-abort)
  (abort-current-continuation
   target-value-tag (lambda () #f)))
(define (target-value-catch-undefined thunk)
  (prompt-at target-value-tag (thunk)))


;; Check if two target values are equal.
(define (target-value-equal? a b)
  (equal? (target-value-eval a)
          (target-value-eval b)))


;; Undefined words will abort. This is internal: used only to
;; recursively evaluate target-value references.
(define (target-value-eval expr)
  (cond
   ((target-value? expr) ((target-value-thunk expr)))
   ((target-word? expr)  (or (target-word-address expr)
                             (target-value-abort)))
   (else expr)))

;; Evaluate the thunks
(define (target-value->number
         expr
         [e (lambda (n)
              (error 'target-value-type-error
                     "not a number: ~a" n))])
  (let ((n (target-value-eval expr)))
    (unless (number? n) (e n))
    n))

(define-syntax target-value-delay
  (syntax-rules ()
    ((_ e1 e2) (make-target-value (lambda () e1) e2))
    ((_ expr)  (target-value-delay expr #f))
    ))


;; FORMATTING

(define (target-word->error-string w)
  (let ((s (target-word-srcloc w)))
    (and s
         (apply
          (lambda (file line column position span)
            (format "~a:~a:~a: ~a"
                    file line column
                    (target-word-name w)))
          s))))

(define (word?->name r)
  (if (not (target-word? r)) r
      (target-word-name r)))

;; Set reasonable defaults for address printing.
(define target-print-address-bits  (make-parameter 16))
(define target-print-word-bytes    (make-parameter 2))

(define (instruction->string ins [term ""])
  (if (not (list? ins))
      (format "~a~a" ins term)
      (let ((asm (car ins))
            (args (map
                   (lambda (x)
                     (cond
                      ((target-value? x) (target-value-source x))
                      ((target-word? x) (target-word-name x))
                      (else x)))
                   (cdr ins))))
        (format "[~a~a]~a"
                (asm-name asm)
                (if (null? args)
                    ""
                    (apply string-append
                           (map (lambda (x)
                                  (format " ~a" ;; (if (number? x) " #x~x" " ~s")
                                          x))
                                  
                                args)))
                term))))

(define (format-target-word w)
  (define port (open-output-string))
  (print-target-word w port)
  (get-output-string port))

  

(define (print-target-word word (port (current-output-port)))
  (for-each (lambda (w)
              (when (target-word-name w) ;; don't print unreachable code
                (print-target-word-head w port)))
            (reverse
             (target-chain->list word))))

;; There are 2 modes: with binary and without. If a word has an
;; address, it is assumed that it also has binary code.
(define (print-target-word-head word
                                port
                                [addr-conv
                                 (lambda (x) (* x (target-print-word-bytes)))])
  (let* ((name (target-word-name word))
         (addr (target-word-address word))
         (bin  (and addr (reverse (target-word-bin word))))
         (code (map instruction->string
                    (reverse
                     (or (target-word-code word)  ;; during compilation this is #f
                         '())
                     ))))
    (print-target-word-internal name port addr bin code addr-conv)))

  
(define (print-target-word-internal
         name port addr bin code addr-conv)
  ;; Ugly code for pretty output..

  (define w->s  word->string)
  (define (a->s x) (string->symbol (hex->string (/ (target-print-address-bits) 4) x)))
  (define (hex x)
    (cond
     ((number? x) (w->s x))
     ((list? x)   (apply string-append
                         (map (lambda (y)
                                (format "~a " (w->s y)))
                              x)))
     (else "")))
  (parameterize ((current-output-port port))
    (let ((name-sym
            (match
             name
             ((list _ v) (a->s (addr-conv (target-value->number v))))
             (sym sym))))
      (when (symbol? name-sym)
        (printf
         (if (and
              ((string-length (symbol->string name-sym)) . < . 7)
              (not (null? code)))
             "~a:"
             "~a:\n")
         name-sym)))
  
    (unless (null? code)
        ;; (printf "\t<not compiled>\n")
        (let next ((a addr)
                   (b bin)
                   (c code))
          (unless (null? c)
            (display "\t")
            (when a
              (printf "~a " (hex a))        ;; raw address
              (printf "~a ~a"
                      (hex (addr-conv a))   ;; converted address
                      (hex (car b))))
            (printf "~a\n" (car c))
            (next (and a (+ (length (car b)) a))
                  (and a (cdr b))
                  (cdr c)))))))


