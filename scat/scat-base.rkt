#lang racket/base

;; Primitives for the base SCAT language. This should bring the
;; language up to a point where it is useful to start writing
;; programs.


(require
 "rpn-scat.rkt"   ;; scat: macro
 ;; use SCAT rpn syntax + namespace tools..
 "scat-syntax.rkt"
 "../ns.rkt"

 ;; ..to define/snarf functionality from
 "rep.rkt"
 "../tools.rkt"
 "print.rkt"

 (lib "pretty.rkt")

 ;; with names not to be exported tucked away here, so a simple
 ;; provide statement can be used.
 "base-utils.rkt"
;;  (for-syntax  racket/base)

 )

(provide
 ;; define-word
 (all-defined-out))


;; For debugging convenience at the command line, this is a macro
;; for defining named code. It returns the value of the defined code
;; so it fits the normal 'language:' behaviour, though has a side
;; effect.

;; This is ONLY a notational shortcut to make life easier on the
;; debug repl. Don't use it in source files!

(define-syntax define-base
  (syntax-rules ()
    ((_ name . body)
     (ns (scat) (define name (scat: . body))))))


;; DEBUG

(define-word ctrace s
  (cons
   (continuation-mark-set->list
    (current-continuation-marks)
    'word) s))



;; Interpret the rest of the code body as a scheme lambda
;; expression. This allows the use of scheme-style lambda
;; expressions as quotations.

;; (define-syntax --base/lambda
;;   (syntax-rules ()
;;     ((_ (formals . body) expr)
;;      (apply (lambda formals . body) expr))))

;; Example 'parsing word'. Transfer control back to the compiler
;; after combining a datum as a quoted immediate value.

;; (define-syntax (--base/qw stx)
;;   (syntax-case stx ()
;;     ((_ (datum . code) expr)
;;      ((rpn-next)
;;       #'code
;;       ((rpn-immediate)
;;        #'(quote datum)
;;        #'expr)))))

;; Quoting word with delegate.




;;   (define-parsing-word
;;     base/qw (word)
;;     (lambda (code expr)
;;       ((rpn-immediate (stx-cons


;; Dynamic environment constructors. These construct a unary function
;; that accepts a thunk, which can be combined with 'dynamic'. This is
;; factored so the code here doesn't need to know about the state
;; abstraction / control passing mechanism.


(define-syntax define-dynamic
  (syntax-rules ()
    ((_ name fn)
     (define-word name (thing . s)
       (make-dynamic fn thing s)))
    ((_ name)
     (define-dynamic name name))))

(define (make-dynamic fn thing s)
  (cons
   (lambda (thunk)
     (fn thing thunk)) s))

(define-dynamic with-output-to-file/safe)
;; (define-dynamic with-io-device)
  

  ;; FIXME

  
  ;; PRIMITIVES

  ;; These are language primitives that can't be directly snarfed from
  ;; scheme functions. The implementation is exposed here: we map the
  ;; input argument list to an single output list value.

  
  (define-word id     s            s)
  
  (define-word stack@ s            (list* s s))
  (define-word stack! (new . s)    new)
  (define-word stack  s            (list s))
  
  (define-word drop   (a . s)      s)
  (define-word dup    (a . s)      (list*  a a  s))
  (define-word swap   (a b . s)    (list*  b a  s))
  (define-word swap3  (a b c . s)  (list*  c b a  s))
  (define-word over   (a b . s)    (list*  b a b  s))
  (define-word rot    (a b c . s)  (list*  c a b  s))
  (define-word -rot   (a b c . s)  (list*  b c a  s))

  (define-word rot4   (a b c d . s) (list* d a b c  s))
  (define-word -rot4  (a b c d . s) (list* b c d a  s))
  
  
  (define-word uncons (pair . s)
    (list* (cdr pair) (car pair) s))

  
  (define-word cons   (kdr kar . s)   (list* (cons kar kdr) s))
  (define-word append (tail head . s) (list* (append head tail) s))
  
  
;;  (define-word union         (a b . s) (list* (lset-union eq? a b) s))
;;  (define-word intersection  (a b . s) (list* (lset-intersection eq? a b) s))
;;  (define-word difference    (a b . s) (list* (lset-difference eq? b a) s))

  (define-word and  (a b . s) (list* (if (number? a) (bitwise-and a b) (and a b)) s))
  (define-word or   (a b . s) (list* (if (number? a) (bitwise-ior a b) (or a b)) s))

  (define-word not  (a . s)   (list* (not a) s))  ;; use -1 xor for bitwise
  
  
  (define-word unlist (l . s)      (foldl cons s l))
  (define-word nil    s            (cons '() s))

  (define-word read-byte   (p . s)   (cons (read-byte-timeout p 1) s))
  (define-word write-byte  (p b . s) (write-byte b p) s)

  (define-word format (fmt l . s) (list* (apply format fmt l) s))



  (define-word lex-stream (p . stack)
    (let next ((l '()))
      (let ((thing (read p)))
        (if (eof-object? thing)
            (list* (reverse l) stack)
            (next (cons thing l))))))


  ;; DELIMITED CONTINUATIONS

  ;; NOTE: Syntactic structures can violate the premise that scat code
  ;; is concatenative. However, the shift/reset macros do not process
  ;; the code stream, so won't violate this.


  (require racket/control)

  ;; Use a specific prompt tag so scat code has no access to the
  ;; scheme driver code that sets up the state threading.
  (define scat-prompt (make-continuation-prompt-tag 'scat-prompt))

;;  (ns (scat) (define-syntax reset (rpn-wrap (expr) #`(reset-at scat-prompt #,expr))))
            
;;  (ns (scat) (define-syntax shift (rpn-wrap (expr) #`(shift-at scat-prompt k #,((rpn-immediate) #'k expr)))))
     

  ;; DYNAMIC ENVIRONMENTS



  ;; SNARFS


  ;; These get functionality straight from scheme with minimal
  ;; hassle. below '+' means define word '+' using scheme's '+' and
  ;; (choose if) means, define word 'choose' using scheme's 'if'.
  
  (snarf as-push (scat)

;;     ((d t)    (dict-find dict-find/false dict-recursive-find))    
;;     ((d t i)  (dict-set dict-shadow dict-recursive-mute))


     ;; FIXME: something fishy going on with '/'
     ((a b)    (= < > >= <= + - * / modulo <<< >>>
                eq? eqv? equal?
                string-append list->table
                min max))

     ((a b)    ((xor bitwise-xor)))

     ((a)      (exp log sin cos tan 2/ 2* << sqrt))

     ;; ((path)   (ns-ls ns-ref))


     ((thing)  (symbol? number? null? procedure? string? list? pair?
                vector? eof-object? ->string))

     ((number) (round floor ceiling inexact->exact exact->inexact
               integer->char))

     ((symbol) (symbol->string))

     ((str)    (string->list string->symbol bytes->string/utf-8))
   
     ((fname)  (open-input-string open-input-file))
     ((port)   (read))     

     ((c a b)   ((choose if)))

;;     ((word)   (word-source))

     ;; For run time compilation to work, the appropriate modules need
     ;; to be loaded into the runtime compiler namespace using
     ;; 'rpn-modules'.
     
     ;; ((code compiler) (rpn-compile))
     ;; ((modules)       (rpn-modules))



     (()    (current-process-milliseconds cpm-mark))

     ((lst) (reverse flatten car caar caaar cdr cadr cddr
             list->string list->bytes eval))


     
    )

  
  ;; Side-effecting words.
  
  (snarf as-void (scat)

    ((datum)     (write display print write-tree pretty-print))
    ((s fmt)     (stack-print))
;    ((exception) ((throw raise)))
    ((filename)  (delete-file close-input-port close-output-port load))
    ((to from)   (rename-file-or-directory))

    ((word)      (print-word))
    
    )


     
  
  ;; COMPOSITE CODE

  (compositions
   (scat) scat:

   ;; (infra     reverse-infra reverse)

   ;; constants
   (true      #t)
   (false     #f)

   ;; math
   (pow       log * exp)
   
   ;; stacks & lists & vectors
   (swons     swap cons)
   (unswons   uncons swap)
   (2dup      over over)
   (nip       swap drop)
   ;; (vector    list list->vector)
   (cycle     uncons reverse cons reverse)
   (sd        stack drop)

   ;; printing
   (cr       "\n" display)
   (space    " "  display)
   (tab      "\t" display)
   (p        print space)
   (d        display space)
   (pp       pretty-print)
   
   ;; (hs       stack@ print-stack) ;; host stack

   (print-stack " ~s" stack-print)

   ;; files
   (read-file  open-input-file   lex-stream)
   (lex-line   open-input-string lex-stream)

   

   ;; CODE / DATA


   ;; (:base     'scat: rpn-compile)   ;; source -> program
    ;; (source    word-source)          ;; get source code, discard semantics
   ;; (semantics word-semantics)


   ;; MONADS

   ;; The 'lift' operation (a.k.a. the 'map' operation) is the only
   ;; one that can be made ignorant of the monad type, given that
   ;; monad state is always implemented as the top of the data stack.

   ;; The other operations: 'join' and 'return' need to be
   ;; monad-specific. Since I don't have type classes, i'm stuck with
   ;; name tagging. But, I didn't run into uses for this.


   ;; This will only work for base syntax.
   
   ;; (lift   unrun:base (dip) compose)
   

   
   
   )

  
;; Convert a list of functions to a function.
(define (scat-compose lst)
  (apply compose (reverse lst)))
  
