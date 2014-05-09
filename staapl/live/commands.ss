#lang scheme/base

;; Live interaction and parsing words.  This glues together target
;; word execution, macro simulation and scat/scheme procedure
;; execution.

(require
 scheme/match
 scheme/control
 "../tools.ss"
 "../scat.ss"
 "../rpn.ss"
 "../ns.ss"

 "reflection.ss"
 "tethered.ss"
 "../target.ss"
 "../code.ss"
 "rpn-live.ss"
 
 (for-syntax
  "../forth/forth-tx.ss"
  scheme/base))

(provide (all-defined-out))

;; LOWLEVEL PARSERS
;; Pass the rest of the input to a prefix parser.
(ns (target) (define-syntax slurp rpn-slurp))


;; (scat) utility functions
(compositions (scat) live:
              
 ;; target I/O
 (hilo> swap 8 <<< or)
 (>hilo dup 8 <<< swap #xFF and)

 ;; Printing words from the stack.  These scat words are wrapped later
 ;; using 1cmd to take their argument from the target stack.
 (ps        8 sign-extend p)
 (px        byte->string d)
 
 ;; DOUBLE WORD EXTENSIONS
 (_ps       16 sign-extend p)
 (_px       word->string d)
 (_p        p)

 )

;; The (host) namespace contains simulated words that run on host,
;; taking precedence over anything defined in (target) or (macro).
;; These words are never overridden by any target/macro definition and
;; are safe for target->host RPC calls.
(define-syntax-rule (host-words . defs) (compositions (host) live: . defs))

(host-words
 ;; Memory access is never overridden by target implementation. FIXME:
 ;; why is this?  The 'access-bank functionality can probably be
 ;; implemented as a concatenative macro.
 (@      t> access-bank t@ >t)
 (!      t> t> swap access-bank t!)
 (|.|    t> p))

(define-syntax-rule (1cmd:  cmd ...) (host-words (cmd t> cmd) ...))
(define-syntax-rule (_1cmd: cmd ...) (host-words (cmd t> t> hilo> cmd) ...))

(1cmd:  kb a! f! abd fbd bd p px ps pc erase-block erase-from-block client target!)
(_1cmd: _p _px _ps)
 


;; Primitive prefix parsers expand to inline (live) code.

;; Note that the use of these should be limited, because they need a
;; separate, ad-hoc composition mechanism `prefix-parsers'.  However,
;; they are useful for quoting data that can't go on the target stack,
;; and handing it over to host code.

(prefix-parsers/meta
 (target) live:

 ((see w)  ('w tsee))
 ((msee w) ('w msee))
 ((vsee w) ('w vsee))

 ((sea w)  ('w print-tword)) ;; sea = see assembly
 
 ((help w)    ('w print-doc))
;; ((load w)    ('w forth-load))
 
 ((start w)   ('w target-find-code tstart/b))
 ((|'| w)     ('w target-find-code  _>t))          ;; FIXME: depend on architecture!
 ((dump w)    ('w target-find-code f! fdump))
 
 ((plot n)    ('n plot))
 ((2plot n)   ('n 2plot))
 

 )

;; Used to be just read-eval-print-loop, but a prompt is nice to have.
;; Explicit (exit) is necessary to leave scheme mode.
(define (racket) (scheme))
(define (scheme)
  (display "Entering Racket REPL.  CTRL-D to exit.\n")
  (let loop ()
    (prompt
     (with-handlers
         ((void
           (lambda (ex)
             (display "ERROR:\n")
             (display ex)
             (loop))))
       (read-eval-print-loop)))))

;; Side effect wrapper.
(define-syntax-rule (!: e ...)
  (lambda (state)
    (begin e ...)
    state))

;; Log interactive state.  Because it's impossible to serialize the
;; the state built up during interactive compilation, we provide a way
;; to log the subset of the evaluations that lead to definitions, so
;; they can be replayed on next start.

(define eval-log-file
  (make-parameter "staapl.log"))

;; The eval-log records scheme expressions because they are validated
;; as proper code.  This thanslates back to Forth syntax.

;; Note that currently we save scheme syntax (= exactly what is passed
;; to `eval') since it's most correct.  I'm not sure if this unparsing
;; actually covers all cases, so right now it is for illustration
;; purpose only.

;; To write out code, postfix needs to be '(forth) to make sure that
;; after every line we switch back to forth mode, which is what the
;; command interpreter does.

(define (format-forth-expr words [postfix '()])
  (let ((ws (append words postfix))) ;; switch back to forth mode
    (apply string-append
           (for/list ((w ws))
             (format "~a " w)))))

(define (replay [file (eval-log-file)])
  ;; (printf "replaying definitions from ~a\n" file)
  (with-input-from-file file
    (lambda ()
      (let next ()
        (let ((expr (read)))
          (unless (eof-object? expr)
            (printf "~a\n" (format-forth-expr expr))
            (eval-forth expr #f)
            (next))))))
  (commit))
      
;; We use the forth-begin macro forth parser, but allow our user to
;; specify an init string, i.e. '(library "pic18").  Currently this is
;; attached to the .dict file by staaplc.ss
(define forth-begin-prefix (make-parameter '()))

(define (eval-forth forth-expr [log #t])
  ;; Eval before logging in case there is an error.
  (eval `(forth-begin ,@(forth-begin-prefix) ,@forth-expr))
   
  (when log
    (with-output-to-file
        (eval-log-file)
      (lambda () (write forth-expr) (newline))
      #:exists 'append)))
  



(prefix-parsers
 (target)

 ;; Compile code from console + commit.
 ((inline-code (c ...))  (,(!: (eval-forth '(c ...))) commit))

 ;; Switch to compile mode for 2 words.  Instead we might try to run
 ;; the (macro) namespace parser directly?  This won't work for ":" as
 ;; it is not terminated.
 ((declare: parser name)  (inline-code (parser name)))

 ;; Switch to compile mode for the rest of the line.
 ((compile-line:)  (slurp inline-code))

 ;; Interaction with full macro language available: compile code and
 ;; execute it.  The procedure is available as "last" and is
 ;; terminated with "exit" for convenience and robustness.
 ((code/last (c ...))  (inline-code (: last c ... exit) last))
 ((::) (slurp code/last))

 )

;; Change the meaning of a word by prefixing it with another word.
;; Make sure the prefixer removes the word from the stream to not get
;; an expansion loop.
(define-syntax-rule (prefix-parsers-wrapped ns wrap (name ...))
  (prefix-parsers ns ((name) (wrap name)) ...))

;; If any of these words are encountered, one extra word is parsed and
;; the result is passed to the compiler.
(prefix-parsers-wrapped
 (target) declare:
 (variable 2variable load require planet staapl))

;; If any of these words are encountered, the entire line is parsed
;; and passed to the compiler.
(prefix-parsers-wrapped
 (target) compile-line:
 (: macro forth))


