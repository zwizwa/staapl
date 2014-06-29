#lang scheme/base

(require
 "../coma/macro.ss"
 "../forth/forth-lex.ss"
 "state.ss")

(provide macro> print-asm> forth-compile clear-state
         (ns-out (macro) clear))


;; (macro-eval-init-state state:compiler)

;; Interactive debug
(define *state* #f)
(define (new-state) (state:compiler))
(define (update-state [s (new-state)]) (set! *state* s))
(define (print-state opts) (comp-print-state *state* opts))
(update-state)


;; Run macro, print state.
(define-syntax-rule (macro> . code)
  (begin
    (update-state ((macro: . code) *state*))
    (print-state  '(store chain current asm ctrl rs))))

;; Same, but only print the code.
(define-syntax-rule (print-asm> . code)
  (begin
    (update-state ((macro: . code) *state*))
    (print-state '(asm))))
    

   
;; .snot uses "forth-compile" to provide a compiler repl.  we simply
;; take the lexer from forth/lexer.ss and have it pass its output to
;; macro>

(define-syntax-rule (forth-compile str)
  (forth-lex-string/cps macro> str))
            

;; from compilation console, allow the state to be reset.
(ns (macro) (define (clear state) (new-state)))

(define (clear-state) (macro> clear))
