#lang scheme/base

(require
 "../coma/macro.ss"
 "../forth/forth-lex.ss"
 "state.ss")

(provide macro> forth-compile
         (ns-out (macro) clear))


;; (macro-eval-init-state state:compiler)

;; Interactive debug
(define *state* #f)
(define (new-state) (state:compiler))
(define (update-state [s (new-state)]) (set! *state* s))
(define (print-state) (comp-print-state *state*))
(update-state)

(define-syntax-rule (macro> . code)
  (begin
    (update-state ((macro: . code) *state*))
    (print-state)))

   
;; .snot uses "forth-compile" to provide a compiler repl.  we simply
;; take the lexer from forth/lexer.ss and have it pass its output to
;; macro>

(define-syntax-rule (forth-compile str)
  (forth-lex-string/cps macro> str))
            

;; from compilation console, allow the state to be reset.
(ns (macro) (define (clear state) (new-state)))
