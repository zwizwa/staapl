#lang racket/unit

(require
 "op.rkt"
 "../coma/op.rkt"
 "../tools.rkt" 
 "../sig.rkt"
 "../coma/pattern.rkt")

(import)
;; (import op-quote^ op-jump^)  ;; FIXME: later

(export cjump^)


(patterns
 (macro)
 
 (([qw sym] jw/false) ([jw/false sym]))
 ((reachable) ())

 )
