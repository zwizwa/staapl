#lang scheme/unit

(require
 "op.ss"
 "../coma/op.ss"
 "../tools.ss" 
 "../sig.ss"
 "../coma/pattern.ss")

(import)
;; (import op-quote^ op-jump^)  ;; FIXME: later

(export cjump^)


(patterns
 (macro)
 
 (([qw sym] jw/false) ([jw/false sym]))
 ((reachable) ())

 )
