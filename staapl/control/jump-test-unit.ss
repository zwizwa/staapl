#lang scheme/unit

(require
 "op.ss"
 "../coma/op.ss"
 "../tools.ss" 
 "../sig.ss"
 "../coma/pattern.ss")

(import)
;; (import op-quote^ op-jump^)  ;; FIXME: later

(export jump^)


(patterns
 (macro)
 ;; LABELS
 
 ;; Stubs for target label operations used in label.ss /
 ;; instantiate.ss to build structured code graphs that allow
 ;; control flow analysis. This allows the forth control words
 ;; (that use only the 2nd stack) to be defined here, for use in
 ;; testing or any other use that doesn't need control flow
 ;; analysis and label management.
   
 ((sym)               ([qw (next-label)]))      ;; labels are symbols
 
 (([qw sym] label:)   ([label sym]))            ;; pseudo op.
 (([qw sym] >label)   ([qw sym]))
 
 (([cw sym] exit)     ([jw sym]))
 ((exit)              ([exit]))
 
 (([qw sym] jw)       ([jw sym]))
 (([qw sym] cw)       ([jw sym]))

 ((reachable) ())

 )
