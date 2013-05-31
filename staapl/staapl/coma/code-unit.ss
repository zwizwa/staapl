#lang scheme/unit

(require
 "../sig.ss"
 "../coma/macro.ss")

(import)
(export code^)

;; Provide context for evaluation of macro.


(patterns
 (macro)
          
 ;; Quoted delayed code.          
 (([qw ma] [qw mb] compose)   ([qw (macro: ,ma ,mb)]))
 
 ;; The basic behaviour is 'i, which will invoke a quoted macro, or
 ;; will delegate a call to the run-time word.

 (([qw (? macro-word? w)] i) w)
 ;; ((i) (macro: ~i))  ;; FIXME

 ((nop) ())

 ;; 'execute has a lower level semantics: it operates on quoted
 ;; numbers/labels instead, and will not execute macros.
 (([qw label] execute) ([cw label]))
 ;; ((execute) (macro: ~i))  ;; FIXME

 ;; 'compile will operate on both macros and labels, but won't
 ;; delegate to run-time.
 (([qw (? target-word? w)] compile) ([cw w]))
 (([qw (? macro-word? w)] compile)  w)
)


