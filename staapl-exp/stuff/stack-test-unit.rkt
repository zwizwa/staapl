#lang racket/unit

(require
 racket/unit
 "../sig.ss"
 "macro.ss")

(import)
(export stack^ comma^)

(patterns
 (macro)

 (([qw a] ",")         ([dw a]))
 ;; Transfer of Scat semantics to Coma (postponed) semantics.
 
 (([qw a] dup)         ([qw a] [qw a]))
 (([qw a] drop)        ())
 (([qw a] [qw b] swap) ([qw b] [qw a]))

 (([qw a] [qw b] +)    ([qw (tv: a b +)]))
 (([qw a] [qw b] -)    ([qw (tv: a b -)]))
 (([qw a] [qw b] *)    ([qw (tv: a b *)]))
 (([qw a] [qw b] /)    ([qw (tv: a b /)]))
 
 (([qw a] not)         ([qw (not a)])))



