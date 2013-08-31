#lang scheme/unit

(require
 scheme/unit
 "../sig.ss"
;; "../target/rep.ss"
 "macro.ss")

(import comma^)
(export comma-extra^)

(patterns
 (macro)
 (([dw a]  dw>)            ([qw a]))

 (([qw sym] sym>bin)         ([qw (->byte-list sym)]))

 ;; Compile time list manipulations
 (([qw car] [qw cdr] l:cons) ([qw (cons car cdr)]))
 (([qw lst] l:length)        ([qw (length lst)]))
 
 (([qw tv-list] [qw glue] |bin,|)
  (list->macro glue tv-list))

)

