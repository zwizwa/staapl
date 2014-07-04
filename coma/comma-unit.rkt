#lang racket/unit

(require
 racket/unit
 "../sig.rkt"
;; "../target/rep.rkt"
 "macro.rkt")

(import comma^)
(export comma-extra^)

(patterns
 (macro)
 (([dw a]  dw>)            ([qw a]))

 (([qw sym] sym>bin)         ([qw (->byte-list sym)]))

 ;; Compile time list manipulations
 (([qw car] [qw cdr] l:cons) ([qw (cons car cdr)]))
 (([qw lst] l:length)        ([qw (length lst)]))

 ;; bin, and 2bin, words compile lists and tables to code.
 (([qw tv-list]  [qw m] for-list)   (list->macro  m tv-list))
 (([qw tv-table] [qw m] for-table)  (table->macro m tv-table))

)


