#lang scheme/unit

(require
 scheme/unit
 "../sig.ss"
 "macro.ss")

(import comma^)
(export comma-extra^)

(patterns
 (macro)
 (([dw a]  dw>)     ([qw a]))
 

 ;; Smart-compile a quoted compile-time object to a Pascal string:
 ;; size byte followed by payload bytes.

 ;; E.g. :   ` ABC  ->  3 , 65 , 66 , 67 ,
 
 (([qw x] |bin,|)  (list->macro
                    (macro: |,|) ;; glue
                    (let ((l (->byte-list x)))
                      (cons (length l) l))))
 )
