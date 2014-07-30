#lang racket/unit
(require
 racket/pretty
 "asm.rkt"
 "../sig.rkt"
 "../sig.rkt"
 "../coma/macro.rkt"
 "../control/op.rkt"
 "../ns.rkt"
 ;"../comp/postprocess.rkt"
 )

(import)
(export machine^
        stack^
        comma^
        cjump^
        rstack^
        ) 


(define-syntax-rule (not-implemented word ...)
  (begin
    (ns (macro) (define (word . a) (error 'word))) ...))

(not-implemented
 ;; machine^
 address code-size
 ;; stack^
 dup swap drop + - * / not
 ;; comma^
 |,| byte-slots
 ;; cjump^
 jw/false
 ;; rstack^
 for next >r r> swap>r r- +r rdrop rl rh
 )
