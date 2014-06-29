#lang racket/base
(require
 racket/control
;; racket/runtime-path
 "tools.ss"
 "target.ss"
 "forth/lexer.ss")

(require/provide
 "mc14/asm.ss"
 "mc14/macro.ss"
 "mc14/const.ss"
 "port/ihex.ss"
 "live.ss"
 )

(provide (all-defined-out))
(asm-debug!)


;; for testing
(forth-compile "macro : INDF 123 ;")

(target-code-unit 1)
(target-code-bits 14)

(define (asm-test!)
  (register-code-hook
   (list
    (lambda (chains . _)
      (assemble-chains chains)
      (print-asm-code chains)))))

(asm-test!) ;; still in scaffolding

(loading "mc14")
