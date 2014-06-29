#lang racket/base
(require
 racket/match
 "tools.rkt")
(require/provide
 "live/tethered.rkt"    ;; Scat code for host<->target interaction.
 
 "live/comm-uart.rkt" 
 "live/comm-pk2.rkt"
 "live/comm-simulator.rkt"  ;; monitor transports and reference implementation
  
 "live/commands.rkt"    ;; A target-local view to the above.
 "live/rpn-target.rkt"  ;; Base implementation of the target command language.
 "live/reflection.rkt"  ;; Namespace operations.
 "live/repl.rkt"        ;; repl functionality

 "live/rpn-live.rkt"    ;; for 'live>'
 
 ;; necessary for reflective operations to work:
; "target.rkt"
; "ns.rkt"
; "scat.rkt"
 "macro.rkt"
 )

(provide (all-defined-out))

;; (loading "live")

;; All uses of 'eval in the staapl/live/ modules will use the current
;; project namespace, as managed by prj/manager.ss code.  However,
;; live/commands.ss has a link to the manager for delegating
;; meta-namespace operations (like reload/switch/kill/...).


;; LIVE CONNECTION

;; (require "pk2/icsp.rkt") (define (debug) (icsp-debug #t))
  

(define (console type dev baud)
  (with-handlers
      ((void (lambda (ex)
               (printf "Error opening console ~a:\n" type)
               (display ex)
               (newline))))
    (match type
           ('uart       (comm-uart dev baud))
           ('pickit2    (comm-pickit2 dev baud))
           ('simulator  (comm-simulator)))))
