#lang scheme/base
(require
 scheme/match
 "tools.ss")
(require/provide
 "live/tethered.ss"    ;; Scat code for host<->target interaction.
 
 "live/comm-uart.ss" 
 "live/comm-pk2.ss"
 "live/comm-simulator.ss"  ;; monitor transports and reference implementation
  
 "live/commands.ss"    ;; A target-local view to the above.
 "live/rpn-target.ss"  ;; Base implementation of the target command language.
 "live/reflection.ss"  ;; Namespace operations.
 "live/repl.ss"        ;; repl functionality

 "live/rpn-live.ss"    ;; for 'live>'
 
 ;; necessary for reflective operations to work:
; "target.ss"
; "ns.ss"
; "scat.ss"
 "macro.ss"
 )

(provide (all-defined-out))

;; (loading "live")

;; All uses of 'eval in the staapl/live/ modules will use the current
;; project namespace, as managed by prj/manager.ss code.  However,
;; live/commands.ss has a link to the manager for delegating
;; meta-namespace operations (like reload/switch/kill/...).


;; LIVE CONNECTION

;; (require "pk2/icsp.ss") (define (debug) (icsp-debug #t))
  

(define (console type dev baud)
  (with-handlers
      ((void (lambda (ex)
               (printf "Error opening console ~a:\n" type)
               (display (ex))
               (newline))))
    (match type
           ('uart       (comm-uart dev baud))
           ('pickit2    (comm-pickit2 dev baud))
           ('simulator  (comm-simulator)))))
