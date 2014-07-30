#lang racket/base
(require
 racket/match
 "tools.rkt")
(require/provide
 "tools.rkt"           ;; misc tools
 "live.rkt"            ;; toplevel interaction
 "code.rkt"            ;; target code registry
 "port/ihex.rkt"       ;; write-ihex
 "pic18-dtc.rkt"       ;; pic18 dtc compiler
 )

;; Change outer interpreter to use on-target 16bit DTC.
(live-push dtc-push)
(live-exec dtc-exec)
(live-find dtc-find)
(live-wordsize 2)

