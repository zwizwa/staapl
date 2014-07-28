#lang racket/base

;; Language
(require 
 (file "/home/tom/staapl/app/dtc.fm")
 staapl/live-pic18-dtc)

;; Console
(begin
  (define version-tag '"460ac52392c0056c7c308e6cc8717f93c68f6653")
  (require readline/rep)
  (forth-begin-prefix '(library "pic18")))
(define *mark* (code-pointers))

(define-namespace-anchor a)
 
(define (empty)
  (let ([ns (make-base-namespace)]
        [nsa (namespace-anchor->namespace a)])
    ;; Attach and require seem to be separate operations.  Attach
    ;; makes sure a subsequent require uses the same instance of the
    ;; module while require exposes the identifiers of the
    ;; instantiated module, or instantiates one if it's not found in
    ;; the registry.
    (namespace-attach-module nsa '(file "/home/tom/staapl/app/dtc.fm") ns)
    (namespace-attach-module nsa 'staapl/live-pic18-dtc ns)
    (namespace-attach-module nsa 'readline/rep ns)
    (parameterize ([current-namespace ns])
      (eval
       '(require
         racket/base
         (for-syntax racket/base)
         (file "/home/tom/staapl/app/dtc.fm")
         staapl/live-pic18-dtc
         readline/rep))
      (code-clear!)
      (console 'uart "/dev/ttyACM0" #f)
      ;;(param-to-toplevel 'command repl-command-hook)
      ;;(param-to-toplevel 'break repl-break-hook)
      (printf "run...\n")
      (run
       (lambda ()
         (code-pointers-set! *mark*)
         (clear-flash)
         (when-file '"/home/tom/staapl/app/dtc.rkt" load))))))



(let next ()
  (empty)
  (printf "again..\n")
  (next)
  )

