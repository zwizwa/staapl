#lang racket/base

;; Language
(require 
 (file "/home/tom/staapl/app/dtc.fm")
 staapl/live-pic18-dtc
 readline/rep)

(begin
  (forth-begin-prefix '(library "pic18"))
  (define mark (code-pointers))
  (define-namespace-anchor anchor)
  (console 'uart "/dev/ttyACM0" #f)
  (empty/run
   '((file "/home/tom/staapl/app/dtc.fm")
     staapl/live-pic18-dtc
     readline/rep)
   anchor
   mark))


