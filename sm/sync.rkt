#lang scheme/base

;; Synchronous multitasking.
;; -------------------------
;;
;; Starting from Actors / Erlang, use the following simplifications:
;;
;; - messages -> synchronous byte streams
;;
;; - static task instances with reset
;;
;; - static i/o connection
;;
;; Fanout is trivial: each write causes another tasks's read until it
;; blocks again in a write.
;;
;; Fanin needs some thought:
;;  - data joins are rare, so can we avoid them completely?
;;  - introduce buffers  (and find a smart way to do this automatically)
;;
;; If the networks are not too complex, it should be possible to
;; determine memory usage by finding worst cases and running them.



;; I wanted to create a separate language for this but it seems much
;; more useful to use the plain Forth and its abstraction mechanisms,
;; and just provide abstract interpretation of such code.

;; This can be done either by:
;; - implementing a language with a different semantics
;; - running everything in a patched simulator

;; Primtives:

;; - read: (blocking).  this suspends the task, returning control to
;;   the task that wrote.  when task resumes, received byte will be
;;   pushed on the parameter stack.

;; - write: (yielding).  receiving task should be blocked in a read.
;;   switch both stacks, transferring one byte (word) to the other
;;   stack.

;; To a task, inputs and outputs are single purpose, so they can both
;; be represented by a read and resp write XT.


