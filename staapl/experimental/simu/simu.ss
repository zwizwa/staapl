#lang scheme/base

;; simulator

;; Writing code for a distributed embedded system is hard. The reasons
;; is that not everything can be practically observed or tested in
;; isolation. In order to be able to WRITE code for parallel systems
;; you need fine-grained modularity, but in order to TEST code you
;; need a lot of eyes, i.e. expensive hardware.

;; The point of this module is to create a fine-grained simulator to
;; make debugging through simulation
;;
;; * easier to understand: replace small functional units by small
;;   test units. (fine grain simulation)
;;
;; * easier/cheaper to implement: make the compiler decide on what's
;;   real and what's simulated, and run as much as possible on REAL
;;   hardware while keeping the testing VIRTUAL (partial evaluation)


;; This is implemented as a special-purpose assembler for a
;; special-purpose machine. One defines a simulator in a simple
;; specification language, and a simulator generator that can
;; interpret this language, or compile it to some fast specific
;; simulator.

;; So, what is necessary, is a simple language to specify processor
;; behaviour. Some RTL thing with a special ALU part.


;; Let's start with a basic architecture, which can then be extended
;; to perform more operations.


;; GENERIC MODELING OPERATIONS

;; (define-io (name <inputs> <outputs>) <par-statements>)

'(define-io (add (A B) (D w n ov z c dc))
  (! (c D) (+ A B))
  (! z (zero? D))
  (! n  (ref D 3))
  (! dc (ref D 7))
  (! ov (xor c n)))


;; PIC18

'(register STATUS - - - N OV Z DC C)
'(register W)

;; convention: literals are lowercase, globals are uppercase.

'(define-instruction
  (addlw (k) "0000 1111 kkkk kkkk")
  (add (W k) (W N OV Z C DC)))

  ;; affects N, OV, C, DC, Z, W
  