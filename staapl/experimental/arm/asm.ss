#lang scheme/base

(require "../asm.ss")

;; ARM assembler

;; http://simplemachines.it/doc/arm_inst.pdf


(instruction-set


 ;; PAGE 13.

 ;; dpr = Data Processing / PSR Transfer
 ;; c = opcode
 ;; n = Rn
 ;; d = Rd
 ;; r = operand
 
 (dpr (i c s n d) "00 i cccc s nnnn dddd rrrrrrrr rrrrrrrr")
  
 

 ;; f = file register
 ;; d = destination register
 ;; w = base register
 ;; b = 0 for word op
 ;; b = 1 for byte op
 ;; q = destination address mode
 ;; k = literal operand
 
;;  (add   (b f d)       "1011 0100 0bdf ffff ffff ffff")   ;; d = f + WREG
;;  (addl  (b k d)       "1011 0000 0bkk kkkk kkkk dddd")   ;; Wd = #10 + Wd
;;  (addls (b w q k d)   "0100 0www wbqq qddd d11k kkkk")   ;; 
;;  (add3  (b w q d p s) "0100 0www wbqq qddd dppp ssss")
 
;;  (addac (a)           "1100 1011 a000 0000 0000 0000")

;;  (addc   (b f d)       "1011 0100 1bdf ffff ffff ffff")
;;  (addcl  (b k d)       "1011 0000 1bkk kkkk kkkk dddd")
;;  (addcls (b w q k d)   "0100 1www wbqq qddd d11k kkkk")
;;  (addc3  (b w q d p s) "0100 1www wbqq qddd dppp ssss")


;;  (_file  (o b f d)       "oooo oooo obdf ffff ffff ffff")
;;  (_lit10 (o b k d)       "oooo oooo obkk kkkk kkkk dddd")
;;  (_lit5  (o b w q k d)   "oooo owww wbqq qddd d11k kkkk")
;;  (_alu3  (o b w q d p s) "oooo owww wbqq qddd dppp ssss")
 
 
 )
 