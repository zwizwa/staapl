#lang racket/base

;; A simple Scheme-based assembly language.

;; The basic idea is to experiment a bit with dsPIC and ARM.  The
;; Staapl assembler used for the PIC18 is not powerful enough to host
;; RISC-style addressing modes.


;; dsPIC33EP32MC202
;; 16-bit MCU and DSC Programmer's Reference Manual
;; http://ww1.microchip.com/downloads/en/DeviceDoc/70157F.pdf

;; So...  dsPIC is rather quirky.  This is not a small project.
