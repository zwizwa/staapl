module Pic18 where

import Data.Bits


-- Code generator for Pic18

-- Not sure what to do here.  Essentially, I want some form of
-- multiple interpretation to be able to run tests on assembler code.
-- For now this is just concrete.

-- EDIT: There are two distinct phases:
-- 1. operations on symbolic assembly code
-- 2. conversion of symbolic code to binary, or to some other form (interpretation?)


-- What would be interesting?
-- Check that transformations are correct
-- Finding new transformations automatically

-- To check correctness, some semantics are needed.  Either an
-- emulator (which also needs to be validated) or some on-target
-- execution, which seems really hard to do.


-- byte-oriented file register operations

-- Allow specification of literals as bit strings.
opc :: String -> (Int, Int)
opc str = f str where
  f [] = (0, 0)
  f (' ':r) = f r -- allow for spaces
  f ('1':r) = f' 1 r
  f ('0':r) = f' 0 r
  f' b r = (n + 1, v + (shift 1 n) * b) where (n, v) = f r
 

-- byte-oriented file register operations
-- (_ (f d a) "____ __da ffff ffff")
opDAF o = \f d a -> [opc o, (1, d), (1, a), (8, f)]
 
addwf   = opDAF "0010 01"
addwfc  = opDAF "0010 00"
andwf   = opDAF "0001 01"
comf    = opDAF "0001 11"
rlcf    = opDAF "0011 01"
rlncf   = opDAF "0100 01"
rrcf    = opDAF "0011 00"
rrncf   = opDAF "0100 00"
subfwb  = opDAF "0101 01"
subwf   = opDAF "0101 11"
subwfb  = opDAF "0101 10"
swapf   = opDAF "0011 10"
xorwf   = opDAF "0001 10"
decf    = opDAF "0000 01"
incf    = opDAF "0010 10"
iorwf   = opDAF "0001 00"
movf    = opDAF "0101 00"

incfsz  = opDAF "0011 11"
infsnz  = opDAF "0100 10"
decfsz  = opDAF "0010 11"
decfsnz = opDAF "0100 11"
 
tstfsz  = opDAF "0110 01"
clrf    = opDAF "0110 10"
cpfseq  = opDAF "0110 00"
cpfsgt  = opDAF "0110 01"
movwf   = opDAF "0110 11"
mulwf   = opDAF "0000 00"
cpfslt  = opDAF "0110 00"
setf    = opDAF "0110 10"
negf    = opDAF "0110 11"

-- (movff (s d) "____ ssss ssss ssss" "1111 dddd dddd dddd")
movff s d = [opc "1100", (12,s), opc "1111", (12,d)]


-- bit-oriented file register operations
-- (_ (f b a) "____ bbba ffff ffff")

opBT o f b a = [opc o, (3,b), (1,a), (8,f)]

bcf   = opBT "1001"
bsf   = opBT "1000"
btfsc = opBT "1011"
btfss = opBT "1010"
btg   = opBT "0111"

-- POLARIZED
-- i = INVERTED bit value
-- 0 -> set   / skip if set
-- 1 -> clear / skip if clear
-- (_ (i f b a) "100i bbba ffff ffff")

-- (_ (i f b a) "___i bbba ffff ffff")
bsfi   i f b a = [opc "100", (1,i), (3,b), (1,a), (8,f)]
btfssi i f b a = [opc "101", (1,i), (3,b), (1,a), (8,f)]


-- control operations (original asm)
-- (_ (n) "____ ____ nnnn nnnn")
opC o r = [opc o, (8, r)]
bc   = opC "1110 0010"
bnc  = opC "1110 0011"
bn   = opC "1110 0110"
bnn  = opC "1110 0111"
bov  = opC "1110 0100"
bnov = opC "1110 0101"
bz   = opC "1110 0000"
bnz  = opC "1110 0001"

-- polarized control operators
-- (_ (i r) "____ ____ nnnn nnnn")
-- i = inverted
opCP o i r = [opc o, (1, i), (8, r)]
bci  = opCP "1110 001"
bni  = opCP "1110 011"
bovi = opCP "1110 010"
bzi  = opCP "1110 000"
 
bra r      = [opc "1101 0", (11, r)]
call s l h = [opc "1110 110", (1, s), (8, l), opc "1111", (12, h)] -- h = high, l = low  (~nop h)
clrwdt     = [opc "0000 0000 0000 0100"]
daw        = [opc "0000 0000 0000 0111"]
goto l h   = [opc "1110 1111", (8, l), opc "1111", (12, h)] -- (~nop h)
nop0       = [opc "0000 0000 0000 0000"]
nop d      = [opc "1111", (12, d)] -- used for extra argument
pop        = [opc "0000 0000 0000 0110"]
push       = [opc "0000 0000 0000 0101"]
reset      = [opc "0000 0000 1111 1111"]
rcall r    = [opc "1101 1", (11,r)]
retfie s   = [opc "0000 0000 0001 000", (1,s)]
retlw k    = [opc "0000 1100", (4,k)]
return s   = [opc "0000 0000 0001 001", (1,s)]
sleep      = [opc "0000 0000 0000 0011"]

-- literal operations
opLIT o l = [opc o, (8, l)]
addlw k = opLIT "0000 1111"
andlw k = opLIT "0000 1011"
iorlw k = opLIT "0000 1001"
movlb k = opLIT "0000 0001"
movlw k = opLIT "0000 1110"
mullw k = opLIT "0000 1101"
sublw k = opLIT "0000 1000"
xorlw k = opLIT "0000 1010"

-- data memory <-> program memory operations
-- * F (fetch)
-- - m (minus)
-- _ p (plus)
tblrdF  = [opc "0000 0000 0000 1000"] -- tblrd*  
tblrdFp = [opc "0000 0000 0000 1001"] -- tblrd*+ 
tblrdFm = [opc "0000 0000 0000 1010"] -- tblrd*- 
tblrdpF = [opc "0000 0000 0000 1011"] -- tblrd+* 
tblwtF  = [opc "0000 0000 0000 1100"] -- tblwt*  
tblwtFp = [opc "0000 0000 0000 1101"] -- tblwt*+ 
tblwtFm = [opc "0000 0000 0000 1110"] -- tblwt*- 
tblwtpF = [opc "0000 0000 0000 1111"] -- tblwt+* 

lfsr f l h = [opc "1110 1110 00", (2,f), (4,h), opc "1111 0000", (8,l)] 

-- ;; FIXME: what would it take to support something like this instead,
-- ;; where word is concatenated/split for asm/dasm?
-- ;; (lfsr    (f (l h)) "1110 1110 00ff hhhh" "1111 0000 llll llll")  ; (~nop l)
