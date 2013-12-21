#lang racket
(provide PIC18F4550)
(define-syntax-rule (configbits . stx) 'stx)

;; TABLE 25-1: CONFIGURATION BITS AND DEVICE IDs
(define PIC18F4550
  (configbits
   (CONFIG1L (-      -     USBDIV CPUDIV1 CPUDIV0 PLLDIV2 PLLDIV1 PLLDIV0)  "--000000")
   (CONFIG1H (IESO   FCMEN -      -       FOSC3   FOSC2   FOSC1   FOSC0)    "00--0101")
   (CONFIG2L (-      -     VREGEN BORV1   BORV0   BOREN1  BOREN0  PWRTEN)   "--011111")
   (CONFIG2H (-      -     -      WDTPS3  WDTPS2  WDTPS1  WDTPS0  WDTEN)    "---11111")
   (CONFIG3L (-      -     -      -       -       -       -       -)        "--------")
   (CONFIG3H (MCLRE  -     -      -       -       LPT1OSC PBADEN  CCP2MX)   "1----011")
   (CONFIG4L (/DEBUG XINST ICPRT  -       -       LVP     -       STVREN)   "100--1-1")
   (CONFIG4H (-      -     -      -       -       -       -       -)        "--------")
   (CONFIG5L (-      -     -      -       CP3     CP2     CP1     CP0)      "----1111")
   (CONFIG5H (CPD    CPB   -      -       -       -       -       -)        "11------")
   (CONFIG6L (-      -     -      -       WRT3    WRT2    WRT1    WRT0)     "----1111")
   (CONFIG6H (WRTD   WRTB  WRTC   -       -       -       -       -)        "111-----")
   (CONFIG7L (-      -     -      -       EBTR3   EBTR2   EBTR1   EBTR0)    "----1111")
   (CONFIG7H (-      EBTRB -      -       -       -       -       -)        "-1------")
   ))


     