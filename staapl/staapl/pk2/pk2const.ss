#lang scheme/base
(provide (all-defined-out))

(define	reqLen	64)			;; PICkit 2 always uses 64-byte transfers



;; ;; Standard script IDs
;; (define-syntax-rule (enum name ...)
;;   (define-values
;;     (name ...)
;;     (apply values
;;            (build-list
;;             (length '(name ...))
;;             (lambda (x) x)))))

;; (enum
;;  SCR_PROG_ENTRY
;;  SCR_PROG_EXIT
;;  SCR_RD_DEVID
;;  SCR_PROGMEM_RD
;;  SCR_ERASE_CHIP_PREP
;;  SCR_PROGMEM_ADDRSET
;;  SCR_PROGMEM_WR_PREP
;;  SCR_PROGMEM_WR
;;  SCR_EE_RD_PREP
;;  SCR_EE_RD
;;  SCR_EE_WR_PREP
;;  SCR_EE_WR
;;  SCR_CONFIG_RD_PREP
;;  SCR_CONFIG_RD
;;  SCR_CONFIG_WR_PREP
;;  SCR_CONFIG_WR
;;  SCR_USERID_RD_PREP
;;  SCR_USERID_RD
;;  SCR_USERID_WR_PREP
;;  SCR_USERID_WR
;;  SCR_OSCCAL_RD
;;  SCR_OSCCAL_WR
;;  SCR_ERASE_CHIP
;;  SCR_ERASE_PROGMEM
;;  SCR_ERASE_EE
;;  SCR_INVALID_1
;;  SCR_ROW_ERASE
;;  SCR_TESTMEM_RD
;;  SCR_EEROW_ERASEy
;;  SCR_INVALID_2)
