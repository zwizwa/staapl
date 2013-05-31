#lang scheme/base

;; The PICkit2 v2.x scripting and command languages with Lisp syntax.

;; For more information see the PICkit2 Interface Guide.

(require "interpreter.ss")
(provide (all-defined-out))

;; For maximum convenience, both the script and command namespacesas
;; are stored in the global namespace (local to the PK2 modules).

(interpreter

 ;; The script commands (indicated by 3 arguments) reduce to a list of
 ;; byte code instructions.
 
 (VDD_ON #xFF 0)
 (VDD_OFF #xFE 0)
 (VDD_GND_ON #xFD 0)
 (VDD_GND_OFF #xFC 0)
 (VPP_ON #xFB 0)
 (VPP_OFF #xFA 0)
 (VPP_PWM_ON #xF9 0)
 (VPP_PWM_OFF #xF8 0)
 (MCLR_GND_ON #xF7 0)
 (MCLR_GND_OFF #xF6 0)
 (BUSY_LED_ON #xF5 0)
 (BUSY_LED_OFF #xF4 0)
 (SET_ICSP_PINS #xF3 1)
 (WRITE_BYTE_LITERAL #xF2 1)
 (WRITE_BYTE_BUFFER #xF1 0)
 (READ_BYTE_BUFFER #xF0 0)
 (READ_BYTE #xEF 0)
 (WRITE_BITS_LITERAL #xEE 2)
 (WRITE_BITS_BUFFER #xED 1)
 (READ_BITS_BUFFER #xEC 1)
 (READ_BITS #xEB 1)
 (SET_ICSP_SPEED #xEA 1)
 (LOOP #xE9 2)
 (DELAY_LONG #xE8 1)
 (DELAY_SHORT #xE7 1)
 (IF_EQ_GOTO #xE6 2)
 (IF_GT_GOTO #xE5 2)
 (GOTO_INDEX #xE4 1)
 (EXIT_SCRIPT #xE3 0)
 (PEEK_SFR #xE2 1)
 (POKE_SFR #xE1 2)
 (ICDSLAVE_RX #xE0 0)
 (ICDSLAVE_TX_LIT #xDF 1)
 (ICDSLAVE_TX_BUF #xDE 0)
 (LOOPBUFFER #xDD 1)
 (ICSP_STATES_BUFFER #xDC 0)
 (POP_DOWNLOAD #xDB 0)
 (COREINST18 #xDA 2)
 (COREINST24 #xD9 3)
 (NOP24 #xD8 0)
 (VISI24 #xD7 0)
 (RD2_BYTE_BUFFER #xD6 0)
 (RD2_BITS_BUFFER #xD5 1)
 (WRITE_BUFWORD_W #xD4 1)
 (WRITE_BUFBYTE_W #xD3 1)
 (CONST_WRITE_DL #xD2 1)
 (MEASURE_PULSE #xBF 0)
 (end-marker #x00 0)

 (SET_AUX #xCF 1)
 
 (I2C_START #xCD 0)
 (I2C_STOP #xCC 0)
 (I2C_WR_BYTE_LIT #xCB 1)
 (I2C_WR_BYTE_BUF #xCA 0)
 (I2C_RD_BYTE_ACK #xC9 0)
 (I2C_RD_BYTE_NACK #xC8 0)

 (SPI_WR_BYTE_LIT #xC7 1)
 (SPI_WR_BYTE_BUF #xC6 0)
 (SPI_RD_BYTE_BUF #xC5 0)
 (SPI_RDWR_BYTE_LIT #xC4 1)
 (SPI_RDWR_BYTE_BUF #xC3 0)
 
 

 ;; The interactive commands (indicated by 4 arguments) get compiled
 ;; to byte code and transferred to the PICkit.  The reply is
 ;; collected in a list.
 
 (ENTERBOOTLOADER #x42 0 0)
 (FIRMWARE_VERSION #x76 0 3)
 (NO_OPERATION #x5A 0 0)
 (POWERCTRL #x56 0 0)
 (GETVERSION #x76 0 3)
 (SETVPP #xA1 3 0)
 (SETVDD #xA0 3 0)
 (READ_STATUS #xA2 0 2)
 (READ_VOLTAGES #xA3 0 4)
 (DOWNLOAD_SCRIPT #xA4 254 0)
 (RUN_SCRIPT #xA5 2 0)
 (EXECUTE_SCRIPT #xA6 255 0)
 (CLR_DOWNLOAD_BFR #xA7 0 0)
 (DOWNLOAD_DATA #xA8 255 0)
 (CLR_UPLOAD_BFR #xA9 0 0)
 (UPLOAD_DATA #xAA 0 255)
 (CLR_SCRIPT_BFR #xAB 0 0)
 (UPLOAD_DATA_NOLEN #xAC 0 0)
 (END_OF_BFR #xAD 0 0)
 (RESET #xAE 0 0)
 (SCRIPT_BFR_CHKSM #xAF 0 0)
 (WR_INTERNAL_EE #xB1 254 0)
 (RD_INTERNAL_EE #xB2 0 0)
 (ENTER_UART_MODE #xB3 2 0)
 (EXIT_UART_MODE #xB4 0 0)
 (LOGIC_ANALYZER_GO #xB8 7 2)
 (SCRIPT_BUFFER_CHKSM #xAF 0 4)
 (COPY_RAM_UPLOAD #xB9 2 0))
