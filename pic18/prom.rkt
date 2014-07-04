#lang staapl/pic18 \ -*- forth -*- 
provide-all

\ flash/eeprom/config programming

macro

\ interrupts
: cli        INTCON GIE low ;              \ disable interrupts
: sti        INTCON GIE high ;             \ enable interrupts

\ target select
: config        EECON1 CFGS high ;
: eeprom        EECON1 EEPGD low EECON1 CFGS low ;
: flash         EECON1 EEPGD high EECON1 CFGS low ;

forth

\ engage in write / erase
\ apparently, setting the WR bit high needs to follow
\ immediately after the 0x55 0xaa sequence
: engage
    dup cli

    #x55 movlw EECON2 movwf 
    #xAA movlw EECON2 movwf

    EECON1 WR high sti drop ;

\ action
: erase         EECON1 WREN high EECON1 FREE high engage ;
: program       EECON1 WREN high EECON1 FREE low 
		tblrd*- engage 
		tblrd*+ ;
 
\ now we have:
\ 'flash program'   -> programs 8 bytes flash in holding registers
\ 'flash erase'     -> erase 64 bytes flash
\ 'eeprom program'  -> program one byte eeprom
\ 'config program'  -> program one byte config

