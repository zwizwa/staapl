\ SPI

\ Serial Data Out (SDO) – RC7/RX/DT/SDO
\ Serial Data In (SDI)  – RB0/AN12/INT0/FLT0/SDI/SDA
\ Serial Clock (SCK)    – RB1/AN10/INT1/SCK/SCL
\ Slave Select (SS)     – RA5/AN4/SS/HLVDIN/C2OUT

\ CPL : clock polarity, clock idle state: 0/1
\ CKE : 1 tx at active -> idle
\       0 tx at idle -> active


macro
: spi-ss LATE 0 ; \ LATA 5 ;  
: config-spi-master | CPL CKE SMP SSPM |
    TRISC 7 low  \ SDO
    TRISB 1 low  \ SCK
    \ TRISA 5 low  \ SS
    TRISE 0 low

    CPL 7 <<<
    CKE 6 <<< or SSPSTAT !

    CPL 4 <<<
    SSPM or
    #x20 or SSPCON1 !
    ;

: init-spi-master 0 0 0 2 config-spi-master ;

: mcp4922-init-spi 1 0 0 0 config-spi-master ;  \ mode 1-1

: mcp4922-tx-A \ byte --
    spi-ss low
    swap-nibble dup
    #x0F and #x70 or spi-tx \ channel A, buffered, no gain, powered
    #xF0 and         spi-tx
    spi-ss high
    ;

: mcp4922-tx-B \ byte --
    spi-ss low
    swap-nibble dup
    #x0F and #xF0 or spi-tx \ channel A, buffered, no gain, powered
    #xF0 and         spi-tx
    spi-ss high
    ;

    
: mcp4922-test \
    mcp4922-init-spi
    0 begin dup mcp4922-tx 1+ again ;
    

: test-spi init-spi-master 0 begin dup spi-tx/rx drop 1+ again ;
   
\ : init-spi-slave \ --
\     TRISC 7 low  \ SDI
\     TRISB 1 high \ SCK
\     TRISA 5 high \ SS

\     #x24 SSPCON1 !  \ master, FOSC/16, CPL=0

\     \ before enabling SPI mode, the clock line must be in the idle state.
    
\     ;

: spi-wait do SSPSTAT 0 high? until ;
: spi-tx/rx \ out - in
    SSPBUF !
    spi-wait
    SSPBUF @ ;

: spi-tx spi-tx/rx drop ;
    
    
forth
