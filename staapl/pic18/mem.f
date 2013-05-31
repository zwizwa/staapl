#lang staapl/pic18
\ convert access bank address to real address in ah:al
: access>a
    dup al !
    #x80 xor rot<<c
    drop ah @ ah --! ;

\ FIXME: find a proper way to add library defers.
: ~@ access>a @a+ ;
: ~! access>a !a+ ;
