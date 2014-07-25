#lang staapl/pic18 \ -*- forth -*-
provide-all
\ Circular buffer macros. Config is two compile time words: buffer
\ address and log2 size.  


macro
: arr-buf   | buf | buf #xFF and buf 8 >>> a!! ; \ buf -- 
: arr-size  | bits | 1 bits <<< ;                \ bits -- size
: arr-mask  arr-size 1 - ;                       \ bits -- mask
: arr-box   | buf bits | buf arr-buf bits arr-mask and al +! ; \ index buf bits -- 
: arr-fill  | buf bits | buf arr-buf bits arr-size a!fill ;
forth
: a!fill    for dup !a+ next drop ; \ el n -- | n>0, a=buf


\ Typical use: 
\ macro
\ : st-spec  #x180 5 ;  \ 2^5 = 32 byte
\ forth
\ : st-box   st-spec arr-box ;        \ index --  | a=box
\ : st-!     st-box !a  ;             \ el index --
\ : st-@     st-box @a  ;             \ index -- el
\ : st-init  0 st-spec arr-fill ;     \ --