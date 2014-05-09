\ General purpose high-level library.

\ The PIC18 macro forth is quite lowlevel - e.g. conditionals don't
\ factor well.  This is a word set for creating more traditional
\ forth-like code.

\ It also serves as a compilation test for .f files.

\ Some low level libraries.

load ad.f       \ A to D conversion
load busyloop.f 
load debug.f


