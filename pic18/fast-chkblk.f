\ Check 512 bytes at once, return a 1-byte bitmask.
: chkblk8 \ -- bitmask
    0 8 for
        #xFF 64 for @f+ and next
        1 + drop  \ sets carry if value is 255
        rot<<c    \ shift carry into accu
    next ;
    
