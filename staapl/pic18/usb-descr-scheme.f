\ USB descriptors are defined as raw byte tables in a Scheme file

staapl pic18/compose-macro

: device-descriptor        \   -- lo hi
    table-> 'scheme desc-device ' , for-list ;

: configuration-descriptor \ n -- lo hi
    drop \ Single configuration
    table-> 'scheme desc-config ' , for-list ;

: string-descriptor        \ n -- lo hi
    route \ FIXME: no bounds checking
        'scheme desc-strings
        [ [ table-> ] swap >macro compose-macro
          [ ' , for-list ]        compose-macro
          i/c . ]
        for-list