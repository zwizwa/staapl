\ Metacircular Forth compiler.

\ This code is bootstrapped in Scheme in the following way:
\   1. Read code as symbolic (name . def) dictionary.
\   2. Identify immediate words
\   3. Associate interpreted semantics to all tokens.
\   4. Instantiate all words one by one.

\ After step 3 the code is runnable in abstract (graph) form as long
\ as there are no circular dependencies in the definitions.  This
\ interpreted compiler is then used to compile the source again.

\ Step 1 + 2 require that there is no run-time code in the source
\ other than an occurance of "immediate" at the end of a definition.
\ Step 3. means the compiler cannot assume anything about its own
\ threading mechanism, otherwise it can't be interpreted.  This
\ excludes some return stack tricks but has the advantage that the
\ code is more portable.

\ The PIC18 target needs RAM buffered compilation before code can be
\ uploaded to Flash ROM, so addresses need a mapper ">buf".

: >buf ;

variable here 

\ Currently fixed to 2-byte cell size and byte addressing.
: 1cell+ 2 + ;
: here@ here @ ;    
: , here@ >buf ! here@ 1cell+ here ! ;
: literal compile dolit , ;    
    
\ Control words
: hole     here@ 0 , ;
: if       postpone ?jump hole ; immediate
: then     >r here@ r> >buf ! ; immediate
: else     >r postpone jump r> hole ; immediate
: begin    here@ ; immediate
: again    postpone jump , ; immediate
: until    postpone ?jump , ; immediate


    