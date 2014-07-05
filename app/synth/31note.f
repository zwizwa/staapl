\ LOGARITHMIC FREQUENCY SCALE

\ one would think a log frequency range: 2Hz - 20kHz 10^3 ~= 2^10 over
\ 256 = 2^8 steps

\ this is close to a 31 tone scale, which is a lot more musical than a
\ 32 tone scale. the latter is easier to use. so i embed the 31 tone
\ scale in a 32 tone scale with doubled octave slot.

: bass31->period \ n -- lo hi
    _table
    61156               \ start at C1
    dup 32 31 / 2 pow / \ 32 notes in a 31 tone scale
    32         \ nb of items
    ,,geo-seq  \ compile geometric sequence


: 31note \ n -- lo hi
    dup >x
    #x1F and bass31->period    \ get basic freq
    x> rot>> rot>> 7 and \ get octave
    nz? if
        for _>> next
    else
        drop
    then ;

: knob->31period
    ad@ #xFF xor 31note ;

