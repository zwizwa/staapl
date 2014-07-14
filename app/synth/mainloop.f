load synth/midi-hw.f

\ : go
\    init-synth
\    begin midi-poll-once cmd-ready? until ;


\ poll midi and command console    
: main/i
    \ command console: acknowledge current command.
    \ remaining commands are handled by poll-interpret.
    ack
: main    
    init-synth
    init-midi
    begin
        \ poll-usb-midi
        poll-hw-midi   \ Something not right here..
        poll-interpreter
    again


    