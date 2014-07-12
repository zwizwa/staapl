: go
    init-synth
    begin midi-poll-once cmd-ready? until ;


\ poll midi and command console    
: go/i
    \ command console: acknowledge current command.
    \ remaining commands are handled by poll-interpret.
    ack 
    init-synth
    begin
        poll-midi
        poll-interpreter
    again


    