

\ this doesn't really work since the interpreter assumes things get
\ done in time before the 1 byte buffer fills up. mostly this is
\ filled with the sync command.

: display-interpreter
    begin
	\ save interpreter context
	ah @ >x al @ >x
	fh @ >x fl @ >x

	\ draw screen
	screen run-hook
	borders

	\ restore interpreter context
	x> fl ! x> fh !
	x> al ! x> ah !
	
	\ interpret if command ready
	rx-ready? if
	    begin
		receive interpret
	    rx-ready? not until
	then
    again ;
