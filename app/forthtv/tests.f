\ black signal without vertical sync    
: test-black-h
    begin
	hsync   
    rx-ready? until ;

\ without vsync, scrolling screen    
: test-bar-h begin barline rx-ready? until ;

\ display loop with 'embedded' interpreter. this doesn't really work well..
: display-interpreter
    begin
	screen run-hook
	borders
	rx-ready? if
	    receive interpret
	then
    again ;
	

