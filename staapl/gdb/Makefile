
gdb_req: gdb_req.c
	gcc -g -o $@ $<

run: gdb_req 
	gdb -x gdb_req.gdb gdb_req

clean:
	rm -f gdb_req reply.gdb req.bin *~

