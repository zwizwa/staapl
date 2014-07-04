all: pk2 usb_rpc

%.o: %.c usb_rpc.h
	gcc -O0 -g -Wall -o $@ -c $<


pk2: pk2.o
	gcc -o $@ $<

usb_rpc: usb_rpc.o
	gcc -o $@ $< -lusb


pk2_ss:
	mzc -v -k pk2.ss

clean:
	rm -f *~ *.o
	rm -rf compiled
