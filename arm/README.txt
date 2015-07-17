make -C .. # makes it run a bit faster
racket demo.rkt

# live:
cd qemu
make
./qemu.sh

# note the pty: /dev/pty/$N

cd ../app
make arm.dict
racket arm.dict /dev/pty/$N

