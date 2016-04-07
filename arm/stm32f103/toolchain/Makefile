all: gcc-arm-none-eabi-4_8-2014q2

clean:
	rm -rf gcc-arm-none-eabi-4_8-2014q2*
	rm -f *~

gcc-arm-none-eabi-4_8-2014q2-20140609-linux.tar.bz2: 
	wget 'https://launchpad.net/gcc-arm-embedded/4.8/4.8-2014-q2-update/+download/gcc-arm-none-eabi-4_8-2014q2-20140609-linux.tar.bz2'

gcc-arm-none-eabi-4_8-2014q2: gcc-arm-none-eabi-4_8-2014q2-20140609-linux.tar.bz2
	tar xf $<

