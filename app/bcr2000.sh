#aconnect BCR2000:0 'USB Device 0x5f9:0xfff0 MIDI 1':0   # knobs
#aconnect BCR2000:1 'USB Device 0x5f9:0xfff0 MIDI 1':0   # midi in

DEV=`aconnect -o|grep 0x5f9:0xfff0|grep client|awk '{print $2}'`0

aconnect BCR2000:0 $DEV   # knobs
aconnect BCR2000:1 $DEV   # midi in
