#aconnect BCR2000:0 'USB Device 0x5f9:0xfff0 MIDI 1':0   # knobs
#aconnect BCR2000:1 'USB Device 0x5f9:0xfff0 MIDI 1':0   # midi in

DEV=20
aconnect BCR2000:0 $DEV:0   # knobs
aconnect BCR2000:1 $DEV:0   # midi in
