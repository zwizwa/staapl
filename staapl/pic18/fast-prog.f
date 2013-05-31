
\ A line programming command that fits in one PK2 packet, avoiding
\ multiple USB round-trip delays.  The intr. prefix indicates that
\ this is an interpreter extension that can be invoked from the
\ ack-less `intr' command.

: intr.fast-prog
    receive fl !
    receive fh !
    8 for receive !f+ next
    fprog ack ;

    