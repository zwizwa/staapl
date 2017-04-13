TODO:
- get the old tutorial working again
- look at flunk presentation



Tilting at Windmills

In this particular story, the imaginary enemy is the C programming
language family tree.  Before diving into any of this I want to make
clear what is my current position.

I love C.  And I hate C.  And I maybe that is what a mature
relationship eventually should become.  Despite all of C's flaws, it
is still the de facto standard for deeply embedded software, and I do
not believe this is going to change any time soon, especially because
C and computer architecture co-evolved.  The processors we use today
are designed to efficiently run C code.

Regarding embedded software, my point is that you should use C only
when there is no other sane way to get the low-level control you need.
But otherwise you should use a high level language with garbage
collection and preferably a strong type system.  My current favorites
are Racket, Erlang, Haskell, Rust, Lua and to some degree Python.

However, there is a gap between assembly language and C that is filled
by another approach: Forth.  If you write a lot of low-level C and
assembly code and have not explored Forth, I think you should.  Just
to get a feel.  Forth is very different.

I dove into Forth around 2004.  Still, I cannot find a satisfactory
articulate explanation about what is so attractive about the idea of
Forth.  But I think it is possible to get an intuitive understand of
this by looking at the life work of Chuck Moore.  His main idea is
radical simplicity when it comes to hardware and software design.

One way of looking at it, is that writing code is "compression by
subdivision and reuse".  What a stack language does, is to allow you
to push this compression very far through the use of implicit local
context -- the data stack.

Essentially, at the bottom line, you are not moving data around
unnecessarily.  What you learn to do when writing Forth code, is to
design the coupling between words in such a way that the number of
stack shuffling operations becomes minimized.  This I believe is the
main reason why Forth can lead to such compact code.

There is obviously a cost to this: it takes more time to write Forth
code.  But the result is almost always elegant.  And this is the
surprising part, maybe.  If you do get the compression right, the code
will look beautiful and simple.  And it will be small.

What I find sad about Chuck's approach makes sense only in Chuck's
world.  It works if you can build an entire product this way, without
the need for code interfaces to the outside world.  It works for
systems that are isolated at communication protocol level.

What I do think he is right about, is that Forth is about hardware.
Forth is a machine language for a stack CPU, and it can lead to
processors that are very simple.

This is where the Staapl project originated: to build a machine
language for the PIC18 that is close to the native machine language,
and see where that goes.






The PIC18 as a Stack Machine

The architecture of the PIC18 is easily mapped to the following
architecture:

- A hardware code stack

- An accumulator

- 3 Pointer register with auto increment/decrement

These map almost directly to:

- Return stack and data stack
- Two pointer registers to use for transfers




A Macro Forth


Compression
