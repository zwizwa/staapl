
Tilting at Windmills

In this particular story, the imaginary enemy is the C programming
language family tree.  Before diving into any of this I want to make
clear what is my current position.

I love C.  And I hate C.  And I maybe that is what a mature
relationship eventually should become.  Despite all of C's flaws, it
is still the de facto standard for deeply embedded software, and I do
not believe this is going to change any time soon.

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
to push this compression very far.  By eliminating explicit local
context.  This forces you to split a problem into many small pieces,
and also to find a way to somehow make the way they are couple
together simple enough so it can be expressed.  Forth makes it
somewhat more difficult to express some problems, but in return will
yield a representation that is very small.  Smaller than what you
would be able to do in C.

As a consequence, Forth can be hard to read, and I believe this is the
main reason why it should be used with care.  My point is that Forth
should be used as a machine language, in a way that C should be used
as a machine language.

What I find sad about Chuck's approach makes sense only in Chuck's
world.  It works if you can build an entire product this way, without
the need for code interfaces to the outside world.  It works for
systems that are isolated at communication protocol level.

So what I am going to show you should probably be understood as an
attempt to create a variant of Chuck's universe, mixed with the
universe created by the Racket Scheme dialect.



The PIC18 as a Stack Machine


A Macro Forth


Compression
