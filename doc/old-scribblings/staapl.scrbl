#lang scribble/doc

@(require 
   scheme/sandbox
   scheme/enter
   scribble/manual
   scribble/eval)

@(define scat-eval (make-base-eval))
@(define-syntax ex
   (syntax-rules ()
     ((_ () . args)
      (interaction #:eval scat-eval . args))
     ((_ . args)
      (defs+int #:eval scat-eval . args))))

       



@title{Staapl}

@section{Introduction}

Staapl brings together two worlds.  This document describes a
top-level view of the system, from a Scheme programmer's point of
view.  However, the Forth in Staapl is perfectly usable as a
stand-alone system.  For people not familiar with Scheme, there is a
separate introduction available that focusses on the Forth and
assembly code side of the project.

@section{The Idea}

Staapl is a collection of microcontroller
@link["http://en.wikipedia.org/wiki/Metaprogramming"]{metaprogramming}
tools for @link["http://www.plt-scheme.org"]{PLT Scheme}. It is based
on the idea that an imperative stack machine model (abstracting a
processor as a
@link["http://en.wikipedia.org/wiki/Forth_(programming_language)"]{Forth}
machine) works well for small embedded microcontrollers, and that
metaprogramming and
@link["http://en.wikipedia.org/wiki/Partial_evaluation"]{partial
evaluation} are almost trivially expressed for functional
@link["http://en.wikipedia.org/wiki/Concatenative_programming_language"]{concatenative}
programming languages due to absence of problems with identifier
hygiene.
 
While the Forth language has been used with great success for decades
in small circles, the benefits of Forth-style programming and
metaprogramming have failed to reach embedded engineering mainstream,
probably due to great differences with the dominant C/C++ based
approach.  Staapl tries to do something about this impedance mismatch
by bringing some of these benefits into the domain of applicative
languages (functions with named arguments). It integrates a
stack-based language into the multi-paradigm Scheme programming
language.

The primary, practical goal of Staapl is to provide a tool chain for
programming low-end (8-bit) microcontrollers in a Forth-style language
extended with powerful metaprogramming facilities. The secondary goal
is to generalize and modularize this approach and extend it in two
directions: to provide a @emph{standard machine model} as an
abstraction point for a large class of small embedded processors, and
to distill a collection of @emph{domain specific languages} (DSLs) on
top of Staapl to integrate the system into existing design flows.

@section{Metaprogramming}

From a code organization point of view, the core idea behind Staapl
can be summarized as follows.  When resource constraints force you to
use a base language with fairly limited abstraction mechanisms, you
are going to resort to metaprogramming of some sort somewhere down the
line. When this inevitability is there, you had better thought about
how to make this metaprogramming as straightforward as possible.

@itemize{

  @item{Use a low-level base language close to the concrete machine
    hardware, well integrated with a sufficiently abstract high-level
    metaprogramming system.}

  @item{Whenever a pattern pops up that cannot be solved effectively
    or efficiently by the low-level language, move complexity to the
    meta level by introducing template code and/or explicit code
    generators. }

}

An approach like this results in a @emph{domain specific language}
(DSL).  Metaprogramming constructs are essentially extensions of base
language.  Starting with a simple base language and building only the
essential extensions on top of that usually leads to more elegant
declarative code that focusses on the @emph{what} instead of the
@emph{how}.


@section{General structure}

The core of the Staapl system is built around @emph{Scat}, a set of
Scheme macros that implements a concatenative language as an extension
of Scheme, and an extension of Scat called @emph{Coma}, which
implements a simple framework for stack machine code generation and
transformation based on partial evaluation.  This framework is also
used to write code transformers for specific machine architectures.
In other words: Staapl contains a tool specifically designed to make
@emph{porting} to a specific microcontroller easier.  Currently, there
is an optimizing backend for Microchip PIC18.

On top of Coma there is a syntax frontend that supports a programming
language based on standard @emph{Forth}, slightly modified for better
integration into the Staapl system.  The most important change is the
move from a low-level macro system that uses explicit code generation
(the @scheme[postpone] and @scheme[literal] standard Forth words), to
a highlevel macro template system based on partial evaluation.  This
Forth frontend serves as an implementation language (for manual use),
but interfaces directly with the Coma language, which has a simpler
syntactic structure and serves mainly as a code generation target.

Care has been taken to make sure the Forth layer can serve as an
abstraction and communication point for larger programming teams.  The
simplicity of the Forth language makes it easy to pick up by engineers
with a focus is on hardware and implementation, without being bothered
by the details of the Scheme/Coma metaprogramming system.  The split
point could be one team that writes code generators for a specific a
domain in Scheme/Coma on top of a hardware abstraction layer (HAL)
provided the hardware team in straightforward Forth.

Because debugging and testing are a significant part of embedded
engineering, Staapl contains tools that simplify interaction between
the code generator and generated code running on a (network of)
microcontroller(s).  The basic idea is that the time between writing
and testing the code should be as small as possible.  Staapl uses the
self-programming facilities of Flash-based microcontrollers to perform
code updates without shutting down the chips, which makes the test
feedback loop incredibly short.  The interactive part is tightly
integrated with the rest of the system.  This also means that it's
possible to integrate test-and-measure feedback loops into the code
generation process.




@section{The tricks}

Looking at the bare essentials, Staapl provides nothing more than an
impedance match between a language based on the lambda calculus
(Scheme) and a flat combinator based language (Coma).  This coupling
is based on the following two mechanisms that rely on hygienic
identifier management and lexical scope.

The first mechanism is provided by the @scheme[patterns] form and
allows the definition of @emph{primitive} stack machine code
transformers.  Stack machine code is implemented as an algebraic data
type, which enables the use of pattern matching to implement stack
machine code rewriting functions in Scheme. This is similar to the
manipulation of abstract syntax trees in an applicative language.

The second mechanism is provided by the @scheme[macro:] form and
allows the @emph{composition} of stack machine code transformers,
extended with @emph{quasiquotation}.  This provides a form of
point-free template programming, analogous to MetaML's Bracket and
Escape operations, but avoiding alpha-equivalence problems.

Most primitive stack machine code transformers perform @emph{partial
evaluation} of the functional subset of Forth: the part of the
language that operates only on the parameter stack.  When all data
necessary to perform a computation is available at code transformation
time, the specification of the computation can be replaced with its
result.  When it applies to numbers, this is usually called
@emph{constant folding}, but it is straightforward to extend to any
kind of abstract object that can be used to control specialzed code
generation.  The key point is that such objects can be manipulated
from @emph{within} Forth code as if they were present at run time,
hiding the compile-time specialization behind the scenes.



@section{The Consequences}

The system is extensible from the Scheme side and the Forth side,
providing an ideal substrate to create DSLs.  The Scheme side is
extensible through the use of PLT Scheme's declarative module system.
Staapl behaves as any other library to PLT Scheme, and uses its name
space management system for optimal integration.

The Forth can be extended by constructing new language features as
code transformers in either Forth (as composite macros relying on
partial evaluation) or Scheme/Coma (providing new primitive
transformers).  On a deeper level, the whole Forth language interface
is programmable.  New dialects or views with @emph{limited} feature
sets are easily constructed.

By relying on partial evaluation, the Forth language can go without
explicit staging annotations (Forth's square brackets).  During code
reading, staging semantics can be easily ignored to grasp the meaning
of code. After non-staged semantics is clear, staging annotations can
be studied to see which parts are actually evaluated at compile time.


@subsection{Combinator Metaprogramming}

What is special about this form of two-stage programming is that it
avoids the usual identifier hygiene problems that pop up in
applicative code generation.

The disadvantage might be that a program needs to be written in a
concatenative language. Whether this is good or bad is ultimately a
matter of taste. My opinion is that as a low-level language to replace
assembly code, Forth is really as good as it gets.  It has an
efficient, almost trivial mapping to concrete machine architectures
and programmable hardware.  It encourages a highly factored
programming style, but this style usually leads to space-efficient
code.  The stack machine lives on the boundary of imperative and
functional programming paradigms, and is a good vehicle to combine
both.



@section{Acknowledgements}

Staapl is the result of a long history of accumulating ideas and
reworking implementations starting with me moving out of my C shoes by
discovering Chuck Moore's colorForth in the fall of 2001, and Scheme
and Joy a bit after that. This has lead to the development of Packet
Forth (2002), Mole (2004), Badnop (2005) and Brood/Staapl
(2006). Staapl is a re-implementation of Badnop. Badnop is a peephole
optimizing Forth cross-compiler written in a Forth dialect, Mole is an
embedded Forth for the graphical programming system Pure Data and
Packet Forth is a Lisp/Forth hybrid that explores multimethods and
linear memory management. The history of these projects is available
in the @link["http://zwizwa.be/darcs"]{archives}. I've made a lot of
mistakes and changes over the years, and I've read a lot about the
approaches and mistakes of others.  While all of the implementation
work has been done by me, the ideas of course come from many places.

The Forth compiler in Staapl, which evolved from Badnop, contains
ideas from Samuel Tardieu's PicForth, Chuck Moore's colorForth, and
Bradford Rodriguez' ``Moving Forth'' series.  Staapl's interaction
system is based on Frank Sergeant's 3-instruction Forth. The ideas
behind Packet Forth are inspired by CLOS and Henry Baker's ideas on
linear lisp.  Scat was called Cat for a while, until I discovered
Christopher Diggins' efforts to build Cat, a typed Joy with a
preprocessing term rewrite system. Cat has fueled my interest in type
systems and statically typed functional languages, which eventually
lead to the discovery of Walid Taha's MetaML as a solid reference
point.  Slava Pestov has been influential by writing about Factor and
the Forth/Lisp tension line.  An invaluable event was the port of
Staapl to PLT Scheme with its excellent support for writing DSLs. The
PLT team has put together a superb collection of documentation which
was instrumental to my education and the evolution of the Staapl
project.  Last but not least, I'd like to mention Manfred von Thun's
Joy language, that seems to have been the necessary nudge that brought
Forth into the modern age.


