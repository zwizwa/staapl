#lang scribble/doc

@(require 
   racket/sandbox
   racket/enter
   scribble/manual
   scribble/eval)

@(define box-eval (make-base-eval))
@(define-syntax ex
   (syntax-rules ()
     ((_ () . args)
      (interaction #:eval box-eval . args))
     ((_ . args)
      (defs+int #:eval box-eval . args))))


@(define (indented strs)
   (apply string-append
          (map (lambda (x) (string-append "    " x))
               strs)))

@(define-syntax-rule (forth . strs)
   (verbatim
    (let ((code (indented 'strs)))
       (box-eval `(forth-compile ,code))
      code)))


@title{Staapl - FLUNK 2013-11-20}

A short presentation about the main ideas behind Staapl, with focus on
the compiler.

Followed by an interactive demo and a Q&A session.


@section{Introduction}


Staapl is:
@itemize{
  @item{A Forth compiler for PIC18}
  @item{An incremental, optimizing, RPN macro assembler}
  @item{A way to use a high level language to generate low level code}
}

Staapl sounds like "stapel" in Dutch, which means stack.

Also a backronym of STack And Array Programming Language.  The APL
part is not yet explored.



@section{PIC18 Stack Machine}

The Staapl Forth dialect is a thin layer on top of the Microchip PIC18
microcontroller assembly language.

Let's load the compiler and some debugging code used in the
presentation.

@ex[() 
(require staapl/pic18
         staapl/comp/debug)
]

In this presentation, the debug command @scheme[print-asm>] is used to
demonstrate the inner working of the compiler.

@ex[() 
(print-asm> +)
]

This PIC18 instruction adds two 8 bit numbers at the top of the stack,
and replaces them with a single result.

The top of the stack is in the working register @litchar{WREG}.

The @litchar{INDF0} pointer register points to the 2nd element on the
stack.



@section{What is a macro?}

A macro is a function that runs at compile time, generating low-level
code from high-level code.

Staapl contains two kinds of macros:
@itemize{
  @item{Racket macros, i.e. hygienic Scheme macros}
  @item{Forth to Assembly compilation macros}
}


@section{Scheme Macros}

The @scheme[print-asm>] command is a @emph{Scheme macro}.  A macro is
sometimes called a @emph{language extension}.  This macro extends
Scheme with a new @emph{special form} that behaves different than
standard Scheme.

By default, a Scheme expression represents a function call.  Nested
scheme expressions are evaluated @emph{inside-out}, and from left to
right.

@ex[() 
(+ 1 2)
(+ (* 2 2) 1)
]

A macro turns this around.  An entire syntax tree is passed to a
function at compile time, instead of being evaluated as Scheme code.
That function can transform the syntax tree into another syntax tree
that is then again evaluated as Scheme code.


@section{Forth Macros}

Some Scheme macros in Staapl allow Forth to be @emph{embedded} in
Scheme.

@ex[()
(macro: 1 2 +)
]

The Staapl special form @scheme[macro:] takes as arguments Forth code
and returns a compilation state transformer function or a @emph{Forth
Macro}, which is the main representation of Forth code in Staapl.

A major point here is that @emph{composition} of Forth macros
corresponds to @emph{Concatenation} of words in Forth source code.


@section{Incremental Compilation}

To demonstrate incremental compilation, the form @scheme[print-asm>]
keeps track of its state throughout this presentation.  Here we reset
that state.

@ex[() 
(print-asm> clear)
]

Each @emph{word} presented as an argument to @scheme[print-asm>]
possibly generates new code while inspecting the code that has been
generated before.  Each word acts independently as a @emph{peephole
optimizer}.   This is a form of @emph{partial evaluation}.

@ex[() 
(print-asm> 1)
(print-asm> 2)
(print-asm> +)
]

Here the first two @emph{words} produce a pseudo instruction
@scheme[qw], while the word @scheme[+] @emph{consumes} earlier
generated code and replaces it with new code.


@section{Expand AND Contract}

The partial evaluation mechanism is essential for using macros
effectively, as they would otherwise generate too much code.

This idea is not new.  It is how a modern C compiler works: inlined
functions behave like macros.

In contrast with C, in Staapl the partial evaluation is @emph{exposed}
to the programmer.  In this case one often uses the term
@emph{multi-stage} language as opposed to macro language.


@section{RPN Assembler}

The partial evaluation mechanism opens up many possibilities.  It
effectively creates a way to compose code generators.

Consider the Forth macros @scheme[addwf] and @scheme[movff], which map
directly to PIC18 instructions.  Both can be programmed to take 1
resp. 2 arguments through the elimination of @scheme[qw] instructions.

@ex[()
(print-asm> clear)
(print-asm> 123)
(print-asm> addwf)
(print-asm> INDF0)
(print-asm> INDF1)
(print-asm> movff)
]

@section{The big picture}

If you generate code, better keep the generator in touch with the
language.  Custom code generation is prone to creating an
unmaintainable mess.

A tool emerges from the interplay of:
@itemize{
  @item{Scheme Macros}
  @item{a low-level stack machine model}
  @item{a programmable, stack-based partial evaluation}
  @item{interactive target access + incremental compilation}
}



@section{Two macro systems}

The Scheme macro system acts as a high level programmer front end.
This reuses Racket's macro system, which is an advanced "programmable
compiler".

The Forth macro system acts as an abstraction of the machine back end:

@itemize{

  @item{Forth is less abstract than assembly: while quite low-level,
  Forth is @emph{local} which promotes reuse through composition.}

  @item{Forth is good enough as a programming language by itself,
  i.e. to write OS and other runtime support code.}

  @item{The partial evaluator is an essential part, and is more
  accessible than the Scheme / Racket optimizer.}

}

@section{Pseudo Instructions}

There is a trade-off between flexible machine-specific optimizations
concentrated in a single compilation pass, and ease of writing a
certain class of partial evaluators.

To find a better sweet spot in this trade-off, some pseudo
instructions have been added.  These get eliminated in a second pass,
after all partial evaluations have executed.

An example of this is @scheme[qw], which pushes a number onto the
stack.  This sequence is very common, but the real PIC18 instruction
sequence for this is too cumbersome to pattern match.




@section{The Core Optimizer}

The Scheme special form @scheme[patterns] defines new pattern matchers
operating on recently compiled opcodes.

@ex[()
(patterns
  (macro)
  (([qw    num] foo) ([retlw (+ num 1)]))
  (([retlw num] foo) ([retlw (+ num 2)])))

(print-asm> clear)
(print-asm> 123)
(print-asm> foo)
(print-asm> foo)
]

The first occurance of @scheme[foo] picks the first matching rule.
The second occurance picks the second matching rule.

Let's redefine one of the standard @scheme[+] behaviors.

@ex[()
(patterns
  (macro)
  (([qw a] [qw b] plus) ([qw (+ a b)])))
(print-asm> clear 1 2)
(print-asm> plus)
]


@section{Criticism}

@itemize{

  @item{Forth works well in a whole-system design, which is rare in
  the modern world.  See "protocol-oriented programming" and the
  USB driver fiasco.}

  @item{Writing good Forth takes more time than C.  Doing stack
  management well requires a way of thinking that does not seem to
  show up in other languages.  However, when done right, the payoff is
  elegant code with a compactness that is hard to beat.}

  @item{The benefits might be a bit specific to the quirky PIC18
  architecture.  It doesn't map so well to machines with a lot of
  registers.  It might map better to a VHDL/Verilog minimal stack
  machine.}

  @item{Forth and Scheme are fringe languages.}

  @item{For real work, I'm back to plain Lambdas, i.e. named variables.}

  @item{Staapl implementation is needs a makeover to take it to the
  next level.}

}





@section{Take home points }

@itemize{

  @item{Forth is a very cheap and effective low-level machine model
  for tiny machines.}

  @item{Code generation is a useful technique.  Acknowledge this by
  making it an integral part of the development system.}

  @item{These idea flourish when combined with a Functional
        Programming approach.}  
}


