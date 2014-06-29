#lang scribble/doc

@(require 
   racket/sandbox
   racket/enter
   scribble/manual
   scribble/eval)

@(define scat-eval (make-base-eval))
@(define-syntax ex
   (syntax-rules ()
     ((_ () . args)
      (interaction #:eval scat-eval . args))
     ((_ . args)
      (defs+int #:eval scat-eval . args))))

@(define (indented strs)
   (apply string-append
          (map (lambda (x) (string-append "    " x))
               strs)))

@(define-syntax-rule (forth-code . strs)
   (verbatim (indented 'strs)))

@(define-syntax-rule (forth . a)
   (verbatim  'a))

;; Find a way to properly quote forth code.


@title{Macro Forth for PIC18}

@section{Introduction}

Staapl is a framework for metaprogramming and code generation which
glues together the Scheme and Forth programming paradigms.  This
document describes the @emph{base language}, a dialect of the Forth
programming language, heceforth called ``The Forth''.

The Scheme metaprogramming facilities are intentionally left out of
the main text of this document to provide a clear view of the bottom
layer of the Staapl system.  Remarks about the meta system are tucked
away in the appendix, which can be safely ignored.  This text aims in
the first place at firmware engineers with a good understanding of
assembler and/or the C programming language.

The main advantages of Forth as a programming language is its quite
direct link to machine code, while retaining a fairly highlevel feel.
One could think of it as a kind of macro assembler, but with a
built-in optimizer.  The highlevel feel comes from the use of a
@emph{stack} to pass parameters from procedure to procedure, or from
macro to macro.

@section{The Forth Idea}

Forth is based on three simple principles.  The first one is the
direct use of a @emph{parameter stack} to store intermediate values.
For example, the procedure @forth[+], which adds two numbers, takes
the two numbers it needs as input from a parameter stack.  To place
numbers on the parameter stack, they just need to be entered.  This
leads to the following code that performs an addition:

@forth-code{
42 1 +
}

This way of arranging numbers and operators is called Reverse Polish
Notation (RPN).  To see how Forth code gets interpreted, just read it
from left to right.  The code above reads: ``Push the number 42; push
the number 1; pop two numbers then add them together and push the
result.''

The second principle is that of @emph{words}.  Because Forth reads
from left to right and does't need parenthesis for parameter passing,
as is for example used in the C programming language, it is easy to
give sequences of words a new name.  For example, if you assign the
name @forth[inc] to the code sequence @forth[inc], the code above can
be replaced by

@forth-code{
42 inc
}

The syntax for defining new words is
@forth-code{
: inc  1 + ;
}

where the colon indicates that the next word @forth[inc] is to be an
alias for the code that follows it @forth[1 +], with the semicolon
indicating the end.

The third principle is that of @emph{memory}.  The code to store the
number @forth[42] in the memory location @forth[100] is:

@forth-code{
42 100 !
}

This means that @forth[!] takes two numbers from the stack.  The first
one is the address, while the second one is the value to store at the
address.  The code to get numbers back out of memory is:

@forth-code["\n100 @\n"]


The word @forth[x] takes the address of the stack, fetches a number
from memory and pushes that number to the stack.
