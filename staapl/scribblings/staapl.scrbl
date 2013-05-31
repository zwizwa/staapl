#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scheme/runtime-path
          (planet cce/scheme:4:1/planet)
)

@;Removed these as they seem to just give broken links, underlined in red.
@;(for-label scheme/base "../macro.ss")


@; @(define-runtime-path demo "../pic18/demo.ss")
@(define box-eval
   (let ((eval (make-base-eval)))
     (eval
      `(begin
         ;; (require (file ,(path->string (simplify-path demo))))
         (require staapl/pic18/demo)
         (require scheme/pretty)
         (pretty-print-columns
          (- (pretty-print-columns) 10))))
     eval))

@(define-syntax ex
   (syntax-rules ()
     ((_ () . args)
      (interaction #:eval box-eval . args))
     ((_ . args)
      (defs+int #:eval box-eval . args))))

@(define-syntax-rule (forth x) (schemeidfont x))
@(define-syntax-rule (asm x)   (schemeidfont x))

@(define (indented strs)
   (apply string-append
          (map (lambda (x) x)
               strs)))

@(define-syntax-rule (forth-ex . strs)
   (verbatim
    (let ((code (indented 'strs)))
       (box-eval `(forth-compile ,(string-append . strs)))
      code)))


@title{Staapl}

@verbatim{
raco pkg install github://github.com/zwizwa/staapl/master
}


@section{Introduction}

Staapl is a Scheme to Forth metaprogramming system.  To illustrate the
general idea we're going to use a concrete application: the Forth
compiler for the 8-bit Microchip PIC18 microcontroller architecture.
This section uses REPL interaction with some example code written on
top of the compiler to demonstrate the code generation process.

@;defmodule/this-package[pic18/demo]
@defmodule[pic18/demo]
@subsection{Forth to Machine Code}

The @scheme[code>] form provided by the demonstration module
interprets the forms in its body as PIC18 Forth code, compiles them,
and prints out the resulting intermediate code with one instruction
per line.

@ex[()
    (code> 123)]

The instruction @scheme[qw] tells the target machine to load the
number @scheme[123] on the run-time parameter stack.  It is short for
``quote word.''  Providing a sequence of numbers in the @scheme[code>]
body will generate concatenated machine code, which is executed by the
target machine from top to bottom.

@ex[()
    (code> 1 2 3)]


Target code is represented in this intermediate form during the first
code generation pass to facilitate code transformations.  It consists
of a mix of pseudo instructions and real PIC18 instructions.  The code
generator will eventually clean up all occurances of @scheme[qw]
before attempting translation to binary machine code.  The
@scheme[pic18>] form performs this extra step and shows real machine code
output.

@ex[()
    (pic18> 123)]

The first instruction stores the contents of the working register in
the 2nd position on the parameter stack, and the second instruction
replaces the contents of the working register with @scheme[123].
Again, concatenating compiler input produces concatenated output:

@ex[()
    (pic18> 1 2 3)]


The intermediate instruction set which contains the @scheme[qw]
instruction is useful for implementing @emph{partial evaluation}
rules.  When compiling a particular Forth word, the compiler can
inspect the code already compiled to determine if it can combine its
effect with the effect to be compiled.

@ex[()(pic18> +)]
@ex[()(pic18> 1 +)]
@ex[()(pic18> 1 2 +)]

This illustrates 3 different modes of computation.  The first program
computes the addition at run-time, taking both input values from the
runtime stack and putting back the result.  The second program adds
the literal value @scheme[1] to the top of the stack using a different
machine instruction.  The third program doesn't perform any run-time
computation at all and simply loads the result of the addition that
was computed at comple-time because both inputs to the addition where
available.

Note that in this last program the result of the compile-time
computation is not shown.  Instead it shows a program @scheme[(1 2 +)]
that gives the result upon evaluation.  The compiler doesn't need to
know the exact value at this point.  It only needs to know that the
value can be determined later when necessary.  This is essential for
integration with the assembler, since these expressions might contain
symbolic representations of code locations that are not yet associated
to numerical addresses.

Using the intermediate form with the @scheme[qw] pseudo-instructions
to compile the Forth program @scheme[1 2 +] shows the key idea: the
target @emph{code list} can be interpreted as a parameter stack, with
the top of the stack at the bottom of the code list.

@ex[()(code> 1 2)]
@ex[()(code> 1 2 +)]

The @emph{code stack} can be used as the argument passing mechanism
for a @emph{language of macros} that is active at compile time.
Machine instructions then become datatypes of this language.  The word
@scheme[+] names a function that operates at compile time.
It inspects the code stack and if it finds one or two @scheme[qw] objects it
can use them as input to the addition operation and compile a simpler
run-time instruction.




@subsection{Scheme as Meta-language}

While traditional Forth has its own metaprogramming facilities,
combining it with a Scheme-based meta system gives the added advantage
of tying into a powerful @emph{lexical scoping} mechanism.  The
ability to assign local names to objects comes in handy when dealing
with complex data structures, which is sometimes difficult to do in a
combinator language like Forth or Joy.  At the same time, the absence
of lexical binding forms in Forth code make it very suitable to be
handled as @emph{data}, avoiding complications associated to name
capture.

This sections introduces the forms @scheme[macro:],
@scheme[compositions], @scheme[patterns] and @scheme[tv:] which
comprise the metaprogramming interface between the Forth and Scheme
languages in Staapl.

@subsubsection{Code and Composition}

A representation of a Forth program can be composed using the
@scheme[macro:] form.

@ex[((define code1 (macro: 123))
     (define code2 (macro: 1 +)))]

The objects created by @scheme[macro:] are called @emph{concatenative
macros}.  These are functions that operate on compilation state
objects.  


@ex[() code1]

The target code associated to the objects @scheme[code1] and
@scheme[code2] can be printed using the function
@scheme[state-print-code] which extracts a code stack from a state object
and prints it, and the function @scheme[state:stack] which produces a
state object with an empty code stack to serve as an initial state.



@ex[((define (print-code code-rep)
       (state-print-code (code-rep (state:stack)))))
    (print-code code1)
    (print-code code2)]

It is seldom necessary to apply concatenative macros to state objects
manually since composition using the @scheme[macro:] form will usually
suffice.  In practice such application is performed by the framework
when generating target code.

The forms residing in the code body of a @scheme[macro:] form have a
one-to-one correspondence to concatenative macros.  @emph{Literals}
found in the body of a @scheme[macro:] are mapped to compilation state
transformers that append @scheme[qw] instructions to the current code
list.  @emph{Identifiers} are mapped to function values using the
@scheme[macro] form, which fishes them out of the @scheme[(macro)]
dictionary.

@ex[() (macro +)]

Note that the objects produced by @scheme[(macro +)] and @scheme[(macro:
+)] are different, although they have the same behaviour.  The former
is a variable reference while the latter creates a new abstraction.
This is similar to the distinction between the Scheme expressions
@scheme[+] and @scheme[(lambda (a b) (+ a b))].

Composing macros that are not in the @scheme[(macro)] dictionary is
possible using the @scheme[unquote] operation.  The @scheme[macro:]
form behaves similar to @scheme[quasiquote].  Unquoted objects come
from the Scheme lexical environment and are interpreted as macros.

@ex[((define code3 (macro: ,code1 ,code2)))
    (print-code code3)]

This illustrates the advantage of keeping target code in
@emph{delayed} form.  The effect of both pieces of code has been
combined into a single target operation.

It is possible to unquote Scheme values as literals by wrapping a
@scheme[quote] form around the @scheme[unquote] form.

@ex[((define value 42)
     (define code4 (macro: ',value)))
    (print-code code4)]

To define new identifiers in a particular dictionary, we can use the
@scheme[compositions] form.

@ex[((compositions (macro) macro:
      (inc  1 +)
      (dec  1 -)))
    (print-code (macro: 100 inc))
    (print-code (macro: 100 dec))]

The first two sub-forms in a @scheme[compositions] form indicate the
target dictionary and body compiler respectively.  The rest of the
body consists of a list of lists, where the first element of each list
is an identifier to which a macro will be associated in the
dictionary, and the rest of the list is a code body that's passed to
the body compiler form.





@subsubsection{Primitives}

The previous section describes how to compose existing code to create
new code by concatenation, and how to evaluate code into a form that
can be passed to the assembler.  This section will describe how to
define primitive macros operating on stacks of target machine
instructions.

Creating an instance of a machine code instruction is done using the
@scheme[op:] form.  It is exactly these objects that are produced when
concatenative macros are evaluated.

@ex[((define ins1 (op: addwf 42 0 0))
     (define ins2 (op: addlw 123))
     (define ins3 (op: qw 123)))]

Real opcodes can be passed to the assembler to produce binary output.
Pseudo instructions cannot.

@ex[()
    (op-apply ins1 0)
    (op-apply ins2 0)
    (op-apply ins3 0)]

However, pseudo instructions can be used to hold intermediate data
during the compilation phase.  The following will illustrate the use
of a form to define new primitive operations.  We'll create a macro
@scheme[add] that will behave like the macro @scheme[+] encountered
before.

Creating new primtive macros is done with the @scheme[patterns] form.
Its first subform specifies the dictionary to which definitions are
associated.  The rest of the forms contains lists of pattern and
template pairs.  The following example defines the @scheme[add] macro
as not taking any input from the code stack, but producing an
@scheme[addwf] instruction as output.

@ex[((patterns (macro)
       ((add) ([addwf POSTDEC0 0 0]))))]

The templates in a @scheme[pattern] form are lists of forms that are
passed on to the @scheme[op:] form, resulting in lists of instruction
objects.  Verifying if it works gives:

@ex[()
    (print-code (macro: add))]

This is an example of a non-optimizing macro which only performs code
generation. To add different behaviour for different input patterns,
extra clauses can be added.

@ex[((patterns (macro)
       (([qw a] add) ([addlw a]))               
       ((add)        ([addwf POSTDEC0 0 0]))))
    (print-code (macro: 123 add))
    (print-code (macro: add))]

When a @scheme[qw] instruction appears in the input, it is
deconstructed and its operand is used as the operand of a
@scheme[addlw] operation.  The @scheme[patterns] form is built on the
PLT Scheme @scheme[match] form, deconstructing a @emph{stack} of
instructions according to input patterns, and constructing lists of
instructions to be added to the compilation state to replace the
matching top of stack.

Upto now our @scheme[add] doesn't really perform compile-time
computation other than selecting a different instruction based on the
presence of literal data.  Using the @scheme[tv:] form we can add
proper computation when there are two literals available.  For now,
think of @scheme[tv:] as an RPN calculator behaving as in:

@ex[()
    (target-value->number (tv: 1 2 +))
    (let ([a 100]
          [b 200])
      (target-value->number (tv: a b +)))
    ]

The @scheme[tv:] form will install wrappers to enable computations
that can only be performed after the assembler has assigned numerical
addresses to code labels.  The use of this compile-time calculator
then leads to an implementation of the @scheme[+] macro whith 3
evaluation modes:

@ex[((patterns (macro)
       (([qw a] [qw b] add) ([qw (tv: a b +)]))
       (([qw a] add)        ([addlw a]))               
       ((add)               ([addwf POSTDEC0 0 0]))))
    (print-code (macro: 1 2 add))
    (print-code (macro: 123 add))
    (print-code (macro: add))]



@subsection{Other RPN Languages}

Staapl contains a generic concatenative language parser in
@scheme[staapl/rpn] which is used to implement the languages
@scheme[macro:], @scheme[tv:], @scheme[scat:] and @scheme[target:].
This language syntax can be extended with @emph{prefix parsers} to
implement a Forth-style prefix syntax for defining words.

For example the @scheme[scat:] form creates unary functions that
operate on a @scheme[state?] object just like the @scheme[macro:]
form.  The state object on which they operate contains a single stack
which can be accessed as a list with the @scheme[state->stack]
function.  This is the same stack which serves as the code stack for
concatenative macros.  The functions in the @scheme[(scat)] dictionary
are thin wrappers around Scheme functions implementing the same
behaviour but operating on the top of the state's stack.

@ex[()
    (state->stack ((scat: 1 2) (state:stack)))
    (state->stack ((scat: 1 2 +) (state:stack)))]

The form @scheme[scat>] is similar to the form @scheme[code>] in that
it creates a concatenative function and applies it to a state with an
empty stack before pretty-printing the result.  The contents of the
parameter stack is printed with the topmost element on the right side.

@ex[()
    (scat> 1 2 3 4)
    (scat> 1 2 3 4 + + +)
    (scat> 123 dup)
    (scat> '(a b c))
    (scat> '(a b c) car)
    (scat> '(a b c) uncons)
    ]

The functions generated by @scheme[scat:] and @scheme[macro:] are
compatible.  However the @scheme[macro:] form creates functions that
operate on tagged values representing target machine instructions.
@ex[()
    (scat> 123 ,(macro: 123))
    (scat> ,(macro: 123) dup)]


@subsection{Representation}

Let's open up the form @scheme[macro:] to see how it is implemented
in terms of Scheme expressions.  All RPN compilers are built around
two forms: the definition compiler form @scheme[rpn-lambda] which
builds @scheme[lambda] expressions out of tagged tokens and the
dictionary compiler form @scheme[rpn-parse] which transforms token
sequences into tagged token sequences according to the RPN language
syntax.

To illustrate the implementation we use the @scheme[pretty-expand]
function to perform macro expansion and pretty-printing with toplevel
Scheme annotations like @scheme[#%app] removed.  The function takes an
optional second argument to indicate the kind of expansion.  The
default is full expansion using @scheme[expand].  In addition single
expansion using @scheme[expand-once] is used.

The form @scheme[(macro: 1 2 +)] expands to an @scheme[rpn-lambda]
form, which nests (folds) the subforms it receives.

@ex[((define stx1
       #'(rpn-lambda
          (macro-push 1)
          (macro-push 2)
          (scat-apply (macro +)))))
    (pretty-expand stx1 expand-once)
    (pretty-expand stx1)]

The @scheme[macro-push] compiles a literal value while
@scheme[scat-apply] compiles a function application.  The
@scheme[macro] form performs dictionary lookup by mapping the
@scheme[+] identifier to @scheme[macro/+] which is bound to the
concatenative macro that compiles an addition operation.  The
@scheme[lit] function transforms a number into a concatenative macro
that will compile the number.  Let's see them in action individually.

@ex[()
    (pretty-expand #'(macro-push 1 state next))
    (pretty-expand #'(scat-apply macro/+ state next))
    ]

The @scheme[rpn-parse] form can be used to attach semantics to the
core RPN syntax.  Let's illustrate it by creating a new RPN language
represented by the @scheme[program:] form.

@ex[((define-syntax-rule (program: code ...)
       (rpn-parse (dictionary
                   (resolve-name)
                   function
                   immediate
                   quoted-program
                   program:
                   (rpn-lambda))
                  code ...))
     (define-syntax-rule (dictionary d) (quote d))
     (define stx2 #'(program: 123 add (foo bar) '(a b c))))
    (pretty-expand stx2)]

The @scheme[(rpn-lambda)] subform in the definition of the
@scheme[program:] form represents the initial content of the first
dictionary element.  The @scheme[rpn-parse] form will interpret the
code forms it finds and append tagged forms to the current dictionary
entry.

The expansion illustrates four syntactic structures in the RPN
language. The @scheme[123] form represents a literal value compiled by
the @scheme[immediate] form.  The identifier @scheme[add] represents a
function reference, with the name reference processed by the
@scheme[resolve-name] form and the result of that compiled by
@scheme[function] form.  The @scheme[(foo bar)] form represents a
quoted program compiled by the @scheme[program:] form and quoted by
the @scheme[quoted-program] form.  The @scheme['(1 2 3)] form is a
quote of a data structure which is a literal value handled by the
@scheme[immediate] form.

The first subform of @scheme[rpn-parse], here defined as
@scheme[dictionary], is the form to which @scheme[rpn-parse] expands,
passing its dictionary output as subforms.  We defined it here as a
@scheme[quote] operation which will stop expansion of the dictionary
form.  By defining @scheme[dictionary] as something that will allow
further expansion, and by defining the other forms passed to
@scheme[rpn-parse] in the definition of @scheme[program:] we can
introduce a semantics.

Suppose the RPN language we want to define uses a list as a state
that's passed from function to function to represent a stack.  We can
define a simple stack machine using these forms:

@ex[((define-syntax-rule (dictionary entry) (begin entry))
     (define-syntax-rule (resolve-name n) n)
     (define-syntax-rule (function fn state subform)
       (let ((state (fn state)))
         subform))
     (define-syntax-rule (immediate expr state subform)
       (let ((state (cons expr state)))
         subform))
     (define-syntax-rule (quoted-program . a) (immediate . a))
     (define (add lst)
       (cons (+ (car lst)
                (cadr lst))
             (cddr lst))))
    (pretty-expand stx2)
    ((program: 1 2 3) '())
    ((program: 1 2 add) '())]

The RPN languages @scheme[macro:], @scheme[target:], @scheme[scat:],
@scheme[live:], @scheme[tv:] have the same syntactic structure with a
different semantics attached by means of forms that are passed to
@scheme[rpn-parse] in the definition of their program quotation forms.
They all use dictionaries with a single element.  Forth compilation
however uses multiple entries.  It uses extensions of
@scheme[rpn-parse] to create new dictionary entries (i.e.  the
@forth{:} word), representing named functionality.


@subsection{Assembler}

The assembler performs two tasks: converting symbolic representation
of machine code to binary and determining code addresses.  The former
is relatively trivial.  The @scheme[staapl/asm] module contains the
special form @scheme[instruction-set] which implements a convenient
syntax for creating assembler functions from strings representing bit
field layout.

@ex[((instruction-set
        (foo (a b c) "0101 aaaa bbbb bbbb" "1111 cccc cccc cccc"))
     (define foo-op (op: foo 1 2 3))
     (define start-address 0))
    foo-op
    (for ((bin-op (in-list (op-apply foo-op start-address))))
      (printf "HEX: ~x\n" bin-op))]

This defines the @scheme[foo] assembler instruction as a 2-word
instruction with 5 bitfields: two constant bitfields and 3 variable
ones.  Whitespace in the strings are ignored, and can be used for
nibble separation.  Instructions defined using this form also define a
corresponding disassembler function.

Target address allocation is non-trivial due to a circular dependency.
Numerical constants can depend on numerical address values, while
address values are determined by the size of instruction words, which
can depend on the value of numerical constants (i.e. shorter
instructions are used for small literal values or short relative
jumps).  To resolve this dependency, the assembler performs multiple
passes using a monotonic relaxation algorithm.  On every pass it
re-evaluates expressions encoded in a @scheme[target-value?] object
until code addresses stabilize.  These expressions can be created by
the compiler using the @scheme[tv:] form.


@subsection{More}

[To be documented].

At the core of the Forth code generator is an incremental compiler
which constructs a control flow graph as its first output pass.
Subsequent optimization passes operate on this structure.

The distribution contains some example Forth code and library routines
that implement the target side of host to target tethering.

There is a significant body of code to perform run-time target access
and incremental code compilation and upload.  This functionality can
be presented to the programmer as a Forth console, which can access
host and target functionality.

Staapl provides the @scheme[staaplc] command line application which
can be used to compile stand-alone Forth programs to binary, for
upload with a microcontroller programmer tool.

@section{PIC18 Forth}

For the Forth language there exist ANSI and ISO standards.  In good
Forth tradition however, the Staapl Forth for the Microchip PIC18 is
non-standard.  It shouldn't be particularly difficult to build a
standard layer on top of it, so we will ignore that issue for now, and
refer to our particular dialect as ``the Forth''.

The Forth is based on a @emph{2--stack machine} model.  One stack is
used to pass data between subroutines while the other is used to keep
track of procedure nesting and other control state.  It is essential
that they are @emph{independent}, as opposed to languages like C which
store parameters and return points on the same stack.  For the PIC18,
the model is a thin layer atop the concrete machine architecture: an
8-bit flash memory based microcontroller.  The implementation actually
uses three stacks: byte size parameter and retain stacks, and a
21--bit wide hardware return stack.

A Forth typically enables direct low--level machine resource access
while providing a base for constructing high--level abstractions.
Traditionally Forth is implemented in machine language.  However,
since the Staapl Forth doesn't need to be self--hosting and so has no
constraints about compiler complexity, it can be written almost
entirely in macro form, instantiated by a meta--system running on a
workstation instead of a small target system.  Staapl's Forth is
subroutine threaded, with each Forth word corresponding to a piece of
inlined machine code or a procedure call.  See the introductory
section for more information about how this works.

It should be noted that the Forth language layer is written on top of
the concatenative macro layer Coma.  Essentially it provides two extra
features: Forth-style @emph{syntax} in the form of prefix parsing
words (like @forth{:} and @forth{variable}) that can manipulate
identifiers by modifying the meaning of subsequent words, and target
code @emph{instantiation} which allows the result of macro expansion
to be invoked at runtime as a machine code procedure.

In the following we use the form @scheme[forth>] from the
demonstration module, which is similar to the forms @scheme[code>] and
@scheme[pic18>] in that it compiles and prints code, but without any
simplifications.  It is different in two respects.  It takes an input
string which is passed to the Forth lexer, and it prints out a
representation of a control flow graph which takes the form of labeled
assembly code, which each basic block starting with a label.

@ex[()(forth> ": foo 1 2 3 ;")]

In addition to the assembly code, it prints two columns.  The first
one is the target Flash ROM address of the instruction, and the second
is the instruction's binary encoding.


@subsection{Syntax}

The Coma language on which the Forth is based uses an s-expression
syntax as it is implemented using Scheme macros.  A parser converts
Forth code into s-expressions.  This parser is based on the
@scheme[rpn-parse] Scheme form we encountered before, and behaves
similarly to Forth parsing words (those that use @forth{accept}).  The
@scheme[rpn-parser] parser form and its exptensions translate a
linear token stream into an s-expression representation of a
Forth-style dictionary data structure.  The token stream passed to
@scheme[rpn-parse] is produced by a Forth-style word/whitespace lexer
which is currently not extensible.

The dictionary is the same as we encountered in the single definition
compilers like @scheme[macro:].  However, it has a different
representation due to a different header structure.  Also in Forth
parsing, the dictionary typically contains more than one entry, with
each one corresponding to a unique name.

Essentially, the dictionary abstracts two actions.  Compilation using
@scheme[rpn-lambda] as encountered before, and identifier definition
using Scheme's @scheme[define] form.

Compared to traditional Forth, the thing to note here is that the
Staapl Forth is not circular in the sense that input parsing isn't
done by Forth words, but by separate entities called @emph{prefix
parsers}.  These parsing words are extensions of the
@scheme[rpn-parse] form, and implement behaviour associated to words
like @forth{:} and @forth{variabe}.  The main reason for this
non--cyclic organization is to keep the Coma core as simple as
possible so it can can support languages other than Forth using
different preprocessing steps.  A simple, non--reflective core
language is easier to meta--program.  However, breaking these
reflective circles looses some flexibility.  Extending the parser (or
lexer) requires the use of a different language.  The upside is that
the language for @scheme[rpn-parse] extensions is a simple high--level
rewrite language based on Scheme's rewrite macro system.  In practice
defining new prefix parsing words isn't often necessary: manipulating
target names (generating collections of words as a module) is better
done in Scheme.

To add some visual to this we can have a look at the intermediate
dictionary s-expression @scheme[rpn-parse] creates before it is
expanded further, and see how it is expanded into Scheme code.  We did
a similar thing in the @scheme[program:] form defined in the
introduction.  The last generated dictionary form is logged for
debugging purposes in the @scheme[forth-dictionary-log] parameter.

@ex[()(pretty-print (forth-dictionary-log))]

This shows a list of two forms, both are macro calls to
@scheme[forth-word].  The first form is an empty definition with only
a header.  In the second form, the forms following the name
@scheme[foo] represent a macro call to @scheme[rpn-lambda] which is
the heart of the RPN to Scheme compilation.  The @scheme[forth-word]
form creates the name @scheme[foo] in the proper namespace and will
use the wrapper function @scheme[wrap-word] to postprocess the result
of the @scheme[rpn-lambda] expansion.

Note that the dictionary data structure looks quite like a typical
Forth dictionary, with some header information describing the
semantics of the code body, and the body a sequence of instructions.
This reflects the typical structure of a Forth parser/compiler.  The
parser effectively executes source code from left to right.  The
prefix parser represented by the word @forth{:} will take the next
identifier from the token stream and create a new dictionary entry
with the proper header forms that implement its semantics.

@;@ex[() (forth> "variable var macro : fetch @ ; forth : def var fetch ;")
@;       (pretty-print (forth-dictionary-log))]





@subsection{Instantiation}

In contrast to standard Forth which supports only direct style
low--level macros based on the @forth{literal}, @forth{postpone} and
@forth{immediate} words, the Staapl Forth uses a high--level
pattern--based syntax for macro definition.  As a result, the syntax
for Forth words and macros is essentially the same.  More
specifically, there exists a syntax subset for which compositions can
either be implemented as meta--system macros or as target words.

Forth words are defined using the @forth{:forth} prefix parsing word,
while macros are defined using the @forth{:macro} word.  Both words
behave as the standard Forth @forth{:} word.  Alternatively the
@forth{forth} and @forth{macro} words can be used to switch the
meaning of the @forth{:} word to mean @forth{:forth} or @forth{:macro}
respectively.

From here on we'll use a more direct interface to the PIC18 compiler.
Code represented as

@forth-ex{
macro
: add + ;
forth
: plusone 1 add ;
}

will be passed as a single string to the @scheme[forth-compile] form.
It accepts a string of Forth code and updates the code registry.
We'll access the registry using the @scheme[code] function:

@ex[((define (code)
       (code-print)
       (code-clear!)))
    (code)]

This is essentially how the compiler works in actual use, except that
code will not be prettyprinted, but exported as a binary file and an
associated symbolic dictionary.

In the Forth code above the word @forth{add} was defined as a macro,
while @forth{plusone} was defined as a target word.  In the output
assembly code it can be seen that all references to @forth{add} have
disappeared.  Only its effect remains: the compilation of an
@asm{addlw} instruction.

Using these language constructs, the Forth programmer can decide which
compositions are to be present as run--time callable code on the
target machine, and which are always inlined.  Whether one or the
other is better depends on the context.  Staapl's Forth allows one to
make these decisions manually, with a convenient way to switch between
the two forms.  Let's define @forth{add} as a target word instead.

@forth-ex{
forth
: add + ;
: plusone 1 add ;
}
@ex[()(code)]

The rule of thumb is that by default one creates Forth words as this
often leads to smaller code and is easier to debug, except when one
wants to use syntactic features not available to Forth words (like
lexical parameters for macros), or when macros yield smaller and/or
faster code because of specialization.  The machine's primitives are
by default implemented as macros that can perform inline code
specialization.

The Forth language in Staapl is inteded as the lowest level system
programming layer.  It is essentially an assembler with a powerful
macro system.  Other languages with a higher level semantics might be
built on top of Coma, but the Forth provides the manual low--level
access when it is needed.



@subsection{Macro vs. Forth}

The differences between macros and forth procedures lie in their
composition system.  Target procedures use the machine instruction
pointer and allow manipulation of the hardware return stack, while
macros are essentially lexically scoped Scheme procedures.

Target words allow multiple entry points with code fallthrough.  This
is specified in source code by not terminating a word definition with
@forth{;} such that execution continues with the following word.
Target names behave essentially as assembler labels.  Sequential code
in Forth source leads to sequential machine code.  Macro words support
only single entry points, as they are separate entities without an
order relation.  Macro words always need to be terminated by a
@forth{;} word.

@forth-ex{
forth
: plustwo 1 +
: plusone 1 + ;
}
@ex[()(code)]
       
Target words allow multiple exit points.  Macro words simulate this
feature by jumping to the end of the macro expansion for each
occurance of the @forth{;} word.

Macro words allow the definition of lexical variables using the
@forth{|} word.  These variables are taken from the compile--time
value stack and behave as constants: occurence of the variable in code
pushes the referred value.  Target words do not support this feature.

@forth-ex{
macro
: 3dup | x | x x x ;
forth
: example 123 3dup ;
}
@ex[()(code)]

Note that lexical names can be introduced anywhere in a macro body.
They are valid until the end of the macro definition.

Lexical variables can be used to create macro closures.  The words
@forth{[} and @forth{]} can be used to create an anonymous macro that
can be passed around as a compile time value.  The word @forth{i}
invokes such a quoted macro.

@forth-ex{
macro
: make-twotimes | x | 
    [ x x ] ;         \ create a nameless macro
: invoke-add 
    i + ;
forth
: example 
    10 make-twotimes  \ create the macro closure
    invoke-add ;      \ pass it to another macro
}
@ex[()(code)]


The target run--time procedure composition mechanism is available to
macros.  This is useful for writing macros that abstract non--standard
control flow.  The @forth{exit} word always means procedure exit,
while @forth{;} means procedure exit in Forth words, but designates
macro exit for macro words.

Another important distinction has to be made regarding arithmetic and
logic operations and numeric representation.  Once instantiated, these
are limited to the target word size.  During compile time computations
however, infinite precision Scheme bignums are used.

@forth-ex{
forth
: one 1000000000000
       999999999999 - ;
}
@ex[()(code)]

Compare the opcode with a previous one and note that it really returns
the 8-bit value 1.

@subsection{Modules}

Forth code can be organized in modules.  These are implemented as PLT
Scheme modules to make it easier to mix Scheme and Forth code in the
same project.  Scheme modules for PIC18 usually contain macros that
expand to a @scheme[pic18-begin] form.

To import a Forth module in a Forth project use the @forth{require},
@forth{planet} or @forth{staapl} words.  This example imports the
@forth{route} macro and its supporting target code which is used in
the subsequent examples.

@forth-ex{
require staapl/pic18/route
}
@ex[()(code)]

This is the same as

@forth-ex{
staapl pic18/route
}

Modules will only be instantiated once, registering target code in a
central repository.


@subsection{Byte cells}

Standard Forth needs a cell size of at least 16 bits.  The Staapl
Forth uses 8--bit cells for data and retain stack, and uses a separate
wide stack for holding return addresses.

Keeping the cell size equal to the native data word size makes it
simpler to directly represent the machine architecture as a stack
machine through a collection of macros that map to simple
instructions.  The small cell size isn't much of a problem for simple
computing tasks.  However, target code addresses do not fit in a
single cell which leads to some non--standard behaviour.  Most notably
the @forth{execute} word now takes two parameters, taking the high
byte of the word address on the top of the parameter stack.  Another
notable point is that loops using @forth{for} @forth{next} are
limited to 256 iterations.

The fact that a word address does not fit in a variable or a single
stack cell promotes the use of @emph{byte tokens} for representing
delayed behaviour, which are implemented in terms of jump tables using
the @forth{route} word.  

@forth-ex{
forth
: one   1 ;
: two   2 ;
: three 3 ;
: four  4 ;

: interpret-byte
    route
       one . two . three . four ;
}
@ex[()(code)]

The @forth{.} word is the same as @forth{exit} except that it does not
mark the subsequent code as unreachable.  The reason this abstraction
works is that a procedure call followed by a procedure exit is always
compiled as a jump.  This means the Staapl Forth is tail-recursion
safe.


@section{Reference}

@defmodule[staapl/macro]

@defform[(scat: rpn-form ...)]{ Most basic RPN form with the following grammar}

@schemegrammar[
rpn-form id
         literal
         (rpn-form ...)
         (quote rpn-data-form)
         (unquote scheme-form)
         ]
@schemegrammar[
rpn-data-form scheme-data-form
              (unquote scheme-form)
              ]

Here @scheme[literal] is any form that is not an identifer or a list,
@scheme[form] is any Scheme form and @scheme[scheme-form] a Scheme
form expanding to a state transformer function.



@defform[(macro: word ...)]

@defform[(op: word ...)]

@defform[(tv: word ...)]{ Creates a composite function similar to
@scheme[scat:] except that it creates a @scheme[target-value?] which
when forced, evaluates the composite function on a @scheme[state?]
with an empty stack.  If this evaluation produces a one-element stack
this value is returned.  Otherwise an exception is raised.  Also,
identifiers in the subforms @scheme[word] are interpreted as quoted
@scheme[target-value?]  if they are bound in the lexical environment
of the form.  Otherwise they come from the @scheme[scat] dictionary.}

@defproc[(target-value? [tv any/c]) boolean?]{ 
A delayed computation that can be forced with
@scheme[target-value-eval] or @scheme[target-value->number]. It may
depend on the @scheme[target-word-address] of any
@scheme[target-word?] it contains.
}

@defproc[(target-word? [tw any/c]) boolean?]{ A symbolic
representation of a target code address.  A node in a control flow
graph.  Assigned a value by the assembler.  Can be used in
@scheme[target-value?] expressions representing a numerical value.}

@defform[(macro id)]

@defform[(patterns dictionary (pattern template) ...)]

@defform[(compositions dictionary composer (id word ...))]

@defproc[(state:stack) state:stack?]{ Creates a @scheme[state?]
object with an empty stack.}

@defproc[(target-value->number [tv target-value?]) number?]

@defproc[(op-apply [op op?] [org number?]) (listof number?)]

@defproc[(state->stack [state state?])  list?]

@defproc[(state-print-code [state state:stack?]) void?]






