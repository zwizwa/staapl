#lang scribble/doc
@(require
   scribble/manual
   scribble/eval
   racket/sandbox)

@(define stdout (current-output-port))

@(define staapl-eval (make-base-eval))

@(define-syntax ex
   (syntax-rules ()
     ((_ () . args)
      (interaction #:eval staapl-eval . args))
     ((_ . args)
      (defs+int #:eval staapl-eval . args))))

@;{ Disables document output during require.  }
@(define-syntax-rule (for-module spec)
   (let ((dummy
           (begin
             (set! staapl-eval (make-base-eval))
             @;(printf "entering ~a\n" 'spec)
             (staapl-eval '(require spec)))))
      (defmodule spec)))



@title{Staapl reference guide}

This document describes the different module components of the Staapl
system, providing an @emph{inside} view.  For interactive use as a
Forth system, these Scheme components are hidden and only the Forth
language and interactive console are exposed through a simpler
interface.

We start with Scat, a concatenative and compositional language with
threaded state, and move onwards to Coma and its composition and
primitive definition mechanisms.  Then we work our way up through
Forth control words, prefix parsing words and the PIC18 backend to end
up at the project management code which enables the use of multiple
instantiated languages.

@section{Representation Language}

@for-module[staapl/scat]


All concatenative code is built on top of a concatenative and
compositional language called Scat with extensible syntax and
semantics.  The term @emph{concatenative} refers to program syntax
being build from concatenation of subprogram names.  The term
@emph{compositional} refers to program semantics where concatenation
is interpreted as composition of functions referred to by names.

Like the Joy language, the meaning function relating syntax to
semantics is a homomorphism from the syntactic monoid onto the
semantic monoid. That is, the syntactic relation of concatenation of
identifiers maps directly onto the semantic relation of composition of
functions. It is a homomorphism instead of an isomorphism because it
is onto but not one-to-one, that is, some sequences of identifiers
have the same meaning (i.e. @scheme[dup +] and @scheme[2 *]) but no
identifier has more than one meaning.


A @scheme[scat:] expression composes Scats functions.

@specform[(scat: word ...)]{}

The @scheme[_word]s refer to Scat functions in the @scheme[(scat)]
namespace, implemented as prefixed identifiers.  If there are no
arguments, the identity function is returned.

A @scheme[scat>] expression can be used to test Scat compositions
interactively.

@specform[(scat> word ...)]{}

This constructs a function by passing the @scheme[_word]s to
@scheme[scat:], and prints the result of applying this function to an
empty parameter stack, top element on the right.

@ex[() 
(scat> 1 2)
(scat> 1 2 +)
]

These forms support the @scheme[unquote] operation to introduce scheme
expressions into Scat code compositions.  This can be done in two
ways.  A non-quoted unquote is interpreted as a Scat function, while a
quoted unquote is interpreted as a literal value.  Note that by
default identifiers in Scat code @emph{always} represent functions.
Literal values occuring in code are a notational shorthand for
functions that push a value to the parameter stack.

@ex[()
(let ([fn (scat: 1 +)]
      [val 123])
  (scat> 1 ,fn)
  (scat> ',val))
]


New functions can be defined using the @scheme[define-ns] form.

@specform[(define-ns namespace name value)]{}

The @scheme[_namespace] refers to a list of prefixes for Scat
identifiers when represented as a Scheme identifier, @scheme[_name] is
the base identifier, and @scheme[_value] is the function value. I.e.:

@ex[()
(define-ns (scat) foo (scat: 1 2 3))
(define-ns (scat) bar (scat: + +))
(scat> foo)
(scat> foo bar)
]

There is a shorthand @scheme[compositions] for defining multiple
words.

@specform[(compositions namespace composer (name word ...) ...)]

The @scheme[_namespace] parameter defines the @emph{destination}
namespace of new definitions.  The @emph{source} namespace is encoded
in the @scheme[_composer]. I.e. @scheme[scat:] will pick from the
@scheme[(scat)] namespace.  This allows the definition of different
primitive Scat semantics by taking functionality from one namespace to
define primitives of another.  The equivalent of the previous example
is:

@ex[()
(compositions (scat) scat:
  (foo  1 2 3)
  (bar  + +))
]


The primitives available in the Scat namespace are currently not
documented.  They are largely compatible with Joy or directly snarfed
from Scheme.  These primitives are not so important for general use,
since we mostly employ a different semantics for Scat called Coma, a
concatenative macro language.

The main idea of a functional representation of concatenative code is
that the threaded state object never needs to be explicitly
manipulated; one only manipulates point-free function compositions.
Internally, Scat functions are modeled as Scheme functions mapping a
single state object derived from the @scheme[stack] structure to
another state object.  Direct access to the state representation is
only necessary within language primitives, and as such the
constructors, field accessors and pattern matching form
@scheme[state-lambda] for (derived) state structures are currently not
documented.



@section{Concatenative Macros}

@for-module[staapl/coma]


The Coma language is a Scat-like language residing in the
@scheme[(macro)] namespace.  It can be accessed using the
@scheme[macro:] and @scheme[macro>] forms analogous to the
@scheme[scat:] and @scheme[scat>] forms in the previous section.

@specform[(macro: word ...)]{}
@specform[(macro> word ...)]{}

Coma's values are lists of @emph{stack machine instructions}, and its
operations transform these lists.  The transformations are always
local to the end of the list, so the list of instructions behaves as a
stack.  Coma is thus a stack language serving as @emph{meta language}
for another stack language, the machine language of an idealized
@emph{stack computer}, which in its most basic form has five instructions. 

@itemize{

@item{@scheme[[qw literal]] Quote Word loads a literal (number) from the
      instruction stream onto the parameter stack.}

@item{@scheme[[cw address]] Call Word performs a procedure call to
      a named function.}

@item{@scheme[[exit]] aborts the execution of the current procedure.}

@item{@scheme[[jw address]] Jump Word performs a jump (tail call).}

@item{@scheme[[jw/if address]] Jump Word If performs a conditional
      jump after popping a boolean off the parameter stack.}

}

The Coma language performs @emph{partial evaluation} on the
@scheme[qw] instructions, but leaves the other instructions alone.
Obviously, to be useful, this instruction set needs to be extended
with logic, arithmetic and memory primitives provided by the concrete
architecture.


@ex[()
(macro> 1 2)
(macro> 1 2 +)
(macro> 123 345 10 3 -)
]

The Coma composition mechanism is the same as Scat's. The difference
of the two languages lies in their primitives, and in the way literals
are treated.  

When writing a compiler for a particular architecture, the task is to
define the semantics of the primitive transformers in terms of (real)
target machine code transformers.  This can be done using the form
@scheme[patterns], a pattern matching mechanism for stack machine
code.

@specsubform/subs[(patterns namespace (lhs rhs) ...)
              ([lhs ([opcode operand ...] ... name)]
               [rhs ([opcode operand ...] ...)
                    expr])]

Each clause @scheme[(_lhs _rhs)] defines how the Coma macro
@scheme[_name] transforms a particular pattern of instructions
@scheme[([opcode operand ...]  ...)] on the top of the code stack.
For each unique name, the corresponding patterns are grouped and tried
from top to bottom until there's a match.  The @scheme[_opcode]s in
@scheme[_lhs] are matched literally as symbols, while the
@scheme[operand]s bind variables in the tagged lists representing
instructions.  The @scheme[_namespace] is most likely
@scheme[(macro)].

If @scheme[_rhs] is a list of lists, the macro will replace the
matched instructions with a list of template instructions with the
@scheme[_opcode]s quoted literally and the @scheme[_operand]s
evaluated. If @scheme[_rhs] is not a list of lists, it is a Scheme
expression that evaluates to a macro which is subsequentially
evaluated on the code stack with the matched instructions removed.





For example, Let's redefine the Coma macro @scheme[+] to use the PIC18
instructions @scheme[addlw].

@ex[()
(patterns (macro)
  (([qw a] [qw b] +) ([qw (+ a b)]))
  (([qw a] +)        ([addlw a])))
(macro> 1 2 +)
(macro> 123 +)
]

On top of the @scheme[+] just defined, we define a macro @scheme[-]
using macro delegation.  The second pattern doesn't produce a macro
but throws an exception indicating that this operation is compile time
only.

@ex[()
(patterns (macro)
  (([qw a] -)        (macro: ',(* -1 a) +))
  ((-)               (error 'runtime-not-implemented "-")))
(macro> 10 3 -)
(macro> -)
]

Observe that @scheme[(macro> 1 2 +)] is now different than before.  We
used direct evaluation in the expression @scheme[(+ a b)].  However,
it is best to use the @scheme[tscat:] form for this.

@specform[(tscat: word ...)]

This is like @scheme[scat:] in that the @scheme[_word]s come from the
@scheme[scat] namespace, but it is different in two ways.  If
@scheme[_word] is present in the lexical context, it is interpolated
as a quoted literal value. In the case above, the expression
@scheme[(tscat: a b +)] is equivalent to @scheme[(scat: ',a ',b +)].
Additionally, the computation is wrapped to be evaluated at assembly
time.  This allows code addresses that are only available at assembly
time to be used in partial evaluation.  The previous example can now
be adapted to:

@ex[()
(patterns (macro)
  (([qw a] [qw b] +) ([qw (tscat: a b +)]))
  (([qw a] +)        ([addlw a])))
(macro> 1 2 +)
(macro> 123 +)
]

Beyond the first level of deconstruction that @scheme[patterns]
performs, enforcing primitives to process lists of opcode . operands
pairs locally as a stack, the pattern matching syntax is PLT Scheme's
@scheme[match] from @scheme[racket/match] and can be used to further
deconstructs operands. This allows the processing of arbitrary data
structures in intermediate code and is the substrate of compile time
computation in Staapl.  One creates primitive code generators and
processors that can be used as components in low-level programs
written in a Forth-like compositional language.

@ex[()
(define-struct vec (x y))
(patterns (macro)
  (([qw x] [qw y] vpack)               ([qw (make-vec x y)]))
  (([qw (struct vec (x y))] vunpack)   ([qw x] [qw y]))
  (([qw (struct vec (x y))] vswap)     ([qw (make-vec y x)])))
(macro> 1 2 vpack)
(macro> 1 2 vpack vswap vunpack)
]

Opcodes are just symbolic tags and can be used inside a
@scheme[patterns] form without the need for declaration.  The only
restriction is that stack machine code produced by a composition of
macros can be processed by the stack machine assembler.  This freedom
allows the introduction of macros that serve as primitives for
arbitrary code generation @emph{idioms}, by allowing arbitrary data to
be passed on the code stack in the form of @emph{compile time data},
represented by the @scheme['qw] tag, or arbitrarily tagged
@emph{pseudo instructions}.  Note that in the future this might change
to a more static approach based on identifiers instead of symbols.

Instead of @scheme[[qw (struct vec (x y))]] we could have introduced
the opcode @scheme['vec] using something like @scheme[[vec v]] or
@scheme[[vec (struct vec (x y))]]. Doing so prevents manipulation of
such objects by generic stack manipulation words like @scheme[dup] and
@scheme[drop].  The general rule is: use @scheme['qw] for anything
that roughly behaves as a target machine value, and use anything else
to prevent the partial evaluator for @scheme['qw] tags to touch the
instruction.

For certain classes of operations one can abstract the possible
patterns over the instances of the class. To this end the macro
@scheme[patterns-class] can be used.

@specform[(patterns-class namespace formals (actuals ...) clause ...)]

The @scheme[_formals] parameter is a list containing identifiers that
parameterize the @scheme[_clause]s, which are pattern clauses passed
to the @scheme[patterns] form.  The @scheme[_actuals] parameters are
values to be filled in for the formal parameters.

The following example defines pattern matching rules for unary
operators on the Microchip PIC18 architecture.  Note that the first
clause implements peephole optimization: it combines a real machine
instruction with the effect of a macro.

@ex[()
(patterns-class (macro)
    [word         opcode]
    [(1+          incf)
     (1-          decf)
     (rot<<c      rlcf)
     (rot>>c      rrcf)
     (rot<<       rlncf)
     (rot>>       rrncf)
     (rot>>4      swapf)]
 (([movf f 0 0] word) ([opcode f 0 0]))
 ((word)              ([opcode WREG 0 0])))
]

There are two functions that allow the evaluation of macros.

@specform[(macro->code macro)]
@specform[(macro->data macro)]

These functions convert the Coma function @scheme[_macro] to assembly
code or data respectively, by applying the function to an empty state
object.  The former produces a naked list, where the first element
corresponds to the top of the machine code stack. The latter only
works if the macro produces a single @scheme[qw] instruction.

@ex[()
    (macro->code (macro: 1 2 3))
    (macro->data (macro: 123))
]

The object returned by @scheme[macro->data] can be an instance of the
@scheme[target-value] structure produced by the @scheme[tscat:] form,
which allows the expression of computations over not yet defined
target code labels.  Such an object can be converted to a number using
the @scheme[target-value->number] function defined in the
@scheme[target] module.  However, when evaluating macros inside the
definition of other macros, it is a better idea to leave the
@scheme[target-value] structure as-is.


@section{Forth: Structured programming, Prefix Macros and Code
         Instantiation}

@for-module[staapl/purrr]

The Forth language built on top of Coma can be seen as a
@emph{programmer interface}, providing a simplified view of the Staapl
system for low-level code development that doesn't depend on knowledge
of the internals of Staapl and the PLT Scheme programming language.
From within this language, code generation primitives defined using
Scheme/Coma can be used in a straightforward way.

Coma is a declarative concatenative language with s-expression syntax
embedded in Scheme.  It is kept as simple as possible in order to
serve as a practical code generation target. Coma deals exclusively
with @emph{function objects} that transform target machine code.  To
obtain executable code an @emph{instantiation} mechanism is necessary
that @emph{evaluates} code transformers.  In addition, Coma does not
allow the manipulation of identifiers from within the language.
Coma's name space management is based on Scheme's @scheme[define] and
@scheme[let], abstracted as the @scheme[patterns] and
@scheme[compositions] forms for definition of primitive and composite
code, and a @scheme[let-ns] form for local names.

Instantiation and name manipulation are managed by two separate
components.  Instantiation is managed by the @scheme[comp] module
which can evaluate code generators to a control flow graph useful for
further optimization.  This is done by giving a meaning to target code
labels and jump instructions, represented by the primitives
@scheme[sym label: exit jump], on which the Forth structured
programming words are built.

The standard Forth structured programming words @scheme[if else then
begin again do while repeat until] defined in the @scheme[control]
module are an extension of purely concatenative Coma code.  They are
implemented the same as in standard Forth, by extending the Coma state
with a @emph{control stack} which is essentially a compile-time
equivalent of the run-time return stack.  (Note that these are
essentially @emph{imperative} constructs, and as such are banned from
the experimental mostly-functional @emph{extended Coma} language,
which is based on a set of higher order macro primitives instead
(i.e. @scheme[ifte]).  However, the Forth structured programming words
are used to implement these higher order macro primitives.)

Name and instantiation manipulation is provided to Forth code by the
words @scheme[create : forth macro require load path], the primitive
@emph{prefix words}. Because prefix parsing lies outside of Coma, a
separate composition mechanism is provided to build Forth-like parsing
words on top of these primtives.  This is done in the form of the
@scheme[substitutions] macro described below.

This Forth frontend that includes code instantiation and prefix
parsing is called @emph{Purrr}.  Together with machine specific
primitive code transformers it yields a complete Forth system.  It is
accessible from within scheme using the @scheme[forth-begin] from.

@specform[(forth-begin word ...)]{}

The @scheme[_word]s refer to Coma transformers or prefix words that
allow the creation of words.  The @scheme[:] prefix word creates
Macros or instantiated Forth words depending on which is the current
active mode.  The current mode is switched using the @scheme[forth]
and @scheme[macro] words.  Forth mode is the default.

Note that the @scheme[purrr] module still generates @emph{pseudo code}
and prints it to the console instead of storing it in a data
structure.  Using it directly only makes sense for testing portable
Forth primitives.  See the next section for a specialization of Purrr.

@ex[() (forth-begin : abc begin 123 again)]


More convenient however is to use the @scheme[forth-compile] form to
use the Forth-style lexer on top of @scheme[forth-begin].

@specform[(forth-compile code-string)]{}

@ex[() (forth-compile "macro : one 1 ; forth : foo one one ;")]


New prefix words can be defined using the @scheme[substitutions] from,
present in the @scheme[forth] module.

@specform[(substitutions namespace ((name word ...) temp) ...)]

This defines a collection of prefix transformers @scheme[_name] which
expand to a template @scheme[_temp] in which the @scheme[_word ...]
metavariables are substituted.  As explained above, its main use is
for words that manipulate identifiers or symbols.

@ex[() (substitutions (macro)
         ((symbol s) ('s)))
       (macro> symbol foo)
]

Have a look at the file @litchar{staapl/purrr/parsing-words.ss} to see
how this is used to define most of Forth's parsing words on top of the
primitive parsing words and quotation.

Note that the Coma and Forth languages are interdependent reusing a
lot of each other's functionality.  They both live in the
@scheme[(macro)] prefixed namespace, but have different functionality.
More specifically, Coma doesn't have Forth's control words and prefix
macros.  These two @emph{language views} on the same core
functionality is based entirely on identifier management by PLT
Scheme's declarative module system.


@section{A Forth compiler for PIC18}

The Purrr language presented in the previous section only has
practical use when it is combined with a code transformer for a
specific machine architecture.  This section describes the backend for
the Microchip PIC18 microcontroller architecture with 8-bit data word
size and 16-bit instruction word size, which maps quite
straightforwardly to a stack machine, making it an ideal primary
architecture for Staapl.

The implementation in Staapl uses the WREG register to implement the
top of stack register, and INDF0 to point to the rest of the parameter
stack in SRAM.  Most arithmetic and conditional jump instructions are
used to implement basic forth data and control operations in a fairly
optimal way.  The compiler produces native code, eliminating the need
for an on-target interpreter.  Forth code can be used in interrupt
routines without trouble.  The PIC18 language includes a full RPN
assembler.  The PIC18F stores code in Flash ROM memory, but has the
ability to program itself.  This is used to provide run-time code
upload in the interaction mode.

The rest of this section documents the basic functionality used to
implement the command line compiler frontend @litchar{staaplc} which
generates Intel HEX files and dictionary information for target
interaction.

@for-module[staapl/prj/pic18]

Note that here we use the @scheme[prj/pic18] module and not the
@scheme[pic18] module directly.  The former is a dynamic version of
the latter, which allows for @emph{incremental} image-based
development on top of the static module code, and supports @emph{live
interaction}.  This is done using a Scheme dynamic namespace object.
Requiring @scheme{prj/pic18} module creates a namespace in which the
PIC18 Forth compiler lives.

@specform[(forth-load/compile path)]{}

This loads a Forth file @scheme[_path].  Relative paths are resolved
to the current directory first, then to the @scheme[pic18] Forth library
path.  

@specform[(save-ihex path)]
@specform[(save-dict path)]

After all forth code is loaded and compiled, these functions can be
used to write out the current accumulated state in the form of binary
code in Intel HEX format, and dictionary metadata as a Scheme script.
Use a device programmer to store the HEX file in the microcontroller
Flash memory.  The device file is a Scheme script that can be
@scheme[load]ed into a Scheme toplevel environment.

@specform[(print-code)]

Prints the current target code state assembly and binary code.

@ex[()
    (forth-load/compile "p18f1220.f")
    (forth-compile ": main begin 1 + again")
    (print-code)]





@section{Interaction}

Upto now we've talked only about the generation of machine code
(syntax).  Staapl contains some functionality to allow the
@emph{execution} of this code on connected target hardware, i.e. for
debugging, testing or profiling purposes.  This system is documented
separately due to dependence of the documentation generation on a live
connection with a microcontroller.

The interactive functionality is provided in two forms.  There is a
set of primitives that allows the interaction with a running target
based on communication with an on-target @emph{monitor}.  This monitor
is a variant of Frank Sergeant's @emph{3-instruction Forth} which
allows access to target memory and is able to execute code.

On top of these communication primitives a @emph{console} interface is
implemented, which simulates traditional Forth interaction, without
the need for an self-hosted Forth.

The console mechanism performs simulation of macros in case a
particular word is not instantiated.  This allows for extremely small
native code kernels.  Interaction words are similar to prefix parsers
and as such are extensible using the @scheme[substitutions] Scheme
form.

