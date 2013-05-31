#lang scribble/doc

@(require 
   scheme/sandbox
   scheme/enter
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

@title{Staapl's PIC18 Forth}

@section{Introduction}

This tutorial document is an introduction to interactive use of the
PIC18 Forth dialect included in the Staapl distribution.  It is
assumed you have a general idea of what Forth is about. The PIC18
Forth is a non-standard dialect. Its macro language and the connection
with Scheme are very different. However, the syntax and the
non-reflective semantics are quite similar to the standard.

This document is structured as a transcript of a live session to give
you an honest view of the work flow.  All code fragments are evaluated
during the generation of this document, and uploaded to the PIC18F1220
chip. No cheats!


The first part illustrates how to compile and upload a complete
application.  On top of this traditional workflow, it is possible to
construct an interactive and incremental development style, where part
of the application is kept constant while another part is
incrementally updated or replaced with the target system running.  The
base application which serves as an example here is actually the
interactive boot monitor interpreter which will later support the
incremental development.

To follow the tutorial, you need a PIC18F1220/1320 microcontroller
connected to a serial port. See
@link["http://zwizwa.be/archive/sheep-schem.ps.gz"]{here} for a
schematic. The essential parts are the ICD2 connector, the serial port
connector, the reset switch, and the serial port pulldown resistor.
To burn the initial interactive monitor code you also need a PIC
programmer that can read Intel HEX files. In this tutorial Microchip's
ICD2 programmer is used, together with the command-line program
@litchar{piklab-prog} from
@link["http://piklab.sourceforge.net/"]{Piklab}. Staapl is part of
PLaneT, so you only need to install PLT Scheme or the command-line
tool MzScheme from
@link["http://www.plt-scheme.org"]{http://www.plt-scheme.org}.


To get going, start MzScheme using the command
@litchar{mzscheme}. Then instantiate the interactive compiler
namespace at the Scheme prompt.

@ex[() 
(require staapl/prj/pic18)
(init-prj)
]

This loads the different components from Staapl and forces
initialization of a project. The first time you enter the
@scheme[require] command, the distribution will be downloaded,
compiled and locally cached.  The output gives some idea of Staapl's
structure.  The PIC18 compiler is built in layers that extend the
functionality of lower layers and specializes some
macros. @litchar{scat} is the functional stack language serving as a
representation layer. @litchar{coma} is a layer of target code
transformers on top of that. @litchar{control} implements Forth style
conditional branching and structured programming. @litchar{comp} is a
compiler that performs simple control-flow analysis. @litchar{asm} is
the assembler. @litchar{forth} provides concrete Forth
syntax. @litchar{live} is the interaction code. @litchar{purrr} is a
link layer between Coma semantics and Forth syntax and @litchar{pic18}
implements the Microchip PIC18 code generator. A line like @litchar{;;
(macro) dup} means the word @scheme[dup] from the @scheme[(macro)]
namespace has been redefined. The internal Scheme code is structured
in a bottom-up manner, but macros are allowed to be
@emph{backpatched}, usually to introduce machine-specific
optimizations.

The simplest way to evaluate code in the compiler namespace is to pass
a string to @scheme[forth-compile] or a filename to
@scheme[forth-load/compile]. Unless otherwise indicated, code that
occurs in an isolated paragraph like this:

@forth{
: foo 123 ;
: bar foo 1 + ;
}

is passed verbatim to the compiler as a string using
@scheme[forth-compile]. The accumulated output assembly code can be
viewed using

@ex[()
    (print-code)
    ]

To save collected binary code to an Intel HEX file use

@ex[()
    (save-ihex "/tmp/example.hex")]

To get rid of code without saving or uploading use
@ex[()
    (kill-code!)
    ]

@section{The Monitor}

Forth traditionally has two modes: compile mode and interpret
mode. We've just met compile mode. In Staapl, interpret mode is called
@emph{command} mode, because it's a bit more general than
interpretating of on-target Forth words.



We'll load the code, save the binary as a
HEX, the dictionary as a Scheme data file and upload the binary code
to the target.

@ex[()
(forth-load/compile "monitor-p18f1220.f")
(save-ihex "/tmp/monitor.hex")
]

Before saving the metadata, we set the console port and baudrate
parameters so they can be included.
@ex[()
(current-console '("/dev/ttyUSB0" 9600))
(save-dict "/tmp/monitor.dict")
]

If you don't have the ICD2 programmer hardware or the
@litchar{piklab-prog} program installed, upload the HEX file to the
PIC manually and start the chip. Otherwise, the @scheme[prog] command
can be used:

@ex[() (prog "/tmp/monitor.hex") ]

Now it's possible to send commands to the PIC using the
@scheme[forth-command] function. The @scheme[ping] command will query
the chip for its identification string.

@ex[()
(forth-command "ping")
]


@section{Incremental development}


A convenient way to write a Forth application on a microcontroller
with flash memory is to split it in two parts: one part is kept
constant: generated as a HEX file and uploaded once, while the other
part can be modified dynamically with the chip running. In our case
the constant part is just the interpreter that enables run-time
interaction.

The data written out by @scheme[save-dict] is the metadata necessary
to interact with the programmed target. For maximum flexibility, it's
implemented as a Scheme script that can contain arbitrary code, so be
careful with trusting dictionary files from others! This is what the
contents looks like.

@ex[() (prj-eval '(write-dict))]

The script starts with a @scheme[require] statement that loads the
compiler code and a command that initializes a clean compiler
namespace. The @scheme[words!] command initialize the target
dictionary which maps symbolic words to word addresses. Further the
script initializes the console the project was connected to and the
memory allocation pointers indicating the free memory region usable
for incremental development.

To use the dictionary file generated in the previous section, start
MzScheme as @litchar{mzscheme -if /tmp/monitor.dict}, or load it at
the prompt:

@ex[()(load "/tmp/monitor.dict")]

Note that loading a dictionary file will create a new project
namespace, and will discard the one that is active if it is not saved
anywhere. At this point, the chip is in an undefined run-time state
and it can still contain leftovers from
previous sessions. To reset the chip do

@ex[()(forth-command "cold")]

to clean the flash leaving only the base application do

@ex[()(forth-command "empty")]

The numbers printed (if any) are the blocks that are erased.  This
will bring the state back to where it was after the dictionary file
got saved to disk. Instead of using @scheme[forth-command] all the
time, we define a shortcut.  

@ex[() (define ~ forth-command) (~ "ping") ]

Note that in practice it might be more convenient to start a REPL that
reads lines and evaluates them using @scheme[forth-command]. This can
be done using @scheme[(repl forth-command)]. We'll see later that for
normal development, the Scheme interface isn't really necessary since
most operations are also available from the command mode. This can be
easily wrapped in a shell script

@verbatim{
#!/bin/sh
[ -z "$1" ] && echo "usage: $0 <dict-file>" && exit 1
exec mzscheme -f $1 -e '(repl forth-command)'
}

Alternatively, it is possible to use the
@link["http://zwizwa.be/darcs/snot"]{Snot} package in Emacs.

Let's try to upload some code. Remember that code that appears in
paragraphs as below will be passed to
@scheme[forth-compile]. Alternatively, it can be placed in a text file
and loaded using @scheme[forth-load/compile].

@forth{
: increment 1 + ;
}

This defines a Forth word @scheme[increment] which takes a number from
the stack, adds @scheme[1] and places the result back on the
stack. The machine code for this word is

@ex[()(~ "print-code")]

The remaining task is to transfer the machine code  to the target. A
dot will be displayed for each uploaded flash unit. For PIC18 this is
8 bytes (4 instructions).

@ex[()(~ "commit")]

To test the word, we place a number on the stack, print the stack,
execute the word, and print the stack again.

@ex[()
(~ "123 ts increment ts")
]

For common operations, if a certain word is not present as
instantiated code on the target chip, the behaviour will be
simulated. This is indicated in the output.

@ex[()
(~ "10 3 + ts")
]

Note that in the interactive example, the @scheme[+] was simulated
because there is no callable code that implements the word in
isolation. However, there is code that impelements the operation
inlined in the definition of the @scheme[increment] word. This is
generally so: the target starts out with no code at all, which means
all words are necessarily macros. They can exist only at compile
time. This is also the reason of existence for the simulation mode:
because so many words are macros, initially, no interaction would be
possible without simulation. This approach allows for the creation of
very small applications on tiny chips.

It is possible to define new macros in Forth code.

@forth{
macro
: add-ten 10 + ;
forth
: add-twenty add-ten add-ten ;
}

There are two important things to note here. Different from standard
Forth, in the Staapl Forth dialect @scheme[macro] words can be defined
with (almost) the same syntax as @scheme[forth] words. The latter will
be instantiated as real code on the target, while the former are used
only as code generators, which means they will be inlined and
optimized. For example, one of the addition inside the
@scheme[add-twenty] word can be performed at compile time and so the
result will be only a single addition.

@ex[() (~ "print-code")]

Now flush out the code and test it.

@ex[() (~ "commit 100 add-twenty ts")]

To inspect the target machine, several commands are
available. Words and locations can be disassembled using
@ex[()
(~ "see boot-40")
]

The symbolic name can be replaced by a numeric address. The assembly
language consists of mostly the Microchip PIC18 opcodes, with some
minor variations to simplify the compiler. To inspect flash memory
code blocks use the Flash Block Dump commands. It prints flash memory
in erase units, which is 64 bytes for the PIC18.

@ex[() (~ "0 fbd")]

For RAM memory, use the Array Block Dump command

@ex[() (~ "0 abd")]

To print a birds-eye view of the first couple of kilobytes of flash
memory in flash erase block units use

@ex[() (~ "4 kb")]

The first line here is the boot monitor code. On the second line
resides the code we just uploaded. The first block in the second line
is the (currently empty) application boot and interrupt vector block,
redirected from the first block.

These last commands give an indication of the general idea behind
the Forth command mode. The code present on the target is kept
minimal, and all the functionality you would normally expect from a
Forth is simulated by the host. This is then provided in a simple
@emph{target view} console interface.

While @scheme[empty] will usually erase the scratch memory properly,
due to target crashes or bad communication links it might sometimes
fail. Erasing individual blocks can be done using

@ex[() (~ "9 erase-block")]

A sequence of dirty blocks can be erased similarly using the
@scheme[erase-from-block] command, which is what @scheme[empty] uses
too. Note that when erasing target flash blocks, the dictionary still
contains references to the now absent code. This is usually not a
problem when re-loading the same file with slight changes, which
overwrites bindings in the namespace, but it can be problematic when
loading different files. In case of confusion, just load the
dictionary from afresh and @scheme[empty] the code.

Finally, we arrive at the the edit-compile-run loop. To work on an
application relative to fixed code, put all the code in a single file
(possibly with nested @scheme[load] statements), and use the command
@scheme[ul <filename>]. This will empty the previously uploaded
incremental code and upload the freshly compiled contents of the file.

@section{Commands and Target Control}

Before we continue, we leave the project management namespace and go
into the compiler namespace directly.

@ex[() (enter-prj)]
 
To leave again use @scheme[(leave-prj)]. Note that it is also possible
to load the compiler straight into the toplevel namespace and get rid
of the project management altogether, i.e. using @scheme[(require
staapl/pic18)].

To create new new interaction commands in the @scheme[(target)]
namespace, use the Scheme @scheme[subsititutions] macro.  For example,
this is the definition of @scheme[ul]. The symbol @scheme[file] in the
left hand side of the substitution rule is a formal parameter that is
replaced in the right hand side.

@ex[()
(substitutions (target)
  ((ul file) (empty mark load file commit)))
]

Essentially, target commands are words that override the default
target interaction semantics, which is to execute the code indicated
by a symbolic word. They do this by influencing the semantics of
subsequent words, and are thus essentially @emph{prefix} words.  The
normal workflow is to create the necessary live interaction tools in
Scheme or Scat, and create a small wrapper around them so they can be
used in command mode. Note that target commands are intended for
manual interactive use only. The composition mechanism doesn't mix
well with function composition.

Alternative to creating target commands, which are really just a
@emph{user interface}, it is possible control the target more directly
from Scheme or Scat code. From within the compiler namespace the the
@scheme[target>] macro can be used, which is like
@scheme[forth-command] but without a string to s-expression parsing
step. It supports the Scat @scheme[unquote] operation. Alternatively
@scheme[scat>] can be used to control the target directly. The
@scheme[target>] command language is in fact implemented in terms of
@scheme[scat>] as a collection of primitive substitution rules. For
details see @litchar{live/commands.ss}. Ultimately, most low-level
utilities defined in @litchar{live/tethered.ss} are exposed.

For example, to transfer a chunk of memory from the target use the
function @scheme[abytes->list]. Note that we're still in the compiler
namespace.

@ex[() 
(with-console 
  (lambda () (abytes->list 0 10)))
] 

