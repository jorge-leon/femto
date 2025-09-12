# fLisp Manual

### Introduction

> A designer knows he has achieved perfection not when there is nothing
> left to add, but when there is nothing left to take away.
>
> — Antoine de Saint-Exupery

*fLisp* is a tiny yet practical interpreter for a dialect of the Lisp
programming language. It is used as extension language for the
[Femto](https://github.com/hughbarney/femto) text editor.

*fLisp* is hosted in the Femto
[Github](https://github.com/hughbarney/femto) repository, it is released
to the public domain.

*fLisp* is a Lisp-1 interpreter with Scheme like lexical scoping,
tailcall optimization and other Scheme influences.

*fLisp* originates from [Tiny-Lisp by
matp](https://github.com/matp/tiny-lisp) (pre 2014), was integrated into
[Femto](https://github.com/hughbarney/femto) by Hugh Barney (pre 2016)
and compacted by Georg Lehner in 2023.

This is a reference manual. If you want to learn about Lisp programming
use other resources eg.

- The [Common Lisp](https://lisp-lang.org) web site,
- [An Introduction to Programming in Emacs
  Lisp](https://www.gnu.org/software/emacs/manual/html_node/eintr/index.html)
  or
- [The Scheme Programming Language](https://www.scheme.org/).

This manual refers to version 0.13 or later of fLisp.

### Table of Contents

1.  [Introduction](#introduction)
2.  Table of Contents
3.  [Notation Convention](#notation)
4.  [Lisp](#lisp)
    1.  [fLisp Interpreter](#interpreter)
    2.  [Syntax](#syntax)
    3.  [Objects and Data Types](#objects_and_data_types)
    4.  [Environments, Functions, Evaluation](#evaluation)
    5.  [Error Handling](#exceptions)
5.  [Primitives](#primitives)
    1.  [Interpreter Operations](#interp_ops)
    2.  [Input / Output and Others](#in_out)
    3.  [Object Operations](#object_ops)
    4.  [Arithmetic Operations](#arithmetic_ops)
    5.  [Bitwise Integer Operations](#bitwise_ops)
    6.  [String Operations](#string_ops)
6.  [File Extension](#file)
7.  [Double Extension](#double)
8.  [Lisp Libraries](#libraries)
    1.  [Library Loading](#startup)
    2.  [Core Library](#core_lib)
    3.  [fLlisp Library](#flisp_lib)
    4.  [Standard Library](#std_lib)
    5.  [Femto Library](#femto_lib)
9.  [Editor Extension](#editor)
    1.  [Buffers](#buffers)
        1.  [Text manipulation](#text)
        2.  [Selection](#selection)
        3.  [Cursor Movement](#cursor)
        4.  [Buffer management](#buffer_management)
    2.  [User Interaction](#ui)
        1.  [Window Handling"](#windows)
        2.  [Message Line](#message_line)
        3.  [Keyboard Handling](#keyboard)
        4.  [Programming and System Interaction](#programming_system)
10. [Embedding fLisp](#embedding)
    1.  [Embedding Overview](#embedding)
    2.  [fLisp C Interface](#c_api)
    3.  [Building Extensions](#extensions)
11. [Implementation Details](implementation.html)
    (Markdown)
    1.  [Garbage Collection](#gc)
    2.  [Memory Usage](#memory)
    3.  [Future Directions](#future)

#### Notation Convention

We use the following notation rule to describe the *fLisp* syntax:

*name*  
*name* is the name of a variable. In Markdown documents it is shown with
guillemots, like this `«name»`.

`[text]`  
`text` can be given zero or one time.

`[text..]`  
`text` can be given zero or more times.

“` `”  
A single space is used to denote an arbitrary sequence of whitespace.

*fLisp* does not use `[`square brackets`]` and double-dots `..` as
syntactical elements.

Variables names convey the following context:

Lisp object of any type:  
*object* *value* *o* *a* *b* *c*

Program elements:  
*arg* *args* *params* *opt* *body* *expr* *pred*

Integer:  
*i* *j* *k*

Double:  
*x* *y* *z*

Any numeric type:  
*n* *n1* *n2*

Symbol:  
*symbol*

String:  
*string* *s* *s1* *s2* …

List/Cons:  
*cons* *l* *l1* *l2* …

Stream:  
*stream* *f* *fd*

Function/lambda:  
*f*

*fLisp* fancies to converge towards Emacs and Common Lisp, but includes
also Scheme functions. Function descriptions are annotated according to
their compatibility:

<u>C</u>  
Interface compatible, though probably less featureful.

<u>D</u>  
Same name, but different behavior.

<u>S: *name*</u>  
*name* is a similar, but not compatible, function in Emacs Lisp, Common
Lisp or Scheme.

<u>B</u>  
Buggy/incompatible implementation.

Compatibility with Emacs is omitted. By default compatibility with
Common Lisp is annotated. The suffix <u>e</u> is used to indicate
reference to Emacs Lisp, <u>s</u> for Scheme. *fLisp* specific function
are annotated with <u>f</u>.

[^](#toc)

### Lisp

#### fLisp Interpreter

When *fLisp* is invoked it follows a three step process:

1.  Read: program text is read in and converted into an internal
    representation.
2.  Evaluate: the internal representation is evaluated
3.  Print: the result of the evaluation is optionally printed and
    returned to the invoker.

Core functions of the language operate on built-in objects only. *fLisp*
can be extended with additional functions. With respect to the
interpreter, extension functions behave the same as core functions.

#### Syntax

Program text is written as a sequence of symbolic expressions -
<span class="abbr"><span class="dfn">sexp</span></span>'s - in
parenthesized form. A [sexp](https://en.wikipedia.org/wiki/S-expression)
is either a single symbol or a sequence of symbols or sexp's enclosed in
parenthesis.

The following characters are special to the reader:

`(`  
Starts a function or macro invocation, a *list* or *cons* object (see
[Objects and Data Types](#objects_and_data_types)).

`)`  
Finishes a function invocation, *list* or *cons* object.

`'` and `:`  
With a single quote or a colon prefix before a
<span class="abbr">sexp</span>, the <span class="abbr">sexp</span> is
expanded to `(quote «sexp»)` before it is evaluated.

`.`  
The expression` («a» . «b»)` evaluates to a *cons* object, holding the
objects *a* and *b*.

`"`  
Encloses strings.

`\`  
Escape character. When reading a string, the next character is read as
character, even if it is special to the reader.

`;`  
Comment character. When the read encounters a semicolon it ignores it
and all characters up to the next newline.

Numbers are read and written in decimal notation. Number notation and
formatting conventions are the same as in the C language. Exponent
notation is not supported by the reader.

A list of objects has the form:

> `([«element» ..])`

A function invocation has the form:

> `(«name» [«param» ..])`

There are two predefined objects. Their symbols are:

`nil`  
represents: the empty list: `()`, the end of a list marker or the false
value in logical operations.

`t`  
“true”, a predefined, non-false value.

#### Objects and Data Types

*fLisp* objects have one of the following data types:

<span class="dfn">integer</span>  
64 bit singed integer

<span class="dfn">string</span>  
character array.

<span class="dfn">cons</span>  
object holding two pointers to objects.

<span class="dfn">symbol</span>  
string with restricted character set:
`[A-Z][0-9][a-z]!#$%&*+-./:<=>?@^_~`

<span class="dfn">lambda</span>  
anonymous function with parameter evaluation

<span class="dfn">macro</span>  
anonymous function without parameter evaluation

<span class="dfn">stream</span>  
An input/output stream

Objects are immutable, functions either create new objects or return
existing ones.

Characters do not have their own type. A single character is represented
by a *string* with length one.

#### Environments, Functions, Evaluation

All operations of the interpreter take place in an environment. An
<span class="dfn">environment</span> is a collection of named objects.
The object names are of type *symbol*. An object in an environment is
said to be <span class="dfn">bound</span> to its name. Environments can
have a parent. Each *fLisp* interpreter starts with a
<span class="dfn">root</span> environment without a parent.

*lambda* and *macro* objects are functions. They have a parameter list
and a sequence of sexp's as body. When functions are invoked a new
environment is created as child of the current environment. Functions
receive zero or more objects from the caller. These are bound one by one
to the symbols in the parameter list in the new environment.

*lambda*s return the result of evaluating the body in the new
environment.

*macro*s first evaluate the body in the calling environment. The
resulting sexp is evaluated in the new environment and that result is
returned. *macro* bodies are typically crafted to return new sexp's in
terms of the parameters.

When a sexp is evaluated and encounters a symbol it looks it up in the
current environment, and then recursively in the environments from which
the lambda or macro was invoked. The symbol of the first found binding
is then replaced by its object.

#### Error handling

Whenever fLisp encounters an error an exception is thrown. Exceptions
have an <span class="dfn">error type</span> symbol a human readable
<span class="dfn">error message</span> and the <span class="dfn">object
in error</span>, which is nil with generic errors. fLisp does not
implement stack backtracking. Exceptions are either caught on the top
level of an evaluation or by a [`catch`](#interp_ops) statement.

The following error type symbols are defined and used internally:

- `end-of-file`
- `read-incomplete`
- `invalid-read-syntax`
- `range-error`
- `wrong-type-argument`
- `invalid-value`
- `wrong-num-of-arguments`
- `arith-error`
- `io-error`
- `out-of-memory`
- `gc-error`

Exceptions can be thrown via the [`throw`](#interp_ops) function. As
long as applicable use one of the existing error codes with `throw`.

*fLisp* outputs an error message formated as `error: «message»` if the
error object is `nil` otherwise as `error: '«object»', «message»`, where
*object* is the serialization of the object causing the error. *message*
is the error message.

[^](#toc)

### *fLisp* Primitives

*fLisp* counts with a set of built-in functions called
<span class="dfn">primitives</span>. They are grouped in the manual by
the type of objects they operate on. The primitives are bound in the
global environment to the names under which they are described.

#### Interpreter Operations

`(progn[ «expr»..])`  
Each *expr* is evaluated, the value of the last is returned. If no
*expr* is given, `progn` returns `nil`.

`(cond[ «clause»..])`  
Each *clause* is of the form `(«pred»[ «action» ..])`. `cond` evaluates
each *clause* in turn. If *pred* evaluates to `nil`, the next *clause*
is tested. If *pred* evaluates not to `nil` and if there is no *action*
the value of *pred* is returned, otherwise `(progn «action» ..)` is
returned and no more *clause*s are evaluated.

`(bind «symbol» «value»[ «globalp»)` ⇒ *value*  
Create or update *symbol* and bind it to *value*. Return value. First
*symbol* is looked up in the current environment, then recursively in
the parent environments. If it is not found, it is created in the
current environment as long as *globalp* is `nil` or omitted. If
*globalp* is not `nil` *symbol* is always created in the global (top
level) environment.

`(define «symbol» «value»[ «symbol» «value»..])` <u>Ss: define, let</u>  
Create or update named objects: If *symbol* is the name of an existing
named object in the current or a parent environment the named object is
set to *value*, if no symbol with this name exists, a new one is created
in the current environment. `define` returns the last *value*.

Note: `define` cannot be used to define functions, its features rather
resemble `let`.

`(lambda «params» «body»)`  
Returns a *lambda* function described by *body*, which accepts zero or
more arguments passed as list in the parameter *params*.

`(lambda ([«param» ..]) «body»)` <u>s</u>  
Returns a *lambda* function which accepts the exact number of arguments
given in the list of *param*s.

`(lambda («param»[ «param»..] . «opt») «body»)` <u>s</u>  
Returns a *lambda* function which requires at least the exact number of
arguments given in the list of *param*s. All extra arguments are passed
as a list in the parameter *opt*.

`(macro «params» «body»)`  
`(macro ([«param» ..]) «body»)` <u>s</u>  
`(macro («param»[ «param»..] . «opt») «body»)` <u>s</u>  
These forms return a macro function. Parameter handling is the same as
with lambda.

`(quote «expr»)`  
Returns *expr* without evaluating it.

`(catch «expr»)` <u>D</u>  
Evaluates *expr* and returns a list with three elements:

*error_type*  
`nil` on success or an error type symbol.

*message*  
A human readable error message.

*object*  
The result of the the expression or the object in error.

`(throw «result» «message»[ «object»])` <u>D</u>  
Throws an exception, stopping any further evaluation. *result* is the
error type symbol, *message* is a human readable error string and
*object* is the object in error, if any.

#### Input / Output and Others

`(open «path»[ «mode»])` <u>S: open</u>  
Open file at string *path* with string *mode* and return a stream
object. *mode* is `"r"`ead only by default.

`open` can open or create files, file descriptors and memory based
streams.

Files:  
*path*: path to file, *mode*: one of `r`, `w`, `a`, `r+`, `w+`, `a+`
plus an optional `b` modifier.

File descriptors:  
*path*: `<«n»` for reading, `>«n»` for writing. *n* is the number of the
file descriptor. Omit *mode*.

Memory streams:  
For reading *path* is the string to read, *mode* must be set to: `<`.
The name of the opened file is set to `<STRING`.

For writing *path* is ignored, *mode* must be set to: `>`. The name of
the opened file is set to `>STRING`.

`(close «stream»)` <u>S: close</u>  
Close *stream* object

`(file-info «stream»)` <u>f</u>  
Returns `(«path» «buf» «fd»)` for *stream*. *buf* is either `nil` or the
text buffer of a memory stream. *fd* is either the integer
representation of the file descriptor or `nil` when *stream* is already
closed.

`(read` *stream*`[ eof-value])` <u>S: read</u>  
Reads the next complete Lisp expression from *stream*. The read in
object is returned. If end of file is reached, an exception is raised,
unless *eof-value* is not `nil`. In that case `eof-value` is returned.

`(write «object»[ «readably»[ «fd»]]` → object  
Formats *object* into a string and writes it to the default output
stream. When *readably* is not `nil` output is formatted in a way which
which gives the same object when read again. When stream *fd* is given
output is written to the given stream else to the output stream. `write`
returns the *object*.

`(eval «object»)`  
Evaluates *object* and returns the result.

#### Object Operations

`(null «object»)`  
Returns `t` if *object* is `nil`, otherwise `nil`.

`(type-of «object»)`  
Returns the type symbol of *object*.

`(consp «object»)`  
Returns `t` if *object* is of type cons, otherwise `nil`.

`(symbol-name «object»)`  
If *object* is of type symbol return its value as string.

Returns `t` if *object* is of type cons, otherwise `nil`.

`(cons «car» «cdr»)`  
Returns a new cons with the first object set to the value of *car* and
the second to the value of *cdr*.

`(car «cons»)`  
Returns the first object of *cons*.

`(cdr «cons»)`  
Returns the second object of *cons*.

`(same «a» «b»)`  
Returns `t` if *a* and *b* are the same object, `nil` otherwise.

#### Arithmetic Operations

`(i+ «i» «j»)`  
Returns the sum of *i* *j*.

`(i* «i» «j»)`  
Returns the product of *i* *j*.

`(i- «i» «j»)`  
Returns *i* minus *j*.

`(i/ «i» «j»)`  
Returns *i* divided by *j*. Throws `arith-error` if *j* is zero.

`(i% «i» «j»)`  
Returns the rest (modulo) of the integer division of *i* by *j*. Throws
`arith-error` if *j* is zero.

`(i= «i» «j»)`  
`(i< «i» «j»)`  
`(i> «i» «j»)`  
`(i<= «i» «j»)`  
`(i>= «i» «j»)`  
These predicate functions apply the respective comparison operator
between *i* *j*.

#### Bitwise Integer Operations

`(& «i» «j»)`  
Returns the bitwise and operation on *i* and *j*.

`(| «i» «j»)`  
Returns the bitwise or operation on *i* and *j*.

`(^ «i» «j»)`  
Returns the bitwise xor operation on *i* and *j*.

`(<< «i» «j»)`  
Returns *i* shift left by *j* bits.

`(>> «i» «j»)`  
Returns *i* shift right by *j* bits.

`(~ «i»)`  
Returns the bitwise negation of *i*.

#### String Operations

`(string-length «string»)`  
Returns the length of *string* as a *number*.

`(string-append «s1» «s2»)`  
Returns a new string consisting of the concatenation of *string1* with
*string2*.

`(substring «string»[ «start» [«end»]])`  
Returns the sub string from *string* which starts with the character at
index *start* and before index *end*. String indexes are zero based,
negative indexes count from the end of *string*. If *end* is not given
it defaults to the end of *string*. If *start* is not given, it defaults
to the start of *string*.

`(string-search «needle» «haystack»)` <u>C</u>  
Returns the position of *needle* if it is contained in *haystack*,
otherwise `nil`.

`(ascii «integer»)`  
Converts *integer* into a *string* with one character, which corresponds
to the ASCII representation of *integer*.

`(ascii->number «string»)`  
Converts the first character of *string* into an *integer* which
corresponds to its ASCII value.

[^](#toc)

### File Extension

<span class="mark">Tbd. carry over comprehensive documentation from
`file.c`</span>

`(fflush[ «stream»])`  
Flush *stream*, output or all streams

`(fseek «stream» «offset»[ «relativep»])`  
Seek position *offset* in *stream* or input. If *offset* is negative
seek from end, if *relativep* is not null seek from current position, be
default seek from start

`(ftell[ «stream»])`  
Return current position in *stream* or input.

`(feof[ «stream»])`  
Return `end-of-file` if stream or input are exhausted, else `nil`

`(fgetc[ «stream»])`  
Return the next character from *stream* or input.

`(fungetc «i»[ «stream»])`  
`ungetc()` integer *i* as char to *stream* or input.

`(fgets[ «stream»])`  
Read a line or up to `INPUT_FMT_BUFSIZ` from *stream* or input.

`(fstat «path»[ «linkp»])`  
Get information about file at *path*.

`(fttyp[ «fd»])`  
Return true if input or stream *fd* is associated with a TTY.

`(fmkdir «path»[ «mode»])`  
Create directory at *path* with *mode*.

`(popen «line»[ «mode»])`  
Run command line and read from/write to it

`(pclose «stream»)`  
Close a *stream* opened with `(popen)`

`(system «string»)` ⇒ *exit_code*  
Execute *string* as command line of a system shell subpprocess according
to the [system(1)](https://man7.org/linux/man-pages/man3/system.3.html)
and return the shell *exit_code* as integer.

`(getenv «name»)` ⇒ *value*  
Return the value of the environment variable *name* as string. If *name*
does not exist return `nil`.

[^](#toc)

### Double Extension

`(d+ «x» «y»)`  
Returns the sum of *x* *y*.

`(d* «x» «y»)`  
Returns the product of *x* *y*.

`(d- «x» «y»)`  
Returns *x* minus *y*.

`(d/ «arg»[ «div»..])`  
Returns *x* divided by *y*, or `inf` if *y* is zero.

`(d% «x» «y»)`  
Returns the rest (modulo) of the integer division of *x* by *y* or
`-nan` if *y* is zero.

`(d= «x» «y»)`  
`(d< «x» «y»)`  
`(d> «x» «y»)`  
`(d<= «x» «y»)`  
`(d>= «x» «y»)`  
These predicate functions apply the respective comparison operator
between *x* *y*.

[^](#toc)

### Lisp Libraries

#### Library Loading

On startup, both `femto` and `flisp` try to load a single Lisp file. The
default location and name of this <span class="dfn">startup file</span>
are hardcoded in the binary and can be overwritten with environment
variables:

Startup file  
femto: `femto.rc`, `FEMTORC`

flisp: `flisp.rc`, `FLISPRC`

Library path  
femto: `/usr/local/share/femto`, `FEMTOLIB`

flisp: `/usr/local/share/flisp`, `FLISPLIB`

The library path is exposed to the Lisp interpreter as the variable
`script_dir`.

The provided startup files implement a minimal library loader, which
allows to load Lisp files from the library path conveniently and without
repetition. The command to load the file `example.lsp` from the library
is `(require 'example)`.

Femto provides the following set of libraries:

core  
Integrated in the `.rc` files, always loaded. The core library
implements the minimum required Lisp features for loading libraries.

flisp  
Implements expected standard Lisp functions and additions expected by
`femto` and `flisp`.

string  
String manipulation library.

femto  
`femto` editor specific functions.

bufmenu, defmacro, dired, info  
`femto` editor utilities

git, grep, oxo  
`femto` editor modules

#### Core Library

`(list` \[*element* ..\]`)` <u>C</u>  
Returns the list of all provided elements.

`(defmacro «name» «params» «body»)` <u>C</u>  
`(defun «name» «params» «body»)` <u>C</u>  
Defines and returns a macro or function, respectively.

`(setq «symbol» «value»[ «symbol» «value»..])`  
Create or update named objects: If *symbol* is the name of an existing
named object in the current or a parent environment the named object is
set to *value*, if no symbol with this name exists, a new one is created
in the top level environment. `setq` returns the last *value*.

`(curry («func» «a»))`  
Returns a lambda with one parameter which returns `(«func» «a» «b»)`.

`(typep («type» «object»))` <u>C</u>  
Returns true if *object* has *type*.

`(integerp «object»)` <u>C</u>  
`(doublep «object»)` <u>C</u>  
`(stringp «object»)` <u>C</u>  
`(symbolp «object»)` <u>C</u>  
`(lamdap «object»)` <u>C</u>  
`(macrop «object»)` <u>C</u>  
`(streamp «object»)` <u>C</u>  
Return `t` if *object* is of the respective type, otherwise `nil`.

`(numberp «object»)` <u>C</u>  
Return `t` if *object* is integer or double, otherwise `nil`.

`(cadr «list»)` <u>C</u>  
Return the second element in *list*, `(car (cdr «list»))`.  
`(cddr «list»)` <u>C</u>  
Return all elements after the second one in *list*, `(cdr (cdr «list»))`.  
`(caddr «list»)` <u>C</u>  
Return the third element in list, `(car (cdr (cdr «list»)))`.  
`(append [list ..][ a])`  
Append all elements in all *list*s into a single list. If atom *a* is
present, make it a dotted list terminating with *a*.

`(fold-left «func» «init» «list»)` <u>Ss: fold-left</u>  
Apply the binary *func*tion to *start* and the first element of *list*
and then recursively to the result of the previous invocation and the
first element of the rest of *list*. If *list* is empty return *start*.

`(flip «func»)` <u>f</u>  
Returns a lambda which calls binary *func* with it's two arguments
reversed (flipped).

`(reverse «l»)`  
Returns a list with all elements of *l* in reverse order

`(apply «f» [«arg» ..][ l])`  
If *arg* is a single list call lambda *f* with all its elements as
parameters, else call *f* with all *arg*s as parameters. If list *l* is
present append all its elements to the parameter list.

`(print «o»[ «fd»])`  
`write` object *o* `:readably` to stream *fd* or output.

`(princ «o»[ «fd»])`  
`write` object *o* as is to stream *fd* or output.

`(string-to-number «string»)`  
Converts *string* into a corresponding *integer* object. String is
interpreted as decimal based integer.

Converts *integer* into a *string* object.

`(eq «a» «b»)`  
Returns `t` if *a* and *b* evaluate to the same object, number or
string, `nil` otherwise.

`(not «object»)` <u>C</u>  
Logical inverse. In Lisp a synonym for `null`

`(length «obj»)` <u>C</u>  
Returns the length of *obj* if it is a string or a list, otherwise
throws a type exception.

`(string «arg»)` <u>C</u>  
Returns the string conversion of argument.

`(concat `\[*arg*..\]`)` <u>Ce</u>  
Returns concatenation of all arguments converted to strings.

`(memq «arg» «list»)`  
If *arg* is contained in *list*, returns the sub list of *list* starting
with the first occurrence of *arg*, otherwise returns `nil`.

`(mapcar «func» «list»)` <u>Se, Dc</u>  
Apply *func* to each element in list and return the list of results.

In Elisp func has to be quoted, in CL variadic *func* operates on a list
of lists.

`(nfold «f» «i» «l»)`  
“Number fold”: `left-fold`s binary function *f* on list *l* with initial
value *i*. Helper function for n-ary generic number type arithmetic.

`(coerce ifunc dfunc x y)`  
If *x* and *y* are `type-integer` apply binary integer arithmetic
function *ifunc* to them and return the result. If any of them is
`type-double` apply binary double arithmethich function *dfunc* instead.
Helper function for n-ary generic number type arithmetic.

`(coercec «ifunc» «dfunc»)`  
“Coerce curry”: return a lambda `coerce`ing parameters *x* and *y* and
applying *ifunc* or *dfunc* respectively. Helper function for n-ary
generic number type arithmetic.

`(fold-leftp «predicate» «start» «list»)`  
“Predicate fold”: `fold-left` binary function *predicate* to *list* with
initial value *start*. Returns `t` if *list* is empty. Helper functions
for n-ary generic number type comparison.

`(let ((«name» «value»)[ («name» «value»)..]) «body»)` <u>C</u>  
Bind all *name*s to the respective *value*s then evaluate body.

`(let «label»((«name» «value»)[ («name» «value»)..]) «body»)` <u>Cs</u>  
Labelled or “named” `let`: define a local function *label* with *body*
and all *name*s as parameters bound to the *values*.

`(prog1 «sexp»[«sexp»..])` <u>C</u>  
Evaluate all *sexp* in turn and return the value of the first.

`(fload ` *stream*`)` <u>f</u>  
Reads and evaluates all Lisp objects in *stream*.

`(load ` *path*`)` <u>C</u>  
Reads and evaluates all Lisp objects in file at *path*.

`(provide «feature»)`  
Used as the final expression of a library to register symbol *feature*
as loaded into the interpreter.

`(require «feature»)`  
If the *feature* is not alreaded loaded, the file *feature*`.lsp` is
loaded from the library path and registers the *feature* if loading was
successful. The register is the variable *features*.

The following arithmethic functions coerce their arguments to double if
any of them is double, then they use double arithmetic operators. If all
arguments are integer they use integer arthmetic.

`(+[ «num»..])`  
Returns the sum of all *num*s or `0` if none is given.

`(*[ «num»..])`  
Returns the product of all *num*s or `1` if none given.

`(-[ «num»..])`  
Returns 0 if no *num* is given, -*num* if only one is given, *num* minus
the sum of all others otherwise.

`(/ «num»[ «div»..])`  
Returns 1/*num* if no *div* is given, *num*/*div*\[/*div*..\] if one or
more *div*s are given, `inf` if one of the *div*s is `0` and the sum of
the signs of all operands is even, `-inf` if it is odd.

`(% «num»[ «div»..])`  
Returns `1` if no *div* is given, *num*%*div*\[%*div*..\] if one or more
*div*s are given. If one of the *div*s is `0`, the program exits with an
arithmetic exception.

`(= «num»[ «num»..])`  
`(< «num»[ «num»..])`  
`(> «num»[ «num»..])`  
`(<= «num»[ «num»..])`  
`(>= «num»[ «num»..])`  
These predicate functions apply the respective comparison operator
between all *num*s and return the respective result as `t` or `nil`. If
only one *num* is given they all return `t`.

#### fLisp Library

This library implements commonly excpected Lisp idioms. *fLisp*
implements a carefully selected minimum set of commonly used functions.

`(listp «o»)` <u>D</u>  
Returns true if *o* is `nil` or a *cons*.

`(and[ o..])`  
Returns `t` or the last object *o* if none is given or all evaluate to
non `nil`, `nil` otherwise.

`(or[ o..])`  
Returns `nil` if all objects o are `nil`, otherwise returns the first
object which evaluates to non `nil`.

`(reduce «func» «list» «start»)` <u>D</u>  
`reduce` applies the binary *func* to the first element of *list* and
*start* and then recursively to the first element of the rest of the
*list* and the result of the previous invocation: it is “right binding”.

Since `reduce` is right associative and *start* is not optional, it
differs significantly both from Common Lisp and Scheme.

`(max «n»[ «n»..])`  
`(min «n»[ «n»..])`  
Return the biggest/smallest number of all given *n*s.

`(nthcdr «i» «l»)`  
Return sub list of *l* starting from zero-based *i*th element to the
last.

`(nth «i» «l»)`  
Return zero-based *i*th element of list *l*

`(fold-right «f» «o» «l»)` <u>Cs</u>  
Apply binary function *f* to last element of *l* and *o*, then
recursively to the previous element and the result.

`(unfold «f» «o» «p»)` <u>Cs</u>  
Create a list starting with *o* followed by the result of successive
application of *f* to *o* until applying *p* to the result is not `nil`
anymore.

`(iota «count»[ «start»[ «step»]])` <u>Cs</u>  
Create a list of *count* numbers starting with *start* or `0` if not
given by successively adding *step* or `1` if not given.

`(atom «o»)`  
`t` if *o* is not a *cons*.

`(zerop «x»)`  
`t` if number *x* is zero.

`(if «p» «then»[ «else»)`  
Evaluate *then* if predicate *p* evaluates to not `nil`, else evaluate
*else*.

`(equal «o1» «o2»)`  
Return `nil` if *o1* and *o2* are not isomorphic.

*name*  
Buffers are identified by their name. If a buffer name is enclosed in
`*`asterisks`*` the buffer receives special treatment.

*text*  
zero or more characters.

*point*  
The position in the text where text manipulation takes place. The first
position in the text is 0. Note: in Emacs the first position is 1.

*mark*  
An optional second position in the text. If the *mark* is set, the text
between *point* and *mark* is called the
<span class="dfn">selection</span> or <span class="dfn">region</span>.

*filename*  
If set the buffer is associated with the respective file.

*flags*  
Different flags determine the behavior of the buffer. Editor specific
flags: `special`, `modified`.

Mode flags determine the syntax highlighter mode: `cmode` and `lispmode`
are available. If none is set `text` mode is used for syntax
hightlighting.

In the following, any mention to one of them refers to the respective
current buffers property.

##### Text manipulation

`(insert-string «string»)`  
Inserts *string* before *point*. <u>S: insert</u>.

`(insert-file-contents-literally «string» `\[*flag*\]`)`  
Inserts the file *string* after *point*. If *flag* is not nil the buffer
is marked as not modified. <u>B</u>

Note: Currently the flag is forced to nil. The function should return
`(«filename» «count»)` but it returns a flag indicating if the operation
succeeded.

`(erase-buffer)`  
Erases all text in the current buffer. <u>C</u>

`(delete)`  
Deletes the character after *point*. <u>S: delete-char</u>

`(backspace)`  
Deletes the character to the left of *point*. <u>S:
delete-backward-char</u>

`(get-char)`  
Returns the character at *point*. <u>S: get-byte</u>

`(copy-region)`  
Copies *region* to the *clipboard*. <u>S: copy-region-as-kill</u>

`(kill-region)`  
Deletes the text in the *region* and copies it to the *clipboard*.
<u>D</u>

`(yank)`  
Pastes the *clipboard* before *point*. <u>C</u>

##### Selection

`(set-mark)`  
Sets *mark* to *point*. <u>D</u>

`(get-mark)`  
Returns the position of *mark*, -1 if *mark* is unset. <u>S: mark</u>

`(get-point)`  
Returns the position of *point*. <u>S: point</u>

`(get-point-max)`  
Returns the maximum accessible value of point in the current buffer.
<u>S: point-max</u>

`(set-clipboard «variable»)`  
`Sets «clipboard» to the contents of «variable».` <u>S:
gui-set-selection</u>

`(get-clipboard)`  
Returns the *clipboard* contents. <u>S: gui-get-selection</u>

##### Cursor Movement

`(set-point «number»)`  
Sets the point to in the current buffer to the position *number*. <u>S:
goto-char</u>

`(goto-line «number»)`  
Sets the point in the current buffer to the first character on line
*number*. <u>S: goto-line</u>, not an Elisp function.

`(search-forward «string»)`  
Searches for *string* in the current buffer, starting from point
forward. If string is found, sets the point after the first occurrence
of *string* and returns `t`, otherwise leaves point alone and returns
`nil`. <u>D</u>

`(search-backward «string»)`  
Searches for *string* in the current buffer, starting from point
backwards. If string is found, sets the point before the first
occurrence of *string* and returns `t`, otherwise leaves point alone and
returns `nil`. <u>D</u>

`(beginning-of-buffer)`  
Sets the point in the current buffer to the first buffer position,
leaving mark in its current position. <u>C</u>

`(end-of-buffer)`  
Sets the point in the current buffer to the last buffer position,
leaving mark in its current position. <u>C</u>

`(beginning-of-line)`  
Sets point before the first character of the current line, leaving mark
in its current position. <u>S: move-beginning-of-line</u>

`(end-of-line)`  
Sets point after the last character of the current line, i.e. before the
end-of-line character sequence, leaving mark in its current position.
<u>S: move-end-of-line</u>

`(forward-word)`  
Moves the point in the current buffer forward before the first char of
the next word. If there is no word left the point is set to the end of
the buffer. If the point is already at the start or within a word, the
current word is skipped. <u>D</u>: **Note**: Elisp moves to the *end* of
the the next word.

`(backward-word)`  
Moves the point in the current buffer backward after the last char of
the previous word. If there is no word left the point is set to the
beginning of the buffer. If the point is already at the end or within a
word, the current word is skipped. <u>D</u>: **Note**: Elisp moves to
the *beginning* of the previous word.

`(forward-char)`  
Moves the point in the current buffer one character forward, but not
past the end of the buffer. <u>C</u>

`(backward-char)`  
Moves the point in the current buffer one character backward, but not
before the end of the buffer. <u>C</u>

`(forward-page)`  
Moves the point of the current buffer to the beginning of the last
visible line of the associated screen and scrolls the screen up to show
it as the first line. <u>S: scroll-up</u>

`(backward-page)`  
Moves the point of the current buffer to the beginning of the first
visible line of the associated screen and scrolls the screen down to
show it as the last line. <u>S: scroll-down</u>

`(next-line)`  
Moves the point in the current buffer to the same character position in
the next line, or to the end of the next line if there are not enough
characters. In the last line of the buffer moves the point to the end of
the buffer. <u>C</u>

`(previous-line)`  
Moves the point in the current buffer to the same character position in
the previous line, or to the end of the previous line if there are not
enough characters. In the first line of the buffer the point is not
moved. <u>C</u>

##### Buffer management

`(list-buffers)`  
Lists all the buffers in a buffer called `*buffers*`.

`(get-buffer-count)`  
Returns the number of buffers, includes all special buffers and
`*buffers*`.

`(select-buffer «string»)`  
Makes the buffer named *string* the current buffer. Note: <u>C</u> to
`set-buffer` in Elisp.

`(rename-buffer «string»)`  
Rename the current buffer to *string*. <u>C</u>

`(kill-buffer «string»)`  
Kill the buffer names *string*. Unsaved changes are discarded. <u>C</u>

`(get-buffer-name)`  
Return the name of the current buffer. Note: <u>C</u> to `buffer-name`
in Elisp.

`(add-mode-global «string»)`  
Sets global mode *string* for all buffers. Currently the only global
mode is <span class="kbd">undo</span>.

`(add-mode «string»)`  
Set a flag for the current buffer.

`(delete-mode «string»)`  
Reset a flag for the current buffer.

`(find-file «string»)`  
Loads file with path *string* into a new buffer. After loading
`(read-hook «string»)` is called. <u>C</u>

`(save-buffer «string»)`  
Saves the buffer named *string* to disk. <u>C</u>

#### User Interaction

This section lists function related to window and message line
manipulation, keyboard input and system interaction.

##### Window Handling

`(delete-other-windows)`  
Make current window the only window. <u>C</u>

`(split-window)`  
Splits the current window. Creates a new window for the current buffer.
<u>C</u>

`(other-window)`  
Moves the cursor to the next window down on the screen. Makes the buffer
in that window the current buffer. <u>D</u>

Note: Elisp `other-window` has a required parameter *count*, which
specifies the number of windows to move down or up.

`(update-display)`  
Updates all modified windows.

`(refresh)`  
Updates all windows by marking them modified and calling
`update-display`.

##### Message Line

`(message «string»)`  
Displays *string* in the message line. <u>D</u>

`(clear-message-line)`  
Displays the empty string in the message line.

`(prompt «prompt» «default»)`  
Displays *prompt* in the command line and sets *default* as initial
value for the user response. The user can edit the response. When
hitting return, the final response is returned.

`(show-prompt «prompt» «default»)`  
Displays *prompt* and *default* in the command line, but does not allow
editing. Returns `t`.

`(prompt-filename «prompt»)`  
Displays *prompt* in the command line and allows to enter or search for
a file name. Returns the relative path to the selected file name or the
response typed by the user.

##### Keyboard Handling

`(set-key «key-name» «lisp-func»)`  
Binds key key-name to the lisp function *lisp-func*.

`(get-key-name)`  
Returns the name of the currently pressed key, eg: `c-k` for control-k.

`(get-key-funcname)`  
Return the name of the function bound to the currently pressed key.

`(execute-key)`  
Executes the function of the last bound key. <span class="mark">Tbd.
bound or pressed?</span>

`(describe-bindings)`  
Creates a listing of all current key bindings, in a buffer named
`*help*` and displays it in a new window. <u>C</u>

`(describe-functions)`  
Creates a listing of all functions bound to keys in a buffer named
`*help*` and displays it in a new window.

`(getch)`  
Waits for a key to be pressed and returns the key as string. See also
`get-key-name`, `get-key-funcname` and `execute-key`.

##### Programming and System Interaction

`(exit)`  
Exit Femto without saving modified buffers.

`(eval-block)`  
Evaluates the *region* in the current buffer, inserts the result at
*point* and returns it. If *mark* in the current buffer is before
*point* `eval-block` evaluates this *region* and inserts the result at
*point*. If *point* is before *mark* `eval-block` does nothing but
returning `t`.

`(log-message «string»)`  
Logs *string* to the `*messages*` buffer.

`(log-debug «string»)`  
Logs string to the file `debug.out`.

`(get-version-string)`  
Returns the complete version string of Femto, including the copyright.

[^](#toc)

### Embedding fLisp

#### Embedding Overview

fLisp can be embedded into a C application. Two examples of embedding
are the `femto` editor and the simplistic `flisp` command line Lisp
interpreter.

Currently embedding can only be done by extending the build system.
Application specific binary Lisp extensions are stored in separated C
files and the interface code is conditionally included into the `lisp.c`
file. Three extensions are provided: the Femto extension which provides
the editor functionality, the file extension which provides access to
the low level stream I/O functions and others and the double extensions
which provides double float arithmetic.

*fLisp* exposes the following public interface functions:

`lisp_new()`  
Create a new interpreter.

`lisp_destroy()`  
Destroy an interpreter, releasing resources.

`lisp_eval()`  
Evaluate a string or the input stream until exhausted or error.

`lisp_write_object()`  
Format and write object to file descriptor.

`lisp_write_error()`  
Format and write the error object and error message of an interpreter to
a file descriptor.

Different flows of operation can be implemented. The *femto* editor
initializes the interpreter without input/output file descriptors and
sends strings of Lisp commands to the interpreter, either when a key is
pressed or upon explicit request via the editor interface.

The `flisp` command line interpreter sets `stdout` as the default output
file descriptors of the *fLisp* interpreter and feeds it with strings of
lines read from the terminal. If the standard input is not a terminal
`stdin` is set as the default input file descriptor and *fLisp* reads
through it until end of file.

After processing the input, the interpreter holds the results
corresponding to a [`catch`](interp_ops) result in its internal
structure. They can be accessed with the following C-macros:

*error_type*  
`FLISP_RESULT_CODE(interpreter)`

*message*  
`FLISP_RESULT_MESSAGE(interpreter)`

*object*  
`FLISP_RESULT_OBJECT(interpreter)`

Check for `(FLISP_RESULT_OBJECT(interpreter) != nil)` to find out if the
result is an error. Then check for
`(FLISP_RESULT_OBJECT(interpreter) == out_of_memory)` to see if a fatal
condition occured.

On error use `lisp_write_error()` to write the standard error message to
a file descriptor of choice, or use the above C-macros and
`FLISP_ERROR_MESSAGE(interpreter)->string` for executing a specific
action.

*fLisp* sends all output to the default output stream. If it is set to
`NULL` on initialization, output is suppressed altogether.

#### fLisp C Interface

*Interpreter*` *lisp_new(char **«argv», char *«library_path», FILE *input, FILE *output, FILE* debug)`  
`lisp_new()` creates and initializes an fLisp interpreter and returns a
pointer to an *Interpreter* struct to be used in the other functions.
The arguments to `lisp_new()` are:

*argv*  
*library_path*  
The fLisp environment is initialized with this two argument to contain
the following symbols:

*argv0*  
The string stored in `*«argv»[0]`, if any

*argv*  
The list of strings stored in *argv*

*script_dir*  
The string stored in *library_path*

*input*  
Default input stream. If *input* is set to `NULL`, the input stream has
to be specified for each invocation of `lisp_eval()`.

*output*  
Default output stream. If *output* is set to `NULL` a memory stream is
created at the first invocation of the interpreter and set as the
default output stream.

*debug*  
Debug output stream. If set to `NULL` no debug information is generated.

`void lisp_destroy(Interpreter *«interp»)`  
Frees all resources used by the interpreter.

`void lisp_eval(Interpreter *«interp», char *«string»)`  
If *string* is not `NULL` evaluates all Lisp expressions in *string*.

If *string* is `NULL` input from the file descriptor in the *input*
field of the *fLisp* interpreter *interp* is evaluated until end of
file.

If no memory can be allocated for the input string or the input file
descriptor is `NULL` no Lisp evaluation takes place and
`FLISP_RESULT_CODE` field of the interpreter is set to an `io-error`.

`void lisp_write_object(Interpreter *«interp», FILE «*fd», Object *«object», bool readably)`  
Format *object* into a string and write it to *stream*. If *readably* is
true, the string can be read in by the interpreter and results in the
same object.

`void lisp_write_error(Interpreter *«interp», FILE «*fd»)`  
Format the error *object* and the error message of the interpreter into
a string and write it to *fd*. The *object* is written with *readably*
`true`.

<span class="mark">Note: currently only creating one interpreter has
been tested.</span>

#### Building Extensions

An extensions has to create C functions with the signature:
`Object *«primitive»(Interpreter *interp, Object **args, Object **env)`,
where *primitive* is a distinct name in C space. This function has to be
added to the global variable `primitives` in the following format:
`{"«name»", «argMin», «argMax», «type_check», «primitive»}`. Here *name*
is a distinct name in Lisp space.

*interp* is the fLisp interpreter in which *primitive* is executed.
*argMin* is the minimum number of arguments, *argMax* is the maximum
number of arguments allowed for the function. If *argMax* is a negative
number, arguments must be given in tuples of *argMax* and the number of
tuples is not restricted.

When type check is set to on of the `TYPE_*` C-macros the interpreter
assures that all arguments are of the given type and creates a
standardized exception otherwise. When type check is set to `0` the
primitive has to take care of type checking by itself. The C-macro
`CHECK_TYPE` helps with this.

When creating more then one new objects within a primitive, care has to
be taken to register them with the garbage collector. Registration is
started with the `GC_CHECKPOINT` CPP macro. `GC_TRACE(«name», «value»`
creates an object variable *name*, sets it to *value* and registers it
with the garbage collector. The macro `GC_RELEASE` must be called to
finalize the registration. The convenience macro `GC_RETURN(«object»)`
calls `GC_RELEASE` and returns *object*.

Some CPP macros are provided to simplify argument access and validation
in primitives:

`FLISP_HAS_ARGS`  
`FLISP_HAS_ARG_TWO`  
`FLISP_HAS_ARG_THREE`  
Evaluate to true if there are arguments or the respective argument is
available.

`FLISP_ARG_ONE`  
`FLISP_ARG_TWO`  
`FLISP_ARG_THREE`  
Evaluate to the respective argument.

`CHECK_TYPE(«argument», «type», «signature»)`  
Assures that the given argument is of the given type. *type* must be a
type variable like `type_string`. *signature* is the signature of the
primitive followed by “` - `” and the name of the argument to be type
checked. This is used to form a standardized `wrong-type-argument` error
message.

[^](#toc)

### Implementation Details

#### Garbage Collection

*fLisp* implements Cheney's copying garbage collector, with which memory
is divided into two equal halves (semi spaces): from- and to-space.
From-space is where new objects are allocated, whereas to-space is used
during garbage collection. The from-space part of the memory is also
called the <span class="dfn">Lisp object space</span>.

When garbage collection is performed, objects that are still in use
(live) are copied from from-space to to-space. To-space then becomes the
new from-space and vice versa, thereby discarding all objects that have
not been copied.

Our garbage collector takes as input a list of root objects. Objects
that can be reached by recursively traversing this list are considered
live and will be moved to to-space. When we move an object, we must also
update its pointer within the list to point to the objects new location
in memory.

However, this implies that our interpreter cannot use raw pointers to
objects in any function that might trigger garbage collection (or risk
causing a SEGV when accessing an object that has been moved). Instead,
objects must be added to the list and then only accessed through the
pointer inside the list.

Thus, whenever we would have used a raw pointer to an object, we use a
pointer to the pointer inside the list instead:

          function:              pointer to pointer inside list (Object **)
          |
          v
          list of root objects:  pointer to object (Object *)
          |
          v
          semi space:             object in memory
        

*GC_TRACE* adds an object to the list and declares a variable which
points to the objects pointer inside the list.

*GC_TRACE*`(«gcX», «X»)`: add object *X* to the list and declare
`Object **«gcX»` to point to the pointer to *X* inside the list.

Information about the garbage collection process and memory status is
written to the debug file descriptor.

#### Memory Allocation

Object allocation adjusts the size of the Lisp object space on demand:
If after garbage collection the free space is less then the required
memory plus some reserved space for exception reporting, the memory is
increased by a multiple of the amount specified in the C-macro
`FLISP_MEMORY_INC`, defined in `lisp.h`. The multiple is calculated to
hold at least the additional requested space.

`lisp_new()` allocates `FLISP_MIN_MEMORY`, defined in `lisp.h`, and then
allocates all initial objects without taking care of garbage collection.
Then it prints out the amount of Lisp object space consumed to the debug
file descriptor. For *fLisp* this is currently about 21 kB, for *femto*
about 34 kB.

In order to reduce garbage collection frequency, especially during
startup, one can set `FLISP_INITIAL_MEMORY` to a desired additional
amount of memory to allocate on startup.

Some other compile time adjustable limits in `lisp.h`:

Input buffer  
2048, `INPUT_FMT_BUFSIZ`, size of the formatting buffer for
`lisp_eval()` and for the input buffer of `(fgets)`.

Output buffer  
2048, `WRITE_FMT_BUFSIZ`, size of the output and message formatting
buffer.

*fLisp* can live with as little as 50k object memory up to startup. The
Femto editor requires much more memory because of the needs of the “OXO”
game.

#### Future Directions

Loops are availble via the labelled let macro and supported by `iota`.
It could made easier, by any combination of:

- loop/while/for macro
- Demoing hand crafted loops including breaking with throw.

Implement backquote and friends.

Pluggable extensions.

Take away more things.
