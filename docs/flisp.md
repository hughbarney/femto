# fLisp Manual

### Introduction

> A designer knows he has achieved perfection not when there is nothing
> left to add, but when there is nothing left to take away.
>
> — Antoine de Saint-Exupery

*fLisp* is a tiny yet practical interpreter for a dialect of the Lisp
programming language. It can be embedded into other applications and is
extensible via C libraries. *fLisp* is used as extension language for
the *Femto* text editor, see the [editor extension
manual](editor.html) [(Markdown)](editor.md) for details.

*fLisp* is hosted in the *Femto*
[Github](https://github.com/hughbarney/femto) repository and released to
the public domain.

*fLisp* is a Lisp-1 interpreter with Scheme like lexical scoping,
tailcall optimization and other Scheme influences. *fLisp* originates
from [Tiny-Lisp by matp](https://github.com/matp/tiny-lisp) (pre 2014),
was integrated into [Femto](https://github.com/hughbarney/femto) by Hugh
Barney (pre 2016) and extended by Georg Lehner since 2023.

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
6.  [Extensions](#extend)
    1.  [File Extension](#file)
    2.  [Double Extension](#double)
7.  [Lisp Libraries](#libraries)
    1.  [Library Loading](#startup)
    2.  [Core Library](#core_lib)
    3.  [fLlisp Library](#flisp_lib)
8.  [fLisp Embedding and
    Development](develop.html) [(Markdown)](develop.md)

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

Variable names convey the following context:

Lisp object of any type:  
*object* *value* *o* *a* *b* *c*

Program elements:  
*arg* *args* *params* *opt* *body* *expr* *pred* *p*

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

*fLisp* provides the following set of libraries:

core  
Integrated in the `.rc` files, always loaded. The core library
implements the minimum required Lisp features for loading libraries.

flisp  
Implements expected standard Lisp functions and additions expected by
`femto` and `flisp`.

string  
String manipulation library.

The *Femto* specific libraries are described together with the
[editor](editor.html) [(Markdown)](editor.md) extension.

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

[^](#toc)
