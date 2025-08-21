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

This manual refers to version 0.6 or later of fLisp.

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
    5.  [String Operations](#string_ops)
6.  [Lisp Libraries](#libraries)
    1.  [Library Loading](#startup)
    2.  [Core Library](#core_lib)
    3.  [fLlisp Library](#flisp_lib)
    4.  [Standard Library](#std_lib)
    5.  [Femto Library](#femto_lib)
7.  [Editor Extension](#editor)
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
8.  [Embedding fLisp](#embedding)
    1.  [Embedding Overview](#embedding)
    2.  [fLisp C Interface](#c_api)
    3.  [Building Extensions](#extensions)
9.  [Implementation Details](#implementation)
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

By default compatibility with Common Lisp is annotated. The suffix
<u>e</u> is used to indicate reference to Emacs Lisp, <u>s</u> for
Scheme. *fLisp* specific function are annotated with <u>f</u>.

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
have an error type symbol and a human readable error message. fLisp does
not implement stack backtracking. Exceptions are either caught on the
top level of an evaluation or by a `catch` statement.

In the `flisp` interpreter the error message is formated as
`error: «message»` if the error object is `nil` otherwise as
`error: '«object»', «message»`, where *object* is the serialization of
the object causing the error and *message* is the error message.

When an exception occurs while calling `lisp_eval()` or
`lisp_eval_string()` from C-code, the `object` field of the interpreter
is set to the object causing the error, the `result` field is set to the
error type symbol and the `msg_buf` field is set to the error message.

Exceptions can be thrown from within in Lisp code via the
[`throw`](#interp_ops) function.

The following error type symbols are defined and used internally:

- `end-of-file`
- `read-incomplete`
- `invalid-read-syntax`
- `range-error`
- `wrong-type-argument`
- `invalid-value`
- `wrong-num-of-arguments`
- `io-error`
- `out-of-memory`
- `gc-error`

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

`(setq «symbol» «value»[ «symbol» «value»..])`  
Create or update named objects: If *symbol* is the name of an existing
named object in the current or a parent environment the named object is
set to *value*, if no symbol with this name exists, a new one is created
in the top level environment. `setq` returns the last *value*.

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

`(catch «expression»)` <u>D</u>  
Evaluates *expression* and returns a list with three elements:

*result*  
`0` on success or any other number indicating an error.

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

Open file at *path* with *mode* and return a stream object. *mode* is
`"r"`ead only by default.

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

`(write «object»[ «keys»..]]`

*keys*:

`:stream` *stream*

`:readably` *flag*

Formats *object* into a string and writes it to the default output
stream. With key `:stream` output is written to the given stream. With
key `:readable` not `nil` output is formatted in a way which which gives
the same object when read again. `write` returns the *object*.

`(eval «object»)`

Evaluates *object* and returns the result.

`(system «string»)`

Executes the
[system(1)](https://man7.org/linux/man-pages/man3/system.3.html)
function with *string* as parameter.

`(os.getenv «string») `

Returns the value of the environment variable named *string*.

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

`(eq «a» «b»)`  
Returns `t` if *a* and *b* evaluate to the same object, `nil` otherwise.

#### Arithmetic Operations

`(i+ «arg1» «arg2»)`  
Returns the sum of *arg1* *arg2*.

`(i* «arg1» «arg2»)`  
Returns the product of *arg1* *arg2*.

`(i- «arg1» «arg2»)`  
Returns *arg1* minus *arg2*.

`(i/ «arg»[ «div»..])`  
Returns *arg1* divided by *arg2*

`(i% «arg»[ «div»..])`  
Returns the rest (modulo) of the integer division of *arg1* by *arg2*.

`(i= «arg1» «arg2»)`  
`(i< «arg1» «arg2»)`  
`(i> «arg1» «arg2»)`  
`(i<= «arg1» «arg2»)`  
`(i>= «arg1» «arg2»)`  
These predicate functions apply the respective comparison operator
between *arg1* *arg2*.

#### String Operations

`(string-length «string»)`  
Returns the length of *string* as a *number*.

`(string-append «string1» «string2»)`  
Returns a new string consisting of the concatenation of *string1* with
*string2*.

`(substring «string»[ «start» [«end»]])`  
Returns the sub string from *string* which starts with the character at
index *start* and before index *end*. String indexes are zero based,
negative indexes count from the end of *string*. If *end* is not given
it defaults to the end of *string*. If *start* is not given, it defaults
to the start of *string*.

`(string-contains «needle» «haystack»)`  
Returns the position of *needle* if it is contained in *haystack*,
otherwise `nil`.

`(string-to-number «string»)`  
Converts *string* into a corresponding *integer* object. String is
interpreted as decimal based integer.

`(number-to-string «integer»)`  
Converts *integer* into a *string* object.

`(ascii «integer»)`  
Converts *integer* into a *string* with one character, which corresponds
to the ASCII representation of *integer*.

`(ascii->number «string»)`  
Converts the first character of *string* into an *integer* which
corresponds to its ASCII value.

[^](#toc)

### Lisp Libraries

#### Library Loading

On startup, both `femto` and `flisp` try to load a single Lisp file. The
default location and name of this <span class="dfn">startup file</span>
are hardcoded in the binary and can be overwritten with environment
variables:

Library path  
femto: `/usr/local/share/femto`, `FEMTOLIB`

flisp: `/usr/local/share/flisp`, `FLISPLIB`

Startup file  
femto: `femto.rc`, `FEMTORC`

flisp: `flisp.rc`, `FLISPRC`

The library path is exposed to the Lisp interpreter as the variable
`script_dir`.

The provided startup files implement a minimal library loader, which
allows to load Lisp files from the library path conveniently and without
repetition. The command to load the file `example.lsp` from the library
is `(require 'example)`.

Femto provides a set of libraries, some of them are required by the
editor

#### Core Library

This library is built into the startup file.

`(list` \[*element* ..\]`)`  
Returns the list of all provided elements.

`(defmacro «name» «params» «body»)`  
`(defun «name» «params» «body»)`  
Defines and returns a macro or function, respectively.

`(curry («func» «arg1»))`  
Returns a lambda with one parameter which returns
`(«func» «arg1» «arg2»)`.

`(typep («type» «object»))`  
Returns true if *object* has *type*.

`(integerp «object»)`  
`(stringp «object»)`  
`(symbolp «object»)`  
`(lamdap «object»)`  
`(macrop «object»)`  
`(streamp «object»)`  
Return `t` if *object* is of the respective type, otherwise `nil`.

`(string «arg»)`  
Returns the string conversion of argument.

`(concat `\[*arg*..\]`)`  
Returns concatenation of all arguments converted to strings.

`(memq «arg» «list»)`  
If *arg* is contained in *list*, returns the sub list of *list* starting
with the first occurrence of *arg*, otherwise returns `nil`.

`(map1 «func» «list»)` <u>S: mapcar</u>  
Apply func to each element in list and return a list of the results.

`map1` is a specialized form of `mapcar` restricted to one list only.

`(cadr «list»)`  
Return the second element in the list, `(car (cdr «list»))`.  
`(cddr «list»)`  
Return all elements after the second one in list, `(cdr (cdr «list»))`.  
`(let ((«name» «value»)[ («name» «value»)..]) «body»)`  
Bind all *name*s to the respective *value*s then evaluate body.

`(let «label»((«name» «value»)[ («name» «value»)..]) «body»)` <u>Cs</u>  
Labelled or “named” `let`: define a local function *label* with *body*
and all *name*s as parameters bound to the *values*.

`(length «obj»)`  
Returns the length of *obj* if it is a string or a list, otherwise
throws a type exception.

`(prog1 «sexp»[«sexp»..])`  
Evaluate all *sexp* in turn and return the value of the first.

`(fload ` *stream*`)` <u>f</u>  
Reads and evaluates all Lisp objects in *stream*.

`(load ` *path*`)`  
Reads and evaluates all Lisp objects in file at *path*.

`(provide «feature»)`  
Used as the final expression of a library to register symbol *feature*
as loaded into the interpreter.

`(require «feature»)`  
If the *feature* is not alreaded loaded, the file *feature*`.lsp` is
loaded from the library path and registers the *feature* if loading was
successful. The register is the variable *features*.

<span class="mark"> Arithmethic functions currently are aliased to the
binary integer operators. n-ary operation is yet to be implemented in
Lisp. The following documents the expected behavior. </span>

`(+[ «arg»..])`  
Returns the sum of all *arg*s or `0` if none is given.

`(*[ «arg»..])`  
Returns the product of all *arg*s or `1` if none given.

`(-[ «arg»..])`  
Returns 0 if no *arg* is given, -*arg* if only one is given, *arg* minus
the sum of all others otherwise.

`(/ «arg»[ «div»..])`  
Returns 1/*arg* if no *div* is given, *arg*/*div*\[/*div*..\] if one or
more *div*s are given, `inf` if one of the *div*s is `0` and the sum of
the signs of all operands is even, `-inf` if it is odd.

`(% «arg»[ «div»..])`  
Returns `1` if no *div* is given, *arg*%*div*\[%*div*..\] if one or more
*div*s are given. If one of the *div*s is `0`, the program exits with an
arithmetic exception.

`(= «arg»[ «arg»..])`  
`(< «arg»[ «arg»..])`  
`(> «arg»[ «arg»..])`  
`(<= «arg»[ «arg»..])`  
`(>= «arg»[ «arg»..])`  
These predicate functions apply the respective comparison operator
between all *arg*s and return the respective result as `t` or `nil`. If
only one *arg* is given they all return `t`.

#### fLisp Library

This library implements commonly excpected Lisp idioms. *fLisp*
implements a carefully selected minimum set of commonly used functions.

`(not «object»)`

Logical inverse. In Lisp a synonym for `null`

listp

and

or

`(reduce «func» «list» «start»)` <u>D</u>

`reduce` applies the binary *func* to the first element of *list* and
*start* and then recursively to the first element of the rest of the
*list* and the result of the previous invocation: it is “right binding”.

Since `reduce` is right associative and *start* is not optional, it
differs significantly both from Common Lisp and Scheme.

max

min

nthcdr

nth

#### Standard Library

This library implements some Common Lisp functions, which are not used
in the editor libraries. They are provided for reference.

atom

zerop

if

equal

append

print

princ

#### Femto Library

This library implements helper function required by the Femto editor. It
is written only in Lisp idioms provided by fLisp itself plus the [fLisp
Library](#flisp_lib).

[^](#toc)

### Editor Extension

The editor extensions introduces several types of objects/functionality:

- <span class="dfn">Buffers</span> hold text
- <span class="dfn">Windows</span> display buffer contents to the user
- <span class="dfn">Keyboard Input</span> allows the user to interact
  with buffers and windows
- The <span class="dfn">Message Line</span> gives feedback to the user
- Several other function for operating system or user interaction

#### Buffers

This section describes the buffer related functions added by Femto to
fLisp. The description is separated in function related to buffer
management and text manipulation. Text manipulation always operates on
the <span class="dfn">current buffer</span>. Buffer management creates,
deletes buffers, or selects one of the existing buffers as the current
buffer.

Buffers store text and allow to manipulate it. A buffer has the
following properties:

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
are the \`femto\` editor and the simplistic \`flisp\` command line Lisp
interpreter.

Currently embedding can only be done by extending the build system.
Application specific binary Lisp extensions are stored in separated C
files and the interface code is conditionally included into the `lisp.c`
file. Two extensions are provided: the Femto extension which provides
the editor functionality and the file extension which provides access to
the low level stream I/O functions and adds some more.

fLisp exposes the following public interface functions:

`lisp_new()`  
Create a new interpreter.

`lisp_destroy()`  
Destroy an interpreter, releasing resources.

`lisp_eval()`  
Evaluate input stream until exhausted or error.

`lisp_eval_string()`  
Evaluate given string until exhausted or error.

`lisp_write_object()`  
Format and write object to file descriptor.

`lisp_write_error()`  
Format and write the error object and error message of an interpreter to
a file descriptor.

Different flows of operation can be implemented. The Femto editor
initializes the interpreter without input/output file descriptors and
sends strings of Lisp commands to the interpreter, either when a key is
pressed or upon explicit request via the editor interface.

The `flisp` command line interpreter sets `stdout` as the default output
file descriptors of the fLisp interpreter and feeds it with strings of
lines read from the terminal. If the standard input is not a terminal
`stdin` is set as the default input file descriptor and fLisp reads it
through until end of file.

After processing the given input, the interpreter puts a pointer to the
object which is the result of the last evaluation into the `object`
field of the interpreter structure. The `result` field is set to the
`nil` and the `msg_buf` field is set to the empty string.

fLisp sends all output to the default output stream. If `NULL` is given
on initialization, output is suppressed altogether.

If an exception is thrown inside the Lisp interpreter an error message
is formatted and copied to the `msg_buf` buffer of the interpreter, A
pointer to the object causing the error is set to the `object` field.
The `result` field is set to the respective error type symbol.

In this <span class="dfn">error state</span> of the interpreter, the
function `lisp_write_error()` can be used to write a standardized error
message including the error object to a file descriptor of choice

#### fLisp C Interface

*Interpreter*` *lisp_new(int «size», char **«argv», char *«library_path», FILE *input, FILE *output, FILE* debug)`  
`lisp_new()` creates and initializes an fLisp interpreter. The initial
environment contains the following symbols:

*argv0*  
The string stored in `*«argv»[0]`, if any

*argv*  
The list of strings stored in *argv*

*script_dir*  
The string stored in *library_path*

A pointer to an *Interpreter* struct is returned, which is used to
operate the interpreter.

The other arguments to `lisp_new()` are:

*size*  
Memory size to allocate for the Lisp objects. This is divided into two
pages for garbage collection. Only one page is used by the interpreter
at any moment.

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

`void lisp_eval(Interpreter *«interp»)`  
Evaluates the input file set in the *input* field of the fLisp
interpreter *interp* until end of file. If no input file is set,
`interp` is set to a respective error state.

`void lisp_eval_string(Interpreter *«interp», char *«string»)`  
Evaluates all Lisp expressions in *string*.

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
`{"«name»", «argMin», «argMax», «primitive»}`. Here *name* is a distinct
name in Lisp space.

*interp* is the fLisp interpreter in which *primitive* is executed.
*argMin* is the minimum number of arguments, *argMax* is the maximum
number of arguments allowed for the function. If *argMax* is a negative
number, arguments must be given in tuples of *argMax* and the number of
tuples is not restricted.

When creating more then one new objects within a primitive, care has to
be taken to register them with the garbage collector. Registration is
started with the `GC_CHECKPOINT` CPP macro. `GC_TRACE(«name», «value»`
creates an object variable *name*, sets it to *value* and registers it
with the garbage collector. The macro `GC_RELEASE` must be called to
finalize the registration. The convenience macro `GC_RETURN(«object»)`
calls `GC_RELEASE` and returns *object*.

Some CPP macros are provided to simplify argument validation in
primitives, all of them receive the *name* of the primitive as a
parameter:

`TWO_STRING_ARGS(«name»)`  
Assures that the first two arguments are of type string. They are
assigned to the `Object *` variables *first* and *second*.

`ONE_STRING_ARG(«name»)`  
Assures that the first argument is of type string. It is assigned to the
`Object *` variable *arg*.

`ONE_NUMBER_ARG(«name»)`  
Assures that the first argument is of type number. It is assigned to the
`Object *` variable *num*.

`ONE_STREAM_ARG(«name»)`  
Assures that the first argument is of type stream. It is assigned to the
`Object *` variable *stream*.

[^](#toc)

### Implementation Details

#### Garbage Collection

fLisp implements Cheney's copying garbage collector, with which memory
is divided into two equal halves (semi spaces): from- and to-space.
From-space is where new objects are allocated, whereas to-space is used
during garbage collection.

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

#### Memory Usage

Some compile time adjustable limits in `lisp.h`:

Input buffer  
2048, `INPUT_FMT_BUFSIZ`, size of the formatting buffer for
`lisp_eval()`.

Output buffer  
2048, `WRITE_FMT_BUFSIZ`, size of the output and message formatting
buffer.

fLisp can live with as little as 300k object memory. The Femto editor
requires 16M since the “OXO” game requires a lot of memory.

#### Future Directions

The two memory pages should be separated and the second one should be
allocated only during garbage collection. When memory runs out, the
garbage collection should be restarted with an increased capacity of the
new page.

It is now possible to catch exceptions within Lisp code and exceptions
return differentiated error codes and use POSIX stream I/O. This,
together with the `(eval)` primitive would allow to write the repl
directly in Lisp, and reading and eval'ing until no more “incomplete
input” result codes are returned.

Integer arithmetic would be sufficient for all current purposes and
increase portability and speed while reducing size.

The file extension only contains `(fflush)`, `(ftell)` and `(fgetc)` and
could easily be extended into something useful. `(fstat)` would be most
pressing for improving `femto.rc` and `flisp.rc`.

The string library should implement Elisp `substring` to replace
`string.substring` and a simplified version of `string-search` without
start index.

Implement `(type «object»)` returning symbols for each type in C and
implement individual type checking predicates in Lisp.

loop programming is availble via the labelled let macro. It could made
easier, by any combination of:

- iota
- loop/while/for macro
- Demoing hand crafted loops including breaking with throw.
