# fLisp Implementation Details

[fLisp Manual](flisp.html) [(Markdown)](flisp.md)

### Table of Contents

1.  [Embedding Overview](#embedding)
2.  [fLisp C Interface](#c_api)
3.  [Building Extensions](#extensions)

[Implementation Details](implementation)

1.  [Garbage Collection](#gc)
2.  [Memory Usage](#memory)
3.  [Future Directions](#future)

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

[^](#toc)
