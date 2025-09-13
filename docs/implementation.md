# fLisp Implementation Details

[fLisp Manual](flisp.html) [(Markdown)](flisp.md)

### Table of Contents

1.  1.  [Garbage Collection](#gc)
    2.  [Memory Usage](#memory)
    3.  [Future Directions](#future)

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
