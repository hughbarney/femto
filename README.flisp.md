# fLisp

fLisp is a tiny yet practical interpreter for a dialect of the Lisp
programming language. It is designed to be embeddable in applications
as extension language.

fLisp is embedded into the
(Femto)[https://github.com/hughbarney/femto] editor.

> A designer knows he has achieved perfection not when there is
> nothing left to add, but when there is nothing left to take away.
> -- <cite>Antoine de Saint-Exupery</cite>


## Why the name fLisp

The choice felt on "fLisp", since femtolisp is already taken (by a not
so "femto" Lisp interpreter). Lower case "f" stands for the femto
metric prefix which represents 10^-15 — a very small number.  fLisp is
meant to be very small.


## Goals of fLisp

- To be the smallest embeddable Lisp interpreter yet powerful enough  
  to drive real world applications.
- To be rock stable, predictable and consistent.
- To consume as little resources as possible.
- To be easy to understand without extensive study (to encourage further  
  experimentation).

Size by version:

	Version	Binary	C-Lines/sloc/Files	Lisp-Lines/sloc/Files
	0.13	85584	3.6k/2.4k/6			373/272/3

## History

The (Femto)[https://github.com/hughbarney/femto] editor came initially
with a modified version of
(Tiny-Lisp)[https://github.com/matp/tiny-lisp] by Matthias Pirstitz.

In 2023 some severe bugs surfaced in Tiny-Lisp.  Georg Lehner decided,
that simplifying the code would help finding them and started
refactoring both C and Lisp code of the Femto project.

- Lisp startup code is loaded from a file, instead of baking it  
  into the C - code.
- Command line arguments are handed over to Lisp.
- A Lisp library directory can be defined at compile time.
- The core Lisp functions are reduced to the bare minimum, all  
  functions which can be derived in Lisp are factored out into a core,  
  flisp, stdlib and femto Lisp library.
- Lisp functions are made consistent with  
  (Elisp)[https://www.gnu.org/software/emacs/manual/elisp.html] or  
  (Common Lisp)[https://lisp-lang.org] as far as it seems to make  
  sense.

The amount of deviation gave merit to renaming the Lisp interpreter
from Tiny-Lisp to fLisp.

In 2024 error and input stream handling was improved in order to
diagnose further known bugs. This lead again to a rewrite of a great
part of the code base:

- A stream object type is added to the Lisp core, together with the  
  minimum primitives needed for the Lisp reader and writer.
- An extension mechanism is added to the build system, which allows to  
  build the Lisp core alone, or together with a selection of C  
  extensions.
- The femto related C - extensions are separated out into an  
  extensions.
- A minimal file extensions exposes the core stream function plus some  
  additional functions for testing.
- The C interface of the fLisp interpreter is changed: an fLisp  
  interpreter is instantiated via the `lisp_init()` function, which  
  returns an interpreter "object".  This is configured with input,  
  output and debug streams and then used for the invocation of  
  `lisp_eval()` or `lisp_eval_string()`. This re-architecturing allows  
  for centralized access to the interpreters state.
- A command line wrapper `flisp` is added, which implements a simple
  repl using only the Lisp core plus the file extension.

In 2025 another set of changes were introduced:

- Garbage collecting of stack items was changed, the garbage  
  collector was tested extensively and surfaced bugs fixed.  
- Exceptions can be catched in Lisp and exception codes are Lisp  
  symbols instead of numbers.
- Lisp Object types are stored as Lisp symbols instead of C  
  constants. This allows for type testing in Lisp.
- Numbers are represented by 64 bit integers instead of double floats.
- The Lisp libraries have been extended and functions have been
  improved for compatibilty.
- Lisp object space adjusts dynamically when needed.
- setq was replaced by bind
- Enough framework was built to implement the repl completely in Lisp.


## Building

fLisp should be buildable with only the standard C libraries.

    make flisp

# TODO

## Future

- Implement the backtick and comma reader macros. See [5]
- Adapt build system to be able to un/install `flisp` binary. Includes  
  preparing a Lisp library and a startup file.
- Extend file extension to be usable for Lisp programs.
- Refactor complete core to be able to run any number of interpreters.
- Tap the potential of the in code documentation via Doxygen.


# References

- [1] Emacs Lisp - https://www.gnu.org/software/emacs/manual/html_mono/elisp.html
- [2] Common Lisp - https://www.lispworks.com/documentation/HyperSpec/Front/
- [32] let - https://blog.veitheller.de/Scheme_Macros_III:_Defining_let.html
- [4] curry - https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Towards_a_Standard_Library
- [5] mal - quasiquote https://github.com/kanaka/mal
- [6] Scheme  https://www.scheme.org/
- [7] Scheme v7 Standard https://standards.scheme.org/official/r7rs.pdf
- [8] TSPL2d https://www.scheme.com/tspl2d/
