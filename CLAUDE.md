# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Femto is a small Emacs-style terminal editor (ncursesw) extended by **fLisp**, a tiny embeddable Lisp interpreter. Two binaries come from one source tree:

- `femto`: the editor. The C core provides buffers, windows, display, and basic edit primitives. Most user-facing features (startup, dired, grep, git, buffer menu, macro recorder, info) are written in Lisp under `lisp/`.
- `flisp`: a standalone fLisp REPL built from `lisp.c` with only the file and double extensions. The test suite drives it.

User docs live in `docs/femto.md` and `docs/flisp.md`. These are generated from `pdoc/*.html` with pandoc, so edit the HTML source, not the `.md` files.

## Commands

```sh
make                 # build femto (also generates femto.rc)
make flisp           # build the standalone interpreter (also generates flisp.rc)
make test            # build both, run every test/*.test with a TAP summary
make run             # run femto against the in-tree Lisp: FEMTORC=femto.rc FEMTOLIB=lisp FEMTO_DEBUG=1
make fl              # run the flisp REPL against the in-tree Lisp (debug log goes to f.log)
make doc             # regenerate docs/*.md and README.html (needs pandoc)
make clean
```

Run one test file or several, from inside `test/`:

```sh
cd test && ./run 6_string.test          # raw TAP output
cd test && SUMMARY=1 ./run 2_core.test  # summarised through ./tapview
cd test && VERBOSE=1 ./run 7_dired.test # shell trace
cd test && ./run -?                     # list the available tests
```

The tests use `../flisp` and `../femto`, so build both first. Each `*.test` file is a sourced shell script. It calls `tap N` with the expected test count (update N when you add tests), pipes Lisp through `flisp_expr`/`flisp_err` (or `femto_expr` for the editor in batch mode), and checks the result with `ok name description`. Most test files set `FLISPRC=../lisp/core.lsp` and `require` the library they test.

## Architecture

### Startup files are generated

`femto.rc` and `flisp.rc` are build products, not sources. `make` produces them from `femto.sht` and `flisp.sht` using `./sht`, a shell heredoc template processor. Each template inlines `$(cat lisp/core.lsp)` and then `require`s further libraries. If you change `lisp/core.lsp`, the `.rc` files must be rebuilt. Editor startup (`femto.sht`) loads `lisp/startup.lsp` unless `FEMTO_BATCH` is set. `startup.lsp` processes argv (`+N file` jumps to line N) and loads the remaining feature libraries.

Runtime environment variables, read in `main.c`:
- `FEMTORC`: init file. `-` reads from stdin, which batch-mode tests use.
- `FEMTOLIB`: Lisp library directory.
- `FEMTO_BATCH`: no UI.
- `FEMTO_DEBUG`: debug logging.

The flisp counterparts are `FLISPRC`, `FLISPLIB` and `FLISP_DEBUG` (the last one names a log file). The compile-time defaults come from `SCRIPTDIR` and `INITFILE` in the makefile.

### fLisp and its compile-time extensions

`lisp.c` is the whole interpreter: reader, evaluator, GC, streams, exceptions, and the `primitives[]` table. Extensions are selected with `-D` flags and pulled in by textual `#include` of `.c` files:

- `FLISP_FILE_EXTENSION` includes `file.c`. Its table entries come from the `FLISP_REGISTER_FILE_EXTENSION` macro in `lisp.h`.
- `FLISP_DOUBLE_EXTENSION` covers `double.c`/`double.h`. Floats are an extension; core numbers are 64-bit integers.
- `FLISP_FEMTO_EXTENSION` includes `femto.primitives.c` (the `e_*` wrapper functions) and `femto.register.c` (their rows in `primitives[]`).

Because of this, `lisp.c` is compiled twice with different flags: once as `lisp.o` for flisp and once as `femto_lisp.o` for femto. Neither `femto.primitives.c` nor `femto.register.c` is compiled on its own.

**To expose an editor C function to Lisp:** implement it in the editor C files and declare it in `header.h`. Add an `Object *e_name(Interpreter *interp, Object **args, Object **env)` wrapper in `femto.primitives.c`. Then add a `{"lisp-name", minArgs, maxArgs, TYPE_xxx, e_name}` row to `femto.register.c`. The fourth field is the type every argument is checked against, or 0 for no check.

### Lisp library layering (`lisp/`)

- `core.lsp`: the bootstrap, inlined into both `.rc` files. Defines `defun`, `defmacro`, `load`, `require`, and other essentials.
- `flisp.lsp`, `stdlib.lsp`, `string.lsp`: general Lisp libraries, loaded with `(require 'name)`. Elisp and Common Lisp compatibility is the stated goal for function names and semantics.
- `femto.lsp`: editor-level Lisp built on the `e_*` primitives.
- Feature modules: `startup.lsp`, `dired.lsp`, `grep.lsp`, `git.lsp`, `bufmenu.lsp`, `defmacro.lsp`, `info.lsp`, `oxo.lsp`.

Keys are bound to Lisp from Lisp with `(set-key "c-k" "(kill-to-eol)")`. The built-in C command bindings are in `key.c`.

### Editor C core

The editor uses a buffer-gap model (`gap.c`). Global types, state, and prototypes all live in `header.h`. The remaining files each cover one area: `buffer.c`, `window.c`, `display.c`, `command.c`, `search.c`, `replace.c`, `undo.c`, `hilite.c`, `complete.c`, `key.c`, `funcmap.c`, `data.c`, and `main.c`.

## Style (from style.md)

- **C:** K&R style, 4-space indent, no tabs, C11. Must compile cleanly with `-Wall -pedantic -pedantic-errors`.
- **Lisp:** Emacs Lisp formatting, 2-space indent, no tabs, trailing parentheses gathered on one line.
- The project values small code size (`make measure` reports it). Prefer implementing new features in Lisp rather than C.

## macOS port
- Target: macOS 27 (arm64), Apple clang 21, GNU make 3.81 (as shipped by Xcode CLT)
- Toolchain: Apple clang; Homebrew deps only if unavoidable
- Keep Linux build working; isolate differences with #ifdef __APPLE__
- No GNU-only make/sed/shell constructs in build scripts
- Build: `make && make flisp`   Test: `make test`
- Links Apple's system `-lncurses` (ncurses 6, wide-char capable); the makefile picks `-lncursesw` elsewhere via `uname -s`
