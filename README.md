# Femto

Femto is an extended version of Atto Emacs with a tiny Lisp extension
language.

![Femto screenshot](https://github.com/hughbarney/femto/blob/master/screenshots/femto-hilite.png)

![Femto screenshot](https://github.com/hughbarney/femto/blob/master/screenshots/femto-startup.jpg)

> A designer knows he has achieved perfection not when there is
> nothing left to add, but when there is nothing left to take away.
> -- <cite>Antoine de Saint-Exupery</cite>


## Documentation

Femto comes with Markdown and HTML documentation. To rebuild the
documentation [Pandoc](https://pandoc.org/) is required. Rebuild both
documentation formats from their respective source files by running:

	make doc

The documentation is prebuilt in this repository and can be found in

* [femto.md](docs/femto.md) ([HTML](pdoc/femto.html)) for Femto
* [flisp.md](docs/flisp.md) ([HTML](pdoc/flisp.html)) for fLisp.



## Goals of Femto Emacs

* To be an extendable version of the Atto Emacs editor using a Tiny
  Lisp extension language
* Provide a number of useful extension packages written in Tiny Lisp
  (these include an interface to **git** (similar to GNU Emacs Magit),
  a small version of **dired**, a buffer management menu (**buffer
  menu**), **defmacro** allows for a macro to be recorded and invoked
  using c-x e, and an interface to **grep**.
* Be easy to understand without extensive study (to encourage further
  experimentation).


## What does Femto bring to the party of Text Editors

As far as I know Femto is the only Emacs style editor to provide a
macro recorder that generates usable Lisp code that can then be used
to build a larger, more complex utility.  Whilst GNU Emacs has a macro
recorder facility it only allows you to dump out the keystrokes used
during macro recording.  Femto does this by writing the lisp code to a
text buffer called **macro**.  Though I have tried dozens of text
editors over the years (mostly on PCs, but a few on mini and mainframe
computers) I am not aware of any other editor that works this way.
This feature was born out of the principle of keeping a small editor
code written in C and where possible using Lisp to implement new
features.  The standard Emacs macro keystrokes [C-x (, C-c ), C-x e]
are all written in Lisp in the file examples/defmacro.lsp. This meant
that no special C code was needed in Femto to know when it was in
macro mode or not.


## Why the name Femto?

The small Emacs naming scheme appears to use sub-unit prefixes in
decending order with each further reduction of functionality. The Nano
and Pico Emacs editors have been around for a while.

* Nano means 10 to the power of minus 9
* Pico means 10 to the power of minus 12
* Femto means 10 to power of minus 15
* Atto means 10 to power of minus 18
* Zepto means 10 to the power of minus 21
* Zep is smaller version of Zepto Emacs

In Defining Atto as the lowest functional Emacs I have had to consider
the essential feature set that makes Emacs, 'Emacs'. I have defined
this point as a basic Emacs command set and key bindings; the ability
to edit multiple files (buffers), and switch between them; edit the
buffers in mutliple windows, cut, copy and paste; forward and reverse
searching, a replace function and basic syntax hilighting. The proviso
being that all this will fit in less than 2000 lines of C.

Femto is an extended version of Atto Emacs with its own extension
language.


## History

* In late 2015 Hugh Barney wrote the Atto editor 'A minimum
  functioning Emacs is less than 2000 lines of C'.  Atto was based on
  Anthony Howe's editor (commonly known as Anthony's Editor or AE,
  [2]).
* **Femto** is based on the Atto codebase [0]
* **Femto** was originally an intermediate project to form a codebase
  for the FemtoEmacs Editor [8], [9] which was a collaboration between
  Hugh Barney, Ed Costa and Lucas Guerra.  FemtoEmacs uses Jeff
  Bezanson's Femtolisp LISP [10] implementation as the basis for its
  extension language.  However the Femtolisp codebase is in excess of
  12K line of code and fairly difficult to understand how to use it
  inside an embedded application.
* In late 2016 Hugh Barney decided to look for a smaller lisp
  implementation for Femto and settled on Tiny-Lisp[7] by Mattias
  Pirstitz.
* **Zepl** was an initial project that established the suitability of
  Tiny-Lisp for use within an Emacs type editor. The results surpassed
  expectations.
* In late 2017 Hugh Barney decided to return to the **Femto** editor
  and extend it using Tiny-Lisp.
* In 2023/24/25 Georg Lehner refactored the Lisp infrastructure and
  started to add additional Emacs functionality.

For a full version history please refer to the file [CHANGE.LOG.md](./CHANGE.LOG.md)


## Comparisons with Other Emacs Implementations

Femto has almost the same level of functionality as MicroEmacs 3.10
for a codebase less then half of the size.

	Editor         Binary   BinSize     KLOC  Files

	atto           atto       33002     1.9k      10
	pEmacs         pe         59465     5.7K      16
	Esatz-Emacs    ee         59050     5.7K      14
	femto          femto     162392  10.8k/7.6k   25/37 **
	GNOME          GNOME      55922     9.8k      13
	Zile           zile      257360    11.7k      48
	Mg             mg        585313    16.5K      50
	uEmacs/Pk      em        147546    17.5K      34
	Pico           pico      438534    24.0k      29
	Nano           nano      192008    24.8K      17
	jove           jove      248824    34.7k      94
	Qemacs         qe        379968    36.9k      59
	ue3.10         uemacs    171664    52.4K      16 ++
	GNUEmacs       emacs   14632920   358.0k     186

Since femto 2.12 C code has been moved out to Lisp. The first number
in the KLOC column is the line count, the second the sloccount. The
first number in the files count are the C-files, the second number
includes the required Lisp files.

## Building

### Build Debendencies

Debian and Ubuntu:

Before ncurses 6 and as of Femto 1.2 you will need to install the
libcurses dev package.

	$ sudo apt-get install libncurses5-dev libncursesw5-dev

Since ncurses 6:

	$ sudo apt-get install ncurses-dev

Alpine Linux:

	$ sudo apk add ncurses-dev

FreeBSD:

	$ sudo pkg install devel/ncurses

### Build, Test and Installation

	$ git clone https://github.com/hughbarney/femto.git
	$ cd femto
	$ make femto
	$ make test
	$ sudo make install


## Future Enhancements

The following enhancements are envisaged.

* Directory and file manegement (Dired) functionality.  A basic start has been made with dired.lsp

* Ability to load a file in read-only-mode

* Ability to setup themes of colors that can be applied to different buffers
  This will allow users to control their own colour scheme

* Pipe a buffer through a shell command and read the output back into a different buffer


## Development

See the [coding style](./style.md) guide.

Pandoc is required for doc generation.
- Debian/Ubuntu: `sudo apt-get install pandoc`
- Alpine: `sudo apk add pandoc-cli`
- NetBSD: `sudo pkgin install pandoc-cli`
- FreeBSD: `pkg install hs-pandoc`

There is a Doxyfile to create full cross references and call graphs.

Usefull build targets:

- femto: build femto binary
- flisp: build fLisp binary
- doc: build some markdown files from Poshdoc and some html files from markdown
- doxygen: build the Doxygen source code documentation
- measure: count # of files and code lines
- test: run unit tests in summary mode
- check: run unit tests and return success if none fails
- ftest: call femto several times with different commandline parameters.
- val: run femto with valgrind. Logs are found in val.log
- clean: clean up build artifacts
- deb: build Debian package
- install/uninstall: install/uninstall locally

Make sure to run:

	make clean test
	make clean ftest
	make clean doc

before commiting code to Github.


## Copying

Femto code is released to the public domain. hughbarney@gmail.com November 2017

## References

 * [0] Atto Emacs - https://github.com/hughbarney/atto
 * [1] Perfect Emacs - https://github.com/hughbarney/pEmacs
 * [2] Anthony's Editor - https://github.com/hughbarney/Anthony-s-Editor
 * [3] MG - https://github.com/rzalamena/mg
 * [4] Jonathan Payne, Buffer-Gap: http://ned.rubyforge.org/doc/buffer-gap.txt
 * [5] Anthony Howe,  http://ned.rubyforge.org/doc/editor-101.txt
 * [6] Anthony Howe, http://ned.rubyforge.org/doc/editor-102.txt
 * [7] Tiny-Lisp,  https://github.com/matp/tiny-lisp
 * [8] FemtoEmacs, https://github.com/FemtoEmacs/Femto-Emacs
 * [9] FemtoEmacs, https://github.com/hughbarney/Femto-Emacs
 * [10] Femtolisp,  https://github.com/JeffBezanson/femtolisp
