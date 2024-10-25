# fLisp Manual

### Introduction

> A designer knows he has achieved perfection not when there is nothing
> left to add, but when there is nothing left to take away.
>
> — Antoine de Saint-Exupery

*fLisp* is a tiny yet practical interpreter for a dialect of the Lisp
programming language. It is used as extension language for the
[Femto](https://github.com/matp/tiny-lisp) text editor.

*fLisp* originates from [Tiny-Lisp by
matp](https://github.com/matp/tiny-lisp) (pre 2014), was integrated into
[Femto](https://github.com/hughbarney/femto) by Hugh Barnes (pre 2016)
and compacted by Georg Lehner in 2023.

This is a reference manual. If you want to learn about Lisp programming
use other resources eg.

- The [Common Lisp](lisp-lang.org) web site,
- [An Introduction to Programming in Emacs
  Lisp](https://www.gnu.org/software/emacs/manual/html_node/eintr/index.html)
  or
- [The Scheme Programming Language](https://www.scheme.org/).

### Lisp

#### Notation Convention

*fLisp* fancies to converge toward Emacs Lisp. Function descriptions are
annoted with a compatibility scale:

<u>C</u>  
Interface compatible, though probably less featureful.

<u>D</u>  
Same name, but different behavior.

<u>S: *name*</u>  
*name* is a similar but not compatible function in Emacs Lisp.

<u>B</u>  
Buggy/incompatible implementation.

Annotation is omitted if the function does not exist in Emacs Lisp.

We use the following notation rule for the *fLisp* syntax:

*name*  
*name* is the name of a variable. In Markdown documents it is shown with
guillements, like this `«name»`.

`[text]`  
`text` can be given zero or one time.

`[text..]`  
`text` can be given zero or more times.

“` `”  
A single space is used to denote an arbitrary sequence of whitespace.

Notes:

- *fLisp* does not use `[`square brackets`]` and double-dots `..` as
  syntactical elements.
- String and number notation and formating conventions are the same as
  in the C language

#### fLisp Interpreter

When *fLisp* is invoked it follows a three step process:

1.  Read: program text is read in and converted into an internal
    representation.
2.  Evaluate: the internal representation is evaluated
3.  Print: the result of the evaluation is returned to the invoker.

Core functions of the language operate on built-in objects only. *fLisp*
is extended with additional functions in order to interact with editor
related objects. With respect to the interpreter, extension functions
behave the same as core functions.

#### Syntax

Program text is written as a sequence of symbolic expressions -
<span class="dfn">sexp</span>'s - in parenthesized form. A sexp is
either a single object or a function invocation enclosed in parens.
Function invocations can be infinitely nested.

The following characters are special to the reader:

`(`  
Starts a function invocation, *list* or *cons* object (see [Objects and
Data Types](#objects_and_data_types)).

`)`  
Finishes a function invocation, *list* or *cons* object

`"`  
Encloses strings.

`'`  
With a single quote prefix before a sexp, the sexp is expanded to
`(quote «sexp»)` before it is evaluated.

`.`  
The expresion` («a» . «b»)` evaluates to a *cons* object, holding the
objects *a* and *b*.

Numbers are represented in decimal notation.

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

*fLisp* objects have exactly one of the following data types:

<span class="dfn">number</span>  
[double precision floating point
number.](https://en.wikipedia.org/wiki/Double-precision_floating-point_format)

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

Objects are unmutable, functions either create new objects or return
existing ones.

Characters do not have their own type. A single character is represented
by a *string* with length one.

#### Environments, Functions, Evaluation

All operations of the interpreter take place in an environment. An
<span class="dfn">environment</span> is a collection of named objects.
The object names are of type symbol. An object in an environment is said
to be <span class="dfn">bound</span> to its name. Environments can have
a parent. Each *fLisp* interpreter starts with a
<span class="dfn">root</span> environment without a parent.

lambda and macro objects are functions. They have a parameter list and a
sexp as body. When functions are invoked a new environment is created as
child of the current environment. Functions receive zero or more objects
from the caller. These are bound one by one to the symbols in the
parameter list in the new environment.

lambdas return the result of evaluating the body in the new environment.

macros first evaluate the body in the calling environment. The resulting
sexp is evaluated in the new environment and that result is returned.
macro bodies are typically crafted to return new sexp's in terms of the
parameters.

When a sexp is evaluated and encounters a symbol it looks it up in the
current environment, and then recursively in the environments from which
the lambda or macro was invoked. The symbol of the first found binding
is then replaced by its object.

*fLisp* counts with a set of built-in functions called
<span class="dfn">primitives</span>. They are grouped in the manual by
the type of objects they operate on. The primitives are bound in the
global environment to the names under which they are described.

#### Primitives

##### Interpreter Operations

`(progn[ «expr»..])`  
Each *expr* is evaluated, the value of the last is returned. If no
*expr* is given, `progn` returns `nil`.

`(cond[ «clause»..])`  
Each *clause* is of the form `(«pred»[ «action»])`. `cond` evaluates
each *clause* in turn. If *pred* evaluates to `nil`, the next *clause*
is tested. If *pred* evaluates not to `nil` and if there is no *action*
the value of *pred* is returned, otherwise `(progn «action»)` is
returned and no more *clause*s are evaluated.

`(setq «symbol» «value»[ «symbol» «value»..])`  
Create or update named objects: If *symbol* is the name of an existing
named object in the current or a parent environment the named object is
set to *value*, if no symbol with this name exists, a new one is created
in the current environment. `setq` returns the last *value*.

`(lambda «params» «body»)`  
Returns a *lambda* function which accepts 0 or more arguments, which are
passed as list in the parameter *params*.

`(lambda ([«param» ..]) «body»)`  
Returns a *lambda* function which accepts the exact number of arguments
given in the list of *param*s.

`(lambda («param»[ «param»..] . «opt») «body»)`  
Returns a *lambda* function which requires at least the exact number of
arguments given in the list of *param*s. All extra arguments are passed
as a list in the parameter *opt*.

`(macro «params» «body»)`  
`(macro ([«param» ..]) «body»)`  
`(macro («param»[ «param»<..] . «opt») «body»)`  
These forms return a macro function. Parameter handling is the same as
with lambda.

`(quote «expr»)`  
Returns *expr* without evaluating it.

`(signal «symbol» «list»)`  
tbd

`(trap «list»)`  
tbd

##### Object Operations

`(null «object»)`  
Returns `t` if *object* is `nil`, otherwise `nil`.

`(symbolp «object»)`  
Returns `t` if *object* is of type symbol, otherwise `nil`.

`(symbol-name «object»)`  
If *object* is of type symbol return its value as string.

`(numberp «object»)`  
Returns `t` if *object* is of type number, otherwise `nil`.

`(stringp «object»)`  
Returns `t` if *object* is of type string, otherwise `nil`.

`(consp «object»)`  
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

`(print «object»)`  
Formats *object* into a string which can be read by the reader and
returns it. As a side effect, the string is printed to the output stream
with a leading and a closing newline. `print` escapes quotes in strings
with a backslash.

`(princ «object»)`  
Formats *object* into a string and returns it, As a side effect, the
string is printed to the output stream.

##### String Operations

`(string.length «string»)`  
Returns the length of *string* as a *number*.

`(string.substring «string» «start» «end»)`  
Returns the substring from *string* which starts with the character at
index *start* and ends with index *end*. String indexes are zero based.

`(string.append «string1» «string2»)`  
Returns a new string consisting of the concatenation of *string1* with
*string2*.

`(string-to-number «string»)`  
Converts *string* into a corresponding *number* object. String is
interpreted as decimal based integer.

`(number-to-string «number»)`  
Converts *number* into a *string* object.

`(ascii «number»)`  
Converts *number* into a *string* with one character, which corresponds
to the ASCII representation of *number*.

`(ascii->number «string»)`  
Converts the first character of *string* into a *number* which
corresponds to its ASCII value.

##### Arithmetic Operations

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
*div*s are given. If one of the divs is `0`, the program exits with an
arithmetic exception.

`(= «arg»[ «arg»..])`  
`(< «arg»[ «arg»..])`  
`(> «arg»[ «arg»..])`  
`(<= «arg»[ «arg»..])`  
`(>= «arg»[ «arg»..])`  
These predicate functions apply the respective comparison operator
between all *arg*s and return the respective result as `t` or `nil`. If
only one *arg* is given they all return `t`.

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
buffer. current buffercode.

Buffers store text and allow to manipulate it. A buffer has the
following properties:

*name*  
Buffers are identified by their name. If a buffer name is enclosed in
`*`asterisks`*` the buffer receives special treatment.

*text*  
0 or more characters.

*point*  
The position in the text where text manipulation takes place.

*mark*  
An optional second position in the text. If the *mark* is set, the text
between *point* and *mark* is called the
<span class="dfn">selection</span> or <span class="dfn">region</span>.

*filename*  
If set the buffer is associated with the respective file.

*flags*  
Different flags determine the behaviour of the buffer.

In the following, all mentions of these variables refer to the current
buffers properties.

##### Text manipulation

`(insert-string «string»)`  
Inserts *string* at *point*. <u>S: insert</u>.

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
Returns the character to the left of *point*. <u>S: get-byte</u>

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
visible line of the associoated screen and scrolls the screen down to
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

`(find-file «string»)`  
Loads file with path string into a new buffer. <u>C</u>

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
value for the user respones. The user can edit the response. When
hitting return, the final response is returned.

`(show-prompt «prompt» «default»)`  
Displays *prompt* and *default* in the commandline, but does not allow
editing. Returns `t`.

`(prompt-filename «prompt»)`  
Displays *prompt* in the commandline and allows to enter or search for a
file name. Returns the relative path to the selected file name or the
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

`(system «string»)`  
Executes the
[system(1)](https://man7.org/linux/man-pages/man3/system.3.html)
function with *string* as parameter.

`(os.getenv «string») `  
Returns the value of the environment variable named *string*.

`(log-message «string»)`  
Logs *string* to the `*messages*` buffer.

`(log-debug «string»)`  
Logs string to the file `debug.out`.

`(get-version-string)`  
Returns the complete version string of Femto, including the copyright.

### Implementation Details

<span class="mark">Tbd.: Memory consumption, limits, hacking, ...</span>
