# *fLisp* Femto Editor Extension and Libraries

### Overview

This is the documentation of the *fLisp* Femto Editor Extension and the
Femto Lisp libraries and applications.

For information on how fLisp extensions are created see the [*fLisp*
project documention](https://github.com/jorge-leon/flisp/doc).

The [editor extension](#primitives) introduces several types of objects:

- <span class="dfn">Buffers</span> hold text
- <span class="dfn">Windows</span> display buffer contents to the user
- <span class="dfn">Keyboard Input</span> allows the user to interact
  with buffers and windows
- The <span class="dfn">Message Line</span> gives feedback to the user
- Several other function for user interaction

Several [Lisp libraries](#libraries) make use of the extensions
primitives to provide advanced functionality.

### Table of Contents

1.  [Overview](#overview)
2.  Table of Contents
3.  [Editor Extension](#primitives)
    1.  [Buffers](#buffers)
        1.  [Text manipulation](#text)
        2.  [Selection](#selection)
        3.  [Cursor Movement](#cursor)
        4.  [Buffer management](#buffer_management)
    2.  [User Interaction](#ui)
        1.  [Window Handling](#windows)
        2.  [Message Line](#message_line)
        3.  [Keyboard Handling](#keyboard)
        4.  [Programming and System Interaction](#programming_system)
4.  [The femto Lisp library](#femto_lib)
    1.  [*Femto* Edit and Naviagtiona Functions](#lib_edit)
    2.  [*Femto* Utility Functions](#lib_util)
    3.  [*Femto* Buffer Functions](#lib_buffer)
5.  [*Femto* Lisp Applications](#femto_apps)
    1.  [`defmacro`](#defmacro) Editor Macros
    2.  [`dired`](#dired) Directory Navigation
    3.  [`bufmenu`](#bufmenu) Buffer Selection Menu
    4.  [`grep`](#grep) File Content Search
    5.  [`git`](#git) Git Repo Helper
    6.  [`info`](#info) Builtin Help
    7.  [`oxo`](#oxo) Tic-Tac-Toe Game

### Editor Extension

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
Different flags determine the behavior of the buffer: `special`,
readonly, `modified,` `overwrite` and `undo`.

mode  
Determine the syntax highlighter mode: `cmode` `python` and `lisp` are
available. If none is set `text` mode is used for syntax hightlighting.

In the following, any mention to one of them refers to the respective
current buffers property.

[^](#toc)

##### Text manipulation

`(backspace)` ⇒ `t`  
Deletes the character to the left of *point*. <u>S:
delete-backward-char</u>

`(delete)` ⇒ `t`  
Deletes the character after *point*. <u>S: delete-char</u>

`(erase-buffer)` ⇒ `t`  
Erases all text in the current buffer. <u>C</u>

`(get-char)` ⇒ *string*  
Returns the character at *point*. <u>S: get-byte</u>

`(insert-string «string»)` ⇒ `t`  
Inserts *string* before *point*. <u>S: insert</u>.

`(kill-region)` ⇒ `t`  
Deletes the text in the *region* and copies it to the *clipboard*.
<u>D</u>

`(yank)` ⇒ `t`  
Pastes the *clipboard* before *point*. <u>C</u>

[^](#toc)

##### Selection

`(copy-region)` ⇒ `t`  
Copies *region* to the *clipboard*. <u>S: copy-region-as-kill</u>

`(get-clipboard)` ⇒ *string*  
Returns the *clipboard* contents or the empty string if none. <u>S:
gui-get-selection</u>

`(get-mark)` ⇒ *integer*  
Returns the position of *mark*, -1 if *mark* is unset. <u>S: mark</u>

`(set-clipboard «string»)` ⇒ `t`\|`nil`  
Sets *clipboard* to the contents of *string*. Returns `t` on success,
`nil` if memory allocation fails. <u>S: gui-set-selection</u>

`(set-mark)` ⇒ `t`  
Sets *mark* to *point*. <u>D</u>

[^](#toc)

##### Cursor Movement and Information

`(backward-char)` ⇒ `t`  
Moves the point in the current buffer one character backward, but not
before the end of the buffer. <u>C</u>

`(backward-word)` ⇒ `t`  
Moves the point in the current buffer backward after the last char of
the previous word. If there is no word left the point is set to the
beginning of the buffer. If the point is already at the end or within a
word, the current word is skipped. <u>D</u>: **Note**: Elisp moves to
the *beginning* of the previous word.

`(beginning-of-buffer)` ⇒ `t`  
Sets the point in the current buffer to the first buffer position,
leaving mark in its current position. <u>C</u>

`(beginning-of-line)` ⇒ `t`  
Sets point before the first character of the current line, leaving mark
in its current position. <u>S: move-beginning-of-line</u>

`(end-of-buffer)` ⇒ `t`  
Sets the point in the current buffer to the last buffer position,
leaving mark in its current position. <u>C</u>

`(end-of-line)` ⇒ `t`  
Sets point after the last character of the current line, i.e. before the
end-of-line character sequence, leaving mark in its current position.
<u>S: move-end-of-line</u>

`(forward-char)` ⇒ `t`  
Moves the point in the current buffer one character forward, but not
past the end of the buffer. <u>C</u>

`(forward-word)` ⇒ `t`  
Moves the point in the current buffer forward before the first char of
the next word. If there is no word left the point is set to the end of
the buffer. If the point is already at the start or within a word, the
current word is skipped. <u>D</u>: **Note**: Elisp moves to the *end* of
the the next word.

`(get-point)` ⇒ *integer*  
Returns the position of *point*. <u>S: point</u>

`(get-point-max)` ⇒ *integer*  
Returns the maximum accessible value of point in the current buffer.
<u>S: point-max</u>

`(goto-line «number»)` ⇒ `t`\|`nil`  
Sets the point in the current buffer to the first character on line
*number*. Returns `t` on success, `nil` when line is not found. <u>S:
goto-line</u>, not an Elisp function.

`(next-line)` ⇒ `t`  
Moves the point in the current buffer to the same character position in
the next line, or to the end of the next line if there are not enough
characters. In the last line of the buffer moves the point to the end of
the buffer. <u>C</u>

`(previous-line)` ⇒ `t`  
Moves the point in the current buffer to the same character position in
the previous line, or to the end of the previous line if there are not
enough characters. In the first line of the buffer the point is not
moved. <u>C</u>

`(scroll-up)` ⇒ `t`  
Moves the point of the current buffer to the beginning of the last
visible line of the associated screen and scrolls the screen up to show
it as the first line. <u>C</u>

`(scroll-down)` ⇒ `t`  
Moves the point of the current buffer to the beginning of the first
visible line of the associated screen and scrolls the screen down to
show it as the last line. <u>C</u>

`(search-forward «string»)` ⇒ `t`\|`nil`  
Searches for *string* in the current buffer, starting from point
forward. If string is found, sets the point after the first occurrence
of *string* and returns `t`, otherwise leaves point alone and returns
`nil`. <u>D</u>

`(search-backward «string»)` ⇒ `t`\|`nil`  
Searches for *string* in the current buffer, starting from point
backwards. If string is found, sets the point before the first
occurrence of *string* and returns `t`, otherwise leaves point alone and
returns `nil`. <u>D</u>

`(set-point «number»)` ⇒ `t`  
Sets the point in the current buffer to the position *number*. <u>S:
goto-char</u>

[^](#toc)

##### Buffer Management and Information

All buffers are registered in a circular list. The current buffer is the
first buffer. If a buffer is selected to be shown it is put in front of
the other buffers. This orders the buffer list by most recently used.

`(find-buffer-visiting «path»)` ⇒ *buffer*\|`nil`  
Returns the buffer name of the buffer associated with the file or
directory *path*.

`(buffer-filename[ «string»)` ⇒ *buffer*\|`nil`  
Return the file name associated with buffer *string* or the current
buffer if not given. If no file is associated returns `nil`. <u>C</u>

`(buffer-fread «stream[» «size]»)` ⇒ *integer*<u>f</u>  
Inserts *size* bytes from *stream* into the current buffer, starting at
point. If *size* is not given, *stream* is read in until end-of-line.
Returns number of bytes read.

`(buffer-fwrite «stream»[ «size»])` ⇒ *integer* <u>f</u>  
Write *size* bytes from the current buffer, starting at point. If *size*
is not given or greater then the rest of the buffer the rest of the
buffer is written. Returns number of bytes written.

`(buffer-mode[ «buffer»[ «mode»])` ⇒ *mode* <u>f</u>  
Return and optionally set the mode of given *buffer* or current buffer
if *buffer* is not given or `nil`.

`(buffer-modified-p[ «buffer» [ «p»])` ⇒ `p`  
`(buffer-overwrite-p[ «buffer» [ «p»])` ⇒ `p`  
`(buffer-readonly-p[ «buffer» [ «p»])` ⇒ `p`  
`(buffer-special-p[ «buffer» [ «p»])` ⇒ `p`  
`(buffer-undo-p[ «buffer» [ «p»])` ⇒ `p`  
Return and optionally set the respective flag of *buffer*, or the
current buffer if *buffer* is not given or `nil`.

`(buffer-next[ «buffer»])` ⇒ *buffer'*  
Return the next buffer after *buffer* in the list, or the current buffer
if none given.

`(buffer-show «buffer»)` ⇒ *buffer*  
Associate *buffer* with the current window and put it first in the
buffer list. Returns *buffer*.

`(delete-buffer «buffer»)` ⇒ *buffer* <u>f</u>  
Kill the buffer named *string*. Unsaved changes are discarded. Throws an
exception if the buffer does not exist or when trying to delete the
scratch buffer or the current buffer. Returns the buffer name.

`(get-buffer-create «name»)` ⇒ *buffer*  
If buffer *name* exists return it, otherwise create a new, empty buffer
with that name and returns it.

`(list-buffers)` ⇒ `t` <u>f</u>  
Lists all the buffers in a buffer called `*buffers*`.

`(set-buffer «string»)` ⇒ *buffer*  
Makes the buffer named *string* the current buffer, returns *buffer*.

`(set-buffer-name «string»)` ⇒ *string* <u>S: rename-buffer</u>  
Set the name of the current buffer to *string*. Returns *string*.

`(set-visited-file-name «string»«|»nil)` ⇒ *string*\|`nil`  
Set the filename of the current buffer to *string* or `nil` - i.e.
deletes it. Returns the argument.

[^](#toc)

#### User Interaction

This section lists function related to window and message line
manipulation, keyboard input and system interaction.

##### Window Handling

`(delete-other-windows)` ⇒ `t`  
Make current window the only window.

`(other-window)` ⇒ `t` <u>D</u>  
Moves the cursor to the next window down on the screen. Makes the buffer
in that window the current buffer.

Note: Elisp `other-window` has a required parameter *count*, which
specifies the number of windows to move down or up.

`(pop-to-buffer «buffer»)` ⇒ *buffer* D<u>:</u>  
Split the window if not already split. Show *buffer* in the other window
and make it current. Throws exception if buffer does not exist.

`(split-window)` ⇒ `t`\|`nil`  
Splits the current window. Creates a new window for the current buffer.
Returns `t` on success, `nil` if the window cannot be split or memory
allocation fails.

`(update-display)` ⇒ `t` <u>f</u>  
Updates all modified windows.

`(refresh)` ⇒ `t` <u>f</u>  
Calls the curses refresh() function.

[^](#toc)

##### Message Line

`(clear-message-line)` ⇒ `t` <u>S: clear-minibuffer-message</u>  
Clears the message line.

`(message «string»)` ⇒ `t` <u>D</u>  
Displays *string* in the message line.

`(prompt «prompt» «default»)` ⇒ *string*\|`ni`l <u>f</u>  
Displays *prompt* in the command line and sets *default* as initial
value for the user response. The user can edit the response. When
hitting return, the final response is returned. If C-g is pressed `nil`
is returned.

`(prompt-filename «prompt»)` ⇒ *string*\|`ni`l <u>f</u>  
Displays *prompt* in the command line and allows to enter or search for
a file name. Returns the relative path to the selected file name or the
response typed by the user or `nil` if C-g is pressed.

[^](#toc)

##### Keyboard Handling

`(describe-bindings)` ⇒ `t`  
Creates a listing of all current key bindings, in a buffer named
`*help*` and displays it in a new window.

`(describe-functions)` ⇒ `t` <u>f</u>  
Creates a listing of all functions bound to keys in a buffer named
`*help*` and displays it in a new window.

`(execute-key)` ⇒ `t` <u>f</u>  
Executes the function of the last pressed key.

`(getch)` ⇒ *string* <u>f</u>  
Calls the curses getch() function: Waits for a key to be pressed and
returns the key as string. If a function key is pressed, the ANSI
terminal input sequence is sent; several calls to `getch` are required
to decode this.

`(get-key)` ⇒ `t` <u>f</u>  
Records keystrokes until a registered key-sequence is gathered, then the
key information is stored and an empty string is returned. If no match
is found, i.e. a normal character is input, the recorded key(s) is
returned as string.

`(get-key-funcname)` ⇒ *string* <u>f</u>  
Return the name of the *lisp-func* bound to the most recent recorded
key-sequence.

`(get-key-name)` ⇒ *string* <u>f</u>  
Returns the *key-name* of the most recenet recorded key-sequence, eg:
`c-k` for control-k.

`(set-key «key-name» «lisp-func»)` ⇒ `t` <u>f</u>  
Binds key key-name to the lisp function *lisp-func*.

[^](#toc)

##### Programming and System Interaction

`(exit)` <u>f</u>  
Exit Femto without saving modified buffers.

`(get-temp-file)` ⇒ `t` <u>f</u>  
Create a file in `/tmp` with a unique name, return filename.

`(get-version-string)` ⇒ *string* <u>f</u>  
Returns the complete version string of Femto, including the copyright.

`(log-debug «string»)` ⇒ `t` <u>f</u>  
Logs string to the file `debug.out`.

`(log-message «string»)` ⇒ `t` <u>f</u>  
Logs *string* to the `*messages*` buffer.

`(suspend)` ⇒ `t` <u>S: suspend-emacs</u>  
Suspend *Femto*.

[^](#toc)

### The `femto` Lisp Library

This library provides standard editor functions, mostly tailored to
Emacs compatibility and wrappers of the editor extension primitives.

Femto does not have the Emacs notion of interactive functions. We use
the suffix `-interactive` or `-i` if we want to emphasize that a
function is the interactive version and `-noselect` to emphasize that it
is the non-interactive version.

Two “normal” hooks are in use:

`after-switch-to-buffer-hook` <u>f</u>  
Run after switching to a buffer. This is used e.g. by `dired` do start
the command loop.

`find-file-hook`  
Run after setting the major mode of a buffer in `(after-find-file)`.

[^](#toc)

##### *Femto* Edit and Navigation Functions

`(eval-expression «expr»)`  
Evaluates the string *expr* as Lisp code and returns the result.

`(eval-expression-i)`  
Prompts for a string to evaluate and shows the result in the message
buffer.

`(eval-block)` ⇒ `t` <u>S: eval-region</u>  
Evaluates the *region* in the current buffer, inserts the result at
*point*. If *mark* in the current buffer is before *point* `eval-block`
evaluates this *region* and inserts the result at *point*. If *point* is
before *mark* `eval-block` does nothing but returning `t`.

`(insert-file)`  
Interactively insert file in current buffer.

`(shell-command[ «command_line»])`  
Execute string *command_line* with the `system()` call. If not given
prompt for commandline.

`(delete-next-word)`  
`(delete-previous-word)`  
`(kill-to-eol)`  
`(transpose-chars)`  
(find_and_eval_sexp)  
Editor commands for common key bindings.

`(describe-key)` ⇒ *p* <u>S</u>  
Read a key (sequence) with (get-key) and print in the message area
either the character, the key name or the function name bound to it.

`(kill-buffer-interactive)`  
Prompt for the buffer to kill and kill it. Default is the current
buffer.

`(find-file)`  
Prompt for a filename. If the file is already in a buffer switch to that
buffer, if not read it into a new buffer and switch to it.

`(save-buffer)`  
Saves the current buffer into it's file, if one is associated and the
buffer is modified. If the file does not exist it is created together
will all missing parent directories.

`(write-file)`  
Ask for a file name to save the current buffer into and then
`save-buffer` it.

`(save-some-buffers)`  
Interactively save modified file buffers.

`(save-buffers-kill-terminal)`  
`save-some-buffers` then exit *Femto*.

[^](#toc)

##### *Femto* Utility Functions

`(load-script «fn»)` <u>f</u>  
deprecated, use `require`.

`(insert-file-contents-literally «path»)`  
 

`(shell-exec «command_line»)` <u>f</u>  
Helper function for `shell-command`.

`(shell-command-lines «command»[ «arg»..])` ⇒ <u>f</u>  
Join string *command* and all *arg*s with a space and execute the
resulting command line with `popen()`. Read output and return it as list
of lines without the trailing newlines.

`(shell_strip_eol `*`line`*`)` ⇒ *line'* <u>f</u>  
Strip newline and carriage return characters from end of string *line*.

`(repeat «n» «func»)` <u>f</u>  
Execute *func* *n* times.

`(string-restrict-chars-p «rl» «s»)` ⇒ *p* <u>f</u>  
Returns `t` if all characters in string *s* are members of list of
charachters *rl*.

`(posix-filename «s»)` ⇒ *p* <u>f</u>  
`(is_ctl_g «k»)` ⇒ *p* <u>f</u>  
`(is_escape «k»)` ⇒ *p* <u>f</u>  
`(is_backspace «k»)` ⇒ *p* <u>f</u>  
`(is_ctl_s «k»)` ⇒ *p* <u>f</u>  
`(is_control_char «k»)` ⇒ *p* <u>f</u>  
Keystroke checks, *k* is the ASCII character as returned by `(get-key)`.

Returns `t` is string *s* is a posix compliant path: does not start with
a dash `-`, is only alphanumeric plus dot, underscore, dash and slash.

`(run-hooks «hooks»)`  
*hooks* is a variable name holding a list of function symbols which are
run in order. This implements Emacs “normal” hooks.

[^](#toc)

##### *Femto* Buffer Functions

`(buffer-name[ «buffer»])` ⇒ *name*  
Return the name of the given buffer of the current buffer if not given.

`(current-buffer)` ⇒ *name*  
Return the current buffer.

`(switch-to-buffer «name»)` ⇒ *name*  
Associate buffer *name* with the current window and run the Femto
specific hook `after-switch-to-buffer-hook`.

`(restore-buffer-modified-p «p»)` ⇒ *p*  
Set modified flag of the current buffer to predicate *p*.

`(set-buffer-modified-p «p»)` ⇒ *p*  
Set modified flag of the current buffer to predicate *p* and refresh all
windows.

(`buffer-list)` ⇒ *l*  
Return list of all buffers.

`(generate-new-buffer-name «name»)` ⇒ *name'*  
Starting with a given buffer name generate a unique buffer name by
appending `/«n»` where *n* is a decimal number.

`(buffer-basename «name»)` ⇒ *name'*  
Strips a trailing `/«n»` of from a buffer name.

`(create-file-buffer «filename»)` ⇒ *name*  
Gets or creates a file buffer and names it uniquely, given a filename.
The base name is the file name without directory components.

`(rename-buffer «name»[ «unique-p»])` ⇒ *name'*  
Renames the current buffer to *name*. If *unique-p* is given and not
null the buffer name is uniquified before renaming.

`(other-buffer)` ⇒ *name*  
Return the next best buffer after the current buffer.

`(insert-buffer-substring-no-properties from-buffer-or-name[ «start»[ «end»])` ⇒ `t`  
Insert the buffer contents of the given buffer *from-buffer-or-name*
into the current buffer at point. *start* and *end* is the region to
insert, *start* defaults to the beginning, *end* is the end of the from
buffer.

`(kill-buffer[ «buffer»])` ⇒ *name*  
Kills the given buffer or the current buffer if not given. Offers saving
modified buffers, switches to `other-buffer` if the buffer to kill is
the current buffer.

`(find-file-noselect «filename»)`  
Read the given file into a buffer and return the buffer. If the file
does not exist create an empty buffer for it and associate the filename.
If *filename* is a directory open the directory editor for it.

`(after-find-file)`  
Run after the buffer for a regular file is created or switched to. Sets
the major mode of the buffer and runs the hook `find-file-hook`.

[^](#toc)

### Femto Lisp Applications

#### `defmacro` Editor Macros

<span class="mark">Note: currently not working because of segfault in
lisp_eval(), supposedly double free/segfault</span>

This applications allows to record a keyboard macros and make it
available in a buffer called \*macro\*.

Keybindings:

c-x (  
starts the recording

c-x )  
stops the recording

c-x e  
executes the macro

esc-e  
executes the macro

[^](#toc)

#### `dired` Directory Navigation

`(dired-interactive)`  
Prompt for directory and open the directory editor on it.

`(dired «directory»)`  
Open the directory editor on path directory.

`(dired-after-switch-to-buffer-function )` <u>f</u>  
Hook run to (re-)start the directory editor.

`(dired-reload)` <u>f</u>  
Reload the current dired buffer.

`(dired-loop «ops»)` <u>f</u>  
Directory editor command loop. *ops* is the safeguard count:
`dired-loop` exists after *ops* commands.

`(dired-get-info)` ⇒ `(«type» . «name»)` <u>f</u>  
Retrrieve information from current line: file *type* and *name*.

`(dired-create-directory-interactive)`  
Interactively create a directory, bound to <span class="kbd">+</span>.

`(dired-do-copy-interactive)`  
bound to <span class="kbd">C</span>.

`(dired-do-delete)`  
bound to <span class="kbd">D</span>.

`(dired-do-chgrp)`  
bound to <span class="kbd">G</span>.

`(dired-do-chmod)`  
bound to <span class="kbd">M</span>.

`(dired-do-chown)`  
bound to <span class="kbd">O</span>.

`(dired-do-rename)`  
Rename or move, bound to <span class="kbd">R</span>.

`(dired-do-symlink)`  
bound to <span class="kbd">S</span>.

`(dired-do-touch)`  
bound to <span class="kbd">T</span>.

[^](#toc)

#### `bufmenu` Buffer Selection Menu

<span class="mark">Tbd.: document this and the following
applications.</span>

#### `grep` File Content Search

#### `git` Git Repo Helper

#### `info` Builtin Help

#### `oxo` Tic-Tac-Toe Game

[^](#toc)
