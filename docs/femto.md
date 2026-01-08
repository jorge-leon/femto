# The Femto Editor

> A designer knows he has achieved perfection not when there is nothing
> left to add, but when there is nothing left to take away. -- Antoine
> de Saint-Exupery

*Femto* is an Emacs like text editor for the console with a minimal
memory footprint which can be customized with a Lisp extension language.
This manual describes how to use and customize *Femto*. It refers
to *Femto* version 2.25.

*Femto* is hosted on its [Github
repository](https://github.com/jorge-leon/femto), it is released to the
public domain by [Hugh Barney](mailto:hughbarney@gmail.com)  in 2017 and
[Georg Lehner](mailto:jorge@magma-soft.at) 2023

The extension language of *Femto* is *fLisp*, it is documented in the
[fLisp
manual](https://github.com/jorge-leon/flisp/blob/main/doc/flisp.html)
([Markdown](https://github.com/jorge-leon/flisp/blob/main/doc/flisp.md)).

Note: This is an unofficial release, all relevant links go to
<https://github.com/jorge-leon/> all credits go to the original author.

## Femto Key Bindings



            C-A   begining-of-line
            C-B   backward-character
            C-D   delete-char
            C-E   end-of-line
            C-F   forward Character
            C-G   Abort (at prompts)
            C-H   backspace
            C-I   handle-tab
            C-J   newline
            C-K   kill-to-eol
            C-L   refresh display
            C-M   Carrage Return
            C-N   next line
            C-P   previous line
            C-R   search-backwards
            C-S   search-forwards
            C-T   transpose-chars
            C-U   Undo
            C-V   Page Down
            C-W   Kill Region (Cut)
            C-X   CTRL-X command prefix
            C-Y   Yank (Paste)
            C-Z   suspend

            M-<   Start of file
            M->   End of file
            M-!   shell-command (prompt for a command which is sent to the shell)
            M-v   Page Up
            M-f   Forward Word
            M-b   Backwards Word
            M-g   goto-line
            M-r   Search and Replace
            M-w   copy-region

            C-spacebar Set mark at current position.

            ^X^B  List Buffers
            ^Xb   List Buffers
            ^X^C  Exit. Any unsaved files will require confirmation.
            ^X^F  Find file; read into a new buffer created from filename.
            ^X^G  git-menu
            ^X^O  Run the OXO game
            ^X^S  Save current buffer to disk, using the filename associated with the buffer
            ^X^W  Write current buffer to disk. Type in a new filename at the prompt
            ^X@   shell-command
            ^Xc   edit-config (edit personal init.lsp configuration file)
            ^Xd   dired-interactive (directory editor)
            ^Xg   grep-command
            ^Xh   show-info (show a help buffer)
            ^Xi   Insert file at point
            ^X=   Show Character at position
            ^X^N  next-buffer
            ^Xn   next-buffer
            ^Xk   kill-buffer
            ^X1   delete-other-windows
            ^X2   split-window
            ^Xo   other-window
            ^Xs   save-some-buffers

            Home  Beginning-of-line
            End   End-of-line
            Del   Delete character under cursor
            Ins   Toggle Overwrite Mode
            Left  Move left
            Right Move point right
            Up    Move to the previous line
            Down  Move to the next line
            Backspace delete caharacter on the left
            Ctrl+Up      beginning of file
            Ctrl+Down    end of file
            Ctrk+Left    Page Down
            Ctrl+Right   Page Up

### Copying and moving



            C-spacebar Set mark at current position
            ^W   Delete region
            ^Y   Yank back kill buffer at cursor
            M-w  Copy Region

A region is defined as the area between this mark and the current cursor
position. The kill buffer is the text which has been most recently
deleted or copied.

Generally, the procedure for copying or moving text is:

1.  Mark out region using M-\<spacebar\> at the beginning and move the
    cursor to the end.
2.  Delete it (with ^W) or copy it (with M-W) into the kill buffer.
3.  Move the cursor to the desired location and yank it back (with ^Y).

### Searching



            C-S or C-R enters the search prompt, where you type the search string
            BACKSPACE - will reduce the search string, any other character will extend it
            C-S at the search prompt will search forward, will wrap at end of the buffer
            C-R at the search prompt will search backwards, will wrap at start of the buffer
            ESC will escape from the search prompt and return to the point of the match
            C-G abort the search and return to point before the search started

## Lisp Interaction

There are two ways to interact with fLisp within Femto.

- You can use C-\] to find the last s-expression above the cursor and
  send it to be evaluated.
- You can mark a region and send the whole region to be evaluated with
  ESC-\].

### Lisp Interaction - finding and evaluating the last s-expression

This works in almost the same way as GNU Emacs in the scratch buffer.

### Lisp Interaction - mark and evaluating a region

Type a lisp function into the editor, for example:



            1: --------------
            2: (defun factorial (n)
            3:   (cond ((= n 0) 1)
            4:     (t (* n (factorial (- n 1))))))
            5:--------------

Place the cursor at the beginning of line 1 and set a mark (hit
control-spacebar).

Now move the cursot to line 5 and evaluate the block of code (hit escape
followed by \])

Femto will pass the code to lisp for it to be evaluated.


    <Lambda (n)>

Now call factorial in the same way (mark the start of the code, move to
the end of the code and hit escape-\])


    (factorial 6)

            720

![Screenshot of Femto editor showing Lisp
evaluation.](https://github.com/hughbarney/femto/blob/master/screenshots/femto-lisp.jpg)

## Femto Startup

The Femto editor itself provides only basic buffer movement and edit
functions, everything else is done by extending the user interface using
the Lisp extension language which is bootstrapped in the following way:

1.  The *fLisp* core library and Femto initialization code is loaded
    from the file `/usr/local/share/femto/init.lsp`.
    - This path can vary according to the installation method and
      operating sytem and can be changed at runtime with the environment
      variable `FEMTORC`.
2.  The *flisp*, *string* and *file* Lisp libraries are loaded from the
    directory `/usr/local/share/flisp`.
    - This path can also vary and can be changed at runtime with
      `FLISPLIB`.
3.  The *fLisp* library load path `script_dir` is changed to
    `/usr/local/share/femto` and `startup.lsp` is loaded from there.
    - This path can also vary and can be changed at runtime with
      `FEMTOLIB`.
4.  `startup.lsp:`
    - `​​​`Loads the *defmacro*, *bufmenu*, *dired*, *grep* and *git*
      libraries.
    - Creates all related key bindings.
    - shows a startup message in the *\*scratch\** buffer.
    - Tries to load the users `init.lsp` file from the directory
      `$HOME/.config/femto`.
    - Parses the command line arguments, i.e. loads all given files into
      buffers and optionally jumps to the given lines.

## Modes

Modes control the behaviour of the editor and the syntax highligher

cmode, python  
Highlights single and double quoted strings, block comments and line
comments.

lisp  
Highlights Lisp commands which start with semicolons.

## Debugging

If the environment variable `FEMTO_DEBUG` exists and is not set to `0`
debug mode is enabled. In debug mode, some internal workings are logged
to the file `debug.out`, as well as the arguments of the invocations of
the `log-debug` Lisp primitive.

## Basic Femto Extension

The fLisp libraries are described in its own
[project](https://github.com/jorge-leon/flisp/doc).

`femto.lsp` provide basic editor functionality. The user visible
functions are:

- `(load-script` `file)` - load file from script directory (obsolete,
  use `require`).
- For Emacs like key bindings:
  - `shell-command`, `insert-file`, `delete-next-word`,
    `delete-previous-word`,
    `kill-to-eol`, `describe-key`, `find_and_eval_sexp`, `transpose-chars`

## Femto Extensions

Additional extensions loaded by `femto.rc`

- **dired** - enbles directory editing (Emacs style filemanager)

  - **C-x C-d** to invoke, then single character keystrokes provide menu
    options
  - **f/Ret** load file or dired directory on current line
  - **q** exit dired
  - **g** reload dired buffer
  - **^** dired parent directory
  - **+** create directory
  - On current line:
    - **C** copy
    - **D** delete 
    - **G** change group
    - **M** change mode
    - **O** change owner
    - **R** rename/move
    - **S** symlink
    - **T** touch (change timestamp)

- **grep** - enables searching for text in files and loading of the
  files at the location of the match into the editor.

  - **C-x g** to invoke, will request a search string and files to
    search
  - **C-x \`** to load the next matching file

- **bufmenu** - the classic Emacs buffer menu

  - **C-x C-b** to invoke, then single character keystrokes provide menu
    options
  - **1** loads the file on the current line in one window
  - **2** loads the file on the current line in a split window
  - **s** saves the file on the current line to disk
  - **k** unloads the file without saving
  - **x** exits bufmenu

- **git** - a simple interface to the git version control tool (similar
  to GNU Emacs magit).

  - **C-x g** to invoke, then single character keystrokes menu options

- **oxo** - a basic implementation of tick-tack-toe that runs in the
  Editor.

  - **C-x C-o** to invoke

![Femto
screenshot](https://github.com/hughbarney/femto/blob/master/screenshots/femto-oxo.jpg)

## Known Issues

`goto-line` will fail to go to the very last line. This is a special
case that could easily be fixed.

Adding a line at the bottom of a window will hide the line until the
cursor moves up and down again or the screen is refreshed.

prompt-filename misbehaves and segfaults on Alpine Linux

dired eventually enters the key query loop recursively: one has to **q**
out of it when already in a file buffer.
