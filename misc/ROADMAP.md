-*- mode: markdown; fill-column: 80 -*-

# Next
- Use posix-filename on save-buffer.
- Move prompt-filename to Lisp.
- Bug: femto segfaults w/o femtorc, batch-mode
- Consider implementing bury-buffer.
- Make buffer mode a Lisp Object, register default symbols for it as self
  evaluating constants.
- Improve readability, stability and reduce code size of Femto libraries:
  - Use if/if-not, when/unless instead of cond.
  - Use intern.
  - Use after-mode-switch-hook
- Consider replacing display_prompt_and_response() with message()


# Future
- Implement add-hook and remove-hook, maybe look into subr.el, maybe need
  (set var val).
- Pipe a buffer through a shell command and read the output back into a
  "*Shell Command Output*" special buffer.
- Change eval_block to eval_region (n.a.: eval_region does a different thing).
- dired: delete/rename buffers of deleted/renamed files/directories.
- Find out how to build on NetBSD: problem is ncurses libraries.
- Improve/support batch mode: output = stdout.
- Implement per buffer/mode keymaps.
- Make buffer name and filename Lisp string objects.
- Move b_flags to individual bits - let the compiler work, not us.
- Make buffer (major) mode a Lisp (symbol) object.
- Write a test file for all lisp functions.


# Femto 2.25.0

The main goal of this release was to fix file saving into non-existing
directories. This lead to a plenty of innovations:

- Buffer list is a ring. Order is recently used, instead of alphabetical
- Buffers with special flags are not considered for saving
- Less C-code, less redundant C-code, each C file has it's own header file.
- main.c/header.h renamed to femto.c/femto.h
- Rewrite of the buffer list implementation.
- Start general adoption of stdbool.h types.
- More Lisp code for higher level functionality - less unflexible C-code.
- More buffer handling primitives for use in Lisp.
- Buffer names and filenames are allocated dynamically.
- Reading files into buffers is done via Lisp primitives.
- More conformance to Emacs/Elisp naming conventions.

The femto.lsp library now implements:
- File loading and saving.
- Buffer creation, saving and killing.
- Buffer naming, unique buffer names are created by a /n suffix.
- "Normal" hooks, switch-to-buffer runs a hook: this simplifies writing special
  modes like dired.


The dired implementation was completely rewritten and supports now:
- Directory traversal
- All basic "direct" file/directory operations: create, delete, chown, chgrp,
  chmod, touch

It can be used as example to improve git, defmacro, grep, oxo.

Bug fixes
+ lisp_eval() segfault on closing output (defmacro.lsp)
+ File save bug by using fmkdir.
  https://github.com/hughbarney/femto/issues/14
+ Make fLisp read from /dev/null to avoid:
  https://github.com/hughbarney/femto/issues/10
+ Documentation
  - https://github.com/hughbarney/femto/issues/40
  - https://github.com/hughbarney/femto/issues/39
  - https://github.com/hughbarney/femto/issues/37
  - https://github.com/hughbarney/femto/issues/36
+ To be closed:
  - https://github.com/hughbarney/femto/issues/1

Some details on changes:

C-code:

- Buffer file name handling: alloc/free string instead of fixed buffer.
- safe_strncpy() moved to undo.c, util.c removed.
- Removed file reading and buffer naming C-code
  + insert_file() in gap.c
  + make_buffer_name_uniq(), make_buffer_name() in utils.c
  + rename_current_buffer() in command.c -> set_buffer_name(),
- Remove all unneeded message strings
- Buffer name and filename handling: alloc/free string instead of fixed and too
  small buffer.
- Fix undo to reset modified flag when no more undo's left.
- Refactored include system
  + rename main.c to femto.c
  + rename header.h to femto.h
- eval_string() does not touch interp->output, eval_block() and repl() needed
  this, they are implemented in Lisp now.

Lisp:

- Implement (pop-to-buffer)
- Implement (rename-buffer).
- (kill-buffer) now fails if named buffer does not exist.
- (show-prompt) removed, use (message "prompt")(update-display) instead
- (prompt-filename) improved to be able to use a default input and recurse
  directories.
- Implement (save-some-buffers) .
- Moved posix file name check to Lisp: can't start with -, all chars must be
  isalnum() or in "._-/".  Not used in current File loading code.
- Aliased global argv to command-line-args, argv0 to invocation-name in femto.lsp.
- Use (catch load ..) again in femto.rc, to reduce dependency on file extension.

# femto 2.24.2

- Make femto.rc use fstat instead of (system "test -f")
