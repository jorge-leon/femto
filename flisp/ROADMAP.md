-*- mode: markdown; fill-column: 80 -*-

# ROADMAP

## Next

- GC starting with the first loaded symbol.
- Make logging mask, and suppress catch messages by default
- Implement more `(interp subcommand[arg..])` introspection and configuration commands.
  - (flisp output[ fd])
  - (flisp error[ fd])
  - (flisp debug[ fd])
  - (flisp types[ type..]) => list of types
  - (flisp body lambda|macro)
  - (flisp gc) => memory info
- Add read and eval tracing.

## Future

- Implement fnmatch() primitive. https://pubs.opengroup.org/onlinepubs/9799919799/functions/fnmatch.html
- Implement "ext"ension Lisp objects - use them for femto buffers, etc.
- Implement backquote and friends.
  - The reader already implements '`', ',' and ',@' as `quasiquote`, `unquote`
    and `splice-unquote`.
- Size reduction:
  - Reduce binary operators to 'and' and 'xor' and write needed rest in Lisp.
- Cleanup cerf: not so easy to do.
- Hash
  - Add new 'hash' field as uintptr_t to Lisp object struct type union.
  - Initialize each string type object with a hash of 0.
- Dynamic types
  - Make 'type' postfix of name instead of prefix. type-string -> string-type.
  - Add Object * interp->types to interpreter struct and register types there.
  - Use the hash of types for type checking.
- String size restriction
  - Memory allocator restricts the size of string objects.
  - Option to dynamically adjust or not.
  - Default size = max input string
  - Rationale: embedding, 32, 16, 8 bit versions
- posit's: https://en.wikipedia.org/wiki/Unum_(number_format)
- Event based I/O
  - Buffered I/O operations throw yield exception if buffers are full (w) /empty
    (r).
- Test more then one interpreter.
- ? CSP between interpreters?


## flisp 0.14
- Extensions are loaded dynamically
- double extension and file extension are optional
- Fixed: (cond ('(0))) tries to evaluate (0) but should return (0)
- Add (intern string) primitive.
- Add (interp ..) intospection primitive.
- Add/Fix: if, if-not, when, unless
- Add accessors: cadr, cddr, caddr, caar, cdar, caaar, cdaar.
- Add let*, prog1, min, max, join
- Remove string-contains Lisp implementation, it is covered by string-search.
- Remove (reduce), was incompatible and unused, replacement fold-left and family.
- Add string-empty-p string-split and string-join to the string
  library. Documentation for the complete library.
- Add new "file" library: mkdir, file-name-directory, file-name-nondirectory,
  file-name-extension.
- Consolidate more features in core.lsp, remove stdlib.lsp
- Add property list accesors.
- Unify error messages.
- Implement getcwd and fttyp.
- Implement repl in Lisp.


## fLisp 0.13
- Implemented interp introspection and configuration command with version and
  input subcommands.
- Implemented simple repl in Lisp and minimized flisp.c
- Replace setq and define with bind in the core. setq is defined in core.lsp
- Moved append, fold-left, flip, reverse, apply, print, princ to core.lsp
- Renamed os.env to getenv and move to file extension.
- Unified stdlib into flisp.lsp (again).
- Moved (system) to file extension.

## fLisp 0.12

- dynamic memory allocation.
- FLISP_RESULT macros have interp as parameter.
- chunk size and initial memory allocations tuned to femto.
- all memory allocation related Note's fixed.
- optional primitive trace mechanism.
- bitwise integer operations.
- file: feof, fgets, popen, pclose, fstat, mkdir; documentation.
- Poor man's unit test framework in Lisp.

## fLisp 0.11

- Showcase lisp_eval2 with (catch (fread)) mechanism.
- string search
- string-to-number in Lisp by using (read f) from a memory stream.

## fLisp 0.10

- double extension
- variadic multi-typed arithmetic
- (same) primitive, (eq) in Lisp
- fold, unfold, iota

## fLisp 0.9

- core uses only 64 bit integers.
- string-search in core.
- type-of
- all type predicates except null and consp in Lisp.

## fLisp 0.8

- error and object types are Lisp symbols instead of C-enums.

