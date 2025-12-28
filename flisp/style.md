# Coding Style

fLisp is coded in C and Lisp.

## C Code

### C Style
For C code use the Kernighan and
Ritchie (K&R) style, using 4 spaces for indentation.

This style can be set up in GNU Emacs using the following elisp code

```lisp

add-hook 'c-mode-hook 'customize-cc-mode) ; cc-mode setup

(defun customize-cc-mode ()
  (local-set-key "\C-h" 'backward-delete-char)
  (setq c-default-style "k&r")
  ;; this will make sure spaces are used instead of tabs
  (setq tab-width 4 indent-tabs-mode nil)
  (setq indent-tabs-mode 'nil)
  (setq c-basic-offset 4)
  (define-key c-mode-map "\C-m" 'newline-and-indent)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'statement-case-open 0)
  (c-set-offset 'case-label 0)
  (c-set-offset 'brace-list-open 0)
)

```

The code below is an example of this style


```C

/* K&R coding style with 4 spaces, no tabs */
#include <stdio.h>

void func1(int);
void func2(int);
int main(int, char **);

int main(int argc, char **argv) {
    int x = 5;
    
    while (x > 1 )  {
        if (x % 2 == 0)
            func1(x);
        else
            func2(x);
        x--;
    }
    return 0;
}

void func1(int n) {
    int i = 1;
    
    for (i = 0; i< n; i++) {
        printf("i=%d\n", i);      
    }
}

void func2(int n) {
    switch(n) {
    case 1:
        printf("1\n");
        break;
    default:
        printf("default %d\n", n);
        break;
    }
}

```

### C Coding Conventions

For flags use `stbool.h`, the `bool` data type and the `true` and
`false` constants.

Use early exits instead of nested if - then - else clauses.

Always use comparision with `NULL` instead of considering the pointer as flag;
onstead of `(!ptr)` use `(ptr == NULL)`.


## Lisp Code

### Lisp Style
For Lisp files lean to Emacs Lisp formatting, specifically:

- Indentation is two spaces.
- Do not use tabs.
- Put trailing parentheses on a single line, separated by a single
  space:

        (defun example ()
		  (+ 3 4) )

### Lisp Coding Conventions

Use a dash '-' as namespace separator for function and variable names,
just like in Emacs. Use an underscore '_' for "private" function
names.

The core library uses single letter parameter symbols with consistent
meanings. This style is not encouraged in user libraries, except for
generic functions.

Only use globally scoped variables and (setq) if you can answer five
whys about it.

fLisp provides a small but powerful set of language features in the
core Lisp library, carefully selected to craft easy to read programs.

(cond) is the generic conditional primitive, however also use the
additional set of conditionals:

* (if) - when the *then* part is a single expresion. The *else* part
  can contain several expresions.
* (if-not) - when the *predicate* would otherwise require logic
  negation.
* (when) - when the *else* part would be empty.
* (unless) - when the *then* part would be empty.
* (cond) - when both *then* and *else* contain serveral expresions.
* (and) - for single decision chains.
* (or) - for providing defaults.

For loops use either tail recursion or the labelled (let) expression
with tail recursion, but always consider using (mapcar) (filter) and
(remove) for operations on lists.  Use (curry), (flip), and the
(ca*d*r) accessors for multi-

Use (assert-type) and (assert-number) for parameter validation.

