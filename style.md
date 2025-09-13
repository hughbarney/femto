# Coding Style

Femto is coded in C and Lisp.  For C code use the Kernighan and
Ritchie (K&R) style, using 4 spaces for indentation.

This style can be setup in GNU Emacs using the following elisp code

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

For Lisp files lean to Emacs Lisp formatting, specifically:

- Indentation is two spaces.
- Do not use tabs.
- Put trailing parentheses on a single line, separated by a single
  space:

        (defun example ()
		  (+ 3 4) )
