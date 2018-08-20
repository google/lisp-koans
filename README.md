# Lisp Koans

## Getting Started

### One-time Method

From a terminal, execute your lisp interpreter on the file 'contemplate.lsp' e.g.

    abcl --noinform --noinit --load contemplate.lsp --eval '(quit)'
    ccl -n -l contemplate.lsp -e '(quit)'
    clisp -q -norc -ansi contemplate.lsp
    ecl -norc -load contemplate.lsp -eval '(quit)'
    sbcl --script contemplate.lsp

### Watching the Koans

On Linux and MacOS systems, the shell scripts `meditate-linux.sh` and
`meditate-macos.sh` can be used to automatically evaluate 'contemplate.lsp'
whenever the koan files are modified, providing immediate feedback on changes
to the koans. To run the MacOS version you need to have
[`fswatch`](https://github.com/emcrisostomo/fswatch) installed. From a terminal:

    $ cd lisp-koans
    $ sh meditate-linux.sh # on Linux
    $ sh meditate-macos.sh # on MacOS

## Results of Contemplation

Running on a fresh version should output the following:

```
Thinking about ASSERTS
    ASSERT-TRUE requires more meditation.

You have not yet reached enlightenment ...
  A koan is incomplete.

Please meditate on the following code:
   File "koans/asserts.lsp"
   Koan "ASSERT-TRUE"
   Current koan assert status is "(INCOMPLETE)"

You are now 0/169 koans and 0/25 lessons closer to reaching enlightenment
```

This indicates that the script has completed, and that the learner should look
to asserts.lsp to locate and fix the problem.  The problem will be within
a define-test expression such as

    (define-test assert-true
        "t is true.  Replace the blank with a t"
        (assert-true ___))

In this case, the test is incomplete, and the student should fill
in the blank (____) with appropriate lisp code to make the assert pass.


In order to test code, or evaluate tests interactively, students may copy
and paste code into the lisp command line REPL.

## Quoting the Ruby Koans instructions

   "In test-driven development the mantra has always been, red, green,
refactor. Write a failing test and run it (red), make the test pass (green),
then refactor it (that is look at the code and see if you can make it any
better). In this case you will need to run the koan and see it fail (red), make
the test pass (green), then take a moment and reflect upon the test to see what
it is teaching you and improve the code to better communicate its
intent (refactor)."

## Content

The Common Lisp koans are based on the Python koans and Ruby koans projects.
Additionally, many of the tests are based on new material that is special
to Common Lisp.

Note that the unit on threads uses an SBCL specific threading API.  A reader
macro will remove this unit on Lisp implementations other than SBCL.
