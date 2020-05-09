# Lisp Koans

## Getting Started

### One-time Method

From a terminal, execute your lisp interpreter on the file 'contemplate.lisp' e.g.

    abcl --noinform --noinit --load contemplate.lisp --eval '(quit)'
    ccl -n -l contemplate.lisp -e '(quit)'
    clisp -q -norc -ansi contemplate.lisp
    ecl -norc -load contemplate.lisp -eval '(quit)'
    sbcl --script contemplate.lisp

### Watching the Koans

On Linux and MacOS systems, the shell scripts `meditate-linux.sh` and
`meditate-macos.sh` can be used to automatically evaluate 'contemplate.lisp'
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
    FILL-IN-THE-BLANKS requires more meditation.

You have not yet reached enlightenment.
    A koan is incomplete.
Please meditate on the following code:
    File "koans/asserts.lisp"
    Koan "FILL-IN-THE-BLANKS"
    Current koan assert status is "(INCOMPLETE INCOMPLETE INCOMPLETE)"

You are now 0/198 koans and 0/31 lessons closer to reaching enlightenment.
```

This indicates that the script has completed, and that the learner should look
to asserts.lisp to locate and fix the problem.  The problem will be within
a define-test expression such as

```lisp
;;; In order to progress, fill in the blanks, denoted via ____ in source code.
;;; Sometimes, you will be asked to provide values that are equal to something.

(define-test fill-in-the-blanks
  (assert-equal ____ 2)
  (assert-equal ____ 3.14)
  (assert-equal ____ "Hello World"))

;;; Sometimes, you will be asked to say whether something is true or false,
;;; In Common Lisp, the canonical values for truth and falsehood are T and NIL.

(define-test assert-true
  (assert-true ____))

(define-test assert-false
  (assert-false ____))
```

In this case, the test is incomplete, and the student should fill
in the blank (\_\_\_\_) with appropriate lisp code to make the assert pass.

In order to test code, or evaluate tests interactively, students may copy
and paste code into the lisp command line REPL.

### Testing

To test the koans, execute your lisp interpreter on the file 'contemplate.lisp' e.g.

    abcl --noinform --noinit --load test.lisp --eval '(quit)'
    ccl -n -l test.lisp -e '(quit)'
    clisp -q -norc -ansi test.lisp
    ecl -norc -load test.lisp -eval '(quit)'
    sbcl --script test.lisp

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

Note that the unit on threads uses bordeaux-threads and bt-semaphore.
The user must have Quicklisp installed and loaded or a reader macro
will remove the instructions to run :threads.
For information and instructions on installing Quicklisp
please see:
https://www.quicklisp.org/beta/
The user can either remove #+quicklisp and uncomment
(load "~/.quicklisp/setup.lisp") in threads.lisp, or  if they know
quicklisp will be loaded while running contemplate.lisp do nothing.
