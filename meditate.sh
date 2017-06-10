#!/bin/bash

if [ $# != 1 ]; then
    echo "usage: sh meditate.sh <lisp implementation>"
    echo "       lisp implementation: one of abcl, ccl, clisp, ecl, or sbcl"
    exit
fi

choose_command_line() {
    case "$1" in
        'abcl' )
            echo "abcl --noinform --noinit --load contemplate.lsp --eval '(quit)'"
        ;;
        'ccl' )
            echo "ccl -n -l contemplate.lsp -e '(quit)'"
        ;;
        'clisp' )
            echo "clisp -q -norc -ansi contemplate.lsp"
        ;;
        'ecl' )
            echo "ecl -norc -load contemplate.lsp -eval '(quit)'"
        ;;
        'sbcl' )
            echo "sbcl --script contemplate.lsp"
        ;;
        * )
            echo ""
            exit
            ;;
    esac
}

CONTEMPLATE=$(choose_command_line $1)
if [ "$CONTEMPLATE" = "" ]; then
    echo "Unknown Lisp implementation."
    exit
else
    echo $CONTEMPLATE
fi

$CONTEMPLATE
while inotifywait -e modify --exclude "\#.*\#" -q -r koans; do
    $CONTEMPLATE
done
