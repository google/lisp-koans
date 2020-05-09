;;; Copyright 2013 Google Inc.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; The function FORMAT is used to create formatted output. It is similar to
;;; the C function printf().
;;; See http://www.gigamonkeys.com/book/a-few-format-recipes.html

;;; T as the first argument to FORMAT prints the string to standard output.
;;; NIL as the first argument to FORMAT causes it to return the string.

(define-test format-basic
  ;; If there are no format directives in the string, FORMAT will return
  ;; a string that is STRING= to its format control.
  (assert-equal "Lorem ipsum dolor sit amet"
                (format nil "Lorem ipsum dolor sit amet")))

(define-test format-aesthetic
  ;; The ~A format directive creates aesthetic output.
  (assert-equal "This is the number 42"
                (format nil "This is the number ~A" 42))
  (assert-equal "This is the keyword FOO"
                (format nil "This is the keyword ~A" :foo))
  (assert-equal "(/ 24 (- 3 (/ 8 3))) evaluates to 72"
                (format nil "~A evaluates to ~A"
                        '(/ 24 (- 3 (/ 8 3)))
                        (/ 24 (- 3 (/ 8 3)))))
  (assert-equal "This is the character C"
                (format nil "This is the character ~A" #\C))
  (assert-equal "In a galaxy far far away"
                (format nil "In a ~A" "galaxy far far away")))

(define-test format-standard
  ;; The ~S format directive prints objects with escape characters.
  ;; Not all Lisp objects require to be escaped.
  (assert-equal "This is the number 42" (format nil "This is the number ~S" 42))
  (assert-equal "(/ 24 (- 3 (/ 8 3))) evaluates to 72"
                (format nil "~S evaluates to ~S"
                        '(/ 24 (- 3 (/ 8 3)))
                        (/ 24 (- 3 (/ 8 3)))))
  ;; Keywords are printed with their leading colon.
  (assert-equal "This is the keyword :FOO"
                (format nil "This is the keyword ~S" :foo))
  ;; Characters are printed in their #\X form. The backslash will need to be
  ;; escaped inside the printed string, just like in "#\\X".
  (assert-equal "This is the character #\\C"
                (format nil "This is the character ~S" #\C))
  ;; Strings include quote characters, which must be escaped:
  ;; such a string might look in code like "foo \"bar\"".
  (assert-equal "In a \"galaxy far far away\""
                (format nil "In a ~S" "galaxy far far away")))

(define-test format-radix
  ;; The ~B, ~O, ~D, and ~X radices print numbers in binary, octal, decimal, and
  ;; hexadecimal notation.
  (assert-equal "This is the number 101010"
                (format nil "This is the number ~B" 42))
  (assert-equal "This is the number 52"
                (format nil "This is the number ~O" 42))
  (assert-equal "This is the number 42"
                (format nil "This is the number ~D" 42))
  (assert-equal "This is the number 2A"
                (format nil "This is the number ~X" 42))
  ;; We can specify a custom radix by using the ~R directive.
  (assert-equal "This is the number 1120"
                (format nil "This is the number ~3R" 42))
  ;; It is possible to print whole forms this way.
  (let ((form '(/ 24 (- 3 (/ 8 3))))
        (result (/ 24 (- 3 (/ 8 3)))))
    (assert-equal "(/ 11000 (- 11 (/ 1000 11))) evaluates to 1001000"
                  (format nil "~B evaluates to ~B" form result))
    (assert-equal "(/ 30 (- 3 (/ 10 3))) evaluates to 110"
                  (format nil "~O evaluates to ~O" form result))
    (assert-equal "(/ 24 (- 3 (/ 8 3))) evaluates to 72"
                  (format nil "~D evaluates to ~D" form result))
    (assert-equal "(/ 18 (- 3 (/ 8 3))) evaluates to 48"
                  (format nil "~X evaluates to ~X" form result))
    (assert-equal "(/ 220 (- 10 (/ 22 10))) evaluates to 2200"
                  (format nil "~3R evaluates to ~3R" form result))))

(define-test format-iteration
  ;; The ~{ and ~} directives iterate over a list.
  (assert-equal "[1][2][3][4][5][6]" (format nil "~{[~A]~}" '(1 2 3 4 5 6)))
  (assert-equal "[1 2][3 4][5 6]" (format nil "~{[~A ~A]~}" '(1 2 3 4 5 6)))
  ;; The directive ~^ aborts iteration when no more elements remain.
  (assert-equal "[1], [2], [3], [4], [5], [6]"
                (format nil "~{[~A]~^, ~}" '(1 2 3 4 5 6))))

(define-test format-case
  ;; The ~( and ~) directives adjust the string case.
  (assert-equal "the quick brown fox"
                (format nil "~(~A~)" "The QuIcK BROWN fox"))
  ;; Some FORMAT directives can be further adjusted with the : and @ modifiers.
  (assert-equal "The Quick Brown Fox"
                (format nil "~:(~A~)" "The QuIcK BROWN fox"))
  (assert-equal "The quick brown fox"
                (format nil "~@(~A~)" "The QuIcK BROWN fox"))
  (assert-equal "THE QUICK BROWN FOX"
                (format nil "~:@(~A~)" "The QuIcK BROWN fox")))
