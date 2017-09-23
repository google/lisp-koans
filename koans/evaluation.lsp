;;   Copyright 2013 Google Inc.
;;
;;   Licensed under the Apache License, Version 2.0 (the "License");
;;   you may not use this file except in compliance with the License.
;;   You may obtain a copy of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS,
;;   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;   See the License for the specific language governing permissions and
;;   limitations under the License.


;; based on http://psg.com/~dlamkins/sl/chapter03-02.html

(define-test test-function-name-is-first-argument
    "In most imperative languages, the syntax of a function call has
     the function name succeeded by a list of arguments.  In lisp,
     the function name and arguments are all part of the same list,
     with the function name the first element of that list."

  "in these examples, the function names are +, -, and *"
  (assert-equal 5 (+ 2 3))
  (assert-equal -2 (- 1 3))
  (assert-equal 28 (* 7 4))
  "'>' and '=' are the boolean functions (predicates) 'greater-than' and
   'equal to'"
  (assert-equal t (> 100 4))
  (assert-equal t (= 3 3))
  "'NUMBERP' is a predicate which returns true if the argument is a number"
  (assert-equal t (numberp 5))
  (assert-equal nil (numberp "five")))


(define-test test-evaluation-order
    "Arguments to functions are evaluated before the function"
  (assert-equal 9 (* (+ 1 2) (- 13 10))))


(define-test test-quoting-behavior
    "Preceding a list with a quote (') will tell lisp not to evaluate a list.
     The quote special form suppresses normal evaluation, and instead returns
     the literal list.
     Evaluating the form (+ 1 2) returns the number 3,
     but evaluating the form '(+ 1 2) returns the list (+ 1 2)"
  (assert-equal 3 (+ 1 2))
  (assert-equal '(+ 1 2) '(+ 1 2))
  "'LISTP' is a predicate which returns true if the argument is a list"
  " the '(CONTENTS) form defines a list literal containing CONTENTS"
  (assert-equal t (listp '(1 2 3)))
  (assert-equal nil (listp 100))
  (assert-equal nil (listp "Word to your moms I came to drop bombs"))
  (assert-equal t (listp nil))
  (assert-equal nil (listp (+ 1 2)))
  (assert-equal t (listp '(+ 1 2)))
  "equalp is an equality predicate"
  (assert-equal t (equalp 3 (+ 1 2)))
  "the '(xyz ghi) syntax is syntactic sugar for the (QUOTE (xyz ghi)) function."
  (true-or-false? t (equalp '(/ 4 0) (quote (/ 4 0)))))
