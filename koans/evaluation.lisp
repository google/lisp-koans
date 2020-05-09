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

;;; In most imperative languages, the syntax of a function call has the function
;;; name succeeded by a list of arguments. In Lisp, the function name and
;;; arguments are all part of the same list, with the function name the first
;;; element of that list.

(define-test function-names
  ;; In these examples, +, -, *, and / are function names.
  (assert-equal ____ (+ 2 3))
  (assert-equal ____ (- 1 3))
  (assert-equal ____ (* 7 4))
  (assert-equal ____ (/ 100 4)))

(define-test numberp
  ;; NUMBERP is a predicate which returns true if its argument is a number.
  (assert-equal ____ (numberp 5))
  (assert-equal ____ (numberp 2.0))
  (assert-equal ____ (numberp "five")))

(define-test evaluation-order
  ;; Arguments to a function are evaluated before the function is called.
  (assert-equal ____ (* (+ 1 2) (- 13 10))))

(define-test basic-comparisons
  ;; The below functions are boolean functions (predicates) that operate on
  ;; numbers.
  (assert-equal ____ (> 25 4))
  (assert-equal ____ (< 8 2))
  (assert-equal ____ (= 3 3))
  (assert-equal ____ (<= 6 (/ 12 2)))
  (assert-equal ____ (>= 20 (+ 1 2 3 4 5)))
  (assert-equal ____ (/= 15 (+ 4 10))))

(define-test quote
  ;; Preceding a list with a quote (') will tell Lisp not to evaluate a list.
  ;; The quote special form suppresses normal evaluation, and instead returns
  ;; the literal list.
  ;; Evaluating the form (+ 1 2) returns the number 3, but evaluating the form
  ;; '(+ 1 2) returns the list (+ 1 2).
  (assert-equal ____ (+ 1 2))
  (assert-equal ____ '(+ 1 2))
  (assert-equal ____ (list '+ 1 2))
  ;; The 'X syntax is syntactic sugar for (QUOTE X).
  (true-or-false? ____ (equal '(/ 4 0) (quote (/ 4 0)))))

(define-test listp
  ;; LISTP is a predicate which returns true if the argument is a list.
  (assert-equal ____ (listp '(1 2 3)))
  (assert-equal ____ (listp 100))
  (assert-equal ____ (listp "Hello world"))
  (assert-equal ____ (listp nil))
  (assert-equal ____ (listp (+ 1 2)))
  (assert-equal ____ (listp '(+ 1 2))))
