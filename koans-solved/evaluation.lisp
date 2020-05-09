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
  (assert-equal 5 (+ 2 3))
  (assert-equal -2 (- 1 3))
  (assert-equal 28 (* 7 4))
  (assert-equal 25 (/ 100 4)))

(define-test numberp
  ;; NUMBERP is a predicate which returns true if its argument is a number.
  (assert-equal t (numberp 5))
  (assert-equal t (numberp 2.0))
  (assert-equal nil (numberp "five")))

(define-test evaluation-order
  ;; Arguments to a function are evaluated before the function is called.
  (assert-equal 9 (* (+ 1 2) (- 13 10))))

(define-test basic-comparisons
  ;; The below functions are boolean functions (predicates) that operate on
  ;; numbers.
  (assert-equal t (> 25 4))
  (assert-equal nil (< 8 2))
  (assert-equal t (= 3 3))
  (assert-equal t (<= 6 (/ 12 2)))
  (assert-equal t (>= 20 (+ 1 2 3 4 5)))
  (assert-equal t (/= 15 (+ 4 10))))

(define-test quote
  ;; Preceding a list with a quote (') will tell Lisp not to evaluate a list.
  ;; The quote special form suppresses normal evaluation, and instead returns
  ;; the literal list.
  ;; Evaluating the form (+ 1 2) returns the number 3, but evaluating the form
  ;; '(+ 1 2) returns the list (+ 1 2).
  (assert-equal 3 (+ 1 2))
  (assert-equal '(+ 1 2) '(+ 1 2))
  (assert-equal '(+ 1 2) (list '+ 1 2))
  ;; The 'X syntax is syntactic sugar for (QUOTE X).
  (true-or-false? t (equal '(/ 4 0) (quote (/ 4 0)))))

(define-test listp
  ;; LISTP is a predicate which returns true if the argument is a list.
  (assert-equal t (listp '(1 2 3)))
  (assert-equal nil (listp 100))
  (assert-equal nil (listp "Hello world"))
  (assert-equal t (listp nil))
  (assert-equal nil (listp (+ 1 2)))
  (assert-equal t (listp '(+ 1 2))))
