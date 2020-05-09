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

;;; Copyright (c) 2004-2005 Christopher K. Riesbeck
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.

;;; This version of testing framework is based on LISP-UNIT, extended
;;; to support the lisp koans. Specifically, it is an unnamed branch from
;;; https://github.com/OdonataResearchLLC/lisp-unit/
;;; with hash 93d07b2fa6e32364916225f6218e9e7313027c1f
;;;
;;; Modifications were made to:
;;;   1) Support incomplete tests in addition to passing and failing ones
;;;   2) End test execution at the first non-passing test
;;;   3) Remove all dead code unrelated to lisp-koans
;;;   4) Rename the system to not collide with the original LISP-UNIT.

;;; Packages
(defpackage #:lisp-koans.test
  (:use #:common-lisp)
  ;; Assertions
  (:export #:assert-eq #:assert-eql #:assert-equal #:assert-equalp #:true-or-false?
           #:assert-expands #:assert-true #:assert-false #:assert-error)
  ;; Manage tests
  (:export #:define-test #:test-count #:test-total-count #:run-koans)
  ;; Test blank
  (:export #:____))

(in-package #:lisp-koans.test)

;; The self-evaluating test blank allows many Lisp forms in the koans to compile
;; without errors.

(defvar ____ '____)

;;; Global unit test database

(defparameter *test-db* (make-hash-table :test #'eq))

(defun package-table (package)
  (multiple-value-bind (value foundp) (gethash (find-package package) *test-db*)
    (if foundp
        value
        (setf (gethash package *test-db*) '()))))

(defun (setf package-table) (new-value package)
  (setf (gethash (find-package package) *test-db*) new-value))

(defmacro define-test (name &body body)
  "Store the test in the test database."
  `(progn
     (pushnew (list ',name ',body) (package-table *package*)
              :test (lambda (x y) (eq (car x) (car y))))
     ',name))

;;; Test statistics

(defun test-count (&optional (package *package*))
  "Returns the number of tests for a package."
  (let ((table (package-table package)))
    (length table)))

(defun test-total-count ()
  "Returns the total number of tests."
  (loop for table being the hash-values of *test-db*
        sum (length table)))

;;; Test passed predicate.

(defun test-passed-p (type expected actual test)
  (ecase type
    (:error (or (eql (car actual) (car expected)) (subtypep (car actual) (car expected))))
    (:equal (and (>= (length expected) (length actual)) (every test expected actual)))
    (:macro (equal (car actual) (car expected)))
    (:result (eql (not (car actual)) (not (car expected))))))

(defun form-contains-blanks-p (form)
  (typecase form
    (symbol (eq form '____))
    (cons (or (form-contains-blanks-p (car form))
              (form-contains-blanks-p (cdr form))))))

(defun notnot (x) (not (not x)))

(defvar *koan-assert-list*)

(defun internal-assert (type form code-thunk expected-thunk test)
  (if (form-contains-blanks-p form)
      (push :incomplete *koan-assert-list*)
      (let* ((expected (multiple-value-list (funcall expected-thunk)))
             (actual (multiple-value-list (funcall code-thunk)))
             (passed (test-passed-p type expected actual test))
             (result (if passed :pass :fail)))
        (push result *koan-assert-list*))))

(defmacro expand-assert (type form body expected &key (test '#'eql))
  `(internal-assert ,type ',form (lambda () ,body) (lambda () ,expected) ,test))

;;; Assert macros

(defmacro assert-eq (form expected)
  "Assert whether expected and form are EQ."
  `(expand-assert :equal ,form ,form ,expected :test #'eq))

(defmacro assert-eql (form expected)
  "Assert whether expected and form are EQL."
  `(expand-assert :equal ,form ,form ,expected :test #'eql))

(defmacro assert-equal (form expected)
  "Assert whether expected and form are EQUAL."
  `(expand-assert :equal ,form ,form ,expected :test #'equal))

(defmacro assert-equalp (form expected)
  "Assert whether expected and form are EQUALP."
  `(expand-assert :equal ,form ,form ,expected :test #'equalp))

(defmacro true-or-false? (form expected)
  "Assert whether expected and form are logically equivalent."
  `(expand-assert :equal ,form (notnot ,form) (notnot ,expected) :test #'eql))

(defmacro assert-error (form condition)
  "Assert whether form signals condition."
  (let ((e (gensym "E")))
    `(expand-assert :error ,form (handler-case ,form (error (,e) (type-of ,e)))
                    ,condition)))

(defmacro assert-expands (form expected)
  "Assert whether form expands to expansion."
  `(expand-assert :macro ',form (macroexpand-1 ',form) ,expected))

(defmacro assert-false (form)
  "Assert whether the form is false."
  `(expand-assert :result ,form ,form nil))

(defmacro assert-true (form)
  "Assert whether the form is true."
  `(expand-assert :result ,form (notnot ,form) t))

;;; Run the tests

(defun run-koan (code)
  (let ((*koan-assert-list* nil))
    (handler-case (funcall (coerce `(lambda () ,@code) 'function))
      (error () (push :error *koan-assert-list*)))
    *koan-assert-list*))

(defun run-koans (package)
  "Run all koans for a given package."
  (loop with results = nil
        for (test-name unit-test) in (reverse (package-table package))
        for koan-result = (run-koan unit-test)
        do (push (list test-name koan-result) results)
        while (every (lambda (x) (eq x :pass)) koan-result)
        finally (return results)))
