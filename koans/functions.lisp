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

(defun some-named-function (a b)
  (+ a b))

(define-test call-a-function
  ;; DEFUN can be used to define global functions.
  (assert-equal ____ (some-named-function 4 5))
  ;; FLET can be used to define local functions.
  (flet ((another-named-function (a b) (* a b)))
    (assert-equal ____ (another-named-function 4 5)))
  ;; LABELS can be used to define local functions which can refer to themselves
  ;; or each other.
  (labels ((recursive-function (a b)
             (if (or (= 0 a) (= 0 b))
                 1
                 (+ (* a b) (recursive-function (1- a) (1- b))))))
    (assert-equal ____ (recursive-function 4 5))))

(define-test shadow-a-function
  (assert-eq 18 (some-named-function 7 11))
  ;; FLET and LABELS can shadow function definitions.
  (flet ((some-named-function (a b) (* a b)))
    (assert-equal ____ (some-named-function 7 11)))
  (assert-equal ____ (some-named-function 7 11)))

(defun function-with-optional-parameters (&optional (a 2) (b 3) c)
  ;; If an optional argument to a function is not provided, it is given its
  ;; default value, or NIL, if no default value is specified.
  (list a b c))

(define-test optional-parameters
  (assert-equal ____ (function-with-optional-parameters 42 24 4224))
  (assert-equal ____ (function-with-optional-parameters 42 24))
  (assert-equal ____ (function-with-optional-parameters 42))
  (assert-equal ____ (function-with-optional-parameters)))

(defun function-with-optional-indication
    (&optional (a 2 a-provided-p) (b 3 b-provided-p))
  ;; It is possible to check whether an optional argument was provided.
  (list a a-provided-p b b-provided-p))

(define-test optional-indication
  (assert-equal ____ (function-with-optional-indication 42 24))
  (assert-equal ____ (function-with-optional-indication 42))
  (assert-equal ____ (function-with-optional-indication)))

(defun function-with-rest-parameter (&rest x)
  ;; A rest parameter gathers all remaining parameters in a list.
  x)

(define-test rest-parameter
  (assert-equal ____ (function-with-rest-parameter))
  (assert-equal ____ (function-with-rest-parameter 1))
  (assert-equal ____ (function-with-rest-parameter 1 :two 333)))

(defun function-with-keyword-parameters (&key (a :something) b c)
  ;; A keyword parameters is similar to an optional parameter, but is provided
  ;; by a keyword-value pair.
  (list a b c))

(define-test keyword-parameters ()
  (assert-equal ____ (function-with-keyword-parameters))
  (assert-equal ____ (function-with-keyword-parameters :a 11 :b 22 :c 33))
  ;; It is not necessary to specify all keyword parameters.
  (assert-equal ____ (function-with-keyword-parameters :b 22))
  ;; Keyword argument order is not important.
  (assert-equal ____ (function-with-keyword-parameters :b 22 :c -5/2 :a 0))
  ;; Lisp handles duplicate keyword parameters.
  (assert-equal ____ (function-with-keyword-parameters :b 22 :b 40 :b 812)))

(defun function-with-keyword-indication
    (&key (a 2 a-provided-p) (b 3 b-provided-p))
  ;; It is possible to check whether a keyword argument was provided.
  (list a a-provided-p b b-provided-p))

(define-test keyword-indication
  (assert-equal ____ (function-with-keyword-indication))
  (assert-equal ____ (function-with-keyword-indication :a 3 :b 4))
  (assert-equal ____ (function-with-keyword-indication :a 11 :b 22))
  (assert-equal ____ (function-with-keyword-indication :b 22))
  (assert-equal ____ (function-with-keyword-indication :b 22 :a 0)))

(defun function-with-funky-parameters (a &rest x &key b (c a c-provided-p))
  ;; Lisp functions can have surprisingly complex lambda lists.
  ;; A &rest parameter must come before &key parameters.
  (list a b c c-provided-p x))

(define-test funky-parameters
  (assert-equal ____ (function-with-funky-parameters 1))
  (assert-equal ____ (function-with-funky-parameters 1 :b 2))
  (assert-equal ____ (function-with-funky-parameters 1 :b 2 :c 3))
  (assert-equal ____ (function-with-funky-parameters 1 :c 3 :b 2)))

(define-test lambda
  ;; A list form starting with the symbol LAMBDA denotes an anonymous function.
  ;; It is possible to call that function immediately or to store it for later
  ;; use.
  (let ((my-function (lambda (a b) (* a b))))
    (assert-equal ____ (funcall my-function 11 9)))
  ;; A LAMBDA form is allowed to take the place of a function name.
  (assert-equal ____ ((lambda (a b) (+ a b)) 10 9))
  (let ((functions (list (lambda (a b) (+ a b))
                         (lambda (a b) (- a b))
                         (lambda (a b) (* a b))
                         (lambda (a b) (/ a b)))))
    (assert-equal ____ (funcall (first functions) 2 33))
    (assert-equal ____ (funcall (second functions) 2 33))
    (assert-equal ____ (funcall (third functions) 2 33))
    (assert-equal ____ (funcall (fourth functions) 2 33))))

(define-test lambda-with-optional-parameters
  (assert-equal ____ ((lambda (a &optional (b 100)) (+ a b)) 10 9))
  (assert-equal ____ ((lambda (a &optional (b 100)) (+ a b)) 10)))

(defun make-adder (x)
  ;; MAKE-ADDER will create a function that closes over the parameter X.
  ;; The parameter will be remembered as a part of the environment of the
  ;; returned function, which will continue refering to it.
  (lambda (y) (+ x y)))

(define-test lexical-closures
  (let ((adder-100 (make-adder 100))
        (adder-500 (make-adder 500)))
    ;; ADD-100 and ADD-500 now close over different values.
    (assert-equal ____ (funcall adder-100 3))
    (assert-equal ____ (funcall adder-500 3))))

(defun make-reader-and-writer (x)
  ;; Both returned functions will refer to the same place.
  (list (function (lambda () x))
        (function (lambda (y) (setq x y)))))

(define-test lexical-closure-interactions
  ;; The macro DESTRUCTURING-BIND is like LET, except it binds the variables
  ;; listed in its first argument to the parts of the list returned by the form
  ;; that is its second argument.
  (destructuring-bind (reader-1 writer-1) (make-reader-and-writer 1)
    (destructuring-bind (reader-2 writer-2) (make-reader-and-writer :one)
      (assert-equal ____ (funcall reader-1))
      (funcall writer-1 0)
      (assert-equal ____ (funcall reader-1))
      ;; The two different function pairs refer to different places.
      (assert-equal ____ (funcall reader-2))
      (funcall writer-2 :zero)
      (assert-equal ____ (funcall reader-2)))))

(define-test apply
  ;; The function APPLY applies a function to a list of arguments.
  (let ((function (lambda (x y z) (+ x y z))))
    (assert-equal ____ (apply function '(100 20 3))))
  ;; FUNCTION is a special operator that retrieves function objects, defined
  ;; both globally and locally. #'X is syntax sugar for (FUNCTION X).
  (assert-equal ____ (apply (function +) '(1 2)))
  (assert-equal ____ (apply #'- '(1 2)))
  ;; Only the last argument to APPLY must be a list.
  (assert-equal ____ (apply #'+ 1 2 '(3)))
  (assert-equal ____ (apply #'max 1 2 3 4 '())))

(define-test funcall
  ;; The function FUNCALL calls a function with arguments, not expecting a final
  ;; list of arguments.
  (let ((function (lambda (x y z) (+ x y z))))
    (assert-equal ____ (funcall function 300 20 1)))
  (assert-equal ____ (funcall (function +) 1 2))
  (assert-equal ____ (funcall #'- 1 2))
  (assert-equal ____ (funcall #'+ 1 2 3))
  (assert-equal ____ (funcall #'max 1 2 3 4)))
