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
    (assert-equal ____ (different-named-function 4 5))))

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
  (assert-equal ____ (func-with-key-params :b 22))
  ;; Keyword argument order is not important.
  (assert-equal ____ (func-with-key-params :b 22 :c -5/2 :a 0))
  ;; Lisp handles duplicate keyword parameters.
  (assert-equal ____ (func-with-key-params :b 22 :b 40 :b 812)))

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

(define-test test-many-kinds-params
  (assert-equal (func-with-funky-parameters 1) ___)
  (assert-equal (func-with-funky-parameters 1 :b 2) ___)
  (assert-equal (func-with-funky-parameters 1 :b 2 :c 3) ___)
  (assert-equal (func-with-funky-parameters 1 :c 3 :b 2) ___))

(define-test test-lambdas-are-nameless-functions
  "A lambda form defines a function, but with no name.  It is possible
     to execute that function immediately, or put it somewhere for later use."
  (assert-equal 19 ((lambda (a b) (+ a b)) 10 9))
  (let ((my-function))
    (setf my-function (lambda (a b) (* a b)))
    (assert-equal ___ (funcall my-function 11 9)))
  (let ((list-of-functions nil))
    (push (lambda (a b) (+ a b)) list-of-functions)
    (push (lambda (a b) (* a b)) list-of-functions)
    (push (lambda (a b) (- a b)) list-of-functions)
    (assert-equal ___ (funcall (second list-of-functions) 2 33))))

(define-test test-lambdas-can-have-optional-params
  (assert-equal ___ ((lambda (a &optional (b 100)) (+ a b)) 10 9))
  (assert-equal ___ ((lambda (a &optional (b 100)) (+ a b)) 10)))


                                        ; returns sign x
(defun sign-of (x) (if (< x 0) (return-from sign-of -1)) (if (eq x 0) (return-from sign-of 0)) 1)

(define-test test-return-from-function-early
  (assert-equal (sign-of -5.5) ___)
  (assert-equal (sign-of 0) ___)
  (assert-equal (sign-of ___) 1))


;; ----


;; Lambdas create "lexical closures", meaning that the resulting function, when
;; called, will execute in an environment wherein the lexical bindings to all
;; referred to names still apply.
;; This example from "Common Lisp The Language" Ch. 7

(defun adder (x)
  "The result of (adder n) is a nameless function with one parameter.
  This function will add n to its argument."
  (lambda (y) (+ x y)))

(define-test test-lexical-closure-over-adder ()
  (let ((add-100 (adder 100))
        (add-500 (adder 500)))
    "add-100 and add-500 now refer to different bindings to x"
    (assert-equal ___ (funcall add-100 3))
    (assert-equal ___ (funcall add-500 3))))


;; ----


;; The closure gives the returned function access to the bindings, not just the
;; values.  This means that two functions which close over the same variables
;; will always see the same values of those variables if one does a setq.

(defun two-funs (x)
  "Returns a list of two functions.
   The first takes no parameters and returns x.
   The second takes one parameter, y, and resets x to the value of y."
  (list (function (lambda () x))
        (function (lambda (y) (setq x y)))))

(define-test test-lexical-closure-interactions
  "An illustration of how lexical closures may interact."
  (let ((tangled-funs-1 (two-funs 1))
        (tangled-funs-2 (two-funs 2)))
    (assert-equal (funcall (first tangled-funs-1)) ___)
    (funcall (second tangled-funs-1) 0)
    (assert-equal (funcall (first tangled-funs-1)) ___)

    (assert-equal (funcall (first tangled-funs-2)) ___)
    (funcall (second tangled-funs-2) 100)
    (assert-equal (funcall (first tangled-funs-2)) ___)))


(define-test test-apply-function-with-apply
  "APPLY calls the function parameter on a list of all the remaining
   parameters"
  (let (f1 f2 f3)
    (setq f1 '+)
    (setq f2 '-)
    (setq f3 'max)

    (assert-equal ___ (apply f1 '(1 2)))
    (assert-equal ___ (apply f2 '(1 2)))

                                        ; after the function name, the parameters are consed onto the front
                                        ; of the very last parameter
    (assert-equal ___ (apply f1 1 2 '(3)))
    (assert-equal ___ (apply f3 1 2 3 4 '()))))


(define-test test-apply-function-with-funcall
  "FUNCALL calls the function parameter on a list of all the remaining
   parameters.  Remaining params do not expect a final list."
  (let (f1 f2 f3)
    (setq f1 '+)
    (setq f2 '-)
    (setq f3 'max)
    (assert-equal ___ (funcall f1 1 2))
    (assert-equal ___ (funcall f2 1 2))
    (assert-equal ___ (funcall f1 1 2 3))
    (assert-equal ___ (funcall f3 1 2 3 4))))
