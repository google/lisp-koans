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


; borrows from about_methods.py

(defun some-named-function (a b)
  (+ a b))

(define-test test-call-a-function
    "DEFUN defines global functions"
  (assert-equal ___ (some-named-function 7 11)))


(define-test test-shadow-a-function
    "Local functions are defined with FLET or LABELS.  One major difference
     between the two is that local functions defined with LABELS may refer
     to themselves, whereas local functions defined with FLET may not."
   (assert-eq 18 (some-named-function 7 11))
   "flet binds a function to a name within a lexical environment"
   (flet ((some-named-function (a b) (* a b)))
     (assert-equal ___ (some-named-function 7 11)))
   (assert-equal ___  (some-named-function 7 11)))


; borrowed from Common Lisp The Language chapter 5.2.2
(defun func-with-opt-params (&optional (a 2) (b 3) )
  ; each optional parameter has a form like (var default-val)
  (list a b))

(define-test test-optional-parameters
    "Optional parameters are filled in with their default value."
   (assert-equal (func-with-opt-params :test-1 :test-2) ___)
   (assert-equal (func-with-opt-params :test-1) ___)
   (assert-equal (func-with-opt-params) ___))


;; ----


(defun func-with-opt-params-and-indication (&optional (a 2 a?) (b 3 b?))
  (list a a? b b?))

(define-test test-optional-parameters-with-indication
   "Common Lisp optional params may bind a symbol which indicate whether the
    value was provided or defaulted.  Each optional parameter binding has the
    form (var default-form supplied-p)."
   (assert-equal (func-with-opt-params-and-indication :test-1 :test-2) ___)
   (assert-equal (func-with-opt-params-and-indication :test-1) ___)
   (assert-equal (func-with-opt-params-and-indication) ___))


;; ----


(defun func-with-rest-params (&rest x)
  x)

(define-test test-func-with-rest-params
  "With &rest, the remaining params, are handed in as a list.  Remaining
   arguments (possibly none) are collected into a list."
  (assert-equal (func-with-rest-params) ___)
  (assert-equal (func-with-rest-params 1) ___)
   (assert-equal (func-with-rest-params 1 :two 333) ___))


;; ----


(defun func-with-key-params (&key a b)
  (list a b))

(define-test test-key-params ()
  "Key params allow the user to specify params in any order"
   (assert-equal (func-with-key-params) ___)
   (assert-equal (func-with-key-params :a 11 :b 22) ___)
   ; it is not necessary to specify all key parameters
   (assert-equal (func-with-key-params :b 22) ___)
   ; order is not important
   (assert-equal (func-with-key-params :b 22 :a 0) ___))

(defun func-key-params-can-have-defaults (&key  (a 3 a?) (b 4 b?))
  (list a a? b b?))

(define-test test-key-params-can-have-defaults
    "key parameters can have defaults also"
   (assert-equal (func-key-params-can-have-defaults) ____)
   (assert-equal (func-key-params-can-have-defaults :a 3 :b 4) ___)
   (assert-equal (func-key-params-can-have-defaults :a 11 :b 22) ___)
   (assert-equal (func-key-params-can-have-defaults :b 22) ___)
   ; order is not important
   (assert-equal (func-key-params-can-have-defaults :b 22 :a 0) ___))


;; ----


;; borrowed from common lisp the language 5.2.2
(defun func-with-funky-parameters (a &rest x &key b (c a))
   (list a b c x))

(define-test test-many-kinds-params
    "CL provides the programmer with more than enough rope to hang himself."
   (assert-equal (func-with-funky-parameters 1) ___)
   (assert-equal (func-with-funky-parameters 1 :b 2) ___)
   (assert-equal (func-with-funky-parameters 1 :b 2 :c 3) ___)
   (assert-equal (func-with-funky-parameters 1 :c 3 :b 2) ___))


;; Note that &rest parameters have to come before &key parameters.
;; This is an error: (defun f (&key a &rest x) () )
;; But this is ok:   (defun f (&rest x &key a) () )


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
(defun sign-of (x)
  (if (< x 0) (return-from sign-of -1))
  (if (eq x 0) (return-from sign-of 0))
  1)

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
