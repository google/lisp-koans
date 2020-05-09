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

;;; In Lisp, it is possible for a function to return more than one value.
;;; This is distinct from returning a list or structure of values.

(define-test multiple-values
  (let ((x (floor 3/2))
        ;; The macro MULTIPLE-VALUE-LIST returns a list of all values returned
        ;; by a Lisp form.
        (y (multiple-value-list (floor 3/2))))
    (assert-equal x 1)
    (assert-equal y '(1 1/2)))
  (assert-equal '(24 3/4) (multiple-value-list (floor 99/4))))

(defun next-fib (a b)
  ;; The function VALUES allows returning multiple values.
  (values b (+ a b)))

(define-test binding-and-setting-multiple-values
  ;; The macro MULTIPLE-VALUE-BIND is like LET, except it binds the variables
  ;; listed in its first argument to the values returned by the form that is its
  ;; second argument.
  (multiple-value-bind (x y) (next-fib 3 5)
    (let ((result (* x y)))
      (assert-equal 40 result)))
  ;; SETF can also set multiple values if a VALUES form is provided as a place.
  (let (x y)
    (setf (values x y) (next-fib 5 8))
    (assert-equal '(8 13) (list x y))))
