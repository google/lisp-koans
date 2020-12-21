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

(define-test let
  ;; The LET form establishes a lexical extent within which new variables are
  ;; created: a symbol that names a variable becomes bound to a value.
  (let ((x 10)
        (y 20))
    (assert-equal ____ (+ x y))
    ;; It is possible to shadow previously visible bindings.
    (let ((y 30))
      (assert-equal ____ (+ x y)))
    (assert-equal ____ (+ x y)))
  ;; Variables bound by LET have a default value of NIL.
  (let (x)
    (assert-equal ____ x)))

(define-test let-versus-let*
  ;; LET* is similar to LET, except the bindings are established sequentially,
  ;; and a binding may use bindings that were established before it.
  (let ((x 10)
        (y 20))
    (let ((x (+ y 100))
          (y (+ x 100)))
      (assert-equal ____ x)
      (assert-equal ____ y))
    (let* ((x (+ y 100))
           (y (+ x 100)))
      ;; Which X is used to compute the value of Y?
      (assert-equal ____ x)
      (assert-equal ____ y))))

(define-test let-it-be-equal
  ;; Fill in the LET and LET* to get the tests to pass.
  (let ((a 1)
        (b :two)
        (c "Three"))
    (let ((____ ____)
          (____ ____)
          (____ ____))
      (assert-equal a 100)
      (assert-equal b 200)
      (assert-equal c "Jellyfish"))
    (let* ((____ ____)
           (____ ____)
           ;; In this third binding, you are allowed to use the variables bound
           ;; by the previous two LET* bindings.
           (____ ____))
      (assert-equal a 121)
      (assert-equal b 200)
      (assert-equal c (+ a (/ b a))))))
