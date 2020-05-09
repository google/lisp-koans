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

;;; Backquote notation is similar to quoting, except it allows for parts of the
;;; resulting expression to be "unquoted".

(define-test backquote-basics
  (let ((x '(123))
        (z '(7 8 9)))
    ;; ' quotes an expression normally.
    (assert-equal ____ '(x 45 6 z))
    ;; ` backquotes an expression; without any unquotes, it is equivalent to
    ;; using the normal quote.
    (assert-equal ____ `(x 45 6 z))
    ;; , unquotes a part of the expression.
    (assert-equal ____ `(,x 45 6 z))
    (assert-equal ____ `(,x 45 6 ,z))
    ;; ,@ splices an expression into the into the list surrounding it.
    (assert-equal ____ `(,x 45 6 ,@z))
    (assert-equal ____ `(,@x 45 6 ,@z))))

(define-test backquote-forms
  ;; Because of its properties, backquote is useful for constructing Lisp forms
  ;; that are macroexpansions or parts of macroexpansions.
  (let ((variable 'x))
    ;; Fill in the blank without without using backquote/unquote notation.
    (assert-equal ____
                  `(if (typep ,variable 'string)
                       (format nil "The value of ~A is ~A" ',variable ,variable)
                       (error 'type-error :datum ,variable
                                          :expected-type 'string))))
  (let ((error-type 'type-error)
        (error-arguments '(:datum x :expected-type 'string)))
    ;; Fill in the blank without without using backquote/unquote notation.
    (assert-equal ____
                  `(if (typep x 'string)
                       (format nil "The value of ~A is ~A" 'x x)
                       (error ',error-type ,@error-arguments)))))

(define-test numbers-and-words
  (let ((number 5)
        (word 'dolphin))
    (true-or-false? ____ (equal '(1 3 5) `(1 3 5)))
    (true-or-false? ____ (equal '(1 3 5) `(1 3 number)))
    (assert-equal ____ `(1 3 ,number))
    (assert-equal _____ `(word ,word ,word word))))

(define-test splicing
  (let ((axis '(x y z)))
    (assert-equal '(the axis are ____) `(the axis are ,axis))
    (assert-equal '(the axis are ____) `(the axis are ,@axis)))
  (let ((coordinates '((43.15 77.6) (42.36 71.06))))
    (assert-equal ____ `(the coordinates are ,coordinates))
    (assert-equal ____ `(the coordinates are ,@coordinates))))
