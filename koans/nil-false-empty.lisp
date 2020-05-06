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

(define-test t-and-nil-are-opposites
  ;; NOT is a function which returns the boolean opposite of its argument.
  (true-or-false? ____ (not nil))
  (true-or-false? ____ (not t)))

(define-test nil-and-empty-list-are-the-same-thing
  ;; In Common Lisp, NIL is also the empty list.
  (true-or-false? ____ '())
  (true-or-false? ____ (not '())))

(define-test in-lisp-many-things-are-true
  ;; In Common Lisp, the canonical values for truth is T.
  ;; However, everything that is non-NIL is true, too.
  (true-or-false? ____ 5)
  (true-or-false? ____ (not 5))
  (true-or-false? ____ "a string")
  ;; Even an empty string...
  (true-or-false? ____ "")
  ;; ...or a list containing a NIL...
  (true-or-false? ____ (list nil))
  ;; ...or an array with no elements...
  (true-or-false? ____ (make-array 0))
  ;; ...or the number zero.
  (true-or-false? ____ 0))

(define-test and
  ;; The logical operator AND can take multiple arguments.
  (true-or-false? ____ (and t t t t t))
  (true-or-false? ____ (and t t nil t t))
  ;; If all values passed to AND are true, it returns the last value.
  (assert-equal ____ (and t t t t t 5)))

(define-test or
  ;; The logical operator OR can also take multiple arguments.
  (true-or-false? ____  (or nil nil nil t nil))
  ;; OR returns the first non-NIL value it encounters, or NIL if there are none.
  (assert-equal ____ (or nil nil nil))
  (assert-equal ____ (or 1 2 3 4 5)))
