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
  (true-or-false? t (not nil))
  (true-or-false? nil (not t)))

(define-test nil-and-empty-list-are-the-same-thing
  ;; In Common Lisp, NIL is also the empty list.
  (true-or-false? nil '())
  (true-or-false? t (not '())))

(define-test in-lisp-many-things-are-true
  ;; In Common Lisp, the canonical values for truth is T.
  ;; However, everything that is non-NIL is true, too.
  (true-or-false? t 5)
  (true-or-false? nil (not 5))
  (true-or-false? t "a string")
  ;; Even an empty string...
  (true-or-false? t "")
  ;; ...or a list containing a NIL...
  (true-or-false? t (list nil))
  ;; ...or an array with no elements...
  (true-or-false? t (make-array 0))
  ;; ...or the number zero.
  (true-or-false? t 0))

(define-test and
  ;; The logical operator AND can take multiple arguments.
  (true-or-false? t (and t t t t t))
  (true-or-false? nil (and t t nil t t))
  ;; If all values passed to AND are true, it returns the last value.
  (assert-equal 5 (and t t t t t 5)))

(define-test or
  ;; The logical operator OR can also take multiple arguments.
  (true-or-false? t  (or nil nil nil t nil))
  ;; OR returns the first non-NIL value it encounters, or NIL if there are none.
  (assert-equal nil (or nil nil nil))
  (assert-equal 1 (or 1 2 3 4 5)))
