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

(define-test test-t-and-nil-are-opposites
    "not is a function which returns the boolean opposite of its argument"
   (true-or-false? t (not nil))
   (true-or-false? nil (not t)))


(define-test test-nil-and-empty-list-are-the-same-thing
  (true-or-false? nil ())
  (true-or-false? t (not ())))


(define-test test-lots-of-things-are-true
   " every value, other than nil, is boolean true"
   (true-or-false? t 5)
   (true-or-false? nil (not 5))
   (true-or-false? t "A String")
   "only nil is nil.  Everything else is effectively true."
   "the empty string"
   (true-or-false? t "")
   "a list containing a nil"
   (true-or-false? t '(nil))
   "an array with no elements"
   (true-or-false? t (make-array '(0)))
   "the number zero"
   (true-or-false? t 0))


(define-test test-and
   "and can take multiple arguments"
   (true-or-false? t (and t t t t t))
   (true-or-false? nil (and t t nil t t))
   "if no nils, and returns the last value"
   (assert-equal 5 (and t t t t t 5)))


(define-test test-or
   "or can also take multiple arguments"
   (true-or-false? t  (or nil nil nil t nil))
   "or returns the first non nil value, or nil if there are none."
   (assert-equal nil (or nil nil nil))
   (assert-equal 1 (or 1 2 3 4 5)))
