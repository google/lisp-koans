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
   (true-or-false? ___ (not nil))
   (true-or-false? ___ (not t)))


(define-test test-nil-and-empty-list-are-the-same-thing
  (true-or-false? ___ ())
  (true-or-false? ___ (not ())))


(define-test test-lots-of-things-are-true
   " every value, other than nil, is boolean true"
   (true-or-false? ___ 5)
   (true-or-false? ___ (not 5))
   (true-or-false? ___ "A String")
   "only nil is nil.  Everything else is effectively true."
   "the empty string"
   (true-or-false? ___ "")
   "a list containing a nil"
   (true-or-false? ___ '(nil))
   "an array with no elements"
   (true-or-false? ___ (make-array '(0)))
   "the number zero"
   (true-or-false? ___ 0))


(define-test test-and
   "and can take multiple arguments"
   (true-or-false? ___ (and t t t t t))
   (true-or-false? ___ (and t t nil t t))
   "if no nils, and returns the last value"
   (assert-equal ___ (and t t t t t 5)))


(define-test test-or
   "or can also take multiple arguments"
   (true-or-false? ____  (or nil nil nil t nil))
   "or returns the first non nil value, or nil if there are none."
   (assert-equal ____ (or nil nil nil))
   (assert-equal ____ (or 1 2 3 4 5)))