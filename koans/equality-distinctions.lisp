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

;; the most common equality predicates are eq, eql, equal and equalp
;; eq is similar to comparing c pointers
(define-test test-eq
    "(eq x y) is true if and only if x and y are the same identical object
     eq is like comparing pointers in c.  If the values are EQ, any non-nil
     value may be returned."
  (true-or-false? ___ (eq 'a 'a))
  (true-or-false? ___ (eq 3 3.0))
  (true-or-false? ___ (eq '(1 2) '(1 2)))
  (true-or-false? ___ (eq "Foo" "Foo"))
  (true-or-false? ___ (eq "Foo" (copy-seq "Foo")))
  (true-or-false? ___ (eq "FOO" "Foo")))

(define-test test-eql
    "(eql x y) is true if (eq x y)
     also it is true if x and y are numeric of the same type
     and represent the same number.
     (eql x y) also if x and y are the same characters."
   (true-or-false? ___ (eql 'a 'a))
   (true-or-false? ___ (eql 3 3))
   (true-or-false? ___ (eql 3 3.0))
   (true-or-false? ___ (eql '(1 2) '(1 2)))
   (true-or-false? ___ (eql  '(:a . :b) '(:a . :b)))
   (true-or-false? ___ (eql #\S #\S))
   (true-or-false? ___ (eql "Foo" "Foo"))
   (true-or-false? ___ (eql "Foo" (copy-seq "Foo")))
   (true-or-false? ___ (eql "FOO" "Foo")))

(define-test test-equal
    "(equal x y) is true if (eql x y), or
     x and y are lists with equal elements, or
     x and y character or bit arrays with equal elements"
   (true-or-false? ___ (equal 'a 'a))
   (true-or-false? ___ (equal 3 3))
   (true-or-false? ___ (equal 3 3.0))
   (true-or-false? ___ (equal '(1 2) '(1 2)))
   (true-or-false? ___ (equal  '(:a . :b) '(:a . :b)))
   (true-or-false? ___ (equal  '(:a . :b) '(:a . :doesnt-match)))
   (true-or-false? ___ (equal #\S #\S))
   (true-or-false? ___ (equal "Foo" "Foo"))
   (true-or-false? ___ (equal "Foo" (copy-seq "Foo")))
   (true-or-false? ___ (equal "FOO" "Foo")))

(define-test test-equalp
    "(equalp x y) if (equal x y) or
     if x and y are strings with the same characters (case independent).
     if x and y are arrays with the same dimensions and equal elements
     if x and y are numeric of different types but one may be upgraded to
     the other type without loss and still exhibit equality."
   (true-or-false? ___ (equalp 'a 'a))
   (true-or-false? ___ (equalp 3 3))
   (true-or-false? ___ (equalp 3 3.0))
   (true-or-false? ___ (equalp '(1 2) '(1 2)))
   (true-or-false? ___ (equalp  '(:a . :b) '(:a . :b)))
   (true-or-false? ___ (equalp  '(:a . :b) '(:a . :doesnt-match)))
   (true-or-false? ___ (equalp #\S #\S))
   (true-or-false? ___ (equalp "Foo" "Foo"))
   (true-or-false? ___ (equalp "Foo" (copy-seq "Foo")))
   (true-or-false? ___ (equalp "FOO" "Foo")))

(define-test test-numeric-equal
    "(= x y) is only for numerics
     and can take multiple arguments
     if x or y is not numeric there will be a compiler error."
   (true-or-false? ___ (= 99.0 99 99.000))
   (true-or-false? ___ (= 0 1 -1))
   (true-or-false? ___ (= (/ 2 3) (/ 6 9) (/ 86 129))))

; EQ, EQL, EQUAL, and EQUALP are general equality predicates.
; Additionally, Lisp also provides the type-specific predicates.
; For example, STRING= and STRING-EQUAL are predicates for strings.
(define-test test-string-equal
  "string-equal is just like string= except that differences in case are ignored."
  (true-or-false? ___ (string= "Foo" "Foo"))
  (true-or-false? ___ (string= "Foo" "FOO"))
  (true-or-false? ___ (string= "together" "frog" :start1 1 :end1 3 :start2 2))
  (true-or-false? ___ (string-equal "Foo" "FOO"))
  (true-or-false? ___ (string-equal "together" "FROG" :start1 1 :end1 3 :start2 2)))
