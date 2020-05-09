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

;;; The most common equality predicates in Common Lisp are, in order of
;;; strictness, EQ, EQL, EQUAL, and EQUALP.

(define-test eq
  ;; EQ checks the identity of the two objects; it checks whether the two
  ;; objects are, in fact, one and the same object.
  ;; It is the fastest of the four; however, not guaranteed to work on numbers
  ;; and characters because of that.
  (true-or-false? ____ (eq 'a 'a))
  (true-or-false? ____ (eq 3 3.0))
  (true-or-false? ____ (eq '(1 2) '(1 2)))
  (true-or-false? ____ (eq "Foo" "Foo"))
  (true-or-false? ____ (eq "Foo" (copy-seq "Foo")))
  (true-or-false? ____ (eq "FOO" "Foo")))

(define-test eql
  ;; EQL works like EQ, except it is specified to work for numbers and
  ;; characters.
  ;; Two numbers are EQL if they are of the same type and represent the same
  ;; number. Two characters are EQL if they represent the same character.
  (true-or-false? ____ (eql 'a 'a))
  (true-or-false? ____ (eql 3 3))
  (true-or-false? ____ (eql 3 3.0))
  (true-or-false? ____ (eql '(1 2) '(1 2)))
  (true-or-false? ____ (eql  '(:a . :b) '(:a . :b)))
  (true-or-false? ____ (eql #\S #\S))
  (true-or-false? ____ (eql "Foo" "Foo"))
  (true-or-false? ____ (eql "Foo" (copy-seq "Foo")))
  (true-or-false? ____ (eql "FOO" "Foo")))

(define-test equal
  ;; EQUAL works like EQL, except works differently for lists, strings, bit
  ;; vectors, and pathnames.
  ;; Two lists, strings, bit arrays, or pathnames are EQUAL if they have EQUAL
  ;; elements.
  (true-or-false? ____ (equal 'a 'a))
  (true-or-false? ____ (equal 3 3))
  (true-or-false? ____ (equal 3 3.0))
  (true-or-false? ____ (equal '(1 2) '(1 2)))
  (true-or-false? ____ (equal  '(:a . :b) '(:a . :b)))
  (true-or-false? ____ (equal  '(:a . :b) '(:a . :doesnt-match)))
  (true-or-false? ____ (equal #\S #\S))
  (true-or-false? ____ (equal "Foo" "Foo"))
  (true-or-false? ____ (equal #*01010101 #*01010101))
  (true-or-false? ____ (equal "Foo" (copy-seq "Foo")))
  (true-or-false? ____ (equal "FOO" "Foo"))
  (true-or-false? ____ (equal #p"foo/bar/baz" #p"foo/bar/baz")))

(defstruct thing slot-1 slot-2)

(define-test equalp
  ;; EQUALP works like EQUAL, except it works differently for characters,
  ;; numbers, arrays, structures, and hash tables.
  ;; Two characters are EQUALP if they represent the same character, ignoring
  ;; the differences in character case.
  ;; Two numbers are EQUALP if they represent the same number, even if they are
  ;; of different types.
  ;; Two arrays are EQUALP if they have the same dimensions and their characters
  ;; are pairwise EQUALP.
  ;; Two structures are EQUALP if they are of the same class and their slots are
  ;; pairwise EQUALP.
  ;; We will contemplate hash tables in the HASH-TABLES lesson.
  (true-or-false? ____ (equalp 'a 'a))
  (true-or-false? ____ (equalp 3 3))
  (true-or-false? ____ (equalp 3 3.0))
  (true-or-false? ____ (equalp '(1 2) '(1 2)))
  (true-or-false? ____ (equalp  '(:a . :b) '(:a . :b)))
  (true-or-false? ____ (equalp  '(:a . :b) '(:a . :doesnt-match)))
  (true-or-false? ____ (equalp #\S #\S))
  (true-or-false? ____ (equalp "Foo" "Foo"))
  (true-or-false? ____ (equalp "Foo" (copy-seq "Foo")))
  (true-or-false? ____ (equalp "FOO" "Foo"))
  (true-or-false? ____ (equalp (make-array '(4 2) :initial-element 0)
                               (make-array '(4 2) :initial-element 0)))
  (true-or-false? ____ (equalp (make-thing :slot-1 42 :slot-2 :forty-two)
                               (make-thing :slot-1 42 :slot-2 :forty-two))))

;;; In additional to the generic equality predicates, Lisp also provides
;;; type-specific predicates for numbers, strings, and characters.

(define-test =
  ;; The function = behaves just like EQUALP on numbers.
  ;; #C(... ...) is syntax sugar for creating a complex number.
  (true-or-false? ____ (= 99.0 99 99.000 #C(99 0) #C(99.0 0.0)))
  (true-or-false? ____ (= 0 1 -1))
  (true-or-false? ____ (= (/ 2 3) (/ 6 9) (/ 86 129))))

(define-test string=
  ;; The function STRING= behaves just like EQUAL on strings.
  ;; The function STRING-EQUAL behaves just like EQUALP on strings.
  (true-or-false? ____ (string= "Foo" "Foo"))
  (true-or-false? ____ (string= "Foo" "FOO"))
  (true-or-false? ____ (string-equal "Foo" "FOO"))
  ;; These functions accept additional keyword arguments that allow one to
  ;; only compare parts of the strings.
  (true-or-false? ____ (string= "together" "frog" :start1 1 :end1 3
                                                  :start2 2))
  (true-or-false? ____ (string-equal "together" "FROG" :start1 1 :end1 3
                                                       :start2 2)))

(define-test char=
  ;; The function CHAR= behaves just like EQL on characters.
  ;; The function CHAR-EQUAL behaves just like EQUALP on characters.
  (true-or-false? ____ (char= #\A (char "ABCDEF" 0)))
  (true-or-false? ____ (char= #\A #\a))
  (true-or-false? ____ (char-equal #\A (char "ABCDEF" 0)))
  (true-or-false? ____ (char-equal #\A #\a)))
