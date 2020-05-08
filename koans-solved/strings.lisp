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

(define-test what-is-a-string
  (let ((string "Do, or do not. There is no try."))
    (true-or-false? t (typep string 'string))
    ;; Strings are vectors of characters.
    (true-or-false? t (typep string 'array))
    (true-or-false? t (typep string 'vector))
    (true-or-false? t (typep string '(vector character)))
    (true-or-false? nil (typep string 'integer))))

(define-test multiline-string
  ;; A Lisp string can span multiple lines.
  (let ((string "this is
                 a multi
                 line string"))
    (true-or-false? t (typep string 'string))))

(define-test escapes-in-strings
  ;; Quotes and backslashes in Lisp strings must be escaped.
  (let ((my-string "this string has one of these \" and a \\ in it"))
    (true-or-false? t (typep my-string 'string))))

(define-test substrings
  ;; Since strings are sequences, it is possible to use SUBSEQ on them.
  (let ((string "Lorem ipsum dolor sit amet"))
    (assert-equal "dolor sit amet" (subseq string 12))
    (assert-equal "ipsum" (subseq string 6 11))
    (assert-equal "orem" (subseq string 1 5))))

(define-test strings-versus-characters
  ;; Strings and characters have distinct types.
  (true-or-false? t (typep #\a 'character))
  (true-or-false? nil (typep "A" 'character))
  (true-or-false? nil (typep #\a 'string))
  ;; One can use both AREF and CHAR to refer to characters in a string.
  (let ((my-string "Cookie Monster"))
    (assert-equal #\C (char my-string 0))
    (assert-equal #\k (char my-string 3))
    (assert-equal #\M (aref my-string 7))))

(define-test concatenating-strings
  ;; Concatenating strings in Common Lisp is possible, if a little cumbersome.
  (let ((a "Lorem")
        (b "ipsum")
        (c "dolor"))
    (assert-equal "Lorem ipsum dolor" (concatenate 'string a " " b " " c))))

(define-test searching-for-characters
  ;; The function POSITION can be used to find the first position of an element
  ;; in a sequence. If the element is not found, NIL is returned.
  (assert-equal 1 (position #\b "abc"))
  (assert-equal 2 (position #\c "abc"))
  (assert-equal nil (position #\d "abc")))

(define-test finding-substrings
  ;; The function SEARCH can be used to search a sequence for subsequences.
  (let ((title "A supposedly fun thing I'll never do again"))
    (assert-equal 2 (search "supposedly" title))
    (assert-equal 12 (search " fun" title))))

