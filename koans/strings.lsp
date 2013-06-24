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

(define-test test-double-quoted-strings-are-strings
    (let ((my-string "do or do not"))
      (true-or-false? t (typep my-string 'string))
      "strings are the same thing as vectors of characters"
      (true-or-false? t (typep my-string 'array))
      (assert-equal (aref "meat" 2) (aref "fiesta" 5))
      "strings are not integers :p"
      (true-or-false? nil (typep my-string 'integer))))


(define-test test-multi-line-strings-are-strings
    (let ((my-string "this is
                      a multi
                      line string"))
      (true-or-false? t (typep my-string 'string))))


(define-test test-escape-quotes
    (let ((my-string "this string has one of these \" in it"))
      (true-or-false? t (typep my-string 'string))))


; This test from common lisp cookbook
(define-test test-substrings
    "since strings are sequences, you may use subseq"
  (let ((my-string "Groucho Marx"))
    (assert-equal "Marx" (subseq my-string 8))
    (assert-equal (subseq my-string 0 7) "Groucho")
    (assert-equal (subseq my-string 1 5) "rouc")))

(define-test test-accessing-individual-characters
  "char literals look like this"
  (true-or-false? t (typep #\a 'character))
  (true-or-false? nil (typep "A" 'character))
  (true-or-false? nil (typep #\a 'string))
  "char is used to access individual characters"
  (let ((my-string "Cookie Monster"))
    (assert-equal (char my-string 0) #\C)
    (assert-equal (char my-string 3) #\k)
    (assert-equal (char my-string 7) #\M)))


(define-test test-concatenating-strings
    "concatenating strings in lisp is a little cumbersome"
  (let ((a "this")
        (b "is")
        (c "unwieldy"))
    (assert-equal "this is unwieldy" (concatenate 'string a " " b " " c))))


(define-test test-searching-for-characters
    "you can use position to detect characters in strings
     (or elements of sequences)"
  (assert-equal 1 (position #\b "abc"))
  (assert-equal 2 (position #\c "abc"))
  (assert-equal nil (find #\d "abc")))


(define-test test-finding-substrings
    "search finds subsequences"
  (let ((title "A supposedly fun thing I'll never do again"))
    (assert-equal 2 (search "supposedly" title))
    (assert-equal 12 (search " fun" title))))

