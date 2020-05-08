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

(define-test setf
  ;; SETF is a macro used to assign values to places. A place is a concept;
  ;; it is an abstract "somewhere" where a value is stored.
  (let ((a 10)
        (b (list 1 20 30 40 50))
        ;; We use COPY-SEQ to create a copy of a string, because using SETF to
        ;; modify literal data (strings, lists, etc.) is undefined behaviour.
        (c (copy-seq "I am Tom.")))
    ;; A place may be a variable.
    (setf a 1000)
    (assert-equal 1000 a)
    ;; A place may be a part of some list.
    (setf (first b) 10)
    (assert-equal '(10 20 30 40 50) b)
    ;; A place may be a character in a string.
    ;; The #\x syntax denotes a single character, 'x'.
    (setf (char c 5) #\B
          (char c 7) #\b)
    (assert-equal "I am Bob." c)
    ;; There are other kinds of places that we will explore in the future.
    ))

(define-test case
  ;; CASE is a simple pattern-matching macro, not unlike C's "switch".
  ;; It compares an input against a set of values and evaluates the code for
  ;; the branch where a match is found.
  (let* ((a 4)
         (b (case a
              (3 :three)
              (4 :four)
              (5 :five))))
    (assert-equal :four b))
  ;; CASE can accept a group of keys.
  (let* ((c 4)
         (d (case c
              ((0 2 4 6 8) :even-digit)
              ((1 3 5 7 9) :odd-digit))))
    (assert-equal :even-digit d)))

(defun match-special-cases (thing)
  ;; T or OTHERWISE passed as the key matches any value.
  ;; NIL passed as the key matches no values.
  ;; These symbols need to passed in parentheses.
  (case thing
    ((t) :found-a-t)
    ((nil) :found-a-nil)
    (t :something-else)))

(define-test special-cases-of-case
  ;; You need to fill in the blanks in MATCH-SPECIAL-CASES.
  (assert-equal :found-a-t (match-special-cases t))
  (assert-equal :found-a-nil (match-special-cases nil))
  (assert-equal :something-else (match-special-cases 42)))

(define-test your-own-case-statement
  ;; We use FLET to define a local function.
  (flet ((cartoon-dads (input)
           (case input
             ;; Fill in the blanks with proper cases.
             (:bart :homer)
             (:stewie :peter)
             (:stan :randy)
             (:this-one-doesnt-happen :fancy-cat)
             (t :unknown))))
    (assert-equal (cartoon-dads :bart) :homer)
    (assert-equal (cartoon-dads :stewie) :peter)
    (assert-equal (cartoon-dads :stan) :randy)
    (assert-equal (cartoon-dads :space-ghost) :unknown)))

(define-test limits-of-case
  ;; So far, we have been comparing objects using EQUAL, one of the Lisp
  ;; comparison functions. CASE compares the keys using EQL, which is distinct
  ;; from EQUAL.
  ;; EQL is suitable for comparing numbers, characters, and objects for whom we
  ;; want to check verify they are the same object.
  (let* ((string "A string")
         (string-copy (copy-seq string)))
    ;; The above means that two distinct strings will not be the same under EQL,
    ;; even if they have the same contents.
    (true-or-false? nil (eql string string-copy))
    (true-or-false? t (equal string string-copy))
    ;; The above also means that CASE might give surprising results when used on
    ;; strings.
    (let ((match (case string
                   ("A string" :matched)
                   (t :not-matched))))
      (assert-equal :not-matched match))
    ;; We will explore this topic further in the EQUALITY-DISTINCTIONS lesson.
    ))

(define-test cond
  ;; COND is similar to CASE, except it is more general. It accepts arbitrary
  ;; conditions and checks them in order until one of them is met.
  (let* ((number 4)
         (result (cond ((> number 0) :positive)
                       ((< number 0) :negative)
                       (t :zero))))
    (assert-equal :positive result)))
