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

;;; Lists in lisp are forms beginning and ending with rounded parentheses.
;;; Atoms are symbols, numbers, or other forms usually separated by whitespace
;;; or parentheses.

(define-test list-or-atom
  ;; The function LISTP will return true if the input is a list.
  ;; The function ATOM will return true if the input is an atom.
  (true-or-false? ____ (listp '(1 2 3)))
  (true-or-false? ____ (atom '(1 2 3)))
  (true-or-false? ____ (listp '("heres" "some" "strings")))
  (true-or-false? ____ (atom '("heres" "some" "strings")))
  (true-or-false? ____ (listp "a string"))
  (true-or-false? ____ (atom "a string"))
  (true-or-false? ____ (listp 2))
  (true-or-false? ____ (atom 2))
  (true-or-false? ____ (listp '(("first" "list") ("second" "list"))))
  (true-or-false? ____ (atom '(("first" "list") ("second" "list")))))

(define-test the-duality-of-nil
  ;; The empty list, NIL, is unique in that it is both a list and an atom.
  (true-or-false? ____ (listp nil))
  (true-or-false? ____ (atom nil)))

(define-test keywords
  ;; Symbols like :HELLO or :LIKE-THIS are keywords. They are treated
  ;; differently in Lisp: they are constants that always evaluate to themselves.
  (true-or-false? ____ (equal :this-is-a-keyword :this-is-a-keyword))
  (true-or-false? ____ (equal :this-is-a-keyword ':this-is-a-keyword))
  (true-or-false? ____ (equal :this-is-a-keyword :this-is-also-a-keyword)))
