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


(define-test test-list-or-atom
    "Lists in lisp are forms beginning and ending with rounded parentheses.
     Atoms are symbols, numbers, or other forms usually separated by
     white-space or parentheses.  The function 'listp' will return true if
     the input is a list.  The function 'atom' will return true if the
     input is an atom."
  (true-or-false? t (listp '(1 2 3)))
  (true-or-false? nil (atom '(1 2 3)))

  (true-or-false? t (listp '("heres" "some" "strings")))
  (true-or-false? nil (atom '("heres" "some" "strings")))

  (true-or-false? nil (listp "a string"))
  (true-or-false? t (atom "a string"))

  (true-or-false? nil (listp 2))
  (true-or-false? t (atom 2))

  (true-or-false? t (listp '(("first" "list") ("second" "list"))))
  (true-or-false? nil (atom '(("first" "list") ("second" "list")))))


(define-test test-empty-list-is-both-list-and-atom
    "the empty list, nil, is unique in that it is both a list and an atom"
  (true-or-false? t (listp nil))
  (true-or-false? t (atom nil)))


(define-test test-keywords
    "symbols like :hello or :like-this are treated differently in lisp.
     Called keywords, they are symbols that evaluate to themselves."
  (true-or-false? t (equal :this-is-a-keyword :this-is-a-keyword))
  (true-or-false? t (equal :this-is-a-keyword ':this-is-a-keyword)))
