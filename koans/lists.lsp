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


;; based on python koans 'about_lists.py'
;; based also on "Lisp 3rd Edition" ch. 17. "List storage, surgery and reclamation"


(define-test test-creating-lists
    "lists can be created using the quote form, or the 'list' function"
  (let ((fruits nil)
        (some-evens nil))
    (setf fruits '(orange pomello clementine))
    (setf some-evens (list (* 2 1) (* 2 2) (* 2 3)))
    (assert-equal fruits '(orange pomello clementine))
    (assert-equal 3 (length some-evens))))


(define-test test-list-cons
    "cons CONStructs new lists, by prefixing some list with
     a new element like (cons new-element some-list)"
    (let ((nums nil))
      (setf nums (cons :one nums))
      (assert-equal '(:one) nums)

      (setf nums (cons :two nums))
      (assert-equal '(:two :one) nums)

      "lists can contain anything, even mixtures of different things"
      (setf nums (cons 333 nums))
      (assert-equal '(333 :two :one) nums)

      "lists can of course contain lists"
      (setf nums (cons '("the" "rest") nums))
      (assert-equal '(("the" "rest") 333 :two :one) nums)))


(define-test test-push-pop
    (let ((stack '(10 20 30 40))
          (firstval nil))
      "push adds an element to the beginning of a list referred to by some symbol"
      (push "last" stack)
      (assert-equal '("last" 10 20 30 40) stack)

       "pop is the opposite of push.
       It removes and returns the first element of a list"
      (setf firstval (pop stack))
      (assert-equal "last" firstval)
      (assert-equal '(10 20 30 40) stack)

      (setf firstval (pop stack))
      (assert-equal 10 firstval)
      (assert-equal '(20 30 40) stack)))


(define-test test-append
    "append attaches one list to the end of another."
  (assert-equal '(:a :b :c) (append '(:a :b) '(:c)))

  (let ((abc '(:a :b :c))
        (xyz '(:x :y :z))
        (abcxyz nil))
    (setf abcxyz (append abc xyz))
    (assert-equal '(:a :b :c) abc)
    (assert-equal '(:x :y :z) xyz)
    (assert-equal '(:a :b :c :x :y :z) abcxyz)))


(define-test test-accessing-list-elements
    (let ((noms '("peanut" "butter" "and" "jelly")))
      (assert-equal "peanut" (first noms))
      (assert-equal "butter" (second noms))
      (assert-equal "jelly" (fourth noms))
      "last returns a singleton list of the final element"
      (assert-equal '("jelly") (last noms))
      (assert-equal "butter" (nth 1 noms)) ; k 1
      (assert-equal "peanut" (nth 0 noms))
      (assert-equal "and" (nth 2 noms))
      "'elt' is similar to 'nth', with the arguments reversed"
      (assert-equal "and" (elt noms 2))))


(define-test test-slicing-lists
    (let ((noms '("peanut" "butter" "and" "jelly")))
      (assert-equal '("peanut") (subseq noms 0 1))
      (assert-equal '("peanut" "butter") (subseq noms 0 2))
      (assert-equal nil (subseq noms 2 2))
      (assert-equal '("and" "jelly") (subseq noms 2))))


(define-test test-list-breakdown
    "car (aka. 'first') returns the first value in a list"
  (assert-equal 1 (car '(1 2 3)))
  (assert-equal nil (car nil))
    "cdr (aka. 'rest') refers to the remainder of the list,
     after the first element"
  (assert-equal '(2 3) (cdr '(1 2 3)))
  (assert-equal nil (cdr nil)))
