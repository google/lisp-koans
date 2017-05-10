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


  ; Special forms are evaluatable lisp forms (LISTs) which are
  ; neither FUNCTIONs nor MACROs.  Here is an introduction to a
  ; few of them.

;; Based on http://psg.com/~dlamkins/sl/chapter03-03.html

(defvar my-name)
(defvar my-clones-name)
(defvar a)
(defvar b)
(defvar c 0)

(define-test test-setf
    "SETF is used to assign values to symbols.  These symbols may refer to
     variables with lexical or dynamic scope."
  (setf my-name "David")
  (assert-equal my-name ____)
    "In SBCL, if the symbol isn't defined as a variable, via a top-level DEFVAR
     or LET statement, the SETF call may result in a warning."
  (setf my-clones-name my-name)
  (assert-equal "David" ____)
  (setf a 5)
  (setf b 10)
  (setf c ___)
  (assert-equal 50 c))


(define-test test-let
    "The LET form establishes a lexical extent, within which explicit symbols
     may be bound to values.  The binding only extends over the extent of the
     lexical form.  After which, the previous value, if it exists, is visible again."
  (setf a 10)
  (setf b 20)
  (assert-equal a ___)
  (assert-equal b ___)
  (let ((a 1111)
        (b 2222))
    (assert-equal a ___)
    (assert-equal b ___))
  (assert-equal a ___)
  (assert-equal b ___))


(define-test test-let-default-value
    "LET vars have a default value."
  (let ((x))
    (assert-equal ___ x)))

(define-test test-let-bindings-are-parallel
    "When defining the bindings in the LET form, later bindings may not depend
     on earlier ones."
  (setf a 100)
  (let ((a 5)
        (b (* 10 a)))
    (assert-equal b ___)))

(define-test test-let*-bindings-are-series
    "LET* is like LET, but successive bindings may use values of previous ones."
  (setf a 100)
  (let* ((a 5)
         (b (* 10 a)))
    (assert-equal b ___))
  (assert-equal a ___))


(define-test write-your-own-let-statement
    "Fix the LET statement to get the tests to pass."
  (setf a 100)
  (setf b 23)
  (setf c 456)
  (let ((a __)
        (b __)
        (c __))
    (assert-equal a 100)
    (assert-equal b 200)
    (assert-equal c "Jellyfish"))
  (let* ((a __)
         ;; Add more here
         )
    (assert-equal a 121)
    (assert-equal b 200)
    (assert-equal c (+ a (/ b a)))))

(define-test test-case
    "The CASE form is like the c switch statement: it
     compares an input with a set of values and evaluates an
     expression once a match is found."
  (setf a 4)
  (setf b
        (case a (4 :four)
                (5 :five)
                  ; T specifies default behavior
                (t :unknown)))
  (assert-equal ____ b)
    "CASE can also check if a list of values contains
     the input."
  (setf c
        (case a (5 :five)
                ((3 4) :three-or-four)))
  (assert-equal ____ c))

(defun cartoon-dads (input)
    "You should be able to complete this CASE statement."
  (case input (:this-one-doesnt-happen :fancy-cat)
              (t :unknown)))

(define-test test-your-own-case-statement
    "Fix this by completing the CARTOON-DADS function above."
  (assert-equal (cartoon-dads :bart) :homer)
  (assert-equal (cartoon-dads :stewie) :peter)
  (assert-equal (cartoon-dads :stan) :randy)
  (assert-equal (cartoon-dads :space-ghost) :unknown))

(define-test test-limits-of-case
    "CASE is not suitable for all kinds of values, because
     it uses the function EQL for comparisons. We will explore
     the implications of this in the equality-distinctions lesson."
  (let* ((name "John")
         (lastname (case name ("John" "Doe")
                              ("Max" "Mustermann")
                              (t "Anonymous"))))
  (assert-equal ____ lastname)))

(define-test test-cond
    "COND is the general purpose form for checking multiple
     conditions, until a condition is met."
  (setf a 4)
  (setf c
        (cond ((> a 0) :positive)
              ((< a 0) :negative)
              (t :zero)))
  (assert-equal ____ c))
