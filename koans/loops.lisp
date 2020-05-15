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

;;; The extended for of LOOP allows for advanced iteration.
;;; See http://www.gigamonkeys.com/book/loop-for-black-belts.html

(define-test loop-collect
  ;; LOOP can collect the results in various ways.
  (let* ((result-1 (loop for letter in '(#\a #\b #\c #\d) collect letter))
         (result-2 (loop for number in '(1 2 3 4 5) sum number))
         (result-3 (loop for list in '((foo) (bar) (baz)) append list)))
    (assert-equal ____ result-1)
    (assert-equal ____ result-2)
    (assert-equal ____ result-3)))

(define-test loop-multiple-variables
  ;; With multiple FOR clauses, the loop ends when any of the provided lists are
  ;; exhausted.
  (let* ((letters '(:a :b :c :d))
         (result (loop for letter in letters
                       for i from 1 to 1000
                       collect (list i letter))))
    (assert-equal ____ result)))

(define-test loop-in-versus-loop-on
  ;; Instead of iterating over each element of a list, we can iterate over each
  ;; cons cell of a list.
  (let* ((letters '(:a :b :c))
         (result-in (loop for thing in letters collect thing))
         (result-on (loop for thing on letters collect thing)))
    (assert-equal ____ result-in)
    (assert-equal ____ result-on)))

(define-test loop-for-by
  ;; Numeric iteration can go faster or slower if we use the BY keyword.
  (let* ((result (loop for i from 0 to 30 by 5 collect i)))
    (assert-equal ____ result)))

(define-test loop-counting-backwards
  ;; We can count downwards instead of upwards by using DOWNTO instead of TO.
  (let ((result (loop for i from 5 downto -5 collect i)))
    (assert-equal ____ result)))

(define-test loop-list-by
  ;; List iteration can go faster or slower if we use the BY keyword.
  (let* ((letters '(:a :b :c :d :e :f))
         (result (loop for letter in letters collect letter))
         (result-cdr (loop for letter in letters by #'cdr collect letter))
         (result-cddr (loop for letter in letters by #'cddr collect letter))
         (result-cdddr (loop for letter in letters by #'cdddr collect letter)))
    (assert-equal ____ result)
    (assert-equal ____ result-cdr)
    (assert-equal ____ result-cddr)
    (assert-equal ____ result-cdddr)))

(define-test loop-across
  ;; LOOP can iterate over a vector with the ACROSS keyword.
  (let* ((vector (make-array '(5) :initial-contents '(0 1 2 3 4)))
         (result (loop for number across vector collect number)))
    (assert-equal ____ result)))

(define-test loop-over-2d-array
  (let ((array (make-array '(3 2) :initial-contents '((0 1) (2 3) (4 5)))))
    ;; LOOP can be combined with ROW-MAJOR-AREF to iterate over the contents of
    ;; a multidimensional array.
    (let* ((result (loop for i from 0 below (array-total-size array)
                         collect (row-major-aref array i))))
      (assert-equal ____ result))
    ;; It is always possible to resort to nested loops.
    (let* ((result (loop with max-i = (array-dimension array 0)
                         for i from 0 below max-i
                         collect (loop with max-j = (array-dimension array 1)
                                       for j from 0 below max-j
                                       collect (expt (aref array i j) 2)))))
      (assert-equal ____ result))))

(define-test loop-hash-table
  (let ((book-heroes (make-hash-table :test 'equal)))
    (setf (gethash "The Hobbit" book-heroes) "Bilbo"
          (gethash "Where The Wild Things Are" book-heroes) "Max"
          (gethash "The Wizard Of Oz" book-heroes) "Dorothy"
          (gethash "The Great Gatsby" book-heroes) "James Gatz")
    ;; LOOP can iterate over hash tables.
    (let ((pairs-in-table (loop for key being the hash-keys of book-heroes
                                  using (hash-value value)
                                collect (list key value))))
      (assert-equal ____ (length pairs-in-table))
      (true-or-false? ____ (find '("The Hobbit" "Bilbo") pairs-in-table
                                 :test #'equal)))))

(define-test loop-statistics
  ;; LOOP can perform basics statistics on the collected elements.
  (let ((result (loop for x in '(1 2 4 8 16 32)
                      collect x into collected
                      count x into counted
                      sum x into summed
                      maximize x into maximized
                      minimize x into minimized
                      finally (return (list collected counted summed
                                            maximized minimized)))))
    (destructuring-bind (collected counted summed maximized minimized) result
      (assert-equal ____ collected)
      (assert-equal ____ counted)
      (assert-equal ____ summed)
      (assert-equal ____ maximized)
      (assert-equal ____ minimized))))

(define-test loop-destructuring
  ;; LOOP can bind multiple variables on each iteration step.
  (let* ((count 0)
         (result (loop for (a b) in '((1 9) (2 8) (3 7) (4 6))
                       do (incf count)
                       collect (+ a b))))
    (assert-equal ____ count)
    (assert-equal ____ result)))

(define-test loop-conditional-execution
  (let ((numbers '(1 1 2 3 5 8 13 21)))
    ;; LOOP can execute some actions conditionally.
    (let ((result (loop for x in numbers
                        when (evenp x) sum x)))
      (assert-equal ____ result))
    (let ((result (loop for x in numbers
                        unless (evenp x) sum x)))
      (assert-equal ____ result))
    (flet ((greater-than-10-p (x) (> x 10)))
      (let ((result (loop for x in numbers
                          when (greater-than-10-p x) sum x)))
        (assert-equal ____ result)))))
