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

;; see http://www.gigamonkeys.com/book/loop-for-black-belts.html
;; "Loop for blackbelts" for more on the loop macro.

(define-test test-basic-loop
    (let* ((letters '(:a :b :c :d))
           (loop-result
             (loop for letter in letters
                   collect letter)))
      (assert-equal loop-result ____)))


(define-test test-compound-loop
    (let* ((letters '(:a :b :c :d))
           (loop-result
             (loop for letter in letters
                   for i from 1 to 1000
                   collect (list i letter))))
      (assert-equal loop-result ____)))


(define-test test-counting-loop-skip-by-syntax
   "with multiple 'for' clauses, loop ends when the first is exhausted"
    (let* ((letters '(:a :b :c :d))
           (loop-result
             (loop for letter in letters
                   for i from 0 to 1000 by 5
                   collect (list i letter))))
      (assert-equal loop-result ____ )))


(define-test test-counting-backwards
    (let ((loop-result
             (loop for i from 10 downto -10 by 5
                   collect i )))
      (assert-equal loop-result ____ )))


(define-test test-loop-in-vs-loop-on
    (let* ((letters '(:a :b :c))
           (loop-result-in
            (loop for letter in letters collect letter))
           (loop-result-on
            (loop for letter on letters collect letter)))
      (assert-equal loop-result-in ____)
      (assert-equal loop-result-on ____ )))


(define-test test-loop-in-skip-by
    (let* ((letters '(:a :b :c :d :e :f))
           (loop-result-in
            (loop for letter in letters collect letter))
           (loop-result-in-cdr
            (loop for letter in letters by #'cdr collect letter))
           (loop-result-in-cddr
            (loop for letter in letters by #'cddr collect letter))
           (loop-result-in-cdddr
            (loop for letter in letters by #'cdddr collect letter)))
      (assert-equal loop-result-in ____)
      (assert-equal loop-result-in-cdr ____)
      (assert-equal loop-result-in-cddr ____)
      (assert-equal loop-result-in-cdddr ____)))


(define-test test-loop-across-vector
    (let* ((my-vector (make-array '(5) :initial-contents '(0 1 2 3 4)))
           (loop-result
            (loop for val across my-vector collect val)))
      (assert-equal ____ loop-result)))


(define-test test-loop-across-2d-array
    (let* ((my-array (make-array '(3 3) :initial-contents '((0 1 2) (3 4 5) (6 7 8))))
           (loop-result
            (loop for i from 0 below (array-total-size my-array) collect (row-major-aref my-array i))))
      (assert-equal loop-result ____ )))


(define-test test-loop-across-2d-array-respecting-shape
    (let* ((my-array (make-array '(3 2) :initial-contents '((0 1) (2 3) (4 5))))
           (loop-result
            (loop for i from 0 below (array-dimension my-array 0) collect
              (loop for j from 0 below (array-dimension my-array 1) collect
                (expt (aref my-array i j) 2)))))
      (assert-equal loop-result ____ )))


(defvar books-to-heros)
(setf books-to-heros (make-hash-table :test 'equal))
(setf (gethash "The Hobbit" books-to-heros) "Bilbo")
(setf (gethash "Where The Wild Things Are" books-to-heros) "Max")
(setf (gethash "The Wizard Of Oz" books-to-heros) "Dorothy")
(setf (gethash "The Great Gatsby" books-to-heros) "James Gatz")


(define-test test-loop-over-hash-tables
    (let* ((pairs-in-table
            (loop for k being the hash-keys in books-to-heros
                  using (hash-value v)
                  collect (list k v))))
      (assert-equal ____ (length pairs-in-table))
      (true-or-false? ____ (find '("The Hobbit" "Bilbo") pairs-in-table :test #'equal))))


(define-test test-value-accumulation-forms
    (let ((loop-1
           (loop for x in '(1 2 4 8 16)
                 collect x into collected
                   count x into counted
                 sum x into summed
                 maximize x into maximized
                 minimize x into minimized
                 finally (return (list collected counted summed maximized minimized)))))
      (destructuring-bind (col count sum max min) loop-1
        (assert-equal col ____)
        (assert-equal count ____)
        (assert-equal sum ____)
        (assert-equal max ____)
        (assert-equal min ____))))


(define-test test-destructuring-bind
    (let* ((count 0)
           (result (loop for (a b) in '((1 9) (2 8) (3 7) (4 6))
                        do (setf count (+ 1 count))
                         collect (+ a b))))
      (assert-equal ____ count)
      (assert-equal ____ result)))


(define-test test-conditional-execution
    (let ((loop-return
           (loop for x in '(1 1 2 3 5 8 13)
                 when (evenp x) sum x)))
      (assert-equal loop-return ____)))


(defun greater-than-10-p (x)
  (> x 10))

(define-test test-conditional-with-defun
    (let ((loop-return
           (loop for x in '(1 1 2 3 5 8 13)
                 when (greater-than-10-p x) sum x)))
      (assert-equal loop-return ____)))


(define-test test-conditional-with-lambda
    (let ((loop-return
           (loop for x in '(1 1 2 3 5 8 13)
                 when ((lambda (z) (equal 1 (mod z 3))) x) sum x)))
      (assert-equal loop-return ____)))