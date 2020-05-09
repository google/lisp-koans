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

;;; A hash table data structure is sometimes known as a dictionary.

(define-test make-hash-table
  (let ((my-hash-table (make-hash-table)))
    (true-or-false? t (typep my-hash-table 'hash-table))
    (true-or-false? t (hash-table-p my-hash-table))
    (true-or-false? nil (hash-table-p (make-array '(3 3 3))))
    ;; The function HASH-TABLE-COUNT returns the number of entries currently
    ;; contained in a hash table.
    (assert-equal 0 (hash-table-count my-hash-table))))

(define-test gethash
  ;; The function GETHASH can be used to access hash table values.
  (let ((cube-roots (make-hash-table)))
    ;; We add the key-value pair 1 - "uno" to the hash table.
    (setf (gethash 1 cube-roots) "uno")
    (assert-equal "uno" (gethash 1 cube-roots))
    (assert-equal 1 (hash-table-count cube-roots))
    (setf (gethash 8 cube-roots) 2)
    (setf (gethash -3 cube-roots) -27)
    (assert-equal -27 (gethash -3 cube-roots))
    (assert-equal 3 (hash-table-count cube-roots))
    ;; GETHASH returns a secondary value that is true if the key was found in
    ;; the hash-table and false otherwise.
    (multiple-value-bind (value foundp) (gethash 8 cube-roots)
      (assert-equal 2 value)
      (assert-equal t foundp))
    (multiple-value-bind (value foundp) (gethash 125 cube-roots)
      (assert-equal nil value)
      (assert-equal nil foundp))))

(define-test hash-table-test
  ;; A hash table can be constructed with different test predicates.
  ;; The programmer may choose between EQ, EQL, EQUAL, and EQUALP to get the
  ;; best performance and expected results from the hash table.
  ;; The default test predicate is EQL.
  (let ((eq-table (make-hash-table :test #'eq))
        (eql-table (make-hash-table))
        (equal-table (make-hash-table :test #'equal))
        (equalp-table (make-hash-table :test #'equalp)))
    ;; We will define four variables whose values are strings.
    (let* ((string "one")
           (same-string string)
           (string-copy (copy-seq string))
           (string-upcased "ONE"))
      ;; We will insert the value of each variable into each hash table.
      (dolist (thing (list string same-string string-copy string-upcased))
        (dolist (hash-table (list eq-table eql-table equal-table equalp-table))
          (setf (gethash thing hash-table) t))))
    ;; How many entries does each hash table contain?
    (assert-equal 3 (hash-table-count eq-table))
    (assert-equal 3 (hash-table-count eql-table))
    (assert-equal 2 (hash-table-count equal-table))
    (assert-equal 1 (hash-table-count equalp-table))))

(define-test hash-table-equality
  ;; EQUALP considers two hash tables to be equal if they have the same test and
  ;; if its key-value pairs are the same under that test.
  (let ((hash-table-1 (make-hash-table :test #'equal))
        (hash-table-2 (make-hash-table :test #'equal)))
    (setf (gethash "one" hash-table-1) "yat")
    (setf (gethash "one" hash-table-2) "yat")
    (setf (gethash "two" hash-table-1) "yi")
    (setf (gethash "two" hash-table-2) "yi")
    (true-or-false? nil (eq hash-table-1 hash-table-2))
    (true-or-false? nil (equal hash-table-1 hash-table-2))
    (true-or-false? t (equalp hash-table-1 hash-table-2))))

(define-test i-will-make-it-equalp
  ;; Disabled on ECL due to a conformance bug.
  ;; See https://gitlab.com/embeddable-common-lisp/ecl/-/issues/587
  #-ecl
  (let ((hash-table-1 (make-hash-table :test #'equal))
        (hash-table-2 (make-hash-table :test #'equal)))
    (setf (gethash "one" hash-table-1) "uno"
          (gethash "two" hash-table-1) "dos")
    (setf (gethash "one" hash-table-2) "eins"
          (gethash "two" hash-table-2) "zwei")
    (assert-false (equalp hash-table-1 hash-table-2))
    ;; Change the first hash table to be EQUALP to the second one.
    (setf (gethash "one" hash-table-1) "eins"
          (gethash "two" hash-table-1) "zwei")
    (assert-true (equalp hash-table-1 hash-table-2))))

(define-test make-your-own-hash-table
  ;; Make your own hash table that satisfies the test.
  (let ((colors (make-hash-table :test #'equal)))
    ;; You will need to modify your hash table after you create it.
    (setf (gethash "blue" colors) '(0 0 1)
          (gethash "green" colors) '(0 1 0)
          (gethash "red" colors) '(1 0 0)
          (gethash "black" colors) '(0 0 0))
    (assert-equal (hash-table-count colors) 4)
    (let ((values (list (gethash "blue" colors)
                        (gethash "green" colors)
                        (gethash "red" colors))))
      (assert-equal values '((0 0 1) (0 1 0) (1 0 0))))))
