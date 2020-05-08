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

;;; Vectors are one-dimensional arrays. This means that general array operations
;;; will work on vectors normally. However, Lisp also defines some functions for
;;; operating on sequences - which means, either vectors or lists.

(define-test vector-basics
  ;; #(...) is syntax sugar for defining literal vectors.
  (let ((vector #(1 11 111)))
    (true-or-false? t (typep vector 'vector))
    (assert-equal 11 (aref vector 1))))

(define-test length
  ;; The function LENGTH works both for vectors and for lists.
  (assert-equal 3 (length '(1 2 3)))
  (assert-equal 3 (length #(1 2 3))))

(define-test bit-vector
  ;; #*0011 defines a bit vector literal with four elements: 0, 0, 1 and 1.
  (assert-equal #*0011 (make-array 4 :element-type 'bit
                                     :initial-contents '(0 0 1 1)))
  (true-or-false? t (typep #*1001 'bit-vector))
  (assert-equal 0 (aref #*1001 1)))

(define-test bitwise-operations
  ;; Lisp defines a few bitwise operations that work on bit vectors.
  (assert-equal #*1000 (bit-and #*1100 #*1010))
  (assert-equal #*1110 (bit-ior #*1100 #*1010))
  (assert-equal #*0110 (bit-xor #*1100 #*1010)))

(defun list-to-bit-vector (list)
  ;; Implement a function that turns a list into a bit vector.
  (coerce list 'bit-vector))

(define-test list-to-bit-vector
  ;; You need to fill in the blank in LIST-TO-BIT-VECTOR.
  (assert-true (typep (list-to-bit-vector '(0 0 1 1 0)) 'bit-vector))
  (assert-equal (aref (list-to-bit-vector '(0)) 0) 0)
  (assert-equal (aref (list-to-bit-vector '(0 1)) 1) 1)
  (assert-equal (length (list-to-bit-vector '(0 0 1 1 0 0 1 1))) 8))


