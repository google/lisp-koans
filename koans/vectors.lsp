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

"vectors are just like rank 1 arrays"

(define-test test-vector-types
  " #(x y z) defines a vector literal containing x y z"
  (true-or-false? ___ (typep #(1 11 111) 'vector))
  (assert-equal ___ (aref #(1 11 111) 1)))


(define-test test-length-works-on-vectors
  (assert-equal (length #(1 2 3)) ___ ))


(define-test test-bit-vector
    "#*0011 defines a bit vector literal with four elements, 0, 0, 1 and 1"
  (assert-equal #*0011 (make-array '4 :element-type 'bit))
  (true-or-false? ____ (typep #*1001 'bit-vector))
  (assert-equal ____ (aref #*1001 1)))


(define-test test-some-bitwise-operations
    (assert-equal ___ (bit-and #*1100 #*1010))
    (assert-equal ___ (bit-ior #*1100 #*1010))
    (assert-equal ___ (bit-xor #*1100 #*1010)))


(defun list-to-bit-vector (my-list)
  nil)

(define-test test-list-to-bit-vector
    "you must complete list-to-bit-vector"
  (assert-true (typep (list-to-bit-vector '(0 0 1 1 0)) 'bit-vector))
  (assert-equal (aref (list-to-bit-vector '(0)) 0) 0)
  (assert-equal (aref (list-to-bit-vector '(0 1)) 1) 1)
  (assert-equal (length (list-to-bit-vector '(0 0 1 1 0 0 1 1))) 8))


