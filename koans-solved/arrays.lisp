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

(define-test basic-array-stuff
  ;; We make an 8x8 array and then fill it with a checkerboard pattern.
  (let ((chess-board (make-array '(8 8))))
    ;; (DOTIMES (X 8) ...) will iterate with X taking values from 0 to 7.
    (dotimes (x 8)
      (dotimes (y 8)
        ;; AREF stands for "array reference".
        (setf (aref chess-board x y) (if (evenp (+ x y)) :black :white))))
    (assert-true (typep chess-board 'array))
    (assert-equal :black (aref chess-board 0 0))
    (assert-equal :white (aref chess-board 2 3))
    ;; The function ARRAY-RANK returns the number of dimensions of the array.
    (assert-equal 2 (array-rank chess-board))
    ;; The function ARRAY-DIMENSIONS returns a list of the cardinality of the
    ;; array dimensions.
    (assert-equal '(8 8) (array-dimensions chess-board))
    ;; ARRAY-TOTAL-SIZE returns the total number of elements in the array.
    (assert-equal 64 (array-total-size chess-board))))

(define-test make-your-own-array
  ;; Make your own array that satisfies the test.
  (let ((color-cube (make-array '(3 3 3))))
    ;; You may need to modify your array after you create it.
    (setf (aref color-cube 0 1 2) :red
          (aref color-cube 2 1 0) :white)
    (if (typep color-cube '(simple-array T (3 3 3)))
        (progn
          (assert-equal 3 (array-rank color-cube))
          (assert-equal '(3 3 3) (array-dimensions color-cube))
          (assert-equal 27 (array-total-size color-cube))
          (assert-equal (aref color-cube 0 1 2) :red)
          (assert-equal (aref color-cube 2 1 0) :white))
        (assert-true nil))))

(define-test adjustable-array
  ;; The size of an array does not need to be constant.
  (let ((x (make-array '(2 2) :initial-element 5 :adjustable t)))
    (assert-equal 5 (aref x 1 0))
    (assert-equal '(2 2) (array-dimensions x))
    (adjust-array x '(3 4))
    (assert-equal '(3 4) (array-dimensions x))))

(define-test make-array-from-list
  ;; One can create arrays with initial contents.
  (let ((x (make-array '(4) :initial-contents '(:one :two :three :four))))
    (assert-equal '(4) (array-dimensions x))
    (assert-equal :one (aref x 0))))

(define-test row-major-index
  ;; Row major indexing is a way to access elements with a single integer,
  ;; rather than a list of integers.
  (let ((my-array (make-array '(2 2 2 2))))
    (dotimes (i (* 2 2 2 2))
      (setf (row-major-aref my-array i) i))
    (assert-equal 0 (aref my-array 0 0 0 0))
    (assert-equal 2 (aref my-array 0 0 1 0))
    (assert-equal 4 (aref my-array 0 1 0 0))
    (assert-equal 15 (aref my-array 1 1 1 1))))
