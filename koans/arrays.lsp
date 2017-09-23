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


;; see http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node157.html


(define-test test-basic-array-stuff
    " the first block of code defines an 8x8 array, then fills
      the elements with a checkerboard pattern"
  (let ((chess-board))
    (setf chess-board (make-array '(8 8)))
    "this dotimes is an iterator which loops x over integers 0 to 7"
    (dotimes (x 8)
      (dotimes (y 8)
        (if (evenp (+ x y))
            (setf (aref chess-board x y) :black)
            (setf (aref chess-board x y) :white)
            )))
    (assert-true (typep chess-board 'array))
    (assert-equal (aref chess-board 0 0) :black)
    (assert-equal (aref chess-board 2 3) :white)
    "array-rank returns the number of dimensions of the array"
    (assert-equal 2 (array-rank chess-board))
    "array-dimensions returns a list of the cardinality of the array dims"
    (assert-equal '(8 8) (array-dimensions chess-board))
    (assert-equal 64 (array-total-size chess-board))))

(define-test test-make-your-own-array
    "make your own array that meets the specifications below."
  (let ((color-cube nil))
    "you may need to modify your array after you make it"
    (setf color-cube (make-array '(3 3 3)))
    (dotimes (x 3)
      (dotimes (y 3)
        (dotimes (z 3)
          (cond ((= x 0) (setf (aref color-cube x y z) :red))
                ((= x 1) (setf (aref color-cube x y z) :blue))
                ((= x 2) (setf (aref color-cube x y z) :white))))))
    (if (typep color-cube '(simple-array T (3 3 3)))
          (progn
            (assert-equal 3 (array-rank color-cube))
            (assert-equal '(3 3 3) (array-dimensions color-cube))
            (assert-equal 27 (array-total-size color-cube))
            (assert-equal (aref color-cube 0 1 2) :red)
            (assert-equal (aref color-cube 2 1 0) :white))
        (assert-true nil))))


(define-test test-adjustable-array
    "one may build arrays that can change size"
  (let ((x (make-array '(2 2) :initial-element 5 :adjustable t)))
    (assert-equal (aref x 1 0) 5)
    (assert-equal (array-dimensions x) '(2 2))
    (adjust-array x '(3 4))
    (assert-equal (array-dimensions x) '(3 4))))


(define-test test-make-array-from-list
  (let ((x))
    (setf x (make-array '(4) :initial-contents '(:one :two :three :four)))
    (assert-equal (array-dimensions x) '(4))
    (assert-equal :one (aref x 0))))


(define-test test-row-major-index
    "row major indexing is a way to access elements with a single integer,
     rather than a list of integers"
  (let ((my-array nil))
    (setf my-array (make-array '(2 2 2 2)))
    (dotimes (i (* 2 2 2 2))
      (setf (row-major-aref my-array i) i))
    (assert-equal (aref my-array 0 0 0 0) 0)
    (assert-equal (aref my-array 1 1 1 1) 15)))
