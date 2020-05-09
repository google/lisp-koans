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

;;; Lisp supports several functional alternatives to imperative iteration.

(define-test mapcar
  (let ((numbers '(1 2 3 4 5 6)))
    ;; Inside MAPCAR, he function 1+ will be applied to each element of NUMBERS.
    ;; A new list will be collected from the results.
    (assert-equal '(2 3 4 5 6 7) (mapcar #'1+ numbers))
    (assert-equal '(-1 -2 -3 -4 -5 -6) (mapcar #'- numbers))
    (assert-equal '((1) (2) (3) (4) (5) (6)) (mapcar #'list numbers))
    (assert-equal '(nil t nil t nil t) (mapcar #'evenp numbers))
    (assert-equal '(t t t t t t) (mapcar #'numberp numbers))
    (assert-equal '(nil nil nil nil nil nil) (mapcar #'stringp numbers))
    ;; MAPCAR can work on multiple lists. The function will receive one argument
    ;; from each list.
    (let ((other-numbers '(4 8 15 16 23 42)))
      (assert-equal '(5 10 18 20 28 48) (mapcar #'+ numbers other-numbers))
      (assert-equal '(4 16 45 64 115 252) (mapcar #'* numbers other-numbers))
      ;; The function MOD performs modulo division.
      (assert-equal '(0 0 0 0 3 0) (mapcar #'mod other-numbers numbers)))))

(define-test mapcar-lambda
  ;; MAPCAR is often used with anonymous functions.
  (let ((numbers '(8 21 152 37 403 14 7 -34)))
    (assert-equal '(8 1 2 7 3 4 7 6) (mapcar (lambda (x) (mod x 10)) numbers)))
  (let ((strings '("Mary had a little lamb"
                   "Old McDonald had a farm"
                   "Happy birthday to you")))
    (assert-equal '(" had a l" "McDonald" "y birthd")
                  (mapcar (lambda (x) (subseq x 4 12)) strings))))

(define-test map
  ;; MAP is a variant of MAPCAR that works on any sequences.
  ;; It allows to specify the type of the resulting sequence.
  (let ((string "lorem ipsum"))
    (assert-equal "LOREM IPSUM" (map 'string #'char-upcase string))
    (assert-equal '(#\L #\O #\R #\E #\M #\Space #\I #\P #\S #\U #\M)
                  (map 'list #'char-upcase string))
    ;; Not all vectors containing characters are strings.
    (assert-equalp #(#\L #\O #\R #\E #\M #\Space #\I #\P #\S #\U #\M)
                   (map '(vector t) #'char-upcase string))))

(define-test transposition
  ;; MAPCAR gives the function as many arguments as there are lists.
  (flet ((transpose (lists) (apply #'mapcar #'list lists)))
    (let ((list '((1 2 3)
                  (4 5 6)
                  (7 8 9)))
          (transposed-list '((1 4 7)
                             (2 5 8)
                             (3 6 9))))
      (assert-equal transposed-list (transpose list))
      (assert-equal list (transpose (transpose list))))
    (assert-equal '(("these" "pretzels" "are")
                    ("making" "me" "thirsty"))
                  (transpose '(("these" "making")
                               ("pretzels" "me")
                               ("are" "thirsty"))))))

(define-test reduce
  ;; The function REDUCE combines the elements of a list by applying a binary
  ;; function to the elements of a sequence from left to right.
  (assert-equal 15 (reduce #'+ '(1 2 3 4 5)))
  (assert-equal 10 (reduce #'+ '(1 2 3 4)))
  (assert-equal 1 (reduce #'expt '(1 2 3 4 5))))

(define-test reduce-from-end
  ;; The :FROM-END keyword argument can be used to reduce from right to left.
  (let ((numbers '(1 2 3 4 5)))
    (assert-equal '((((1 . 2) . 3) . 4) . 5) (reduce #'cons numbers))
    (assert-equal '(1 2 3 4 . 5) (reduce #'cons numbers :from-end t)))
  (let ((numbers '(2 3 2)))
    (assert-equal 64 (reduce #'expt numbers))
    (assert-equal 512 (reduce #'expt numbers :from-end t))))

(define-test reduce-initial-value
  ;; :INITIAL-VALUE can supply the initial value for the reduction.
  (let ((numbers '(1 2 3 4 5)))
    (assert-equal 120 (reduce #'* numbers))
    (assert-equal 0 (reduce #'* numbers :initial-value 0))
    (assert-equal -120 (reduce #'* numbers :initial-value -1))))

(define-test inner-product
  ;; MAPCAR and REDUCE are powerful when used together.
  ;; Fill in the blanks to produce a local function that computes an inner
  ;; product of two vectors.
  (flet ((inner-product (x y) (reduce #'+ (mapcar #'* x y))))
    (assert-equal 32 (inner-product '(1 2 3) '(4 5 6)))
    (assert-equal 310 (inner-product '(10 20 30) '(4 3 7)))))
