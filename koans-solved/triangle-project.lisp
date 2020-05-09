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

(define-condition triangle-error (error)
  ;; Fill in the blank with a suitable slot definition.
  ((triangle-error-sides :reader triangle-error-sides :initarg :sides)))

(defun triangle (a b c)
  (check-type a (real (0)))
  (check-type b (real (0)))
  (check-type c (real (0)))
  ;; Fill in the blank with a function that satisfies the below tests.
  (let* ((min (min a b c))
         (max (max a b c))
         (mid (car (remove min (remove max (list a b c) :count 1) :count 1))))
    (cond ((<= (+ min mid) max) (error 'triangle-error :sides (list a b c)))
          ((= max mid min) :equilateral)
          ((= max mid) :isosceles)
          (t :scalene))))

(define-test equilateral-triangles
  ;; Equilateral triangles have three sides of equal length,
  (assert-equal :equilateral (triangle 2 2 2))
  (assert-equal :equilateral (triangle 10 10 10)))

(define-test isosceles-triangles
  ;; Isosceles triangles have two sides of equal length,
  (assert-equal :isosceles (triangle 3 4 4))
  (assert-equal :isosceles (triangle 4 3 4))
  (assert-equal :isosceles (triangle 4 4 3))
  (assert-equal :isosceles (triangle 10 10 2)))

(define-test scalene-triangles
  ;; Scalene triangles have three sides of different lengths.
  (assert-equal :scalene (triangle 3 4 5))
  (assert-equal :scalene (triangle 10 11 12))
  (assert-equal :scalene (triangle 5 4 2)))

(define-test illegal-triangles
  ;; Not all triplets make valid triangles.
  (flet ((triangle-failure (a b c)
           (handler-case (progn (triangle a b c) (error "Test failure"))
             (error (condition) condition))))
    (let ((condition (triangle-failure 0 0 0)))
      (assert-true (typep condition 'type-error))
      (assert-equal 0 (type-error-datum condition))
      ;; The type (REAL (0)) represents all positive numbers.
      (assert-true (subtypep (type-error-expected-type condition) '(real (0))))
      ;; If two type specifiers are SUBTYPEP of one another, then they represent
      ;; the same Lisp type.
      (assert-true (subtypep '(real (0)) (type-error-expected-type condition))))
    (let ((condition (triangle-failure 3 4 -5)))
      (assert-true (typep condition 'type-error))
      (assert-equal -5 (type-error-datum condition))
      (assert-true (subtypep (type-error-expected-type condition) '(real (0))))
      (assert-true (subtypep '(real (0)) (type-error-expected-type condition))))
    (let ((condition (triangle-failure 1 1 3)))
      (assert-true (typep condition 'triangle-error))
      (assert-equal '(1 1 3) (triangle-error-sides condition)))
    (let ((condition (triangle-failure 2 4 2)))
      (assert-true (typep condition 'triangle-error))
      (assert-equal '(2 4 2) (triangle-error-sides condition)))))
