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

(define-test shadowing
  (assert-equal '(4 2) (let ((z 4)) (list z (let ((z 2)) z)))))

(defun block-1 ()
  (block here
    (return-from here 4)
    5))

(defun block-2 ()
  (block outer
    (block inner
      (return-from outer 'space)
      (return-from inner 'tube))
    (return-from outer 'valve)))

(define-test block-return-from
  (assert-equal 4 (block-1))
  (assert-equal 'space (block-2)))

;;; See http://www.gigamonkeys.com/book/variables.html

(define-test lexical-variables-can-be-enclosed
  (assert-equal 10 (let ((f (let ((x 10))
                              (lambda () x))))
                     (let ((x 20))
                       (funcall f)))))

(define-test dynamic-variables-are-affected-by-execution-path
  (assert-equal 20 (let ((f (let ((x 10))
                              (declare (special x))
                              (lambda () x))))
                     (let ((x 20))
                       (declare (special x))
                       (funcall f)))))
