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

(define-test if
  ;; IF only evaluates and returns one branch of a conditional expression.
  (assert-equal :true (if t :true :false))
  (assert-equal :false (if nil :true :false))
  ;; This also applies to side effects that migh or might not be evaluated.
  (let ((result))
    (if t
        (setf result :true)
        (setf result :false))
    (assert-equal :true result)
    (if nil
        (setf result :true)
        (setf result :false))
    (assert-equal :false result)))

(define-test when-unless
  ;; WHEN and UNLESS are like one-branched IF statements.
  (let ((when-result nil)
        (when-numbers '())
        (unless-result nil)
        (unless-numbers '()))
    (dolist (x '(1 2 3 4 5 6 7 8 9 10))
      (when (> x 5)
        (setf when-result x)
        (push x when-numbers))
      (unless (> x 5)
        (setf unless-result x)
        (push x unless-numbers)))
    (assert-equal 10 when-result)
    (assert-equal '(10 9 8 7 6) when-numbers)
    (assert-equal 5 unless-result)
    (assert-equal '(5 4 3 2 1) unless-numbers)))

(define-test and-short-circuit
  ;; AND only evaluates forms until one evaluates to NIL.
  (assert-equal 5
                (let ((x 0))
                  (and
                   (setf x (+ 2 x))
                   (setf x (+ 3 x))
                   nil
                   (setf x (+ 4 x)))
                  x)))

(define-test or-short-circuit
  ;; OR only evaluates forms until one evaluates to non-NIL.
  (assert-equal 2
                (let ((x 0))
                  (or
                   (setf x (+ 2 x))
                   (setf x (+ 3 x))
                   nil
                   (setf x (+ 4 x)))
                  x)))
