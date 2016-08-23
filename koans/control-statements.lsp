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

(define-test test-if-then-else
    (let ((result))
      (if t
          (setf result "true value")
          (setf result "false value"))
      (assert-equal result "true value")
      (if nil
          (setf result "true value")
          (setf result "false value"))
      (assert-equal result "false value")))


(define-test test-when-and-unless
    (let ((result-1 nil)
          (result-2 nil)
          (when-nums nil)
          (unless-nums nil))
      (dolist (x '(1 2 3 4 5 6 7 8 9 10))
        (when (> x 5)
          (setf result-1 x)
          (push x when-nums))
        (unless (> x 5)
          (setf result-2 x)
          (push x unless-nums)))
      (assert-equal result-1 10)
      (assert-equal result-2 5)
      (assert-equal when-nums '(10 9 8 7 6))
      (assert-equal unless-nums '(5 4 3 2 1))))


(define-test test-and-short-circuits
    "and only evaluates forms until one evaluates to nil"
  (assert-equal
   2
   (let ((x 0))
     (and
      (setf x (+ 1 x))
      (setf x (+ 1 x))
      nil ;; <- ends execution of and.
      (setf x (+ 1 x)))
     x)))


(define-test test-or-also-short-circuits
    "or only evaluates until one argument evaluates to non-nil"
  (assert-equal
   1
   (let ((x 0))
     (or
      (setf x (+ 1 x))
      (setf x (+ 1 x))
      nil
      (setf x (+ 1 x)))
     x)))
