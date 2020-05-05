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
      (assert-equal result ____)
      (if nil
          (setf result "true value")
          (setf result "false value"))
      (assert-equal result ____)))


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
      (assert-equal result-1 ___)
      (assert-equal result-2 ___)
      (assert-equal when-nums ___)
      (assert-equal unless-nums ___)))


(define-test test-and-short-circuits
    "and only evaluates forms until one evaluates to nil"
  (assert-equal
   ____
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
   ____
   (let ((x 0))
     (or
      (setf x (+ 1 x))
      (setf x (+ 1 x))
      nil
      (setf x (+ 1 x)))
     x)))