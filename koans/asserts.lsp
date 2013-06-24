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


; Concept: What do you do to go through the lisp koans?  You fill in
; the blanks, or otherwise fix the lisp code so that the
; code within the 'define-koan' blocks passes.


; In common lisp, "True" and "False" are represented by "t" and "nil".
; More in a future lesson, but for now, consider t to be true,
; and nil to be false.


(define-test assert-true
    "t is true.  Replace the blank with a t"
    (assert-true t))

(define-test assert-false
    "nil is false"
    (assert-false nil))

(define-test fill-in-the-blank
    "sometimes you will need to fill the blank to complete"
    (assert-equal 2 2))

(define-test fill-in-the-blank-string
    (assert-equal "hello world" "hello world"))

(define-test test-true-or-false
    "sometimes you will be asked to evaluate whether statements 
     are true (t) or false (nil)"
    (true-or-false? t (equal 34 34))
    (true-or-false? nil (equal 19 78)))


